;;; Selecting voices

;; Copyright (C) 2004, 2006, 2007 Brailcom, o.p.s.

;; Author: Milan Zamazal <pdm@brailcom.org>

;; COPYRIGHT NOTICE

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA.


(require 'util)


;;; User configuration


(defvar language-codes
  '((en english
        (US american)
        (BR british))
    (cs czech)
    (de german)
    (es spanish)
    (fi finnish)
    (fr french)
    (hi hindi)
    (it italian)
    (mr marathi)
    (te telugu))
  "Alist mapping ISO language codes to Festival language names.
Each element of the alist is of the form (LANGUAGE-CODE LANGUAGE-NAME).
Optionally, elements can have an extended
form (LANGUAGE-CODE LANGUAGE-NAME . DIALECTS), where DIALECTS is a list of
pairs (DIALECT-CODE DIALECT-NAME).")

(defvar voice-select-defaults
  '((language nil)
    (dialect nil)
    (gender nil)
    (age nil)
    (variant nil)
    (name nil))
  "Alist of default voice parameters.
Each entry is of the form (NAME VALUE), where VALUE can be either the actual
parameter value, or `nil' meaning the value is unspecified.")


;;; Internal functions and variables


(define (voice-select-code-language code)
  (let* ((sep (if (string-matches code ".*_.*") "_" "-"))
         (lang-code (string-before code sep))
         (dialect-code (string-after code sep)))
    (when (string-equal lang-code "")
      (set! lang-code code)
      (set! dialect-code nil))
    (let* ((spec (cdr (assoc_string lang-code language-codes)))
           (language (car spec))
           (dialect (avalue-get dialect-code (cdr spec))))
      (list language dialect))))

(define (voice-property voice-name property)
  (avalue-get property (cadr (voice.description voice-name))))

(define (voice-select-parameter name value)
  (cond
   ((eq? value t)
    nil)
   ((eq? value nil)
    (avalue-get name voice-select-current-defaults))
   (t
    value)))

(define (voice-select-subset pname pvalue voices)
  (if pvalue
      (remove-if (lambda (voice-name)
                   (not (string-equal (voice-property voice-name pname)
                                      pvalue)))
                 voices)
      voices))

(define (select-voice-internal voices dialect gender age variant name)
  (cond
   ;; No voice for the given language at all
   ((not voices)
    (string-after voice_default "voice_"))
   ;; Name has the highest priority
   ((member name voices)
    name)
   ;; Let's try the best match
   (t
    (let* ((dialect-voices (voice-select-subset 'dialect dialect voices))
           (gender-voices (voice-select-subset 'gender gender voices))
           (matching-voices (remove-if
                             (lambda (voice-name)
                               (not (member voice-name gender-voices)))
                             dialect-voices))
           (variant* (read-from-string variant))
           (choose-variant (lambda (voices)
                             (or (and variant* (nth (- variant* 1) voices))
                                 (first voices)))))
      ;; Select variant from what remained (age is ignored)
      (choose-variant (or matching-voices
                          ;; Dialect is preferred over gender
                          dialect-voices
                          gender-voices
                          voices))))))


;;; External functions and variables


(defvar voice-select-current-defaults voice-select-defaults)


(define (voice-list)
  (let ((voices-1 (voice.list))
        (voices-2 (mapcar car Voice_descriptions))
        (voices '()))
    (while voices-2
      (unless (member (car voices-2) voices-1)
        (push (car voices-2) voices))
      (set! voices-2 (cdr voices-2)))
    (append voices-1 (reverse voices))))

(define (voice-list-language-codes)
  (let ((r-language-codes (mapcar (lambda (entry)
                                    (cons (second entry)
                                          (cons (first entry)
                                                (nth_cdr 2 entry))))
                                  language-codes)))
    (mapcar (lambda (voice)
              (let* ((language (voice-property voice 'language))
                     (dialect (voice-property voice 'dialect))
                     (language-entry (assoc language r-language-codes))
                     (language-code (second language-entry))
                     (dialects (mapcar (lambda (entry)
                                         (list (second entry) (first entry)))
                                       (nth_cdr 2 language-entry)))
                     (dialect-code (second (assoc dialect dialects))))
                (list voice language-code dialect-code)))
            (voice.list))))

(define (select-voice language dialect gender age variant name)
  (let* ((language* (voice-select-parameter 'language language))
         (dialect* (voice-select-parameter 'dialect dialect))
         (gender* (voice-select-parameter 'gender gender))
         (age* (voice-select-parameter 'age age))
         (variant* (voice-select-parameter 'variant variant))
         (name* (voice-select-parameter 'name name))
         (voices (voice-select-subset 'language language* (voice-list)))
         (voice-name (select-voice-internal
                      voices dialect* gender* age* variant* name*)))
    (voice.select voice-name)
    (set! voice-select-current-defaults
          `((language ,language*)
            (dialect ,dialect*)
            (gender ,gender*)
            (age ,age*)
            (variant ,variant*)
            (name ,name*)))
    voice-name))

(define (select-voice* lang-code gender age variant name)
  (let ((language-dialect (if lang-code
                              (voice-select-code-language lang-code)
                              '(nil nil))))
    (select-voice (first language-dialect) (second language-dialect)
                  gender age variant name)))

(define (reset-voice)
  (set! voice-select-current-defaults voice-select-defaults)
  (select-voice nil nil nil nil nil nil))

(define (current-language-voices)
  (voice-select-subset
   'dialect (avalue-get 'dialect voice-select-current-defaults)
   (voice-select-subset
    'language (avalue-get 'language voice-select-current-defaults)
    (voice-list))))


(provide 'voice-select)
