;;; Punctuation modes

;; Copyright (C) 2003, 2004, 2005, 2006, 2008 Brailcom, o.p.s.

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


(require 'oo)
(require 'word-mapping)


(defvar punctuation-mode 'default)

(defvar punctuation-chars "[- !\"#$%&'()*+,./\\\\^_`:;<=>?@{|}~]")
(defvar punctuation-chars-2 "[][]")

(defvar punctuation-punc-languages '(english britishenglish americanenglish))

;; Default English voice doesn't have defined pronunciation of punctuation
;; characters
(defvar punctuation-pronunciation
  '(("." "dot")
    ("," "comma")
    (";" "semicolon")
    (":" "colon")
    ("!" "exclamation" "mark")
    ("?" "question" "mark")
    ("-" "dash")
    ("'" "right" "quote")
    ("`" "left" "quote")
    ("\"" "double" "quote")
    ("(" "left" "parenthesis")
    (")" "right" "parenthesis")
    ("{" "left" "brace")
    ("}" "right" "brace")))

(define (punctuation-character string)
  (or (string-matches string punctuation-chars)
      (string-matches string punctuation-chars-2)))

(define (punctuation-split-token token name ttw)
  ;; We must be careful not to discard whole token here.  It may contain
  ;; annotations such as index marks and they would be discarded together with
  ;; the token.
  (let ((words (punctuation-split-token* token name ttw)))
    (or words
        (ttw token (if (eq punctuation-mode 'all) "" " ")))))
(define (punctuation-split-token* token name ttw)
  (cond
   ;; No punctuation
   ((and (not (string-matches name
                              (string-append ".*" punctuation-chars ".*")))
         (not (string-matches name
                              (string-append ".*" punctuation-chars-2 ".*"))))
    (ttw token name))
   ;; Punctuation at start
   ((punctuation-character (substring name 0 1))
    (append (if (eq? punctuation-mode 'all)
                (ttw token (substring name 0 1)))
            (punctuation-split-token token
                                     (substring name 1 (- (length name) 1))
                                     ttw)))
   ;; Punctuation inside
   (t
    (let ((i 1))
      (while (not (punctuation-character (substring name i 1)))
        (set! i (+ i 1)))
      (append (ttw token (substring name 0 i))
              (punctuation-split-token
               token (substring name i (- (length name) i)) ttw))))))
             
(define-wrapper (token_to_words token name) punctuation
  (if (eq? punctuation-mode 'default)
      ((next-func) token name)
      (punctuation-split-token token name (next-func))))

(define (punctuation-process-words utt)
  (cond
   ((eq? punctuation-mode 'all)
    (if (member (intern (Param.get 'Language)) punctuation-punc-languages)
        ;; Standard English lexicon has no notion of punctuation pronounciation
        (do-relation-items (w utt Word)
          (let ((trans (assoc (item.name w) punctuation-pronunciation)))
            (if (and trans
                     (not (word-mapping-of w)))
                (begin
                  (item.set_name w (car (cdr trans)))
                  (set! trans (cdr (cdr trans)))
                  (while trans
                    (let ((i (item.insert w (list (car trans)))))
                      (item.append_daughter (item.parent (item.relation w 'Token))
                                            i))
                    (set! trans (cdr trans)))))))
        ;; We assume other languages don't insert punctuation words themselves
        (do-relation-items (w utt Word)
          (let* ((w* (item.relation w 'Token))
                 (token (item.parent w*)))
            (when (and (not (item.prev w*))
                       (item.has_feat token 'prepunctuation))
              (dolist (p (reverse (symbolexplode (item.feat token 'prepunctuation))))
                (let ((i (item.insert w `(,p ((name ,p))) 'before)))
                  (item.prepend_daughter token i))))
            (when (and (not (item.next w*))
                       (item.has_feat token 'punc))
              (dolist (p (reverse (symbolexplode (item.feat token 'punc))))
                (let ((i (item.insert w `(,p ((name ,p))))))
                  (item.append_daughter token i))))))))
   ;; Delete punctuation when punctuation-mode is none
   ;; (We actually don't delete the words as this might discard annotations
   ;; such as index marks.  So we just make the word names empty.)
   ((eq punctuation-mode 'none)
    (do-relation-top-items (token utt Token)
      (if (punctuation-character (item.name token))
          (dolist (w (mapcar (lambda (i) (item.relation i 'Word))
                             (item.daughters token)))
            (if w
                (item.set_feat w 'name "")))))
    (do-relation-top-items (w utt Word)
      (if (punctuation-character (item.name w))
          (item.set_feat w 'name "")))))
  utt)

(Param.wrap Token_Method punctuation
  (lambda (utt)
    (apply* (next-value) (list utt))
    (punctuation-process-words utt)))

(Param.wrap Word_Method punctuation
  ;; This is here to avoid deletion of punctuation in standard functions
  (lambda (utt)
    (if (eq? punctuation-mode 'all)
        (do-relation-items (w utt Word)
          (if (string-matches (item.feat w 'pos) "f?punc")
              (item.set_feat w 'pos 'allpunc))))
    (apply* (next-value) (list utt))))

(define (set-punctuation-mode mode)
  (or (member mode '(default all none))
      (error "Unknown punctuation mode" mode))
  (oo-ensure-function-wrapped 'token_to_words)
  (set! punctuation-mode mode))


(provide 'punctuation)
