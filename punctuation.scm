;;; Punctuation modes

;; Copyright (C) 2003, 2004 Brailcom, o.p.s.

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
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA.


(require 'oo)


(defvar punctuation-mode 'default)

(defvar punctuation-chars "[- !\"#$%&'()*+,./\\^_`:;<=>?@{|}~]")
(defvar punctuation-chars-2 "[][]")

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
    ("(" "right" "brace")))

(define (punctuation-character string)
  (or (string-matches string punctuation-chars)
      (string-matches string punctuation-chars-2)))

(define (punctuation-split-token token name ttw)
  (cond
   ((and (not (string-matches name
                              (string-append ".*" punctuation-chars ".*")))
         (not (string-matches name
                              (string-append ".*" punctuation-chars-2 ".*"))))
    (ttw token name))
   ((let ((char (substring name 0 1)))
      (punctuation-character char))
    (append (if (eq? punctuation-mode 'all)
                (let ((char (substring name 0 1)))
                  (if (and (member current-voice '(kal_diphone ked_diphone))
                           (assoc char punctuation-pronunciation))
                      (if (assoc char word-mapping)
                          (list char)
                          (cdr (assoc char punctuation-pronunciation)))
                      (ttw token char))))
            (punctuation-split-token token
                                     (substring name 1 (- (length name) 1))
                                     ttw)))
   (t
    (let ((i 1))
      (while (let ((char (substring name i 1)))
               (not (punctuation-character char)))
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
   ((and (eq? punctuation-mode 'all)
         (member (Param.get 'Language)
                 '(english britishenglish americanenglish
                   "english" "britishenglish" "americanenglish")))
    (mapcar
     (lambda (w)
       (let ((trans (assoc (item.name w) punctuation-pronunciation)))
         (if (and trans
                  (not (assoc (item.name w) word-mapping)))
             (begin
               (item.set_name w (car (cdr trans)))
               (set! trans (cdr (cdr trans)))
               (while trans
                 (item.insert w (list (car trans)))
                 (set! trans (cdr trans)))))))
     (utt.relation.items utt 'Word)))
   ((eq punctuation-mode 'none)
    (mapcar
     (lambda (w)
       (if (punctuation-character (item.name w))
           (item.delete w)))
     (utt.relation.items utt 'Word))))
  utt)

(define (punctuation-process-final-punctuation utt)
  (if (eq? punctuation-mode 'all)
      (mapcar
       (lambda (w)
         (let ((token (item.root (item.relation w 'Token))))
           (if (and (equal? (item.feat token 'punc) "0")
                    (or (not (item.next w))
                        (not (equal? token
                                     (item.root
                                      (item.relation (item.next w) 'Token))))))
               (begin
                 (item.insert w '(what-should-be-here? ((name "."))))
                 (item.append_daughter token (item.next w))))))
       (utt.relation.items utt 'Word)))
  utt)

(Param.wrap Token_Method punctuation
  (lambda (utt)
    (apply* (next-value) (list utt))
    (punctuation-process-final-punctuation utt)
    (punctuation-process-words utt)))

(Param.wrap Word_Method punctuation
  (lambda (utt)
    (if (eq? punctuation-mode 'all)
        (mapcar
         (lambda (w)
           (let ((pos (item.feat w "pos")))
             (if (or (string-equal "punc" pos)
                     (string-equal "fpunc" pos))
                 (item.set_feat w "pos" "allpunc"))))
         (utt.relation.items utt 'Word)))
    (apply* (next-value) (list utt))))

(define (set-punctuation-mode mode)
  (or (member mode '(default all none))
      (error "Unknown punctuation mode" mode))
  (oo-ensure-function-wrapped 'token_to_words)
  (set! punctuation-mode mode))


(provide 'punctuation)
