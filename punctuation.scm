;;; Punctuation modes

;; Copyright (C) 2003, 2004, 2005 Brailcom, o.p.s.

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
(require 'word-mapping)


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
    ("}" "right" "brace")))

(define (punctuation-character string)
  (or (string-matches string punctuation-chars)
      (string-matches string punctuation-chars-2)))

(define (punctuation-split-token token name ttw)
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
   ;; Standard English lexicon has no notion of punctuation pronounciation
   ((and (eq? punctuation-mode 'all)
         (member (Param.get 'Language)
                 '(english britishenglish americanenglish
                   "english" "britishenglish" "americanenglish")))
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
                (set! trans (cdr trans))))))))
   ;; Delete punctuation when punctuation-mode is none
   ((eq punctuation-mode 'none)
    (do-relation-top-items (token utt Token)
      (if (punctuation-character (item.name token))
          (dolist (w (mapcar (lambda (i) (item.relation i 'Word))
                             (item.daughters token)))
            (if w
                (item.delete w)))))
    (do-relation-top-items (w utt Word)
      (if (punctuation-character (item.name w))
          (item.delete w)))))
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
