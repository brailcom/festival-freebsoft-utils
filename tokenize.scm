;;; Tokenization and utterance chunking

;; Copyright (C) 2004 Brailcom, o.p.s.

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


(require 'tts)

(require 'util)


(define (token-utterance)
  (let ((utt (Utterance Tokens)))
    (utt.relation.create utt 'Token)))

(define (get-token utt text)
  (let* ((l-whitespace (symbolexplode token.whitespace))
         (l-punc (symbolexplode token.punctuation))
         (l-prepunctuation (symbolexplode token.prepunctuation))
         (r-whitespace (lambda (c) (member_string c l-whitespace)))
         (r-punc (lambda (c) (member_string c l-punc)))
         (r-prepunctuation (lambda (c) (member_string c l-prepunctuation)))
         (r-name (lambda (c)
                   (not
                    (or (r-whitespace c) (r-punc c) (r-prepunctuation c)))))
         (name "")
         (whitespace "")
         (punc "")
         (prepunctuation ""))
    (let ((read-part (lambda (matcher)
                       (let ((result-chars '())
                             (char (substring text 0 1)))
                         (while (and (not (string-equal char ""))
                                     (matcher char))
                           (set! result-chars (cons char result-chars))
                           (set! text (substring text 1 (- (length text) 1)))
                           (set! char (substring text 0 1)))
                         (apply string-append (reverse result-chars))))))
      (set! whitespace (read-part r-whitespace))
      (set! prepunctuation (read-part r-prepunctuation))
      (set! name (read-part r-name))
      (set! punc (read-part r-punc))
      (while (and (not (string-equal text ""))
                  (not (r-whitespace (substring text 0 1))))
        (set! name (string-append name punc (read-part r-name)))
        (set! punc (read-part r-punc)))
      ;; Presence of empty words is questionable and the built-in tokenization
      ;; process doesn't generate them
      (when (string-equal name "")
        (cond
         ((not (string-equal punc ""))
          (set! name (substring punc 0 1))
          (set! punc (substring punc 1 (- (length punc) 1))))
         ((not (string-equal prepunctuation ""))
          (set! name (substring prepunctuation 0 1))
          (set! prepunctuation
                (substring prepunctuation 1 (- (length prepunctuation) 1))))
         (t ; final whitespace -- ignored
          (set! text "")))))
    (unless (string-equal name "")
      (utt.relation.append utt 'Token
                           `(,name
                             ((name ,name) (whitespace ,whitespace)
                              (punc ,punc) (prepunctuation ,prepunctuation)))))
    text))

(define (next-chunk text)
  (let ((utt (token-utterance))
        (finished nil))
    (while (not finished)
      (let* ((new-text (get-token utt text))
             (token (item.prev (utt.relation.last utt 'Token))))
        (cond
         ((string-equal new-text "")
          (set! text "")
          (set! finished t))
         ((and token (string-equal (wagon_predict token eou_tree) 1))
          (item.delete (item.next token))
          (set! finished t))
         (t
          (set! text new-text)))))
    (list utt text)))


(provide 'tokenize)
