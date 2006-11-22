;;; Tokenization and utterance chunking

;; Copyright (C) 2004, 2005, 2006 Brailcom, o.p.s.

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


(require 'tts)

(require 'spell-mode)
(require 'util)


(defvar max-number-of-tokens 100)

(defvar max-number-of-token-chars 100)


(define (token-utterance)
  (let ((utt (Utterance Tokens)))
    (utt.relation.create utt 'Token)))

(define (token-utterance-append utt name whitespace punc prepunc)
  (token-utterance-append* utt name whitespace punc prepunc 'Token))

(define (token-utterance-append* utt name whitespace punc prepunc relation)
  (utt.relation.append utt relation
                       `(,name
                         ((name ,name) (whitespace ,whitespace)
                          (punc ,punc) (prepunctuation ,prepunc)))))

(define (get-regular-token utt text)
  (let* ((l-whitespace (symbolexplode token.whitespace))
         (l-punc (symbolexplode token.punctuation))
         (l-prepunctuation (symbolexplode token.prepunctuation))
         (r-whitespace (lambda (c) (member_string c l-whitespace)))
         (r-punc (lambda (c) (member_string c l-punc)))
         (r-prepunctuation (lambda (c) (member_string c l-prepunctuation)))
         (r-name (lambda (c)
                   (not (or (r-whitespace c) (r-punc c)))))
         (name "")
         (whitespace "")
         (punc "")
         (prepunctuation "")
         (oversized nil))
    (let ((read-part (lambda (matcher)
                       (let ((result-chars '())
                             (nchars 0)
                             (char (substring text 0 1)))
                         (while (and (not (string-equal char ""))
                                     (not oversized)
                                     (matcher char))
                           (set! result-chars (cons char result-chars))
                           (set! text (substring text 1 (- (length text) 1)))
                           (set! nchars (+ nchars 1))
                           (when (>= nchars max-number-of-token-chars)
                             (set! oversized t))
                           (set! char (substring text 0 1)))
                         (apply string-append (reverse result-chars))))))
      (set! whitespace (read-part r-whitespace))
      (set! prepunctuation (read-part r-prepunctuation))
      (set! name (read-part r-name))
      (set! punc (read-part r-punc))
      (while (and (not oversized)
                  (not (string-equal text ""))
                  (not (r-whitespace (substring text 0 1))))
        (set! name (string-append name punc (read-part r-name)))
        (set! punc (read-part r-punc))
        (when (>= (length name) max-number-of-token-chars)
          (set! oversized t)))
      ;; Presence of empty words is questionable and the built-in tokenization
      ;; process doesn't generate them
      (when (string-equal name "")
        (cond
         (oversized
          nil)
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
      (token-utterance-append utt name whitespace punc prepunctuation))
    text))

(define (get-single-char-token utt text)
  (let ((name (substring text 0 1)))
    (set! text (substring text 1 (length text)))
    (unless (string-equal name "")
      (token-utterance-append utt name "" "" "")))
  text)

(define (get-token utt text)
  ((if spell-mode get-single-char-token get-regular-token) utt text))

(define (next-chunk text)
  (let ((utt (token-utterance))
        (finished nil)
        (ntokens 1))
    (while (not finished)
      (let* ((new-text (get-token utt text))
             (token (utt.relation.last utt 'Token))
             (ptoken (item.prev token)))
        (cond
         ((string-equal new-text "")
          (set! text "")
          (set! finished t))
         ((and ptoken (string-equal (wagon_predict ptoken eou_tree) 1))
          (item.delete token)
          (set! finished t))
         ((>= ntokens max-number-of-tokens)
          ;; This could be handled (probably less efficiently) by eou_tree, but
          ;; we don't want to touch the tree just because of this
          (set! text new-text)
          (set! finished t))
         (t
          (set! text new-text)
          (set! ntokens (+ ntokens 1))))))
    (list utt text)))

(define (append-token-utterances . utts)
  (let ((u (token-utterance)))
    (while utts
      (let ((i (utt.relation (car utts) 'Token)))
        (while i
          (token-utterance-append u (item.name i)
                                  (item.feat i 'whitespace)
                                  (item.feat i 'punc)
                                  (item.feat i 'prepunctuation))
          (set! i (item.next i))))
      (set! utts (cdr utts))))
  u)

(provide 'tokenize)
