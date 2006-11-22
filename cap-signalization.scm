;;; Capital character signalization

;; Copyright (C) 2003, 2004, 2006 Brailcom, o.p.s.

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
(require 'oo)


(defvar cap-signalization-mode nil)

(define-wrapper (token_to_words token name) cap-signalization
  (if cap-signalization-mode
      (let ((ttw* (lambda (token name)
                    (append (if (string-matches name "^[A-Z¡»œ…ÃÕ“”ÿ©´⁄Ÿ›Æ].*")
                                ;; token_to_words allows only strings and
                                ;; symbols as feature values, so we have to use
                                ;; capital-event, to be transformed later
                                (list '((name "") (capital-event "")
                                        (event-stick-to next))))
                            ((next-func) token name)))))
        (if (string-matches name "^..*[A-Z¡»œ…ÃÕ“”ÿ©´⁄Ÿ›Æ].*")
            (let ((i 1))
              (while (not (string-matches (substring name i 1)
                                          "^[A-Z¡»œ…ÃÕ“”ÿ©´⁄Ÿ›Æ].*"))
                     (set! i (+ i 1)))
              (append (ttw* token (substring name 0 i))
                      (token_to_words
                       token
                       (substring name i (- (length name) i))
                       (next-func))))
            (ttw* token name)))
      ((next-func) token name)))

(Param.wrap Token_Method cap-signalization
  (lambda (utt)
    (apply* (next-value) (list utt))
    (do-relation-items (w utt Word)
      (if (item.has_feat w 'capital-event)
          (add-event utt w '(logical capital) nil)))
    utt))

(define (set-cap-signalization-mode mode)
  "(set-cap-signalization-mode MODE)
Enable or disable capital letter signalization mode.
If MODE is non-nil, enable the mode, otherwise disable it."
  (oo-ensure-function-wrapped 'token_to_words)
  (set! cap-signalization-mode mode))


(provide 'cap-signalization)
