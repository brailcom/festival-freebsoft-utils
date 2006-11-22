;;; Mapping words to events

;; Copyright (C) 2004, 2006 Brailcom, o.p.s.

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


(defvar word-mapping
  '(("(" sound "guitar-13.wav")
    (")" sound "guitar-12.wav")
    ("@" sound "cembalo-6.wav"))
  "Alist mapping words to events.
Each element of the list is of the form (WORD EVENT-TYPE EVENT-VALUE), where
WORD is a string representing a word or a punctuation character and EVENT-TYPE
and EVENT-VALUE are the same as in `logical-event-mapping'.")

(define (word-mapping-of word)
  (cdr (assoc (item.name word) (langvar 'word-mapping))))

(Param.wrap Word_Method word-mapping
  (lambda (utt)
    (do-relation-items (w utt Word)
      (let ((event (word-mapping-of w)))
        (if (and event
                 (not (item-events utt w)))
            (begin
              (item.set_name w "")
              (add-event utt w event
                         (let ((tw (item.relation w 'Token)))
                           (if (and (not (item.prev tw))
                                    (item.next tw))
                               'next
                               'prev)))))))
    (apply* (next-value) (list utt))))


(provide 'word-mapping)
