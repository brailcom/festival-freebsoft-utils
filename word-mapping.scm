;;; Mapping words to events

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
                 (not (item.has_feat w 'event)))
            (begin
              (item.set_name w "")
              (item.set_feat w 'event event)))))
    (apply* (next-value) (list utt))))


(provide 'word-mapping)
