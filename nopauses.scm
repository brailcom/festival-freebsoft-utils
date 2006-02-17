;;; Inhibition of initial pauses

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
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA.


(require 'oo)
(require 'util)


(defvar inhibit-initial-pauses nil
  "If non-nil, inhibit initial pauses in words.")

(define-wrapper (insert_initial_pause utt) nopauses
  (unless inhibit-initial-pauses
    ((next-func) utt)))

(define (nopauses-insert-initial-pause utt)
  (when inhibit-initial-pauses
    ((oo-unwrapped 'insert_initial_pause) utt)
    (item.set_feat (utt.relation.first utt 'Segment) 'end 0.0))
  utt)

(add-hook after_analysis_hooks nopauses-insert-initial-pause)


(provide 'pause-util)
