;;; Wave data handling utilities

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


(define (wave-concat waves)
  (let ((first-wave (car waves)))
    (if (<= (length waves) 1)
        first-wave
        (wave.append first-wave (wave-concat (cdr waves))))))

(define (wave-subwave wave from to)
  (let ((ifile (make_tmp_filename))
        (ofile (string-append (make_tmp_filename) ".sph")))
    (unwind-protect*
        (begin
          (wave.save wave ifile 'nist nil)
          (system (format nil "sox %s %s trim %s %s"
                          ifile ofile from (- to from)))
          (wave.load ofile nil nil nil))
      (delete-file ifile)
      (delete-file ofile))))

(define (wave-utt wave)
  (let ((utt (Utterance Wave nil)))
    (utt.relation.create utt 'Wave)
    (item.set_feat (utt.relation.append utt 'Wave nil) 'wave wave)
    utt))


(provide 'wave)
