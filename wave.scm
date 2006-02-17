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
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA.


(require 'util)


(define (wave-load filename)
  (let ((wave (and (not (string-matches filename ".*\.ogg"))
                   (wave.load filename))))
    (when (or (not wave) (<= (cadr (assoc 'num_samples (wave.info wave))) 0))
      (let ((wavfile (make-temp-filename "%s.wav")))
        (system (format nil "sox %s %s" filename wavfile))
        (unwind-protect* (set! wave (wave.load wavfile))
          (delete-file wavfile))))
    wave))

(define (wave-concat waves)
  (let ((first-wave (car waves)))
    (if (<= (length waves) 1)
        first-wave
        ;; The order of concatenation matters -- the resulting sample of
        ;; wave.append has parameters (rate etc.) of the first argument sample
        (wave-concat (cons (wave.append first-wave (cadr waves))
                           (cddr waves))))))

(define (wave-subwave wave from to)
  (let ((ifile (make_tmp_filename))
        (ofile (string-append (make-temp-filename "%s.sph"))))
    (unwind-protect*
        (let ((length (- to from)))
          (if (<= length 0)
              ;; sox doesn't trim if the length argument is 0
              (set! length 0.001))
          (wave.save wave ifile 'nist nil)
          (system (format nil "sox %s %s trim %s %s" ifile ofile from length))
          (wave.load ofile nil nil nil))
      (delete-file ifile)
      (delete-file ofile))))

(define (wave-utt wave)
  (let ((utt (Utterance Wave nil)))
    (utt.relation.create utt 'Wave)
    (item.set_feat (utt.relation.append utt 'Wave nil) 'wave wave)
    utt))

(define (wave-import-utt filename)
  (wave-utt (wave-load filename)))


(provide 'wave)
