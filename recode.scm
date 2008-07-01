;;; Character set conversions

;; Copyright (C) 2004, 2005, 2006, 2008 Brailcom, o.p.s.

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


(require 'fileio)


(define (recode string from to)
  (if (string-equal string "")
      string
      (with-temp-file-data (tmpfile string)
        (with-temp-file out-tmpfile
          (system (format nil "iconv -c -f %s -t %s//TRANSLIT -o %s %s" from to out-tmpfile tmpfile))
          (read-file out-tmpfile)))))

(defvar recode-special-utf8-translations
  '(("​" " ")))

(define (recode-utf8->current string)
  (let ((translations recode-special-utf8-translations))
    (while translations
      (let ((translation (car translations)))
        (set! string (string-replace string (car translation) (cadr translation))))
      (set! translations (cdr translations))))
  (let ((coding (current-voice-coding)))
    (if (eq? coding 'utf-8)
        string
        (recode string 'utf-8 coding))))


(provide 'recode)
