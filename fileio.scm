;;; File input/output utilities

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


(defmac (with-open-file form)
  (let* ((spec (nth 1 form))
         (body (nth_cdr 2 form))
         (var (nth 0 spec))
         (filename (nth 1 spec))
         (how (or (nth 2 spec) "r")))
    `(let ((,var (fopen ,filename ,how)))
       (unwind-protect* (begin ,@body)
         (fclose ,var)))))
       
(defmac (with-temp-file-data form)
  (let* ((spec (nth 1 form))
         (body (nth_cdr 2 form))
         (filename (nth 0 spec))
         (data (nth 1 spec)))
    `(with-temp-file ,filename
       (write-file ,filename ,data)
       ,@body)))

(define (write-file filename string)
  (with-open-file (f filename "w")
    (fwrite (if (symbol? string) (format nil "%s" string) string) f)))

(define (read-file filename)
  (with-open-file (f filename)
    (let* ((strings '())
           (buffer (format nil "%1024s" ""))
           (buflen (length buffer))
           (n 0)
           (reading t))
      (while reading
        (set! n (fread buffer f))
        (if n
            (begin
              (push (substring buffer 0 n) strings)
              (when (< n buflen)
                (set! reading nil)))
            (set! reading nil)))
      (apply string-append (reverse strings)))))

(define (make-read-line-state)
  (list ""))

(define (read-line file state)
  (let* ((text (car state))
         (line (and text (string-before text "\n"))))
    (cond
     ((not text)
      nil)
     ((equal? line "")
      (let* ((buffer (format nil "%256s" ""))
             (n (fread buffer file)))
        (cond
         ((and (not n) (eqv? text ""))
          (set! line nil)
          (set! text nil))
         ((not n)
          (set! line text)
          (set! text nil))
         (t
          (set! text (string-append text (substring buffer 0 n)))
          (let ((state* (list text)))
            (set! line (read-line file state*))
            (set! text (car state*)))))))
     (t
      (set! text (string-after text "\n"))))
    (set-car! state text)
    line))


(provide 'fileio)
