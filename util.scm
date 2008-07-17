;;; Miscellaneous utilities

;; Copyright (C) 2003, 2004, 2005, 2006, 2008 Brailcom, o.p.s.

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


;;; Commonly used Lisp constructs

(defmac (when form)
  `(if ,(cadr form)
       (begin
         ,@(cddr form))))

(defmac (unless form)
  `(if (not ,(cadr form))
       (begin
         ,@(cddr form))))

(defmac (prog1 form)
  `(let ((result ,(cadr form)))
     ,@(cddr form)
     result))

(define (let*-bindings bindings body)
  (if bindings
      `(let (,(car bindings))
         ,(let*-bindings (cdr bindings) body))
      `(progn ,@body)))

(defmac (let* form)
  (let ((bindings (cadr form))
        (body (cddr form)))
    (let*-bindings bindings body)))
  
(defmac (unwind-protect* form)
  (let ((protected-form (nth 1 form))
        (cleanup-forms (nth_cdr 2 form)))
    `(unwind-protect
       (prog1 ,protected-form
         ,@cleanup-forms)
       (begin
         ,@cleanup-forms))))

(define (first list)
  (car list))

(define (second list)
  (cadr list))

(define (third list)
  (nth 2 list))

(define (fourth list)
  (nth 3 list))

(define (butlast list)
  (if (null? (cdr list))
      '()
      (cons (car list) (butlast (cdr list)))))

(define (min x y)
  (if (< x y) x y))

(define (max x y)
  (if (>= x y) x y))

(define (abs x)
  (if (>= x 0) x (- 0 x)))

(define (remove-if test list)
  (let ((result '()))
    (while list
      (let ((elt (car list)))
        (unless (test elt)
          (push elt result)))
      (set! list (cdr list)))
    (reverse result)))

(define (identity x)
  x)

(define (complement func)
  (lambda args (not (apply func args))))

(define (apply* function list)
  (apply (if (eq? (typeof function) 'string) (intern function) function) list))

(defmac (dolist form)
  (let ((var (first (nth 1 form)))
        (items (second (nth 1 form)))
        (body (nth_cdr 2 form)))
    `(mapcar (lambda (,var) ,@body) ,items)))

;;; General utilities

(defmac (add-hook form)
  (let ((hook-var (nth 1 form))
        (hook (nth 2 form))
        (to-end? (nth 3 form)))
    `(if (member ,hook ,hook-var)
         ,hook-var
         (set! ,hook-var (if ,to-end?
                             (append ,hook-var (list ,hook))
                             (cons ,hook ,hook-var))))))

(define (assoc-set lst key value)
  (cons (cons key value)
        (remove (assoc key lst) lst)))

(define (avalue-get key alist)
  (cadr (assoc_string key alist)))

(define (avalue-set! key alist value)
  (set-cdr! (assoc_string key alist) (list value))
  alist)

(define (avg . args)
  (let ((n (length args)))
    (if (<= n 0)
        0
        (/ (apply + args) n))))

(define (dirname path)
  (path-as-directory
   (substring path 0 (- (length path) (length (basename path))))))

;; There is a bug in speech-tools preventing it to honor $TMPDIR.
;; We can work around the bug by setting some other environment variables.
(let ((tempdir (getenv "TMPDIR")))
  (when (and tempdir (not (equal? tempdir "")))
    (setenv "TEMP" tempdir)
    (setenv "TMP" tempdir)))
(define (make-temp-filename template)
  (let ((tmp-name (string-append (make_tmp_filename) "%d")))
    (set! template (format nil "%s%s" (dirname tmp-name)
                           (format nil template (basename tmp-name))))
    (let* ((i 0)
           (format-name (lambda (i) (format nil template i)))
           (name (format-name i))
           (max-attempts 1000))
      (while (and (< i max-attempts)
                  (probe_file name))
        (set! i (+ i 1))
        (set! name (format-name i)))
      (if (eqv? i max-attempts)
          (error "Temporary file not created" nil))
      name)))

(defmac (with-temp-file form)
  (let ((filename (nth 1 form))
        (body (nth_cdr 2 form)))
    `(let ((,filename (make-temp-filename "ffu%s")))
       (unwind-protect* (begin ,@body)
         (delete-file ,filename)))))

(define (string-replace string from to)
  (string-replace* string from to ""))
(define (string-replace* string from to result)
  (let ((before (string-before string from)))
    (if (or (equal? before string)
            (and (equal? before "")
                 (not (equal? (substring string 0 (length from)) from))))
        (string-append result string)
        (string-replace* (string-after string from) from to (string-append result before to)))))

;;; Festival specific utilities

(define (item.has_feat item feat)
  (assoc feat (item.features item)))

(define (langvar symbol)
  (let ((lsymbol (intern (string-append symbol "." (Param.get 'Language)))))
    (symbol-value (if (symbol-bound? lsymbol) lsymbol symbol))))

(define (current-voice-coding)
  (or (cadr (assoc 'coding (cadr (voice.description current-voice))))
      'ISO-8859-1))

(define (utt-relation-top-items utt relation)
  (let ((items '())
        (i (utt.relation.first utt relation)))
    (while i
      (push i items)
      (set! i (item.next i)))
    (reverse items)))

(defmac (do-relation-items form)
  (let ((var (first (nth 1 form)))
        (utt (second (nth 1 form)))
        (relation (third (nth 1 form)))
        (body (nth_cdr 2 form)))
    `(dolist (,var (utt.relation.items ,utt (quote ,relation)))
       ,@body)))

(defmac (do-relation-top-items form)
  (let ((var (first (nth 1 form)))
        (utt (second (nth 1 form)))
        (relation (third (nth 1 form)))
        (body (nth_cdr 2 form)))
    `(dolist (,var (utt-relation-top-items ,utt (quote ,relation)))
       ,@body)))

(define (get-annotations utt item)
  (let ((annotation-list (item.feat item 'ffu-annotation)))
    (if (pair? annotation-list) annotation-list '())))

(define (add-annotation utt item annotation)
  (item.set_feat item 'ffu-annotation
                 (append (get-annotations utt item) (list annotation))))

(define (set-annotations utt item annotations)
  (item.set_feat item 'ffu-annotation annotations))


(provide 'util)
