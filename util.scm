;;; Miscellaneous utilities

;; Copyright (C) 2003, 2004 Brailcom, o.p.s.

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

(define (identity x)
  x)

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

(define (avg . args)
  (let ((n (length args)))
    (if (<= n 0)
        0
        (/ (apply + args) n))))

;;; Festival specific utilities

(define (item.has_feat item feat)
  (assoc feat (item.features item)))

(define (langvar symbol)
  (let ((lsymbol (intern (string-append symbol "." (Param.get 'Language)))))
    (symbol-value (if (symbol-bound? lsymbol) lsymbol symbol))))

(defmac (do-relation-items form)
  (let ((var (first (nth 1 form)))
        (utt (second (nth 1 form)))
        (relation (third (nth 1 form)))
        (body (nth_cdr 2 form)))
    `(dolist (,var (utt.relation.items ,utt (quote ,relation)))
       ,@body)))

                                     
(provide 'util)
