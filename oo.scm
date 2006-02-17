;;; Support of some object oriented features for Festival

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


;;; General wrapping mechanism


(define (oo-first-wrapper-func wrappers)
  (cdar wrappers))

(define (oo-wrapper-func wrappers name)
  (cdr (assoc name wrappers)))

(define (oo-next-wrapper-func wrappers name)
  (cond
   ((null? wrappers)
    nil)
   ((equal? (car (first wrappers)) name)
    (cdr (second wrappers)))
   (t
    (oo-next-wrapper-func (cdr wrappers) name))))
  
(define (oo-set-wrapper-func! wrappers-var name function)
  (let* ((wrappers (symbol-value wrappers-var))
         (spec (assoc name wrappers)))
    (if spec
        (set-cdr! spec function)
        (set-symbol-value!
         wrappers-var
         (cons (cons name function) (symbol-value wrappers-var))))
    nil))

(define (oo-wrapped-var funcname)
  (intern (string-append funcname "!shadow")))

(define (oo-wrappers-var funcname)
  (intern (string-append funcname "!wrappers")))

(define (oo-gen-wrapper wrappers-var)
  (lambda args
    (apply (oo-first-wrapper-func (symbol-value wrappers-var)) args)))

(define (oo-ensure-function-wrapped funcname)
  (let ((shadow (oo-wrapped-var funcname)))
    (when (or (not (boundp shadow))
              (not (eq (symbol-value funcname)
                       (symbol-value shadow))))
      (let ((wrapper (oo-gen-wrapper (oo-wrappers-var funcname)))
            (wrappers-var (oo-wrappers-var funcname)))
        (oo-set-wrapper-func! wrappers-var nil (symbol-value funcname))
        (set-symbol-value! funcname wrapper)
        (set-symbol-value! shadow wrapper)))))

(define (oo-unwrapped funcname)
  (let ((wrappers-var (oo-wrappers-var funcname)))
    (if (boundp wrappers-var)
        (cdar (last (symbol-value wrappers-var)))
        (symbol-value funcname))))

(defmac (define-wrapper form)
  ;; (define-wrapper (FUNCTION ARG ...) WRAPPER-NAME . BODY)
  (let ((func (car (cadr form)))
        (args (cdr (cadr form)))
        (name (car (cddr form)))
        (body (cdr (cddr form))))
    (let ((wrappers-var (oo-wrappers-var func)))
      (unless (boundp wrappers-var)
        (set-symbol-value! wrappers-var (list (cons nil (symbol-value func))))
        (oo-ensure-function-wrapped func))
      `(oo-set-wrapper-func! (quote ,wrappers-var) (quote ,name)
         (lambda ,args
           (let ((next-func (lambda ()
                              (oo-next-wrapper-func ,wrappers-var
                                                    (quote ,name)))))
             ,@body))))))


;;; Parameter wrapping


(define (oo-param-wrappers-var param-name)
  (when (consp param-name)
    (set! param-name (car param-name)))
  (intern (string-append param-name "!P!wrappers")))

(define oo-Param.get-wrapper-enabled t)

(define-wrapper (Param.get name) Param.get-wrapper
  (let ((wrappers-var (oo-param-wrappers-var name)))
    (if (and (boundp wrappers-var) oo-Param.get-wrapper-enabled)
        ((oo-first-wrapper-func (symbol-value wrappers-var)))
        ((next-func) name))))

(defmac (Param.wrap form)
  ;; (Param.wrap PARAM-NAME WRAPPER-NAME . BODY)
  (let* ((param-name (nth 1 form))
         (wrapper-name (nth 2 form))
         (body (nth_cdr 3 form))
         (wrappers-var (oo-param-wrappers-var param-name)))
    (unless (boundp wrappers-var)
      (set-symbol-value!
       wrappers-var
       (list (cons nil (lambda ()
                         (set! oo-Param.get-wrapper-enabled nil)
                         (unwind-protect*
                             (Param.get param-name)
                           (set! oo-Param.get-wrapper-enabled t)))))))
    `(oo-set-wrapper-func! (quote ,wrappers-var) (quote ,wrapper-name)
       (lambda ()
         (let ((next-value
                (lambda ()
                  ((oo-next-wrapper-func ,wrappers-var
                                         (quote ,wrapper-name))))))
           ,@body)))))


;;; Variables


(defvar oo-glet-stack '())

(define (oo-push-let-value var)
  (push (cons var (symbol-value var)) oo-glet-stack))

(define (oo-pop-let-value var)
  (cond
   ((null oo-glet-stack)
    (error "No restore value on the variable stack"))
   ((not (eq? (caar oo-glet-stack) var))
    (error "Variable stack mismatch"))
   (t
    (cdr (pop oo-glet-stack)))))

(defmac (glet* form)
  (let ((bindings (nth 1 form))
        (body (nth_cdr 2 form)))
    (if bindings
        (let* ((spec (car bindings))
               (var (car spec))
               (value (cadr spec)))
          `(begin
             (oo-push-let-value (quote ,var))
             (unwind-protect*
                 (begin
                   (set! ,var ,value)
                   (glet* ,(cdr bindings) ,@body))
               (set! ,var (oo-pop-let-value (quote ,var))))))
        `(begin ,@body))))

           
;;; Announce

(provide 'oo)
