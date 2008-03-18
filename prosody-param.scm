;;; Changing prosody parameters

;; Copyright (C) 2004, 2006, 2008 Brailcom, o.p.s.

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


(defvar prosody-testing-word "babebibobub")


;;; Prosody parameter accessors


(define (prosody-get-pitch)
  ;; This is now easy thing, there are many intonation methods in Festival,
  ;; more methods can be added and the Int_Target_Method parameter can be
  ;; wrapped
  (cond
   ((prosody-general-method?)
    (or (avalue-get 'f0_mean int_general_params)
        (prosody-general-pitch)))
   ((prosody-lr-method?)
    (avalue-get 'target_f0_mean int_lr_params))
   ((prosody-simple-method?)
    (avalue-get 'f0_mean int_simple_params))))

(define (prosody-set-pitch value)
  (cond
   ((prosody-general-method?)
    (if (avalue-get 'f0_mean int_general_params)
        (set! int_general_params
              (assoc-set int_general_params 'f0_mean (list value)))
        (prosody-set-general-pitch value)))
   ((prosody-lr-method?)
    (set! int_lr_params
          (assoc-set int_lr_params 'target_f0_mean (list value))))
   ((prosody-simple-method?)
    (set! int_simple_params
          (assoc-set int_simple_params 'f0_mean (list value))))))

(define (prosody-get-pitch-range)
  (cond
   ((prosody-general-method?)
    (or (avalue-get 'f0_std int_general_params)
        (prosody-general-pitch-range)))
   ((prosody-lr-method?)
    (avalue-get 'target_f0_std int_lr_params))
   ((prosody-simple-method?)
    (avalue-get 'f0_mean int_simple_params))))

(define (prosody-set-pitch-range value)
  (cond
   ((prosody-general-method?)
    (if (avalue-get 'f0_std int_general_params)
        (set! int_general_params
              (assoc-set int_general_params 'f0_std (list value)))
        (prosody-set-general-pitch-range value)))
   ((prosody-lr-method?)
    (set! int_lr_params
          (assoc-set int_lr_params 'target_f0_std (list value))))
   ((prosody-simple-method?)
    (set! int_simple_params
          (assoc-set int_simple_params 'f0_std (list value))))))

(defvar prosody-volume 1)

(define (prosody-get-volume)
  prosody-volume)

(define (prosody-set-volume value)
  (add-hook after_synth_hooks prosody-adjust-volume t)
  (set! prosody-volume value))

(define (prosody-get-rate)
  (/ 1 (Param.get 'Duration_Stretch)))

(define (prosody-set-rate value)
  (Param.set 'Duration_Stretch (/ 1 value)))


;;; Internal utilities


(defvar prosody-parameters '())

(define (prosody-change-parameter value get-func set-func min max)
  (let* ((old-value (get-func))
         (new-value (if (eq? (typeof value) 'closure)
                        (value old-value)
                        value)))
    (cond
     ((> new-value max)
      (set! new-value max))
     ((< new-value min)
      (set! new-value min)))     
    (set-func new-value)
    old-value))

(define (prosody-adjust-volume utt)
  (utt.wave.rescale utt prosody-volume))


;;; Internal utilities -- general intonation method handling


(defvar prosody-voice-f0-alist '()) ; items: (VOICE PITCH RANGE)
(defvar prosody-voice-pitch-factor '()) ; (CURRENT-VOICE PITCH RANGE)

(defmac (define-prosody-method-test form)
  (let* ((method-name (nth 1 form))
         (method-function-name (intern (string-append "Int_Targets_" method-name)))
         (function-name (intern (string-append "prosody-" (downcase method-name) "-method?"))))
    `(define (,function-name)
       (let ((int-method (Param.get 'Int_Target_Method)))
         (or (eq? int-method ,method-function-name)
             (equal? int-method (quote ,method-function-name))
             (equal? (Param.get 'Int_Method) ,method-name))))))
(define-prosody-method-test "General")
(define-prosody-method-test "LR")
(define-prosody-method-test "Simple")

(define (prosody-general-base-f0)
  (or (second (assoc current-voice prosody-voice-f0-alist))
      (let ((orig-targ-func (avalue-get 'targ_func int_general_params))
            (pitch-list '()))
        (avalue-set! 'targ_func int_general_params
                     (lambda (utt syl)
                       (let ((result (orig-targ-func utt syl)))
                         (set! pitch-list (append pitch-list
                                                  (mapcar cadr result)))
                         result)))
        (unwind-protect*
            (SynthText prosody-testing-word)
          (avalue-set! 'targ_func int_general_params orig-targ-func))
        (set! pitch-list (or (butlast (cdr pitch-list)) '(100)))
        (let* ((n (length pitch-list))
               (pitch (/ (apply + pitch-list) n))
               (range (/ (apply + (mapcar (lambda (p) (abs (- p pitch))) pitch-list)) n)))
          (set! prosody-voice-f0-alist (assoc-set prosody-voice-f0-alist
                                                  current-voice (list pitch range)))
          pitch))))

(define (prosody-general-f0-range)
  (prosody-general-base-f0)
  (third (assoc current-voice prosody-voice-f0-alist)))

(define (prosody-general-pitch)
  (* (prosody-general-base-f0) (prosody-current-voice-pitch-factor)))

(define (prosody-general-pitch-range)
  (* (prosody-general-f0-range) (prosody-current-voice-pitch-range-factor)))

(define (prosody-current-voice-pitch-factor)
  (when (or (null? prosody-voice-pitch-factor)
            (not (equal? current-voice (first prosody-voice-pitch-factor))))
    (set! prosody-voice-pitch-factor (list current-voice 1 1)))
  (second prosody-voice-pitch-factor))

(define (prosody-current-voice-pitch-range-factor)
  (prosody-current-voice-pitch-factor) ; ensure the factor is defined
  (third prosody-voice-pitch-factor))

(define (prosody-ensure-targ-func-wrapped)
  (unless (assoc 'prosody-wrapper-enabled int_general_params)
    (prosody-general-base-f0)           ; store original base f0
    (let ((orig-func (avalue-get 'targ_func int_general_params)))
      (avalue-set! 'targ_func int_general_params
                   (lambda (utt syl)
                     (prosody-change-general-pitch utt syl orig-func)))
      (set! int_general_params (cons '(prosody-wrapper-enabled t)
                                     int_general_params)))))

(define (prosody-set-general-pitch freq)
  (prosody-ensure-targ-func-wrapped)
  (set! prosody-voice-pitch-factor
        (list current-voice
              (/ freq (prosody-general-base-f0))
              (or (third prosody-voice-pitch-factor) 1))))

(define (prosody-set-general-pitch-range range)
  (prosody-ensure-targ-func-wrapped)
  (set! prosody-voice-pitch-factor
        (list current-voice
              (or (second prosody-voice-pitch-factor) 1)
              (/ range (prosody-general-f0-range)))))

(define (prosody-change-general-pitch utt syl next-func)
  (let ((base-pitch (prosody-general-base-f0))
        (pitch-factor (prosody-current-voice-pitch-factor))
        (range-factor (prosody-current-voice-pitch-range-factor)))
    (mapcar (lambda (spec)
              (cons (first spec)
                    (cons (* pitch-factor (+ base-pitch (* range-factor (- (second spec) base-pitch))))
                          (cddr spec))))
            (next-func utt syl))))


;;; Exported functions


(define (prosody-shifted-value shift)
  (lambda (x) (+ shift x)))

(define (prosody-relative-value coef)
  (lambda (x) (* coef x)))

(define (set-pitch pitch)
  ;; Hz or a function
  (prosody-change-parameter pitch prosody-get-pitch prosody-set-pitch 50 500))

(define (set-pitch-range pitch-range)
  ;; mean-in-% or a function
  (prosody-change-parameter pitch-range prosody-get-pitch-range
                            prosody-set-pitch-range 0 100))

(define (set-volume volume)
  ;; 0..1 or a function
  (prosody-change-parameter volume prosody-get-volume prosody-set-volume 0 1))

(define (set-rate rate)
  ;; 0+..1..inf- or a function
  (prosody-change-parameter rate prosody-get-rate prosody-set-rate 0.1 10))

(define (change-prosody function param)
  (prog1 (function param)
    (set! prosody-parameters (assoc-set prosody-parameters function param))))

(define (restore-prosody)
  (let ((parameters prosody-parameters))
    (while parameters
      ((caar parameters) (cdar parameters))
      (set! parameters (cdr parameters)))))

(define (reset-prosody)
  (set! prosody-parameters '()))


(provide 'prosody-param)
