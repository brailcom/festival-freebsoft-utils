;;; Changing prosody parameters

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


;;; Prosody parameter accessors


(define (prosody-get-pitch)
  ;; This is now easy thing, there are many intonation methods in Festival,
  ;; more methods can be added and the Int_Target_Method parameter can be
  ;; wrapped
  (let ((int-method (Param.get 'Int_Target_Method)))
    (cond
     ((eq? int-method Int_Targets_General)
      (avalue-get 'f0_mean int_general_params))
     ((eq? int-method Int_Targets_LR)
      (avalue-get 'target_f0_mean int_lr_params))
     ((eq? int-method Int_Targets_Simple)
      (avalue-get 'f0_mean int_simple_params)))))

(define (prosody-set-pitch value)
  (let ((int-method (Param.get 'Int_Target_Method)))
    (cond
     ((eq? int-method Int_Targets_General)
      (set! int_general_params
            (assoc-set int_general_params 'f0_mean (list value))))
     ((eq? int-method Int_Targets_LR)
      (set! int_lr_params
            (assoc-set int_lr_params 'target_f0_mean (list value))))
     ((eq? int-method Int_Targets_Simple)
      (set! int_simple_params
            (assoc-set int_simple_params 'f0_mean (list value)))))))

(define (prosody-get-pitch-range)
  (let ((int-method (Param.get 'Int_Target_Method)))
    (cond
     ((eq? int-method Int_Targets_General)
      (avalue-get 'f0_std int_general_params))
     ((eq? int-method Int_Targets_LR)
      (avalue-get 'target_f0_std int_lr_params))
     ((eq? int-method Int_Targets_Simple)
      (avalue-get 'f0_mean int_simple_params)))))

(define (prosody-set-pitch-range value)
  (let ((int-method (Param.get 'Int_Target_Method)))
    (cond
     ((eq? int-method Int_Targets_General)
      (set! int_general_params
            (assoc-set int_general_params 'f0_std (list value))))
     ((eq? int-method Int_Targets_LR)
      (set! int_lr_params
            (assoc-set int_lr_params 'target_f0_std (list value))))
     ((eq? int-method Int_Targets_Simple)
      (set! int_simple_params
            (assoc-set int_simple_params 'f0_std (list value)))))))

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
