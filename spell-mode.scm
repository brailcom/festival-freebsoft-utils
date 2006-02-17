;;; Spelling mode

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
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA.


(require 'util)


(defvar spell-orig-token.singlecharsymbols nil)

(defvar spell-orig-pos-method nil)

(defvar spell-mode nil)

(define (spell-pos utt)
  (do-relation-items (w utt Word)
    (item.set_feat w 'pos 'sym))
  utt)

(define (spell_init_func)
  (set! spell-orig-token.singlecharsymbols token.singlecharsymbols)
  (set! token.singlecharsymbols "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏĞÑÒÓÔÕÖ×ØÙÚÛÜİŞßàáâãäåæçèéêëìíîïğñòóôõö÷øùúûüış")
  (set! spell-orig-pos-method (Param.get 'POS_Method))
  (Param.set 'POS_Method spell-pos)
  (set! spell-mode t))

(define (spell_exit_func)
  (set! token.singlecharsymbols spell-orig-token.singlecharsymbols)
  (Param.set 'POS_Method spell-orig-pos-method)
  (set! spell-mode nil))

(set! tts_text_modes
      (cons
       (list
        'spell
        (list
         (list 'init_func spell_init_func)
         (list 'exit_func spell_exit_func)))
       tts_text_modes))

(provide 'spell-mode)
