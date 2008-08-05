;;; Speech Dispatcher interface

;; Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008 Brailcom, o.p.s.

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


(require 'cap-signalization)
(require 'events)
(require 'multiwave)
(require 'prosody-param)
(require 'punctuation)
(require 'spell-mode)
(require 'ssml-mode)
(require 'util)
(require 'voice-select)


(defvar speechd-base-pitch nil)
(defvar speechd-spelling nil)


(define (speechd-set-lang-voice lang voice)
  (let* ((voice* (downcase voice))
         (name (or voice t))
         (gender (cond ((string-matches voice* ".*female.*") 'female)
                       ((string-matches voice* ".*male.*") 'male)
                       (t t)))
         (age (if (string-matches voice* "child.*") 8 40))
         (variant (if (string-matches voice* ".*[0-9]")
                      (substring voice* (- (length voice*) 1) 1)
                      t)))
    (if (eq? lang t)
        (begin
          (voice.select name)
          (set! voice-select-current-defaults
                (mapcar (lambda (item)
                          (if (eq? (car item) 'name)
                              (list 'name name)
                              item))
                        voice-select-defaults)))
        (select-voice* lang gender age variant name))
    (set! speechd-base-pitch (prosody-get-pitch))
    (restore-prosody)
    (current-voice-coding)))

(define (speechd-send-to-client wave)
  (if (or (symbol? wave) (not wave))
      wave
      (let ((file-type (Param.get 'Wavefiletype)))
        (Param.set 'Wavefiletype 'nist)
        (unwind-protect* (utt.send.wave.client wave)
          (Param.set 'Wavefiletype file-type)))))

(define (speechd-maybe-send-to-client wave)
  (unless speechd-multi-mode
    (speechd-send-to-client wave)))

(define (speechd-event-synth type value)
  ((if speechd-multi-mode multi-synth event-synth) type value))

(define (speechd-refresh-modes)
  (set-punctuation-mode punctuation-mode)
  (set-cap-signalization-mode cap-signalization-mode))

(define-wrapper (ssml-change-voice . args) speechd-ssml-change-voice
  (let ((result (apply (next-func) args)))
    (speechd-refresh-modes)
    result))


;;; Commands


(defvar speechd-multi-mode nil)
  
(define (speechd-enable-multi-mode mode)
  "(speechd-set-punctuation-mode MODE)
Enable (if MODE is non-nil) or disable (if MODE is nil) sending multiple
synthesized wave forms."
  (set! speechd-multi-mode mode))

(define (speechd-next*)
  (unless speechd-multi-mode
    (error "Not in multi mode"))
  (let ((wave (if speechd-spelling
                  (begin
                    (spell_init_func)
                    (unwind-protect
                      (prog1 (multi-next)
                        (spell_exit_func))
                      (spell_exit_func)))
                  (multi-next))))
    (cond
     ((symbol? wave) wave)
     (wave (wave-utt wave))
     (t nil))))
(define (speechd-next)
  "(speechd-next)
Return next synthesized wave form."
  (let ((utt (speechd-next*)))
    (when utt
      (speechd-send-to-client utt))))

(define (speechd-speak* text)
  (set! speechd-spelling nil)
  (speechd-event-synth 'text (recode-utf8->current text)))
(define (speechd-speak text)
  "(speechd-speak TEXT)
Speak TEXT."
  (speechd-maybe-send-to-client (speechd-speak* text)))

(define (speechd-speak-ssml* ssml-text)
  (set! speechd-spelling nil)
  (oo-ensure-function-wrapped 'ssml-change-voice)
  (speechd-event-synth 'ssml ssml-text))
(define (speechd-speak-ssml ssml-text)
  "(speechd-speak-ssml SSML-TEXT)
Speak SSML-TEXT."
  (speechd-maybe-send-to-client (speechd-speak-ssml* ssml-text)))

(define (speechd-spell* ssml-text)
  (oo-ensure-function-wrapped 'ssml-change-voice)
  (set! speechd-spelling t)
  (spell_init_func)
  (unwind-protect
      (prog1 (speechd-event-synth 'ssml ssml-text)
        (spell_exit_func))
    (spell_exit_func)))
(define (speechd-spell text)
  "(speechd-spell SSML-TEXT)
Spell SSML-TEXT."
  (speechd-maybe-send-to-client (speechd-spell* text)))

(define (speechd-sound-icon* name)
  (set! speechd-spelling nil)
  (speechd-event-synth 'logical name))
(define (speechd-sound-icon name)
  "(speechd-sound-icon NAME)
Play the sound or text bound to the sound icon named by the symbol NAME."
  (speechd-maybe-send-to-client (speechd-sound-icon* name)))

(define (speechd-character* character)
  (set! speechd-spelling nil)
  (speechd-event-synth 'character (recode-utf8->current character)))
(define (speechd-character character)
  "(speechd-character CHARACTER)
Speak CHARACTER, represented by a string."
  (speechd-maybe-send-to-client (speechd-character* character)))

(define (speechd-key* key)
  (set! speechd-spelling nil)
  (speechd-event-synth 'key (recode-utf8->current key)))
(define (speechd-key key)
  "(speechd-key KEY)
Speak KEY, represented by a string."
  (speechd-maybe-send-to-client (speechd-key* key)))

(define (speechd-set-language language)
  "(speechd-set-language language)
Set current language to LANGUAGE, where LANGUAGE is the language ISO code,
given as a two-letter string."
  (speechd-set-lang-voice language "male1"))

(define (speechd-set-punctuation-mode mode)
  "(speechd-set-punctuation-mode MODE)
Set punctuation mode to MODE, which is one of the symbols `all' (read all
punctuation characters), `none' (don't read any punctuation characters) or
`some' (default reading of punctuation characters)."
  (if (eq? mode 'some)
      (set! mode 'default))
  (set-punctuation-mode mode))

(define (speechd-set-voice voice)
  "(speechd-set-voice VOICE)
Set voice, which is one of the Speech Dispatcher voice strings."
  (speechd-set-lang-voice nil voice))

(define (speechd-set-festival-voice name)
  (speechd-set-lang-voice t name))

(define (speechd-set-rate rate)
  "(speechd-set-rate RATE)
Set speech RATE, which must be a number in the range -100..100."
  ;; Stretch the rate to the interval 0.5..2 in such a way, that:
  ;; f(-100) = 0.5 ; f(0) = 1 ; f(100) = 2
  (change-prosody set-rate (pow 2 (/ rate 100.0))))

(define (speechd-set-pitch pitch)
  "(speechd-set-pitch PITCH)
Set speech PITCH, which must be a number in the range -100..100."
  ;; Stretch the pitch to the interval 0.5*P..2*P, where P is the default pitch
  ;; of the voice, in such a way, that:
  ;; f(-100) = 0.5*P ; f(0) = P ; f(100) = 2*P
  (unless speechd-base-pitch
    (set! speechd-base-pitch (prosody-get-pitch)))
  (let ((relative-pitch (pow 2 (/ pitch 100.0))))
    (change-prosody set-pitch (* relative-pitch speechd-base-pitch))))

(define (speechd-set-capital-character-recognition-mode mode)
  "(speechd-set-capital-character-recognition-mode MODE)
Enable (if MODE is non-nil) or disable (if MODE is nil) capital character
recognition mode."
  (set-cap-signalization-mode mode))

(define (speechd-list-voices)
  "(speechd-list-voices)
Return the list of the voice names (represented by strings) available for the
current language."
  (current-language-voices))
