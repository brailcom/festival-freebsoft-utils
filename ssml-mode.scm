;;; Speech Synthesis Markup Language 1.0 support

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
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA.

;; speak: supported
;; lexicon: ignored (no reasonable specification of the element)
;; meta: ignored (no reasonable use here known)
;; metadata: ignored (no reasonable use here known)
;; p: supported
;; s: supported
;; say-as: trivially rendered (attribute values undefined in the standard)
;; phoneme: unsupported
;; sub: supported
;; voice: supported, but prosody not yet retained (TODO)
;; emphasis: not properly supported (TODO)
;; break: partially supported (TODO)
;; prosody: partially supported (TODO)
;; audio: supported (TODO more complicated alternative text may fail to render)
;; mark: supported
;; desc: ignored (no reasonable use here known)


(require_module 'rxp)

(require 'prosody-param)
(require 'recode)
(require 'tokenize)
(require 'util)
(require 'voice-select)


;;; User configurable features


(defvar ssml-break-values '((none     0.0)
                            (x-weak   0.01)
                            (weak     0.05)
                            (medium   0.1)
                            (strong   0.2)
                            (x-strong 0.5)))

(defvar ssml-pitch-values '((x-low    70)
                            (low     100)
                            (medium  130)
                            (high    170)
                            (x-high  220)
                            (default 130)))

(defvar ssml-range-values '((x-low     0)
                            (low       2)
                            (medium    5)
                            (high     10)
                            (x-high   20)
                            (default   5)))

(defvar ssml-rate-values '((x-slow  0.5)
                           (slow    0.7)
                           (medium  1.0)
                           (fast    1.5)
                           (x-fast  2.0)
                           (default 1.0)))

(defvar ssml-volume-values '((silent    0)
                             (x-soft  0.2)
                             (soft    0.4)
                             (medium  0.6)
                             (loud    0.8)
                             (x-loud  1.0)
                             (default 1.0)))


;;; Auxiliary functions


(defvar ssml-base-uri nil)

(defvar ssml-voices '())
(defvar ssml-voice-parameters '())
(defvar ssml-prosodies '())


(define (ssml-attval attlist att)
  (let ((attval (car (xxml_attval att attlist))))
    (if attval
        (recode-utf8->current attval))))

(define (ssml-attval-time attlist att)
  (let ((time (string-before (ssml-attval attlist 'time) "s")))
    (and time
         (not (equal? time ""))
         (let ((ms-time (string-before time "m")))
           (if (equal? ms-time "")
               (read-from-string time)
               (* (read-from-string ms-time) 0.001))))))

(define (ssml-val-complex value alist)
  (cond
   ((string-matches value "[a-z].*")
    (cadr (assoc_string value alist)))
   ((or (string-matches value "[0-9]+\\(\\.\\([0-9]+\\)?\\)?")
        (string-matches value "\\.[0-9]+"))
    (read-from-string value))
   (t
    (when (string-matches value "\\+.*")
      (set! value (string-after value "+")))
    (cond
     ((string-matches value ".*%$")
      (prosody-relative-value
       (+ 1 (* (read-from-string (string-before value "%")) 0.01))))
     ((string-matches value ".*st$")
      (prosody-relative-value
       (pow 2 (/ (read-from-string (string-before value "st")) 12))))
     ((string-matches value ".*Hz$")
      (prosody-shifted-value (read-from-string (string-before value "Hz"))))
     (t
      (prosody-shifted-value (read-from-string value)))))))

(define (ssml-attval-complex attlist att alist)
  (ssml-val-complex (ssml-attval attlist att) alist))

(define (ssml-change-voice lang-code gender age variant name)
  (push current-voice ssml-voices)
  (push voice-select-current-defaults ssml-voice-parameters)
  (select-voice* lang-code gender age variant name)
  (restore-prosody))

(define (ssml-unchange-voice)
  (voice.select (pop ssml-voices))
  (restore-prosody)
  (set! voice-select-current-defaults (pop ssml-voice-parameters)))

(define (ssml-change-language attlist)
  (ssml-change-voice (ssml-attval attlist 'xml:lang) nil nil nil nil))

(define ssml-unchange-language ssml-unchange-voice)

(defvar ssml-prosody-setters `((pitch ,set-pitch)
                               (range ,set-pitch-range)
                               (volume ,set-volume)
                               (rate ,set-rate)))

(define (ssml-set-prosody prosody)
  (let ((old-prosody '()))
    (while prosody
      (let ((id (first (car prosody)))
            (value (second (car prosody))))
        (push (list id (change-prosody (avalue-get id ssml-prosody-setters)
                                       value))
              old-prosody))
      (set! prosody (cdr prosody)))
    old-prosody))

(define (ssml-change-prosody prosody)
  (push (ssml-set-prosody prosody) ssml-prosodies))

(define (ssml-unchange-prosody)
  (ssml-set-prosody (pop ssml-prosodies)))

(define (ssml-get-url url tmpfile)
  (get_url (cond
            ((string-matches url "[a-z]+://.*") url)
            (ssml-base-uri (path-append ssml-base-uri url))
            (t (string-append "file:" url)))
           tmpfile))

(define (ssml-process-utt utt)
  (xxml_synth utt))

(define (ssml-process-sound file)
  (wave.play (wave-load file)))

(define (ssml-process-mark mark)
  (format t "Mark reached: %s\n" mark))

(define (ssml-utt-text utt)
  (let* ((last-token (and utt (utt.relation.last utt 'Token)))
         (token last-token)
         (token-list nil))
    (when last-token
      (while (and token (not (eq? (item.feat token 'ssml-tag) 'noticed)))
        (push (item.name token) token-list)
        (set! token (item.prev token)))
      (item.set_feat last-token 'ssml-tag 'noticed))
    (apply string-append token-list)))


;;; Markup handlers


(define (ssml.speak.start attlist utt)
  (set! ssml-base-uri (ssml-attval attlist 'xml:base))
  (ssml-change-language attlist)
  nil)
(define (ssml.speak.end attlist utt)
  (ssml-process-utt utt)
  (ssml-unchange-language)
  (set! ssml-base-uri nil)
  nil)

(define (ssml.lexicon attlist utt)
  (ssml-process-utt utt)
  nil)

(define (ssml.meta attlist utt)
  (ssml-process-utt utt)
  nil)

(define (ssml.metadata.start attlist utt)
  (ssml-process-utt utt)
  nil)
(define (ssml.metadata.end attlist utt)
  nil)

(define (ssml.p.start attlist utt)
  (ssml-process-utt utt)
  (ssml-change-language attlist)
  nil)
(define (ssml.p.end attlist utt)
  (ssml-process-utt utt)
  (ssml-unchange-language)
  nil)

(define (ssml.s.start attlist utt)
  (ssml-process-utt utt)
  (ssml-change-language attlist)
  nil)
(define (ssml.s.end attlist utt)
  (ssml-process-utt utt)
  (ssml-unchange-language)
  nil)

(define (ssml.say-as.start attlist utt)
  (ssml-process-utt utt)
  nil)
(define (ssml.say-as.end attlist utt)
  (ssml-process-utt utt)
  nil)

(define (ssml.phoneme.start attlist utt)
  (ssml-process-utt utt)
  nil)
(define (ssml.phoneme.end attlist utt)
  (ssml-process-utt utt)
  nil)

(define ssml-sub-utt nil)
(define (ssml.sub.start attlist utt)
  (let ((text (format nil "%s" (ssml-attval attlist 'alias))))
    (while (not (string-equal text ""))
      (set! text (get-token utt text))))
  (set! ssml-sub-utt utt)
  nil)
(define (ssml.sub.end attlist utt)
  ssml-sub-utt)

(define (ssml.voice.start attlist utt)
  (ssml-process-utt utt)
  (apply ssml-change-voice (mapcar (lambda (att) (ssml-attval attlist att))
                                   '(xml:lang gender age variant name)))
  nil)
(define (ssml.voice.end attlist utt)
  (ssml-process-utt utt)
  (ssml-unchange-voice)
  nil)

(define (ssml.emphasis.start attlist utt)
  (ssml-process-utt utt)
  (ssml-change-prosody `((pitch ,(prosody-relative-value 1.1))
                         (rate ,(prosody-relative-value 0.8))))
  nil)

(define (ssml.emphasis.end attlist utt)
  (ssml-process-utt utt)
  (ssml-unchange-prosody)
  nil)

(define (ssml.break attlist utt)
  ;; TODO: Process the assigned time breaks in pause and duration processing
  (let* ((strength (or (ssml-attval attlist 'strength) 'medium))
         (time (string-before (ssml-attval attlist 'time) "s"))
         (length (if (and time (not (equal? time "")))
                     (let ((ms-time (string-before time "m")))
                       (if (equal? ms-time "")
                           (read-from-string time)
                           (* (read-from-string ms-time) 0.001)))
                     (cadr (assoc_string strength ssml-break-values)))))
    (when length
      (let ((token (utt.relation.last utt 'Token)))
        (if token
            (begin
              (item.set_feat token 'pbreak "B")
              (item.set_feat token 'breaklen length))
            (utt.relation.append utt 'Token
                                 `(token ((name "") (whitespace " ") (punc "")
                                          (prepunctuation "") (pbreak "B")
                                          (breaklen ,length))))))))
  utt)

(define (ssml.prosody.start attlist utt)
  (ssml-process-utt utt)
  (let* ((pitch (ssml-attval-complex attlist 'pitch ssml-pitch-values))
         (contour (ssml-attval attlist 'contour))
         (range (ssml-attval-complex attlist 'range ssml-range-values))
         (rate (ssml-attval-complex attlist 'rate ssml-rate-values))
         (duration (ssml-attval-time attlist 'duration))
         (volume (ssml-attval-complex attlist 'volume ssml-volume-values))
         (prosody '())
         (set-param (lambda (id value) (push (list id value) prosody))))
    (when volume
      (set-param 'volume volume))
    (cond
     (duration
      ;; TODO: implement
      )
     (rate
      (set-param 'rate rate)))
    (if contour
        (let ((values '()))
          (while (not (string-equal contour ""))
            (let* ((pair (string-after (string-before contour ")") "("))
                   (time (string-before pair "%"))
                   (value (string-before (string-after pair ",") ")")))
              (push (cons time (ssml-val-complex value nil)) values))
            (set! contour (string-after contour ")")))
          ; TODO: implement
          ; (set-param 'contour (reverse values))
          )
        (begin
          (when pitch
            (set-param 'pitch pitch))
          (when range
            (set-param 'pitch-range range))))
    (ssml-change-prosody prosody))
  nil)
(define (ssml.prosody.end attlist utt)
  (ssml-process-utt utt)
  (ssml-unchange-prosody)
  nil)

(define ssml-audio-uri nil)
(define (ssml.audio.start attlist utt)
  (ssml-process-utt utt)
  (set! ssml-audio-uri (ssml-attval attlist 'src))
  nil)
(define (ssml.audio.end attlist utt)
  (let ((uri ssml-audio-uri)
        (tmpfile (make-temp-filename "ssml-audio-%s"))
        (played nil))
    (unwind-protect*
      (begin
        (ssml-get-url uri tmpfile)
        (ssml-process-sound tmpfile)
        (set! played t))
      (delete-file tmpfile))
    (when (and (not played) utt)
      (ssml-process-utt utt))
    nil))
(define (ssml.audio attlist utt)
  (ssml.audio.start attlist utt)
  (ssml.audio.end attlist nil))

(define (ssml.mark attlist utt)
  (ssml-process-utt utt)
  (ssml-process-mark (ssml-attval attlist 'name))
  nil)

(define (ssml.desc.start attlist utt)
  nil)
(define (ssml.desc.end attlist utt)
  nil)


;;; Setup


(defvar ssml-xxml-elements.orig nil)

(defvar ssml-tags '("speak" "lexicon" "meta" "metadata" "p" "s" "say-as"
                    "phoneme" "sub" "voice" "emphasis" "break" "prosody"
                    "audio" "mark" "desc"))

(defvar ssml-parsed nil)
(defvar ssml-utterances nil)
(defvar ssml-current-text nil)
(defvar ssml-current-utt nil)
(defvar ssml-in-volatile nil)

(defvar ssml-elements-parsing
  (apply
   append
   (mapcar (lambda (elt)
             (list
              `(,(format nil "(%s" elt) (ATTLIST UTT)
                (push (list (quote ,(intern (format nil "ssml.%s.start" elt)))
                            ATTLIST (ssml-utt-text UTT))
                      ssml-parsed)
                nil)
              `(,(format nil ")%s" elt) (ATTLIST UTT)
                (push (list (quote ,(intern (format nil "ssml.%s.end" elt)))
                            ATTLIST (ssml-utt-text UTT))
                      ssml-parsed)
                nil)
              `(,(format nil "%s" elt) (ATTLIST UTT)
                (push (list (quote ,(intern (format nil "ssml.%s" elt)))
                            ATTLIST (ssml-utt-text UTT))
                      ssml-parsed)
                nil)))
           ssml-tags)))

(defvar ssml-elements-speaking
  (apply
   append
   (mapcar (lambda (elt)
             (list
              (list
               (format nil "(%s" elt) '(ATTLIST UTT)
               (list (intern (format nil "ssml.%s.start" elt)) 'ATTLIST 'UTT))
              (list
               (format nil ")%s" elt) '(ATTLIST UTT)
               (list (intern (format nil "ssml.%s.end" elt)) 'ATTLIST 'UTT))
              (list
               (format nil "%s" elt) '(ATTLIST UTT)
               (list (intern (format nil "ssml.%s" elt)) 'ATTLIST 'UTT))))
           ssml-tags)))

(define ssml-elements ssml-elements-speaking)

(define (ssml_init_func)
  (set! ssml-xxml-elements.orig xxml_elements)
  (set! xxml_elements ssml-elements)
  (set! ssml-voices (list current-voice))
  ;; reset-voice used to be called here, but it's not much useful and it resets
  ;; current speechd settings, which is not what we want.
  )

(define (ssml_exit_func)
  (voice.select (car (last ssml-voices)))
  (set! xxml_elements ssml-xxml-elements.orig))

(set! tts_text_modes
   (cons
    (list
     'ssml
     (list
      (list 'init_func ssml_init_func)
      (list 'exit_func ssml_exit_func)
      '(analysis_type xml)
      ))
    tts_text_modes))


;;; Special functions


(define (ssml-say ssml-text)
  (with-temp-file ssml-file
    (let ((fd (fopen ssml-file "w")))
      (fwrite ssml-text fd)
      (fclose fd))
    (tts_file ssml-file 'ssml)))

(define (ssml-parse ssml-text)
  (set! ssml-parsed '())
  (set! ssml-utterances '())
  (set! ssml-current-text "")
  (set! ssml-current-utt nil)
  (set! ssml-in-volatile nil)
  (glet* ((ssml-elements ssml-elements-parsing)
          (token.singlecharsymbols "")
          (token.punctuation "")
          (token.prepunctuation "")
          (token.whitespace ""))
    (ssml-say ssml-text))
  (set! ssml-parsed (reverse ssml-parsed)))

(define (ssml-next-chunk)
  ;; It makes no sense to give ssml-parsed as an argument to this function,
  ;; since the function currently globally affects SSML prosody settings and so
  ;; it can't process more than one tree simultaneously.
  (cond
   ((and ssml-current-utt
         (not (equal? ssml-current-text ""))
         (not ssml-in-volatile))
    (let ((utt ssml-current-utt))
      (set! ssml-current-utt nil)
      (if (utt.relation.items utt 'Token)
          utt
          (ssml-next-chunk))))
   ((not (equal? ssml-current-text ""))
    (let* ((utt-text (next-chunk ssml-current-text))
           (utt (first utt-text))
           (text (second utt-text)))
      (set! ssml-current-text text)
      (if (utt.relation.items utt 'Token)
          utt
          (ssml-next-chunk))))
   (ssml-utterances
    (let ((utt (pop ssml-utterances)))
      (if (eq? (typeof utt) 'string)
          (set! utt (intern utt)))
      (if (or (symbol? utt) (utt.relation.items utt 'Token))
          utt
          (ssml-next-chunk))))
   (ssml-parsed
    (let* ((elt (pop ssml-parsed))
           (func-name (nth 0 elt))
           (attlist (nth 1 elt))
           (text (nth 2 elt))
           (utt (or ssml-current-utt
                    (set! ssml-current-utt (token-utterance)))))
      (glet* ((ssml-process-utt
               (lambda (utt)
                 (set! ssml-utterances (append ssml-utterances (list utt)))))
              (ssml-process-sound
               (lambda (file)
                 (set! ssml-utterances
                       (append ssml-utterances
                               (list (wave-import-utt file))))))
              (ssml-process-mark
               (lambda (mark)
                 (set! ssml-utterances (append ssml-utterances (list mark))))))
        ;; Texts of some marks should be processed in a single step
        (cond
         ((member func-name '(sub.start prosody.start audio.start))
          (push t ssml-in-volatile))
         ((member func-name '(sub.end prosody.end audio.end))
          (pop ssml-in-volatile)))
        ;; If there's a text, recode it and add it to the utterance
        (if text
            (begin
              (if (ssml-attval attlist 'xml:lang)
                  (begin
                    (ssml-change-language attlist)
                    (set! text (recode-utf8->current text))
                    (ssml-unchange-language)))
              (if (or ssml-in-volatile (eq? func-name 'ssml.break))
                  (while (not (equal? text ""))
                    (set! text (get-token utt text)))
                  (set! ssml-current-text text))
              (push (list func-name attlist nil) ssml-parsed))
            ;; If there's no text, just call the mark function
            (set! ssml-current-utt ((symbol-value func-name) attlist utt))))
      (ssml-next-chunk)))))


(provide 'ssml-mode)
