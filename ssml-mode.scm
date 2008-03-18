;;; Speech Synthesis Markup Language 1.0 support

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

;; speak: supported
;; lexicon: ignored (no reasonable specification of the element)
;; meta: ignored (no reasonable use here known)
;; metadata: ignored (no reasonable use here known)
;; p: supported
;; s: supported
;; say-as: trivially rendered (attribute values undefined in the standard);
;;   special value detail="spell"
;; phoneme: unsupported
;; sub: supported
;; voice: supported, but prosody not yet retained (TODO)
;; emphasis: not properly supported (TODO)
;; break: partially supported (TODO)
;; prosody: partially supported (TODO)
;; audio: supported (TODO more complicated alternative text may fail to render)
;; mark: supported
;; desc: ignored (no reasonable use here known)


(require 'duration)
(require_module 'rxp)

(require 'prosody-param)
(require 'recode)
(require 'spell-mode)
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


(define (ssml-attval attlist att)
  (let ((attval (car (xxml_attval att attlist))))
    (if attval
        (recode-utf8->current (string-append attval)))))

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
      `(prosody-relative-value
        ,(+ 1 (* (read-from-string (string-before value "%")) 0.01))))
     ((string-matches value ".*st$")
      `(prosody-relative-value
        ,(pow 2 (/ (read-from-string (string-before value "st")) 12))))
     ((string-matches value ".*Hz$")
      `(prosody-shifted-value ,(read-from-string (string-before value "Hz"))))
     (t
      `(prosody-shifted-value ,(read-from-string value)))))))

(define (ssml-attval-complex attlist att alist)
  (ssml-val-complex (ssml-attval attlist att) alist))

(define (ssml-current-voice)
  current-voice)

(define (ssml-change-voice lang-code gender age variant name)
  (prog1 (select-voice* lang-code gender age variant name)
    (restore-prosody)))

(define (ssml-change-language attlist)
  (let ((lang (ssml-attval attlist 'xml:lang)))
    (when lang
      (ssml-change-voice lang nil nil nil nil))))

(define (ssml-change-prosody utt prosody)
  (ssml-set-feature utt 'ssml-prosody prosody))

(define (ssml-process-prosody utt)
  (let* ((item (ssml-find-feature utt 'ssml-prosody))
         (prosody (ssml-get-feature item 'ssml-prosody)))
    (while item
      (set! item (item.next item))
      (when item
        (dolist (p prosody)
          (ssml-set-feature* utt item 'prosody p))))))

(define (ssml-get-url url tmpfile)
  (get_url (cond
            ((string-matches url "[a-z]+://.*") url)
            (ssml-base-uri (path-append ssml-base-uri url))
            (t (string-append "file:" url)))
           tmpfile))

(define (ssml-set-feature* utt item feature value)
  (add-annotation utt item (list feature value)))

(define (ssml-set-feature utt feature value)
  (ssml-set-feature* utt (ssml-last-token utt) feature value))

(define (ssml-feature-1 value)
  (second (first value)))

(define (ssml-get-feature utt item feature)
  (remove-if (lambda (a) (not (eq? (car a) feature)))
             (get-annotations utt item)))

(define (ssml-get-feature-1 utt item feature)
  (ssml-feature-1 (ssml-get-feature utt item feature)))

(define (ssml-has-feature utt item feature)
  (ssml-get-feature utt item feature))

(define (ssml-find-feature utt feature)
  (let ((item (utt.relation.last utt 'Token)))
    (while (and item (not (ssml-has-feature utt item feature)))
      (set! item (item.prev item)))
    item))

(define (ssml-find-feature-value utt feature)
  (let ((item (ssml-find-feature utt feature)))
    (and item (ssml-get-feature utt item feature))))

(define (ssml-find-feature-value-1 utt feature)
  (ssml-feature-1 (ssml-find-feature-value utt feature)))

(define (ssml-spread-feature utt starting-feature feature value)
  (let ((item (ssml-find-feature utt starting-feature)))
    (while item
      (set! item (item.next item))
      (when item
        (ssml-set-feature* utt item feature value)))))

(define (ssml-delete-feature utt item feature)
  (set-annotations utt item
                   (remove-if (lambda (a) (not (eq? (car a) feature)))
                              (get-annotations utt item))))

(define (ssml-delete-items-from-feature utt feature)
  (let ((item (utt.relation.last utt 'Token))
        (value nil))
    (while (not value)
      (set! value (ssml-get-feature utt item feature))
      (unless value
        (let ((prev-item (item.prev item)))
          (item.delete item)
          (set! item prev-item))))
    value))

(define (ssml-utt-text utt)
  (let* ((last-token (and utt (utt.relation.last utt 'Token)))
         (token last-token)
         (token-list nil))
    (when last-token
      (while (and token (not (eq? (ssml-get-feature-1 utt token 'ssml-tag) 'noticed)))
        (push (item.name token) token-list)
        (set! token (item.prev token)))
      (ssml-set-feature* utt last-token 'ssml-tag 'noticed))
    (apply string-append token-list)))

(define (ssml-append-text utt text)
  (while (not (string-equal text ""))
    (set! text (get-token utt text))))

(define (ssml-last-token utt)
  (or (utt.relation.last utt 'Token)
      (token-utterance-append utt "" "" "" "" 'Token)))


;;; Synthesis handlers


;; Breaks: Ensure they are present wherever needed.
(define-wrapper (Classic_Pauses utt) ssml-classic-pauses
  (do-relation-top-items (token utt Token)
    (when (ssml-has-feature utt token 'ssml-break)
      (let ((token* token))
        (while (and token* (not (item.daughtern token*)))
          (set! token* (item.prev token*)))
        (when token*
          (item.set_feat (item.daughtern token*) 'pbreak "B")))))
  ((next-func) utt))

;; Breaks: Adjust silence durations.
(define-wrapper (Duration utt) ssml-duration
  ((next-func) utt)
  (let ((token (utt.relation utt 'Token)))
    (if (and token (ssml-has-feature utt token 'ssml-break))
        (let ((length (apply + (mapcar second (ssml-get-feature utt token 'ssml-break))))
              (starting-token token))
          (set! token (item.next token))
          (while (and token (not (item.daughtern token)))
            (when (ssml-has-feature utt token 'ssml-break)
              (set! length (+ length (apply + (mapcar second(ssml-get-feature utt token 'ssml-break))))))
            (set! token (item.next token)))
          (while (and starting-token (and (not item.daughtern starting-token)))
            (set! starting-token (item.prev starting-token)))
          (when starting-token
            (let* ((seg find_last_seg (item.daughtern starting-token))
                   (silence (and seg (item.next seg))))
              (when silence
                ;; The final step remains unimplemented for now.
                ;; We should adjust features of all the following segments here.
                (sslm-set-feature utt silence 'ssml-duration length)))))
        (set! token (item.next token))))
  utt)


;;; Markup handlers


(define (ssml.speak.start attlist utt)
  (set! ssml-base-uri (ssml-attval attlist 'xml:base))
  (ssml-change-voice nil nil nil nil nil)
  nil)
(define (ssml.speak.end attlist utt)
  nil)

(define (ssml.lexicon attlist utt)
  nil)

(define (ssml.meta attlist utt)
  nil)

(define (ssml.metadata.start attlist utt)
  nil)
(define (ssml.metadata.end attlist utt)
  nil)

(define (ssml.p.start attlist utt)
  nil)
(define (ssml.p.end attlist utt)
  nil)

(define (ssml.s.start attlist utt)
  nil)
(define (ssml.s.end attlist utt)
  nil)

(define (ssml.say-as.start attlist utt)
  (if (string-equal (ssml-attval attlist 'detail) "spell")
      (begin
        (ssml-set-feature utt 'ssml-say-as 'spell)
        (spell_init_func))
      (ssml-set-feature utt 'ssml-say-as 'dummy)))
(define (ssml.say-as.end attlist utt)
  (let* ((item (ssml-find-feature utt 'ssml-say-as))
         (value (ssml-get-feature-1 utt item 'ssml-say-as)))
    (ssml-delete-feature utt item 'ssml-say-as)
    (when (string-equal value 'spell)
      (unless (string-equal (ssml-find-feature-value-1 utt 'ssml-say-as) 'spell)
        (spell_exit_func))))
  nil)

(define (ssml.phoneme.start attlist utt)
  nil)
(define (ssml.phoneme.end attlist utt)
  nil)

(define (ssml.sub.start attlist utt)
  (let ((text (format nil "%s" (ssml-attval attlist 'alias))))
    (ssml-set-feature utt 'ssml-sub text))
  nil)
(define (ssml.sub.end attlist utt)
  (ssml-append-text utt (ssml-feature-1 (ssml-delete-items-from-feature utt 'ssml-sub))))

(define (ssml.voice.start attlist utt)
  (apply ssml-change-voice (mapcar (lambda (att) (ssml-attval attlist att))
                                   '(xml:lang gender age variant name))))
(define (ssml.voice.end attlist utt)
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
      (ssml-set-feature utt 'ssml-break length)))
  nil)

(define (ssml.emphasis.start attlist utt)
  (ssml-change-prosody utt `((pitch ,(prosody-relative-value 1.1))
                             (rate ,(prosody-relative-value 0.8))))
  nil)

(define (ssml.emphasis.end attlist utt)
  (ssml-process-prosody utt)
  nil)

(define (ssml.prosody.start attlist utt)
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
    (ssml-change-prosody utt prosody))
  nil)
(define (ssml.prosody.end attlist utt)
  (ssml-process-prosody utt)
  nil)

(define (ssml.audio.start attlist utt)
  (ssml-set-feature utt 'ssml-audio-uri (ssml-attval attlist 'src))
  nil)
(define (ssml.audio.end attlist utt)
  (dolist (uri (mapcar second (ssml-find-feature-value utt 'ssml-audio-uri)))
    (let ((tmpfile (make-temp-filename "ssml-audio-%s.delete-after-play"))
          (sound-available nil))
      (unwind-protect
        (begin
          (ssml-get-url uri tmpfile)
          (set! sound-available t))
        (delete-file tmpfile))
      (when sound-available
        (add-event utt (ssml-last-token utt) (list 'sound tmpfile) nil))))
  (ssml-delete-items-from-feature utt 'ssml-audio-uri)
  nil)
(define (ssml.audio attlist utt)
  (ssml.audio.start attlist utt)
  (ssml.audio.end attlist utt))

(define (ssml.mark attlist utt)
  (add-event utt (ssml-last-token utt)
             (list 'mark (ssml-attval attlist 'name)) nil)
  nil)

(define (ssml.desc.start attlist utt)
  nil)
(define (ssml.desc.end attlist utt)
  nil)

(define (ssml.cdata attlist utt)
  ;; Dummy element representing just text.
  nil)

;;; Setup


(defvar ssml-xxml-elements.orig nil)
(defvar ssml-eou_tree.orig nil)

(defvar ssml-tags '("speak" "lexicon" "meta" "metadata" "p" "s" "say-as"
                    "phoneme" "sub" "voice" "emphasis" "break" "prosody"
                    "audio" "mark" "desc"))

(defvar ssml-parsed nil)

(defvar ssml-elements
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

(define (ssml_init_func)
  (set! ssml-xxml-elements.orig xxml_elements)
  (set! xxml_elements ssml-elements)
  (set! ssml-eou_tree.orig eou_tree)
  (set! eou_tree '((0)))
  (set! ssml-voices (list current-voice))
  ;; reset-voice used to be called here, but it's not much useful and it resets
  ;; current speechd settings, which is not what we want.
  )

(define (ssml_exit_func)
  (voice.select (car (last ssml-voices)))
  (set! xxml_elements ssml-xxml-elements.orig)
  (set! eou_tree ssml-eou_tree.orig))

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


(define (ssml-parse-xml ssml-text)
  (with-temp-file ssml-file
    (let ((fd (fopen ssml-file "w")))
      (fwrite ssml-text fd)
      (fclose fd))
    (tts_file ssml-file 'ssml)))

(define (ssml-parse ssml-text)
  (set! ssml-parsed '())
  (glet* ((token.singlecharsymbols "")
          (token.punctuation "")
          (token.prepunctuation "")
          (token.whitespace ""))
    (ssml-parse-xml (ssml-fix-text ssml-text)))
  (set! ssml-parsed (reverse ssml-parsed)))

(define (ssml-fix-text text)
  ;; Work around Festival XML parsing bug
  (let ((pieces '()))
    (while (string-matches text ".*\\\\.*")
      (push (string-before text "\\") pieces)
      (push "\\\\" pieces)
      (set! text (string-after text "\\")))
    (push text pieces)
    (apply string-append (reverse pieces))))

(define (ssml-say ssml-text)
  (ssml-parse ssml-text)
  (ssml-speak-chunks))

(define ssml-say* ssml-say)

(define ssml-utterance-break-functions
  '(ssml.p.start ssml.p.end ssml.s.start ssml.s.end))

(define ssml-unbreakable-functions
  '(ssml.sub.start ssml.audio.start))

(define (ssml-next-parsed-part)
  (let ((open-elements '())
        (next-part '())
        (text "")
        (accepted-text "")
        (remaining-text "")
        (voice (ssml-current-voice))
        (unbreakable '())
        (finished nil))
    ;; get next parts
    (while (and ssml-parsed (not finished))
      (let* ((element (car ssml-parsed))
             (function (first element))
             (attlist (second element))
             (element-text (third element))
             (prev-voice voice))
        (set! ssml-parsed (cdr ssml-parsed))
        ;; recode text
        (unwind-protect
          (let ((orig-voice (ssml-current-voice)))
            (voice.select voice)
            (set! element-text (if (pair? element-text)
                                   (first element-text)
                                   (recode-utf8->current element-text))))
          (voice.select orig-voice))
        ;; add text
        (unless (string-equal element-text "")
          (set! text (string-append text element-text))
          (set! remaining-text (second (next-chunk text)))
          (if unbreakable
              (begin
                (set! remaining-text "")
                (set! accepted-text element-text))
              (begin
                ;; Attention, sometimes the whole text makes utterance break within
                ;; *previous* element (e.g. in "Some <element>word.  Another</element>
                ;; word."
                (when (> (length remaining-text) (length element-text))
                  (set! remaining-text element-text))
                (set! accepted-text (substring
                                     element-text
                                     0 (- (length element-text)
                                          (length remaining-text))))))
          (push (list 'ssml.cdata '() accepted-text) next-part))
        ;; update element's text
        (set! element (list (first element) (second element)
                            (if (string-equal remaining-text "")
                                ""
                                (list remaining-text))))
        ;; utterance break (either implicit or explicit)?
        (when (or (not (string-equal remaining-text ""))
                  (and (member function ssml-utterance-break-functions)
                       (not unbreakable) ; incorrect but currently required
                       (not (string-equal text ""))))
          (set! finished t))
        ;; voice change?
        (when (and (not finished) (not unbreakable))
          ;; Of course, break must be allowed on voice changed.
          ;; But in the current implmementation it is not.
          (let ((new-voice (ssml-change-language attlist)))
            (when (and new-voice (not (eq? new-voice voice)))
              (if (string-equal text "")
                  (set! voice new-voice)
                  (set! finished t)))))
        (if finished
            ;; finished -- return element and its remaining text to ssml-parsed
            (push element ssml-parsed)
            ;; not finished -- update open elements and next part
            (begin
              (cond
               ((string-matches function ".*\.start$")
                (push (list function attlist "" prev-voice) open-elements)
                (when (member function ssml-unbreakable-functions)
                  (push t unbreakable)))
               ((string-matches function ".*\.end$")
                (let* ((closed-element (pop open-elements))
                       (function (first closed-element))
                       (restored-voice (fourth closed-element)))
                  (when (member function ssml-unbreakable-functions)
                    (pop unbreakable))
                  (unless (eq? restored-voice voice)
                    (set! finished t)))))
              (push element next-part)))))
    ;; if tree split is necessary, finish it
    (when (or ssml-parsed remaining-text)
      (set! ssml-parsed (append (reverse open-elements) ssml-parsed)))
    ;; done
    (reverse next-part)))

(define (ssml-process-parsed-part parsed)
  (let ((utt (token-utterance))
        (orig-voice (ssml-current-voice))
        (voice (ssml-current-voice)))
    ;; process elements
    (while parsed
      (let* ((element (car parsed))
             (function (first element))
             (attlist (second element))
             (text (third element)))
        (ssml-append-text utt text)
        (set! voice (ssml-change-language attlist))
        (set! voice (or ((symbol-value function) attlist utt) voice)))
      (set! parsed (cdr parsed)))
    ;; restore original voice and return the resulting utterance
;    (voice.select orig-voice)
    utt))

(define (ssml-next-chunk)
  (let ((parsed (ssml-next-parsed-part)))
    (if parsed
        (ssml-process-parsed-part parsed)
        nil)))

(define (ssml-play object)
  (if (symbol? object)
      (print object)
      (wave.play object)))

(define (ssml-speak-chunks)
  (let ((utt (ssml-next-chunk)))
    (when utt
      (utt.synth utt)
      (event-eat-utt utt ssml-play)
      (ssml-speak-chunks))))

(provide 'ssml-mode)
