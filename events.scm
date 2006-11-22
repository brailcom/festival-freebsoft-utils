;;; Support of miscellaneous kinds of speech events

;; Copyright (C) 2003, 2004, 2005, 2006 Brailcom, o.p.s.

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
(require 'punctuation)
(require 'ssml-mode)
(require 'util)
(require 'wave)
(require 'word-mapping)

;;; Configuration variables

(defvar sound-icon-directory "/usr/share/sounds/sound-icons"
  "Directory where sound icons are placed by default.")

(defvar logical-event-mapping
  '((capital sound "capital")
    (empty-text sound "empty-text")
    (start sound "start")
    (prompt sound "prompt")
    (message sound "message")
    (finish sound "finish")
    (beginning-of-line sound "beginning-of-line")
    (end-of-line sound "end-of-line"))
  "Alist mapping logical sound events to any events.
Each element of the alist is of the form (LOGICAL-EVENT EVENT-TYPE
EVENT-VALUE), where LOGICAL-EVENT is the symbol naming the event to transform,
EVENT-TYPE is the symbol naming the type of the transformed event and
EVENT-VALUE is the corresponding transformed event value.
The following event types are supported:
`logical' -- just this event type again, the value is a symbol naming the event
`text' -- plain text (the event value) to be synthesized
`sound' -- a WAV file to be played, the value is a string naming the file;
  either as an absolute pathname starting with the slash character or a
  pathname relative to `sound-icon-directory'
`key' -- a key event, the value is a string naming the key
`character' -- a character event, the value is a string naming the character
")

(defvar key-event-mapping
  '(("control_i" text "tab")
    ("control_m" text "enter")
    ("control_[" text "escape")
    ("f1" text "f 1")
    ("f2" text "f 2")
    ("f3" text "f 3")
    ("f4" text "f 4")
    ("f5" text "f 5")
    ("f6" text "f 6")
    ("f7" text "f 7")
    ("f8" text "f 8")
    ("f9" text "f 9")
    ("f10" text "f 10")
    ("f11" text "f 11")
    ("f12" text "f 12"))
  "Alist mapping key events to any events.
The form of the alist is the same as in `logical-event-mapping', except
LOGICAL-EVENT is replaced by a string naming the key.")

(defvar character-event-mapping
  '(("\000" text "control space")
    ("\001" text "control a")
    ("\002" text "control b")
    ("\003" text "control c")
    ("\004" text "control d")
    ("\005" text "control e")
    ("\006" text "control f")
    ("\007" text "control g")
    ("\010" text "control h")
    ("\t" text "tab")
    ("\n" text "newline")
    ("\013" text "control k")
    ("\014" text "control l")
    ("\r" text "control m")
    ("\016" text "control n")
    ("\017" text "control o")
    ("\020" text "control p")
    ("\021" text "control q")
    ("\022" text "control r")
    ("\023" text "control s")
    ("\024" text "control t")
    ("\025" text "control u")
    ("\026" text "control v")
    ("\027" text "control w")
    ("\030" text "control x")
    ("\031" text "control y")
    ("\032" text "control z")
    ("\0240" text "hard space"))
  "Alist mapping character events to any events.
The form of the alist is the same as in `logical-event-mapping', except
LOGICAL-EVENT is replaced by a string naming the character.")

(defvar event-mappings
  (list
   (list 'logical logical-event-mapping)
   (list 'key key-event-mapping)
   (list 'character character-event-mapping))
  "Alist mapping event types to new events.
Each element of the alist is of the form (EVENT-TYPE EVENT-MAPPING), where
EVENT-TYPE is one of the symbols `logical', `text', `sound', `key',
`character', and EVENT-MAPPING is the of the same form as
`logical-event-mapping'.")


(defvar event-debug nil)

;;; Internal functions

(define (event-print object)
  (if event-debug
      (if (and (symbol-bound? 'server_log_file)
               (eq (typeof server_log_file) 'string))
          (unwind-protect
            (let ((f (fopen (string-append server_log_file "-e") "a")))
              (format f "%l\n" object)
              (fclose f))
            (print object))
          (print object))))

(defmac (event-with-mode form)
  (let ((mode-name (nth 0 (nth 1 form)))
        (mode-value (nth 1 (nth 1 form)))
        (body (nth_cdr 2 form)))
    (let ((mode-var (intern (string-append mode-name "-mode")))
          (mode-func (intern (string-append "set-" mode-name "-mode"))))
      `(let ((,mode-name ,mode-var))
         (,mode-func ,mode-value)
         (unwind-protect* (begin ,@body)
           (,mode-func ,mode-name))))))

(define (event-find-seg-1 utt word placement)
  (cond
   ((not word)
    (list ((if (eq? placement 'after) utt.relation.first utt.relation.last) utt 'Segment)
          placement))
   ((not (string-equal (item.feat word "R:SylStructure.daughter1.daughter1.name") 0))
    (let ((d (if (eq placement 'after) item.daughtern item.daughter1)))
      (list (d (d (item.relation word 'SylStructure))) placement)))
   (t
    (event-find-seg-1 utt ((if (eq? placement 'after) item.prev item.next) word) placement))))

(define (event-find-seg utt word placement)
  (if (utt.relation.items utt 'Segment)
      (if (eq? placement 'after)
          (event-find-seg-1 utt (item.next word) 'before)
          (event-find-seg-1 utt (item.prev word) 'after))          
      (begin
        (utt.relation.append
         utt 'Segment (list (caar (cdar (PhoneSet.description '(silences))))))
        (list (utt.relation.first utt 'Segment) placement))))

(define (event-eat-utt utt wave-eater)
  (utt.relation.create utt 'Event)
  (do-relation-items (w utt Word)
    (let* ((events '())
           (get-events (lambda (item)
                         (let ((events* (item-events utt item)))
                           (when events*
                             (set! events (append (mapcar (lambda (e)
                                                            (list (first e)
                                                                  (if (string-equal (second e) 'prev)
                                                                      'before
                                                                      'after)))
                                                          events*)
                                                  events)))))))
      (get-events w)
      (let ((token (item.parent (item.relation w 'Token))))
        (if (and token
                 (or (not (item.next w))
                     (not (equal? token (item.parent (item.relation (item.next w) 'Token))))))
            (while token
              (get-events token)
              (set! token (item.next token))
              (when (and token (item.daughters token))
                (set! token nil)))))
      (mapcar (lambda (event-direction)
                (let* ((event (first event-direction))
                       (direction (second event-direction))
                       (seg-placement (event-find-seg utt w direction))
                       (seg (first seg-placement))
                       (placement (second seg-placement))
                       (event* (utt.relation.append
                                utt 'Event
                                `(event ((event ,event)
                                         (event-placement ,placement)
                                         (end ,(item.feat seg 'end))
                                         (pend ,(item.feat seg "R:Segment.p.end")))))))
                  (item.set_feat seg 'event event*)))
              (reverse events))))
  (let ((w (utt.wave utt)))
    (if (utt.relation.items utt 'Event)
        (let ((last-break 0.0))
          (do-relation-items (event utt Event)
            (let ((break (if (string-equal (item.feat event 'event-placement)
                                           'after)
                             (item.feat event 'end)
                             (or (item.feat event 'pend) 0.0)))
                  (event* (item.feat event 'event)))
              (wave-eater (wave-subwave w last-break break))
              (event-synth-plain (first event*) (second event*) wave-eater)
              (set! last-break break)))
          (wave-eater (wave-subwave
                       w last-break
                       (item.feat (utt.relation.last utt 'Segment) 'end))))
        (wave-eater w)))
  utt)

(define (event-synth-text text wave-eater)
  (unless (string-equal text "")
    (event-eat-utt (SynthText text) wave-eater)))

(define (event-synth-ssml value wave-eater)
  (ssml-parse value)
  (let ((utt (ssml-next-chunk))
        (last-utt nil))
    (while utt
      (set! last-utt utt)
      (unless (symbol? utt)
        (utt.synth utt)
        (event-eat-utt utt wave-eater))
      (set! utt (ssml-next-chunk)))
    last-utt))

(define (event-synth-key value wave-eater)
  (let ((text (string-append value)))
    (while (string-matches text ".*_.*")
      (aset text (length (string-before text "_")) 32))
    (event-synth-text text wave-eater)))

(define (event-synth-character value wave-eater)
  (event-synth-text value wave-eater))

(define (event-synth-sound value wave-eater)
  (let ((utt (wave-import-utt
              (if (string-matches value "^/.*")
                  value
                  (string-append sound-icon-directory "/" value)))))
    (wave-eater (utt.wave utt))
    (when (string-matches value "^.*\.delete-after-play$")
      (delete-file value))
    utt))

(define (event-synth-plain type value wave-eater)
  (cond
   ((eq? type 'text)
    (event-synth-text value wave-eater))
   ((eq? type 'ssml)
    (event-synth-ssml value wave-eater))
   ((eq? type 'sound)
    (event-synth-sound value wave-eater))
   ((eq? type 'mark)
    (wave-eater (intern value)))
   (t
    (let ((transformed
           (cdr (assoc value (cadr (assq type (langvar 'event-mappings)))))))
      (cond
       (transformed
        (event-synth-1 (first transformed) (second transformed) wave-eater))
       ((or (eq? type 'key) (eq? type 'character))
        (event-with-mode (punctuation 'all)
          (event-with-mode (cap-signalization t)
            ((if (eq? type 'key) event-synth-key event-synth-character)
             value wave-eater))))
       ((eq? type 'logical)
        (event-synth-text (if (string-matches value "^_.*") "" value)
                          wave-eater))
       (t
        (error "Event description not found" (cons type value))))))))

(define (event-synth-1 type value wave-eater)
  (event-print (list 'event event-debug type value))
  (if (and (eq? type 'logical)
           (string-matches value "^_.*"))
      (cond
       ((string-matches value "^_debug_on.*")
        (set! event-debug (string-after value '_debug_on))
        (set_backtrace t)
        (event-print value))
       ((string-matches value "^_debug_off.*")
        (set! event-debug nil)
        (set_backtrace nil)
        (event-print value))))
  (event-synth-plain type value wave-eater))

;;; External functions

(define (event-synth type value)
  (let* ((waves '())
         (utt (event-synth-1 type value
                             (lambda (w) (set! waves (cons w waves))))))
    (if (<= (length waves) 1)
        utt
        (wave-utt (wave-concat (reverse waves))))))

(define (event-play type value)
  (utt.play (event-synth type value)))

(define (set-event-mapping! type value new-type new-value)
  (set! event-mappings
        (assoc-set event-mappings type
                   (assoc-set (cadr (assoc type event-mappings))
                              value
                              (list new-type new-value)))))

(define (item-events utt item)
  (mapcar (lambda (event) (list (nth 1 event) (nth 3 event)))
          (remove-if (lambda (annotation) (not (eq? (car annotation) 'event)))
                     (get-annotations utt item))))

(define (add-event utt item event stick-to)
  (add-annotation utt item (list 'event event 'event-stick-to stick-to)))
  

;;; Announce

(provide 'events)
