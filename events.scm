;;; Support of miscellaneous kinds of speech events

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


(require 'cap-signalization)
(require 'punctuation)
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
    ("\032" text "control z"))
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
  (let ((neighbor ((if (eq? placement 'after) item.prev item.next) word)))
    (cond
     ((not neighbor)
      (if (eq? placement 'after)
          (list (utt.relation.first utt 'Segment) 'before)
          (list (utt.relation.last utt 'Segment) 'after)))
     ((not (string-equal
            (item.feat neighbor "R:SylStructure.daughter1.daughter1.name")
            0))
      (let ((d (if (eq placement 'after) item.daughtern item.daughter1)))
        (list (d (d (item.relation neighbor 'SylStructure))) placement)))
     (t
      (event-find-seg-1 utt neighbor placement)))))

(define (event-find-seg utt word placement)
  (if (utt.relation.items utt 'Segment)
      (event-find-seg-1 utt word placement)
      (begin
        (utt.relation.append
         utt 'Segment (list (caar (cdar (PhoneSet.description '(silences))))))
        (list (utt.relation.first utt 'Segment) placement))))

(define (event-synth-text text)
  (let ((utt (SynthText text)))
    (utt.relation.create utt 'Event)
    (do-relation-items (w utt Word)
      (if (item.has_feat w 'event)
          (let* ((placement* (if (string-equal (item.feat w 'event-stick-to)
                                               'next)
                                 'before
                                 'after))
                 (seg-placement (event-find-seg utt w placement*))
                 (seg (first seg-placement))
                 (placement (second seg-placement)))
            (item.set_feat seg 'event (item.feat w 'event))
            (item.set_feat seg 'event-placement placement)
            (utt.relation.append utt 'Event seg))))
    (if (utt.relation.items utt 'Event)
        (let ((w (utt.wave utt))
              (waves '())
              (last-break 0.0))
          (do-relation-items (seg utt Event)
            (let ((break (cond
                          ((string-equal (item.feat seg 'event-placement)
                                         'after)
                           (item.feat seg 'end))
                          ((item.prev (item.relation seg 'Segment))
                           (item.feat seg "R:Segment.p.end"))
                          (t
                           0.0)))
                  (event (item.feat seg 'event)))
              (push (wave-subwave w last-break break) waves)
              (push (utt.wave (event-synth-plain (first event) (second event)))
                    waves)
              (set! last-break break)))
          (push (wave-subwave
                 w last-break
                 (item.feat (utt.relation.last utt 'Segment) 'end))
                waves)
          (wave-utt (wave-concat (reverse waves))))
        utt)))

(define (event-synth-key value)
  (let ((text (string-append value)))
    (while (string-matches text ".*_.*")
      (aset text (length (string-before text "_")) 32))
    (event-synth-text text)))

(define (event-synth-character value)
  (event-synth-text value))

(define (event-synth-sound value)
  (wave-import-utt (if (string-matches value "^/.*")
                       value
                       (string-append sound-icon-directory "/" value))))

(define (event-synth-plain type value)
  (cond
   ((eq? type 'text)
    (event-synth-text value))
   ((eq? type 'sound)
    (event-synth-sound value))
   (t
    (let ((transformed
           (cdr (assoc value (cadr (assq type (langvar 'event-mappings)))))))
      (cond
       (transformed
        (apply event-synth transformed))
       ((or (eq? type 'key) (eq? type 'character))
        (event-with-mode (punctuation 'all)
          (event-with-mode (cap-signalization t)
            ((if (eq? type 'key) event-synth-key event-synth-character)
             value))))
       ((eq? type 'logical)
        (event-synth-text (if (string-matches value "^_.*") "" value)))
       (t
        (error "Event description not found" (cons type value))))))))

;;; External functions

(define (event-synth type value)
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
  (event-synth-plain type value))

(define (event-play type value)
  (utt.play (event-synth type value)))

(define (set-event-mapping! type value new-type new-value)
  (set! event-mappings
        (assoc-set event-mappings type
                   (assoc-set (cadr (assoc type event-mappings))
                              value
                              (list new-type new-value)))))

;;; Announce

(provide 'events)
