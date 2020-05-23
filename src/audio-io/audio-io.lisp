;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-
;;;
;;; Contains Input / Output interfaces.
;;;
;;; Copyright © 2019 Karthik Kumar <karthikkumar.s@protonmail.com>
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the
;;; "Software"), to deal in the Software without restriction, including
;;; without limitation the rights to use, copy, modify, merge, publish,
;;; distribute, sublicense, and/or sell copies of the Software, and to
;;; permit persons to whom the Software is furnished to do so, subject to
;;; the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included
;;; in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
;;; OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(in-package #:signal)


;;; Wave Class
(defclass wave-signal ()
  ((duration :initarg :duration
             :initform *seconds*
             :accessor duration
             :type 'single-float)
   (sample-rate :initarg :sample-rate
                :initform *sample-rate*
                :accessor sample-rate
                :type 'double-float)
   (num-channels :initarg :num-channels
                 :initform *num-channels*
                 :accessor num-channels
                 :type 'integer)
   (sample-interval :initarg :sample-interval
                    :initform *sample-interval*
                    :accessor sample-interval
                    :type 'single-float)
   (sample-points :initarg :sample-points
                  :initform 0
                  :accessor sample-points
                  :type 'integer)
   (audio-buffer-size :initform +audio-buffer-size+
                      :reader audio-buffer-size
                      :type 'integer))
  (:documentation "Wave class - Audio Signal"))


;;;  Wave Header
(defclass wave-header (wave-signal)
  ((chunk-id :accessor chunk-id
             :initarg :chunk-id
             :initform "RIFF"
             :type 'string)
   (chunk-size :accessor chunk-size
               :initarg :chunk-size
               :initform 0)
   (riff-format :accessor riff-format
                :initform "WAVE"
                :initarg :riff-format
                :type 'string)

   ;; fmt subchunk:
   (subchunk1-id :reader subchunk1-id
                 :initform "fmt ")
   (subchunk1-size :reader subchunk1-size
                   :initform 16
                   :type 'integer)
   (audio-format :reader audio-format
                 :initform 1)
   (byte-rate :accessor byte-rate
              :initarg :byte-rate
              :initform 0d0)
   (block-align :accessor block-align
                :initarg :block-align)
   (bits-per-sample :accessor bits-per-sample
                    :initarg :bits-per-sample
                    :initform 16
                    :type 'integer)

   ;; data subchunk
   (subchunk2-id :reader subchunk2-id
                 :initform "data"
                 :type 'string)
   (audio-sample-size :accessor audio-sample-size
                      :initarg :audio-sample-size
                      :initform 0
                      :type 'integer)
   (subchunk2-size :accessor subchunk2-size
                   :initarg :subchunk2-size))
  (:documentation "Wave Header."))


;;; Constructor
(defun make-wave-header (sample-size)
  (make-instance 'wave-header :audio-sample-size sample-size :sample-rate 44100))


;;; Initialization
(defmethod initialize-instance :after ((header wave-header) &key)
  (setf (byte-rate header)
        (* (sample-rate header)
           (num-channels header)
           (/ (bits-per-sample header) 8)))

  (setf (block-align header)
        (* (num-channels header)
           (/ (bits-per-sample header) 8)))

  (setf (subchunk2-size header)
        (* (audio-sample-size header)
           (num-channels header)
           (/ (bits-per-sample header) 8)))

  (setf (chunk-size header)
        (+ (subchunk2-size header)
           36)))


;;;; Audio signal from Microphone. Inherits from wave-signal.
(defclass audio-from-mic (wave-signal)
  ()
  (:documentation "Real time audio signal from Microphone."))


;;;; Audio from a file. Inherits from wave-signal.
(defclass audio-from-file (wave-header) 
  ((audio-data :initarg :audio-data
               :accessor audio-data))
  (:documentation "Audio signal from a file."))

(defun make-audio-from-file (&key duration sample-rate bits-per-sample audio-data num-channels)
  "Constructor function for the class audio from file."
  (make-instance 'audio-from-file :duration duration
                                  :sample-rate sample-rate
                                  :bits-per-sample bits-per-sample
                                  :audio-data audio-data
                                  :num-channels num-channels))


;;; Wave created from math function. Inherits from wave-signal.
(defclass wave-from-function (wave-signal)
  ((wave-function :initform 'sin
                  :initarg :wave-function
                  :accessor wave-function)
   (frequency :initarg :frequency
              :initform *frequency*
              :accessor frequency
              :type 'double-float))
  (:documentation "Wave object created using a function."))


;;; Initialization
(defmethod initialize-instance :after ((wave wave-from-function) &key)
  (setf (sample-interval wave) (/ 1.0 (sample-rate wave)))
  (setf (sample-points wave) (round (/ (duration wave)
                                       (sample-interval wave)))))

(defmethod initialize-instance :after ((wave audio-from-mic) &key)
  (setf (sample-interval wave) (/ 1.0 (sample-rate wave)))
  (setf (sample-points wave) (round (/ (duration wave)
                                       (sample-interval wave)))))

(defmethod initialize-instance :after ((wave audio-from-file) &key)
  (setf (sample-interval wave) (/ 1.0 (sample-rate wave)))
  (setf (sample-points wave) (round (/ (duration wave)
                                       (sample-interval wave)))))


(defun audio-from-audio-stream (audio source-buffer buffer-len &key (idx 0))
  "Creates audio object. Stores the data into source buffer."
  (with-audio
    (with-default-audio-stream (astream
                                (num-channels audio)
                                (num-channels audio)
                                :sample-format :float
                                :sample-rate (sample-rate audio)
                                :frames-per-buffer (audio-buffer-size audio))
      (format t "~& <<< Starting of Recording >>> ~%")
      (iter
        (while (< idx (array-total-size source-buffer)))
        (let ((buf (read-stream astream)))
          (write-buffer-with-raw-data buf
                                      source-buffer
                                      idx
                                      (+ idx buffer-len)
                                      :buffer-len buffer-len)
          (incf idx buffer-len)))
      (format t "~& <<< End of Recording. >>> ~%"))))


;;; Record audio
(defun record-audio (&key (record-time *seconds*) (save-to-file nil)
                       (audio-filename nil) (plot-data nil)
                       (plot-filename nil)
                       (dft-analysis nil))
  "Records audio using a Microphone."
  (let* ((audio (make-instance 'audio-from-mic
                               :duration record-time))
         (source-buffer-length (round (* (num-channels audio)
                                         (duration audio)
                                         (sample-rate audio))))
         (idx 0)
         (buf-len (* (audio-buffer-size audio)
                     (num-channels audio)))
         (source-buffer (make-array source-buffer-length
                                    :element-type 'single-float
                                    :initial-element 0.0)))
    (format t "~&Audio buffer size (from record audio): ~A" source-buffer-length)
    (audio-from-audio-stream audio
                             source-buffer
                             buf-len
                             :idx idx)
    (when save-to-file
      ;; (save-to-file audio-filename source-buffer)
      (save-as-wav-file source-buffer audio-filename))
    (when plot-data
      (plot-signal source-buffer plot-filename 'normal))
    (when dft-analysis
      source-buffer)))



#+test
(record-audio :record-time 5
              :save-to-file t
              :audio-filename "new-mic.wav"
              :plot-data nil
              :plot-filename "record-plot.png")



(defun play-wav-audio (filename)
  "Function to play WAV file."
  (let* ((wav-file (load-wav-file filename :verbose t))
         (total-audio-data-size (length (audio-data wav-file)))
         (idx 0)
         (buffer-len (* (audio-buffer-size wav-file) (num-channels wav-file)))
         (buffer (make-array buffer-len :element-type 'single-float :initial-element 0.0)) 
         (audio-source-buffer (alexandria:coercef (audio-data wav-file) 'vector)))
    (with-audio
      (with-default-audio-stream (astream
                                  (num-channels wav-file)
                                  (num-channels wav-file)
                                  :sample-format :float
                                  :sample-rate (sample-rate wav-file)
                                  :frames-per-buffer (audio-buffer-size wav-file))
        (format t "~& <<< Starting Playback >>> ~%")
        (iter
          (while (< idx total-audio-data-size))
          (fill-buffer buffer
                       audio-source-buffer
                       idx
                       (+ idx buffer-len))
          (write-stream astream buffer)
          (incf idx buffer-len))))))

;; (play-wav-audio "/home/karthik/quicklisp/local-projects/signal/new-mic.wav")
