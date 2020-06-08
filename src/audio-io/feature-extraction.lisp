;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-
;;;
;;; Contains Feature Extraction Modules
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

(defparameter *frame-size* 0.020
  "Short frame size in ms.")

(defparameter *frame-stride* 0.01
  "Frame stride size in ms.")

(defparameter *NFFT* 512
  "FFT bin value.")


(defun frequency-to-mel-scale (freq)
  "Function to convert value from frequency to mel-scale."
  (* 1127 (log (1+ (/ freq 700)) (exp 1))))


(defun mel-scale-to-frequency (mel)
  "Function to convert value from mel-scale to frequency."
  (* 700 (- (exp (/ mel 1127)) 1)))


(defun chunk-audio-data (audio seconds)
  "Returns the chunk of audio data seconds."
  (let* ((audio-data (audio-data audio))
         (audio-len (length audio-data))
         (sample-rate (sample-rate audio))
         (chunk-len (* sample-rate seconds)))
    (if (>= chunk-len audio-len)
        (progn
          (format t "~&Cannot snip the audio; The audio size is less than the required time seconds. Returning the original audio.~%")
          audio-data)
        (let ((chunk-audio (make-array (ceiling chunk-len) :element-type 'single-float)))
          (print (length chunk-audio))
          (iter
            (for i :from 0 :below chunk-len)
            (setf (aref chunk-audio i)
                  (aref audio-data i)))
          chunk-audio))))

;; (defparameter *snip*
;;   (chunk-audio-data *wav* 6.0))
;; (save-to-file "snip.txt" *snip*)



(defun fill-value (dimensions value &key (type 'integer))
  "Fills the value to the dimensions with the specified type."
  (make-array dimensions
              :element-type type
              :initial-element value))

(defun zeros (dimensions &key (type 'integer))
  "Returns zeros of dimensions with the specified type."
  (fill-value dimensions (coerce 0 type) :type type))


(defclass signal-processing ()
  ((audio-data :initarg :audio-data
               :accessor audio-data
               :type 'single-float)
   (frame-size :initarg :frame-size
               :initform *frame-size*
               :accessor frame-size
               :type 'single-float)
   (frame-stride :initarg :frame-stride
                 :initform *frame-stride*
                 :accessor frame-stride
                 :type 'single-float)
   (frame-length :initarg :frame-length
                 :accessor frame-length)
   (sample-rate :initarg :sample-rate
                :initform *sample-rate*
                :accessor sample-rate
                :type (type-of *sample-rate*))
   (total-number-of-frames :initarg :total-number-of-frames
                           :accessor total-number-of-frames
                           :type 'integer)
   (pre-emphasis :initarg :pre-emphasis
                 :initform 0.97
                 :accessor pre-emphasis)
   (emphasized-signal :initarg :emphasized-signal
                      :accessor emphasized-signal)
   (frame-step :initarg :frame-step
               :accessor frame-step)
   (audio-length :initarg :audio-length
                 :accessor audio-length
                 :type 'integer)
   (pad-audio-length :initarg :pad-audio-length
                     :accessor pad-audio-length
                     :type 'integer)
   (padding-required :initarg :padding-required
                     :accessor padding-required
                     :type 'integer)
   (padded-audio :initarg :padded-audio
                 :accessor padded-audio)
   (NFFT :initarg :NFFT
         :initform 512
         :accessor NFFT
         :type integer)
   (splitted-frames :initarg :splitted-frames
                    :accessor splitted-frames))
  
  (:documentation "Signal processing Class"))


(defun find-frame-length (sample-rate frame-size)
  "Function returns the frame length. Convert from seconds to samples"
  (round (* frame-size sample-rate)))


(defun find-frame-step (sample-rate frame-stride)
  "Function returns the frame step. Convert from seconds to samples."
  (round (* frame-stride sample-rate)))


(defmethod initialize-instance :after ((signal-processing-obj signal-processing) &key)
  (setf (frame-length signal-processing-obj)
        (find-frame-length (sample-rate signal-processing-obj)
                           (frame-size signal-processing-obj)))
  (setf (frame-step signal-processing-obj)
        (find-frame-step (sample-rate signal-processing-obj)
                         (frame-stride signal-processing-obj)))
  (setf (audio-length signal-processing-obj)
        (length (audio-data signal-processing-obj)))
  (setf (total-number-of-frames signal-processing-obj)
        (abs (ceiling (float (/ (abs (- (audio-length signal-processing-obj)
                                        (frame-length signal-processing-obj)))
                                (frame-step signal-processing-obj))))))
  (setf (pad-audio-length signal-processing-obj)
        (+ (* (total-number-of-frames signal-processing-obj)
              (frame-step signal-processing-obj))
           (frame-length signal-processing-obj)))
  (setf (padding-required signal-processing-obj)
        (- (pad-audio-length signal-processing-obj)
           (audio-length signal-processing-obj)))
  )

(defgeneric pre-emphasis-audio-data (signal-processing-obj)
  (:documentation "Generic function to apply pre-emphasis on the audio data."))

(defmethod pre-emphasis-audio-data ((signal-processing-obj signal-processing))
  (setf (emphasized-signal signal-processing-obj)
        (pre-emphasis-filter (audio-data signal-processing-obj)))
  'SUCCESS)

(defgeneric pad-audio-with-zeros (signal-processing-obj)
  (:documentation "Finds the required padding for the audio signal and pads the zeros."))


(defmethod pad-audio-with-zeros ((signal-processing-obj signal-processing))
  (setf (padded-audio signal-processing-obj)
        (concatenate 'vector (emphasized-signal signal-processing-obj)
                     (zeros (padding-required signal-processing-obj) :type 'single-float)))
  'COMPLETED)


(defun make-signal-processing (audio-data)
  "Constructor function for MFCC Class."
  (alexandria:with-gensyms (signal-processing-obj)
    (setf signal-processing-obj (make-instance 'signal-processing
                                  :audio-data audio-data))
    (pre-emphasis-audio-data signal-processing-obj)
    (pad-audio-with-zeros signal-processing-obj)
    signal-processing-obj))





(defmethod print-parameters-signal-processing ((signal-processing-obj signal-processing))
  (format t "~%~& sample-rate: ~A ~%" (sample-rate signal-processing-obj))
  (format t "~& frame-size:  ~A ~%" (frame-size signal-processing-obj))
  (format t "~& frame-step: ~A ~%" (frame-step signal-processing-obj))
  (format t "~& frame-length: ~A ~%" (frame-length signal-processing-obj))
  (format t "~& total-number-of-frames: ~A ~%" (total-number-of-frames signal-processing-obj))
  (format t "~& audio-length: ~A ~%" (audio-length signal-processing-obj))
  (format t "~& pad-audio-length: ~A ~%" (pad-audio-length signal-processing-obj))
  (format t "~& padding-required: ~A ~%" (padding-required signal-processing-obj))
  )





(defun split-padded-audio-into-frames (audio-data frame-length frame-step)
  (let* ((audio-data-length (length audio-data))
         (total-num-frames (floor (/ audio-data-length frame-step)))
         (result-list-frames '()))
    (iter
      (for i :from 0 :to (- total-num-frames 2))
      (let* ((start-idx (* i frame-step))
             (end-idx (+ frame-length start-idx))
             (splitted-frame '()))
        (format t "~&~% start-idx: ~A ~%" start-idx)
        (format t "~& end-idx: ~A ~%" end-idx)
        (format t "~& total-num-frames: ~A and i: ~A  ~%" total-num-frames i)
        (format t "~& audio-data-length: ~A ~%" audio-data-length)
        (when (< audio-data-length end-idx)
          (setf splitted-frame (append (subseq audio-data start-idx total-num-frames)
                                       (zeros (- end-idx total-num-frames)))))
        (setf splitted-frame (subseq audio-data start-idx end-idx))

        ;(format t "~& start-idx: ~A ~%" start-idx)
        (push splitted-frame result-list-frames)))
    (nreverse result-list-frames)))



(defun pad-audio-with-proper-zeros (audio-data)
  (setf audio-data (padded-audio (make-signal-processing audio-data))))



(defun split-audio-into-frames (audio-data frame-length frame-step &key (padded-p t))
  (if padded-p
      (split-padded-audio-into-frames audio-data frame-length frame-step)
      (progn
        (format t "~& The audio data is padded with additional frames for computation. ~%")
        (setf audio-data (pad-audio-with-proper-zeros audio-data))
        (split-padded-audio-into-frames audio-data frame-length frame-step))))


;; (defun spectrum-power (frames)
;;   "Computes the power spectrum of frames."

;;   )


(defun apply-filter-to-frame (frame &key (filter-function 'hamming))
  "Function that applies the selected type of window to the frame."
  (let* ((frame-len (length frame))
         (filtered-frame (make-array frame-len
                                     :element-type 'double-float)))
    (format t "~& frame-len: ~A ~%" frame-len)
    (iter
      (for i :from 0 :below frame-len)
      (setf (aref filtered-frame i)
            (* (funcall filter-function i frame-len)
               (aref frame i))))
    filtered-frame))


(defun apply-filter-to-audio (audio-data &key (filter-function 'hamming))
  (let ((filtered-audio '()))
    (iter
      (for i :from 0 :below (length audio-data))
      (push (apply-filter-to-frame (elt audio-data i) :filter-function filter-function)
            filtered-audio))
    filtered-audio))



(defun hann (idx window-len)
    "Hanning window -- ref *Discrete-time Signal Processing - Alan V. Oppenheim, Ronald W. Schafer.*
idx - index of the window
window-len - length of the filter window" 
  (* 0.5 (- 1.0
            (cos (/ (* 2 pi idx)
                        (1- window-len))))))


(defun hamming (idx window-len)
  "Hamming window -- ref *Discrete-time Signal Processing - Alan V. Oppenheim, Ronald W. Schafer.*
idx - index of the window
window-len - length of the filter window" 
  (- 0.54
     (* 0.46
        (cos (/ (* 2 pi idx)
                (1- window-len))))))


(defparameter *wav-file* "/home/karthik/quicklisp/local-projects/signal/low-freq.wav") 
(defparameter *wav* (load-wav-file *wav-file*))
(defparameter *audio-final* (chunk-audio-data *wav* 1.0))
(defparameter *signal-processing-obj* (make-signal-processing *audio-final*))
#+test
(plot-signal (emphasized-signal *signal-processing-obj*) "audio.png" :x-label "Samples"
                                                 :y-label "Amplitude"
                                                 :title "audio-data")
#+test
(print-parameters-signal-processing *signal-processing-obj*)
#+test
(defparameter *ex* (split-audio-into-frames (audio-data *signal-processing-obj*)
                                            (frame-length *signal-processing-obj*)
                                            (frame-step *signal-processing-obj*)
                                            :padded-p nil))
#+test
(defparameter *filtered* (apply-filter-to-audio *ex*))

#+test
(dft-analysis-core (pad-power-of-two (third *ex*) :verbose t))

