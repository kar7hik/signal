 ;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-
;;;
;;; Exploring Signal Processing Module
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

;;; Wave Generate - User API
(defun generate-wave-data (wave &key (with-time t)
                                  (data-size +audio-buffer-size+))
  (let* ((buffer-size nil)
         (buffer nil))
    (when with-time
      (setf buffer-size (round (sample-points wave)))
      (setf buffer (make-array buffer-size :element-type 'single-float
                                           :initial-element 0.0))
      (format t "~&Buffer Size: ~A" buffer-size)
      (create-wave-samples buffer
                           (wave-function wave)
                           (frequency wave)
                           (sample-interval wave)))
    (unless with-time
      (setf buffer (arange 0 (duration wave) :step (/ 1 (sample-rate wave))))
      (setf buffer-size (length buffer))
      (format t "~&Buffer Size: ~A" (array-total-size buffer))
      (create-wave-samples buffer
                           (wave-function wave)
                           (frequency wave)
                           (sample-interval wave)))
    buffer))


(defun create-wave (wave &key (plot-data nil) (plot-filename)
                           (save-to-file nil) (filename nil))
  (let ((buffer (generate-wave-data wave)))
    (when plot-data
      (plot-signal buffer plot-filename 'normal))
    (when save-to-file
      (save-as-wav-file buffer filename))))


;;; For testing
(defparameter *wave* (make-instance 'wave-from-function
                                    :duration *seconds*
                                    :frequency *frequency*
                                    :num-channels *num-channels*))


#+test
(create-wave *wave*
             :plot-data nil
             :plot-filename "sine-example.png"
             :save-to-file t
             :filename "low-freq.wav")

;;;;;;;;;;;;;;;;;;;
;; DFT Analysis: ;;
;;;;;;;;;;;;;;;;;;;
;;; http://www.bitweenie.com/listings/fft-frequency-axis/
;; The FFT shows the frequency-domain view of a time-domain signal in the frequency space
;; of -fs/2 to fs/2, where fs is the sampling frequency. This frequency space is split
;; into N points, where N is the number of points in the FFT. The spacing between
;; points in frequency domain is simply expressed as:
;;
;; del_R = f_s / N_fft
;;
(defun dft-analysis-core (buffer &key (save-to-file nil)
                                   (audio-filename nil audio-filename-supplied)
                                   (plot-data nil)
                                   (plot-filename nil plot-filename-supplied)
                                   (verbose nil))
  "Core function carrying out discrete fourier transform."
  (let* ((padded-array (pad-power-of-two buffer :verbose t))
         (raw-complex (sfft padded-array))
         (freq (arange 0 (/ *sample-rate* 2)
                       :step (/ *sample-rate*
                                (array-total-size padded-array))))
         (freq-magnitude (make-array (/ (array-total-size padded-array) 2)
                                     :element-type 'double-float)))
    (iter
      (for i :index-of-vector freq-magnitude)
      (setf (aref freq-magnitude i) (abs (aref raw-complex i))))

    (when save-to-file
      (save-as-wav-file buffer audio-filename))

    (when plot-data
      (plot-signal freq-magnitude plot-filename 'fft
                   :x-sequence freq))

    (when verbose
      (format t "~&length of data: ~A" (array-total-size buffer))
      (format t "~&length of padded array: ~A" (array-total-size padded-array))
      (format t "~&length of freq: ~A" (array-total-size freq))
      (format t "~&length of freq magnitude: ~A" (array-total-size freq-magnitude)))
    ))


(defun dft-analysis-generated-wave (duration &key (save-to-file nil)
                                           (audio-filename nil audio-filename-supplied)
                                           (plot-fft nil)
                                           (fft-filename nil plot-filename-supplied)
                                           )
  "Takes in wave object and does fft on it."
  (let* ((wave (make-instance 'wave-from-function
                              :duration duration
                              :frequency *frequency*
                              :num-channels *num-channels*))
         (signal-buffer (generate-wave-data wave :with-time t)))
    (dft-analysis-core signal-buffer :save-to-file save-to-file
                                     :audio-filename audio-filename
                                     :plot-data plot-fft
                                     :plot-filename fft-filename)))

#+test
(dft-analysis-generated-wave 2 :save-to-file t
                               :audio-filename "./results/record-gen-fft.wav"
                               :plot-fft t
                               :fft-filename "./results/record-gen-fft.png")

;;; Having an unified core function to handle dft-analysis and multiple API controlling
;;; the source data (e.x: From Mic, Generated Audio etc.,)
(defun dft-analysis-from-mic (duration &key (save-to-file nil)
                                         (audio-filename nil audio-filename-supplied)
                                         (plot-fft nil)
                                         (fft-filename nil plot-filename-supplied))
  "For FFT analysis - Real time audio from Microphone"
  (let* ((audio-data-buffer (record-audio :record-time duration
                                          :dft-analysis t)))
    (dft-analysis-core audio-data-buffer :save-to-file save-to-file
                                         :audio-filename audio-filename
                                         :plot-data plot-fft
                                         :plot-filename fft-filename)))
#+test
(dft-analysis-from-mic 3 :save-to-file t
                         :audio-filename "./results/record-fft.wav"
                         :plot-fft t
                         :fft-filename "./results/record-fft.png")



;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pre-emphasis Filter: ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pre-emphasis filter used on signal mainly to amplify the high frequencies.
;; A pre-emphasis filter is useful in several ways:
;; --- balance the frequency spectrum (By amplifying magnitudes of high fz)
;; --- avoid numerical problems during the Fourier transform operation
;; --- may also improve the Signal-to-Noise Ratio (SNR)
;; The pre-emphasis filter can be applied to a signal x
;; using the first order filter in the following equation:
;;
;; y(t)=x(t)−αx(t−1)
;;
;; pre-emphasized_sample [i] = sample[i] - alpha * sample[i - 1]
;;
;; Filter Coefficient, α (alpha) = 0.95 or 0.97
;; pre_emphasis = 0.97
(defun pre-emphasis-filter (sample-array-buffer &key (alpha 0.97))
  "Takes in sample buffer."
  (let ((pre-emphasized-samples (copy-seq sample-array-buffer)))
    (iter
      (for idx :from 1 :to (1- (array-total-size pre-emphasized-samples)))
      (setf (elt pre-emphasized-samples idx)
            (- (elt pre-emphasized-samples idx)
               (* alpha (elt pre-emphasized-samples (1- idx))))))
    pre-emphasized-samples))


#+test
(pre-emphasis-filter #(5 2 3 18 6 2 37))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Short Time Fourier Transform: ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
