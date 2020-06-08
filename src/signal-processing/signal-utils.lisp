;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-
;;;
;;; Contains functions needed for Feature Extraction
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Discrete Fourier Transform ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Discrete Time Fourier Transform (DTFT) computes an infinite number of frequency bins,
;;; and the Discrete Fourier Transform (DFT) picks out a finite number of those.

;;; Zero padding for FFT:

;;; https://www.mechanicalvibration.com/Zero_Padding_FFTs.html
;; "Zero-padding" means adding additional zeros to a sample of data
;; FFT is slow for prime numbers, but much faster for powers of two. We can add an extra
;; zero to the end of the sample and thus get much better performance.

;; The other reason that zero-padding is used is to get better frequency resolution
;; Zero padding allows us to take more samples of the DTFT.
;; For example, if we have 1000 points of data, sampled at 1000 Hz, and perform the standard
;; FFT, we get a frequency bin every 1 Hz. But if we pad with 1000 zeros and then run a 2000
;; point FFT, now we get frequency bins every 0.5 Hz. This allows us to get around some of
;; the disadvantages of the DFT
(defun pad-power-of-two (sample-list &key (verbose nil))
  "Function takes in sample list, checks for power of 2.
If sample length is not power of 2, pads zero at the end of the sample list."
  (declare (type simple-array sample-list))
  (let* ((sample-length (array-total-size sample-list))
         (power (ceiling (log sample-length 2)))
         (required-sample-length (expt 2 power))
         (number-of-samples-added (- required-sample-length
                                     sample-length))
         (offset (floor (/ number-of-samples-added 2)))
         (array (make-array (list required-sample-length)
                            :initial-element 0.0
                            :element-type 'single-float)))
    (setf (subseq array offset (+ sample-length offset)) sample-list)

    ;; For verbose output:
    (when verbose
      (format t "~&sample length: ~A~&power: ~A" sample-length power)
      (format t "~&required: ~A~&number of sample: ~A ~%" required-sample-length
              number-of-samples-added))
    array))




