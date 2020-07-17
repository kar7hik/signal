;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-
;;;
;;; Contains global variables
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

;;; Audio Processing
;;; Constants:
(defconstant +pi+ (coerce pi 'single-float)
  "Pi value coerced into single float.")
(defconstant +tau+ (* 2 +pi+)
  "2-PI Value")
(defconstant +audio-buffer-size+ 1024
  "The number of samples in the audio buffer.")


;;; Global Variables
(defparameter *sample-rate* 44100d0
  "Default audio sample rate.")
(defparameter *seconds* 5
  "Default time period")
(defparameter *sample-interval* 1
  "Default time interval.")
(defparameter *num-channels* 2
  "Default number of channels.")
(defparameter *frequency* 440
  "Default tone frequency.")

(defparameter *num-worker-threads* 8
  "Number of worker threads for lparallel.")


(defparameter *project-name* "signal/")
(defparameter *local-working-directory* (merge-pathnames *project-name*
                                                         "quicklisp/local-projects/"))
(defparameter *current-directory* (merge-pathnames *local-working-directory*
                                                   (user-homedir-pathname)))
