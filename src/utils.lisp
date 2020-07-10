6;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-
;;;
;;; Utility Functions for Signal Module
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


(defun range-helper (start stop step)
  (iter
    (for i :from start :to stop :by step)
    (collect i)))


(defun range (start &key (stop nil stop-supplied-p) (step 1))
  "Creates an array for given start and stop conditions. default step value is 1. The resulting array will be the type of step."
  (if stop-supplied-p
      (range-helper start stop step)
      (range-helper 0 start step)))


(defun create-file-path (filename)
  "Creates a file path from the filename."
  (merge-pathnames filename
                   *current-directory*))

    

(defun coerce-sequence (seq conversion-type)
  (iter
    (for elt :in-sequence seq)
    (alexandria:coercef elt conversion-type)
    (collect elt result-type 'vector)))
