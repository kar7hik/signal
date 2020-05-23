;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-
;;;
;;; Learning Common lisp
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


;;; Learn Iterate:
;;; Simple example:

(in-package #:signal-test)


(format t "~&Simple Example: ~%")
(iter
 (for i :from 0 :to 10)
 (collect i))              ; => (0 1 2 3 4 5 6 7 8 9 10)


;;; Collects all the odd numbers in a list:
(format t "~&Collecting odd numbers: ~%")
(iter
 (for el :in '(0 1 2 3 4 5 6 7 8 9))
 (if (and (numberp el) (oddp el))
	 (collect el)))          ; => (1 3 5 7 9)


;;;; Driver:
;;; Repeat:
(format t "~&Using Repeat: ~%")
(iter
 (repeat 1)
 (print "Hello, World!!!"))


;;; Numerical Iteration:
;; Downto
(format t "~&Using Downto: ~%")
(iter
 (for i :from 3 :downto 0)
 (format t "~&~A~%" i))

;; Below and Above:
(format t "~&Using Below and Above: ~%")
(iter
 (for i :from 0 :below 3)
 (format t "~&~A~%" i))

(iter
 (for i :from 3 :above 0)
 (format t "~&~A~%" i))

;; upfrom is synonym to from

;; By - Stepping:
(format t "~&Stepping using by: ~%")
(iter
 (for i :from 0 :to 10 :by 3)
 (format t "~&~A~%" i))


;;;; Sequence Iteration:
;;; List:
(format t "~&List Iteration: Using \":in\" ~%")
(iter
 (for el :in '(0 8 9))
 (format t "~&~A~%" el))


(format t "~&List Iteration: Using \":on\" ~%")
(iter
 (for el :on '(8 9 0))
 (format t "~&~A~%" el))


;;; Vector:
(format t "~&Using Vector:~%")
(iter
 (for item :in-vector (vector 1 2 3))
 (format t "~&~A~%" item))


(format t "~&Does not access the vector - Only uses the indices:~%")
(iter
 (for idx :index-of-vector (vector 5 8 9))
 (format t "~&~a~%" idx))


;;; String:
(format t "~&Using String:~%")
(iter
 (for item :in-string "Athena")
 (format t "~&~s~%" item))


(format t "~&Does not access the string - Only uses the indices:~%")
(iter
 (for idx :index-of-string "Athena")
 (format t "~&~a~%" idx))




;;;; Generators:
(format t "Using Generate:")
(iter
 (for el :in '(a nil b c))
 (generate i :upfrom 1)
 (if el
	 (collect (cons el (next i)))))


(format t "Similar to \"for loop\" - but uses \"generate\"")
(iter
 (generating i :from 0 :to 3)
 (next i)
 (format t "~&~a" i))


;;;; Variable Binding and Setting:
(iter
 (for el in '(1 2 3 4))
 (counting (oddp el)))



;;; Binary Basics:
(defun read-u2 (in)
  (+ (* (read-byte in) 256)
	 (read-byte in)))



;;; Working on Fourier Transform:
