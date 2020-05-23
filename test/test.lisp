;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-
;;;
;;; Testing functions before adding them to original files
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

(in-package #:signal-test)


(defparameter *data* nil
  "doc")


;;; Reading and Writing to File:
(defun store-data-from-file (filename)
  "Reads the file and Print them."
  (declare (optimize (speed 3)))
  (with-open-file (fstream filename
                           :direction :input
                           :if-does-not-exist nil)	
	(setf *data* (iter
                   (for line = (read-line fstream nil))
                   (while line)
                   (collect line)))))

(defun create-local-filename (filename)
  (merge-pathnames filename
                   *current-directory*))


#+test
(time (store-data-from-file (create-local-filename "ex1.txt")))


;;; Write data to file:
(defun write-data-to-file (filename &key
                                      (data-given nil)
                                      (data nil)
                                      (start-x 0)
                                      (end-x 500))
  "Creates a file with a random or specified data."
  (when data-given
	(unless data
	  (error "Please provide data to write to the file.")))

  (when data-given
    (when data
	  (with-open-file (fstream filename
                               :if-exists :supersede
                               :direction :output)
		(iter
		  (for el :in data)
		  (format fstream "~&~A" el)))))
  
  (unless data-given
	(with-open-file (fstream filename
                             :if-exists :supersede
                             :direction :output)
	  (iter
		(for i :from start-x :to end-x)
		(format fstream "~&~A" i)))))



#+test
(write-data-to-file "/home/karthik/quicklisp/local-projects/learn-cl/ex1.txt"
                    :data-given nil
                    :data nil
                    :end-x 1000)


(defun parse-string-to-floats (string)
  (let ((*read-eval* nil)))
  (with-input-from-string (stream string)
    (iter)
    (for number = (read stream nil nil))
    (while number)
    (collect number)))


(defun parse-list-of-strings (list)
  (mapcan #'parse-string-to-floats list))


#+test
(parse-list-of-strings *data*)

