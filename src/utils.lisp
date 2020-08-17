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
    (collect i result-type 'vector)))


(defun range (start &key (stop nil stop-supplied-p) (step 1))
  "Creates an array for given start and stop conditions. default step value is 1. The resulting array will be the type of step."
  (if stop-supplied-p
      (range-helper start stop step)
      (range-helper 0 start step)))


(defun create-file-path (filename)
  "Creates a file path from the filename."
  (merge-pathnames filename
                   *current-directory*))


(defun create-data-file-path (filename)
  "Creates a file path from the filename."
  (merge-pathnames filename
                   *local-data-directory*))


(defun create-result-file-path (filename)
  "Creates a file path from the filename."
  (merge-pathnames filename
                   *local-result-directory*))


(defun coerce-sequence (seq conversion-type)
  (iter
    (for elt :in-sequence seq)
    (alexandria:coercef elt conversion-type)
    (collect elt result-type 'vector)))



(defun dot-product (a b)
  "Function returns the dot-product of the input sequences."
  ;; (declare (optimize (speed 3))
  ;;          (type (simple-array single-float) a b))
  (reduce #'+ (map 'simple-vector #'* a b)))




(defun list-to-2d-array (list)
  (make-array (list (length list)
                    (length (first list)))
              :initial-contents list))


(defun get-row (arr)
  (car (array-dimensions arr)))

(defun get-col (arr)
  (cadr (array-dimensions arr)))


(defun coerce-to-matrix (x)
  (setf x (lazy-array x))
  (trivia:ematch (shape x)
    ((~)
     (reshape x (~ 1 ~ 1)))
    ((~l (list range))
     (reshape x (~ 1 (range-size range) ~ 1)))
    ((~l (list range-1 range-2))
     (reshape x (~ 1 (range-size range-1) ~ 1 (range-size range-2))))))


(defun coerce-to-scalar (x)
  (setf x (lazy-array x))
  (trivia:ematch (shape x)
    ((~) x)
    ((~ i)
     (reshape x (make-transformation :input-mask (vector i) :output-rank 0)))
    ((~ i ~ j)
     (reshape x (make-transformation :input-mask (vector i j) :output-rank 0)))))


(defun matmul (A B)
  (β #'+
     (α #'*
        (reshape (coerce-to-matrix A) (τ (m n) (n m 1)))
        (reshape (coerce-to-matrix B) (τ (n k) (n 1 k))))))


(defun dot (x y)
  (coerce-to-scalar
   (matmul
    (transpose x)
    (coerce-to-matrix y))))



(defun transpose (x)
  (reshape
   (coerce-to-matrix x)
   (τ (m n) (n m))))



(defun transpose% (arr)
  (let* ((row (get-row arr))
         (col (get-col arr))
         (tran (make-array (list col row))))
    (iter
      (for i :from 0 :below row)
      (iter
        (for j :from 0 :below col)
        (setf (aref tran j i)
              (aref arr i j))))
    tran))


;; (defun matrix-multiplication (A B)
;;   ;; (declare (type (simple-array double-float *) A B))
;;   (let* ((m (car (array-dimensions A)))
;;          (n (cadr (array-dimensions A)))
;;          (l (cadr (array-dimensions B)))
;;          (out (make-array `(,m ,l) :initial-element 0d0
;;                           :element-type 'double-float)))
;;     (declare (type fixnum m n l))
;;     (iter
;;       (for i :from 0 :below m)
;;       (iter
;;         (for k :from 0 :below l)
;;         (setf (aref out i k)
;;               (iter
;;                 (for j :from 0 :below n)
;;                 (sum (* (aref A i j)
;;                         (aref B j k)))))))
;;     out))


(defun matrix-multiplication (A B)
  (setf lparallel:*kernel* (lparallel:make-kernel *num-worker-threads*))
  (let* ((m (car (array-dimensions A)))
         (n (cadr (array-dimensions A)))
         (l (cadr (array-dimensions B)))
         (out (make-array `(,m ,l) :initial-element 0.0
                          :element-type 'single-float)))
    (declare (type fixnum m n l))
    (pdotimes (i m)
      (dotimes (k l)
        (setf (aref out i k)
              (iter
                (for j :from 0 :below n)
                (sum (* (aref A i j)
                        (aref B j k)))))))
    out))



(defun save-2d-array-to-file (filename array)
  (with-open-file (fstream
                   (create-result-file-path filename)
                   :direction :output
                   :if-exists :supersede)
    (let ((rows (get-row array))
          (cols (get-col array)))
      (iter
        (for row :from 0 :below rows)
        (iter
          (for col :from 0 below cols)
          (format fstream "~10A" (aref array row col)))
        (format fstream "~&~%")))))






(declaim (ftype (function (matrix matrix &optional (or null fixnum) (or null fixnum) (or null fixnum)) matrix) matprod))
(defun matprod (fst snd &optional d1 d2 d3)
  (declare (optimize (speed 3) (debug 0) (space 0) (safety 0)))
  (if d1
     (let ((prod (make-array (list d1 d3) :element-type (array-element-type fst))))
        (loop :for i :from 0 :below d1 :do
          (loop :for j :from 0 :below d3 :do
            (setf (aref prod i j)
                  (loop :for k :from 0 :below d2
                        :sum (the integer (* (the integer (aref fst i k)) (the integer (aref snd k j))))))))
      prod)
     (destructuring-bind (m n) (array-dimensions fst)
       (destructuring-bind (n1 l) (array-dimensions snd)
         (assert (and (= n n1)
                      (eql (array-element-type fst) (array-element-type fst))))
         (let ((prod (make-array (list m l) :element-type (array-element-type fst))))
             (loop :for i :from 0 :below m :do
               (loop :for j :from 0 :below l :do
                 (setf (aref prod i j)
                       (loop :for k :from 0 :below n
                             :sum (the integer (* (the integer (aref fst i k)) (the integer (aref snd k j))))))))
           prod)))))



;; (time (matprod (list-to-2d-array (power-spectrum (audio-obj *mfcc-obj*)))
;;                (transpose (filter-banks *mfcc-obj*))))




(defun apply-log (seq &key (base 10) (multiplier 1))
  (iter
    (for elt :in-sequence seq)
    (collect (* multiplier
                (log elt base)))))



(defun product (x &optional axis)
  (compute (β* #'* 1 x axis)))


(defun sum (x &optional axis)
  (compute (β* #'+ 0 x axis)))


(defun elementwise-subtract (a b)
  (compute
   (α #'- a b)))


(defun 2d-mean (2d-array &key (axis 0) (type 'single-float))
  (let ((divisor (if (zerop axis)
                     (get-row 2d-array)
                     (get-col 2d-array))))
    (compute (alpha #'/ (sum 2d-array axis)
                    (coerce divisor type)))))


(defun square (value)
  (* value value))

(defun square-sequence (input-array)
  (declare (type (simple-array * *) input-array))
  (with-gensyms (square-result)
    (setf square-result (alexandria:copy-array input-array))
    (dotimes (i (array-total-size square-result))
      (setf (row-major-aref square-result i)
            (square (row-major-aref square-result i))))
    square-result))



(defun sqrt-sequence (input-array)
  (declare (type (simple-array * *) input-array))
  (with-gensyms (sqrt-result)
    (setf sqrt-result (alexandria:copy-array input-array))
    (dotimes (i (array-total-size sqrt-result))
      (setf (row-major-aref sqrt-result i)
            (sqrt i)))))
