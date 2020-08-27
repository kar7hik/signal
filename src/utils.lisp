;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-
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


;; (declaim (ftype (function ((simple-array * (* *)))
;;                           (simple-array single-float (* *))) transpose%))
(defun transpose% (arr)
  (declare (type (simple-array * (* *)) arr))
  (declare (optimize (speed 3)))
  (let* ((row (get-row arr))
         (col (get-col arr))
         (tran (make-array `(,col ,row) :element-type 'single-float
                                        :initial-element 0.0)))
    (dotimes (i row)
      (dotimes (j col)
        (setf (aref tran j i)
              (aref arr i j))))
    tran))


(declaim (ftype (function ((simple-array * (* *))
                           (simple-array * (* *)))
                          (simple-array * (* *))) matrix-multiplication))
(defun matrix-multiplication (arr1 arr2)
  (declare (type (simple-array * (* *)) arr1 arr2))
  (declare (optimize (speed 3) (debug 0) (safety 0) (space 0)))
  (setf lparallel:*kernel* (lparallel:make-kernel *num-worker-threads*))
  (let* ((m (the fixnum (car (array-dimensions arr1))))
         (n (the fixnum (cadr (array-dimensions arr1))))
         (l (the fixnum (cadr (array-dimensions arr2))))
         (arr-type (array-element-type arr1))
         (out (make-array `(,m ,l) :element-type arr-type)))
    (declare (type fixnum m n l))
    (pdotimes (i m)
      (declare (fixnum i))
      (dotimes (k l)
        (declare (fixnum k))
        (setf (aref out i k)
              (iter
                (for j :from 0 :below n)
                (declare (fixnum j))
                (the arr-type
                     (sum (* (the arr-type (aref arr1 i j))
                             (the arr-type (aref arr2 j k)))))))))
    out))


;; (matrix-multiplication #2A((1.0 2.0) (3.0 4.0))
;;                        #2A((1.0 2.0) (3.0 4.0)))

;; (matrix-multiplication #2A((1d0 2d0) (3d0 4d0))
;;                        #2A((1d0 2d0) (3d0 4d0)))

;; (matrix-multiplication #2A((1 2) (3 4))
;;                        #2A((1 2) (3 4)))

;; (declaim (ftype (function (matrix matrix &optional (or null fixnum) (or null fixnum) (or null fixnum)) matrix) matprod))
(defun matprod (fst snd)
  (declare (optimize (speed 3) (debug 0) (space 0) (safety 0)))
  (setf lparallel:*kernel* (lparallel:make-kernel *num-worker-threads*))
  (destructuring-bind (m n) (array-dimensions fst)
    (destructuring-bind (n1 l) (array-dimensions snd)
      (assert (and (= n n1)
                   (eql (array-element-type fst) (array-element-type fst))))
      (let ((prod (make-array (list m l) :element-type (array-element-type fst))))
        (pdotimes (i m)
          (dotimes (j l)
            (setf (aref prod i j)
                  (loop :for k :from 0 :below n
                        :sum (the integer (* (the integer (aref fst i k))
                                             (the integer (aref snd k j))))))))
        prod))))



;; (time (matprod (list-to-2d-array (power-spectrum (audio-obj *mfcc-obj*)))
;;                (transpose (filter-banks *mfcc-obj*))))




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



(defun apply-log (seq &key (base 10) (multiplier 1))
  (iter
    (for elt :in-sequence seq)
    (collect (* multiplier
                (log elt base)))))



(defun product (x &optional axis)
  (compute (β* #'* 1 x axis)))


(defun addition (x &optional axis)
  (compute (β* #'+ 0 x axis)))


(defun elementwise-subtract (a b)
  (compute
   (α #'- a b)))


(defun 2d-mean (2d-array &key (axis 0) (type 'single-float))
  (let ((divisor (if (zerop axis)
                     (get-row 2d-array)
                     (get-col 2d-array))))
    (compute (alpha #'/ (addition 2d-array axis)
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





(defun matrix-multiply-mat-mat (b c &optional a)
  (declare (optimize (speed 3)))
  (let ((l (array-dimension b 0))		; rows of B
	    (m (array-dimension c 0))		; rows of C
	    (n (array-dimension c 1)))		; cols of C
    (declare (fixnum l m n))
    (or a
        (setf a (make-array (list l n)
                            :element-type 'single-float
                            :initial-element 0.0)))
    
    (if (or (not (= l (array-dimension a 0)))
	        (not (= n (array-dimension a 1)))
	        (not (= m (array-dimension b 1))))
	    (error "MATRIX-MULTIPLY given bad dimensions"))
    
    (do ((i 0 (1+ i)))
	    ((>= i l) a)
      (declare (fixnum i))
      (do ((j 0 (1+ j))
	       (sum 0.0 0.0))
	      ((>= j n))
	    (declare (fixnum j)
		         (float sum))
	    (do ((k 0 (1+ k)))
	        ((>= k m))
	      (declare (fixnum k))
	      (setq sum (+ sum (* (aref b i k)
			                  (aref c k j)))))
	    (setf (aref a i j) sum)))))


;;;; Multiply ROW x MAT and MAT x COL

;;; ROW * MAT
;;; 
(defun matrix-multiply-row-mat (b c &optional a)
  (declare (optimize (speed 3)))
  (let ((n (array-dimension c 1))
	    (m (array-dimension b 0)))
    (declare (fixnum m n))
    (or a
        (setf a (make-array n :element-type 'single-float
                              :initial-element 0.0)))
    (do ((i 0 (1+ i)))
	    ((>= i n) a)
      (declare (type fixnum i))
      (do ((j 0 (1+ j))
	       (sum 0.0))
	      ((>= j m)
	       (setf (aref a i)
                 sum)
	       nil)
	    (declare (fixnum j)
		         (float sum))
	    (setf sum (+ sum (* (aref b j)
			                (aref c j i))))))))

;;; MAT * COL
;;;
(defun matrix-multiply-mat-col (b c &optional a)
  (declare (optimize (speed 3)))
  (let ((n (array-dimension b 0))
	    (m (array-dimension c 0)))
    (declare (fixnum m n))

    (or a
        (setf a (make-array n :element-type 'single-float
                              :initial-element 0.0)))
    (do ((i 0 (1+ i)))
	    ((>= i n) a)
      (declare (fixnum i))
      (do ((j 0 (1+ j))
	       (sum 0.0))
	      ((>= j m)
	       (setf (aref a i) sum)
	       nil)
	    (declare (fixnum j)
		         (float sum))
	    (setf sum (+ sum (* (aref b i j)
			                (aref c j))))))))


;;;; Multiply Matrix by a Scalar

;;; b is a scalar, c is a scalar, vector, or matrix

(defun matrix-multiply-num-mat (b c &optional a)
  (declare (optimize (speed 3)))
  (cond ((numberp c) (* b c))
	    ((= (array-rank c) 1)
         ;; times a vector
	     (or a
             (setf a (make-array (array-dimension c 0) :element-type 'single-float
                                                       :initial-element 0.0)))
	     (do ((i 0 (1+ i))
	          (n (array-dimension c 0)))
	         ((>= i n) a)
	       (declare (fixnum i n))
	       (setf (aref a i)
		         (* b (aref c i)))))
	    ((= (array-rank c) 2)
         ;; times a matrix
	     (or a
             (setf a
                   (make-array (list (array-dimension c 0) (array-dimension c 1))
                               :element-type 'singlefloat
                               :initial-element 0.0)))
	     (do ((i 0 (1+ i))
	          (n (array-dimension c 0)))
	         ((>= i n) a)
	       (declare (fixnum i n))
	       (do ((j 0 (1+ j))
		        (m (array-dimension c 0)))
	           ((>= j m))
	         (declare (fixnum j m))
	         (setf (aref a i j)
		           (* b (aref c i j))))))
	    (t (error "MATMUL-SCALAR not given vector or matrix"))))
  
;;;; Matrix Multiply

;;; There are several varieties of matrix multiply:
;;;
;;; scalar x matrix --> matrix		MATRIX-MULTIPLY-NUM-MAT
;;; matrix x matrix --> matrix		MATRIX-MULTIPLY-MAT-MAT
;;; row    x column --> scalar		MATRIX-MULTIPLY-ROW-COL
;;; column x row    --> matrix		MATRIX-MULTIPLY-COL-ROW
;;; row    x matrix --> row		MATRIX-MULTIPLY-ROW-MAT
;;; matrix x column --> column		MATRIX-MULTIPLY-MAT-COL

(defun matrix-multiply (b c &optional a)
  (declare (optimize (speed 3)))  
  (cond ((numberp b) (matrix-multiply-num-mat b c a))
	    ((numberp c) (matrix-multiply-num-mat c b a))
	    ((and (= 2 (array-rank b))
	          (= 2 (array-rank c))
	          (= (array-dimension b 1)
		         (array-dimension c 0)))
	     (matrix-multiply-mat-mat b c a))
	    ((and (= 1 (array-rank b))
	          (= 2 (array-rank c))
	          (= (array-dimension b 0)
		         (array-dimension c 0)))
	     (matrix-multiply-row-mat b c a))
	    ((and (= 2 (array-rank b))
	          (= 1 (array-rank c))
	          (= (array-dimension b 1)
		         (array-dimension c 0)))
	     (matrix-multiply-mat-col b c a))
	    (t (error "MATMUL blew it"))))



;; (declaim (ftype (function ((simple-array * (* *))
;;                            fixnum)
;;                           (vector *)) array-slice))
(defun array-slice (arr index)
  (declare (type (simple-array * (* *)) arr)
           (fixnum index))
  (when (< index 1)
    (error "Invalid index"))
  (when (>= index (the fixnum (get-row arr)))             
    (error "Invalid index"))
  (let* ((col (the fixnum (get-col arr)))
         (location (the fixnum (* col index)))
         (elem-type (the symbol (array-element-type arr))))
    (declare (fixnum col)
             (symbol elem-type))
    (make-array col
                :element-type elem-type
                :displaced-to arr
                :displaced-index-offset location)))



#+test
(progn (defparameter *ra* (aops:rand* 'double-float '(100 100)))
       (time (array-slice *ra* 15))
       (time (aops:sub *ra* 86)))




