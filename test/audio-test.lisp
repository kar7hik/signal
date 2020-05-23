;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-
;;;
;;; Audio testing
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

;;; Writing WAV File:
;;; Copied from https://github.com/RyanTKing/cl-wave

;;; Functions to read/write binary files:
;; (iter
;;   (for i :below (* 4 8) :by 8)
;;   (format t "~&~A ~%" i)) => 0 8 16 24
;;;; same
;; (iter
;;   (for i :below 32 :by 8)
;;   (format t "~&~A ~%" i))



;; LDB => To extract from the middle. (load byte)

;; (Ex: Bytes => 2,
;;      (* Bytes 8) => (* 2 8) => 16
;;      by 4
;;      (0 4 8 12))
(defun write-uint (stream bytes uint)
  "Writes an unsigned integer to the stream with the specified number of bytes."
  (iter
    (for n :below (* bytes 8) :by 8)
    (write-byte (ldb (byte 8 n) uint) stream)))


(defun print-bits (n)
  (unless (integerp n)
    (error "The input must be an integer."))
  (format t "~%")
  (iter
    (for i :from 7 :downto 0)
    (format t "~A " (ldb (byte 2 i) n))
    ))

#+test
(print-bits 55)


(defun write-u2 (out value)
  (write-byte (ldb (byte 8 8) value) out)
  (write-byte (ldb (byte 8 0) value) out))

(defun write-uint (stream uint bytes)
  "Writes an unsigned integer to the stream with the specified number of bytes."
  (iter
    (for n :below (* bytes 8) :by 8)
    (write-byte (ldb (byte 8 n) uint) stream)))


#+test
(with-open-file (fstream "example.txt"
                         :direction :output
                         :if-exists :supersede
                         :element-type 'unsigned-byte)
  (write-uint fstream 63 2))

(defun write-sint (stream sint bytes)
  "Writes an signed integer to the stream with the specified number of bytes."
  (when (< sint 0)
    (incf sint (expt 2 (* bytes 8))))
  (iter
    (for n :below (* bytes 8) :by 8)
    (write-byte (ldb (byte 8 n) sint) stream)))


(defun signed-to-unsigned (value size)
  "Return the unsigned representation of a signed byte with a given size. Size is the number of BYTES."
  (ldb (byte size 0) value))



(defun unsigned-to-signed (value size)
  "Return the signed representation of an unsigned byte with a given size. Size is the number of BYTES."
  (if (logbitp (1- size) value)
      (dpb value (byte size 0) -1)
      value))



;;; integer-length - returns number of bits.
;;;  Returns the number of bits needed to represent integer in binary two's-complement format.

(defun print-bits-binary (integer)
  (let ((bits '()))
    (dotimes (position (integer-length integer) bits)
      (push (ldb (byte 8 position) integer)
            bits))))

#+test
(print-bits-binary 55)



;;; Working on Fourier Transform:
