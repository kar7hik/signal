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

(defparameter *NFFT* 1024
  "FFT bin value.")


(defclass fourier-transform ()
  ((fft-applied-p :initarg :fft-applied-p
                  :initform nil
                  :type 'boolean
                  :accessor fft-applied-p
                  :documentation "Flag to check whether the FFT has been applied already.")
   (NFFT :initarg :NFFT
         :initform *nfft*
         :accessor NFFT
         :type 'integer
         :documentation "Fourier Transform bin length.")
   (fft-result :initarg :fft-result
               :type (simple-array single-float)
               :accessor fft-result
               :documentation "List of simple arrays containing the result of FFT.")
   (pow-spectrum :initarg :pow-spectrum
                 :accessor pow-spectrum
                 :type 'list
                 :documentation "List of simple arrays - power spectrum values of fft-results."))
  (:documentation "Fourier Transform Class."))



(defclass signal-processing (fourier-transform)
  ((audio-data :initarg :audio-data
               :accessor audio-data
               ;:type 'double-float
               )
   (frame-size :initarg :frame-size
               :initform *frame-size*
               :accessor frame-size
               ;:type 'double-float
               )
   (frame-stride :initarg :frame-stride
                 :initform *frame-stride*
                 :accessor frame-stride
                 ;:type 'double-float
                 )
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
   (padded-frames :initarg :padded-frames
                 :accessor padded-frames)
   (filtered-frames :initarg :padded-frames
                    :accessor filtered-frames)
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
  (pre-emphasis-audio-data signal-processing-obj)
  (pad-audio-with-zeros signal-processing-obj)
  (setf (splitted-frames signal-processing-obj)
        (split-audio-into-frames (padded-frames signal-processing-obj)
                                 (frame-length signal-processing-obj)
                                 (frame-step signal-processing-obj)
                                 :padded-p t))
  (setf (filtered-frames signal-processing-obj)
        (apply-filter-to-audio (splitted-frames signal-processing-obj)))
  (save-fft signal-processing-obj)
  (find-power-spectrum signal-processing-obj)
  )



(defun split-padded-audio-into-frames (audio-data frame-length frame-step &key (verbose nil))
  (let* ((audio-data-length (length audio-data))
         (total-num-frames (floor (/ audio-data-length frame-step)))
         (result-list-frames '()))
    (iter
      (for i :from 0 :to (- total-num-frames 2))
      (let* ((start-idx (* i frame-step))
             (end-idx (+ frame-length start-idx))
             (splitted-frame '()))
        (when verbose
          (format t "~&~% start-idx: ~A ~%" start-idx)
          (format t "~& end-idx: ~A ~%" end-idx)
          (format t "~& total-num-frames: ~A and i: ~A  ~%" total-num-frames i)
          (format t "~& audio-data-length: ~A ~%" audio-data-length))
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


(defun apply-filter-to-frame (frame &key (filter-function 'hamming))
  "Function that applies the selected type of window to the frame."
  (let* ((frame-len (length frame))
         (filtered-frame (make-array frame-len)))
    ;; (format t "~& frame-len: ~A ~%" frame-len)
    (iter
      (for i :from 0 :below frame-len)
      (setf (aref filtered-frame i)
            (* (funcall filter-function i frame-len)
               (aref frame i)))
      ;;(format t "~&filtered frame: ~A" (aref filtered-frame i))
      )
    filtered-frame))

;; (apply-filter-to-frame (elt (splitted-frames *sig-obj*) 0))


(defun apply-filter-to-audio (splitted-frames &key (filter-function 'hamming))
  (let ((filtered-audio '()))
    (iter
      (for i :from 0 :below (length splitted-frames))
      (push (apply-filter-to-frame (elt splitted-frames i) :filter-function filter-function)
            filtered-audio))
    (nreverse filtered-audio)))



(defun apply-fft-to-frame (frame)
  "Given a frame, this function applies FFT to it."
  (alexandria:with-gensyms (fft-result)
    (setf fft-result
          (dft-analysis-core (pad-power-of-two frame)))
    fft-result))


(defun apply-fft-to-audio (filtered-audio-data)
  "Given a audio-data, the function returns the FFT of the it."
  (let ((fft-result nil)
        (total-num-frames (length filtered-audio-data)))
    ;;(format t "~& total-num-frames: ~A ~%" total-num-frames)
    ;;(format t "~& len filtered audio: ~A ~%" (length (first filtered-audio-data)))
    (iter
      (for i :from 0 :below total-num-frames)
      (push (apply-fft-to-frame (elt filtered-audio-data i))
            fft-result))
    ;;(format t "~& length of fft-result: ~A~%" (length (first fft-result)))
    (nreverse fft-result)))


(defun power-spectrum (frame NFFT)
  "Computes the power spectrum of frames."
  (iter
    (for elt :in-sequence frame)
    (collect (/ (expt elt 2) NFFT) result-type 'vector)))


(defun power-spectrum-audio (frames NFFT)
  "Given the frames, the function returns the Power spectrum."
  (iter
    (for frame :in-sequence frames)
    (collect (power-spectrum frame NFFT))))


(defgeneric pre-emphasis-audio-data (signal-processing-obj)
  (:documentation "Generic function to apply pre-emphasis on the audio data."))

(defmethod pre-emphasis-audio-data ((signal-processing-obj signal-processing))
  (setf (emphasized-signal signal-processing-obj)
        (pre-emphasis-filter (audio-data signal-processing-obj))))

(defgeneric pad-audio-with-zeros (signal-processing-obj)
  (:documentation "Finds the required padding for the audio signal and pads the zeros."))


(defmethod pad-audio-with-zeros ((signal-processing-obj signal-processing))
  (setf (padded-frames signal-processing-obj)
        (concatenate 'vector (emphasized-signal signal-processing-obj)
                     (zeros (padding-required signal-processing-obj) :type 'double-float))))


(defmethod save-fft ((signal-processing-obj signal-processing))
  (setf (fft-result signal-processing-obj)
        (apply-fft-to-audio (filtered-frames signal-processing-obj))))


(defmethod find-power-spectrum ((signal-processing-obj signal-processing))
  (setf (pow-spectrum signal-processing-obj)
        (power-spectrum-audio (fft-result signal-processing-obj)
                              (nfft signal-processing-obj))))

(defun make-signal-processing (audio-data)
  "Constructor function for MFCC Class."
  (alexandria:with-gensyms (signal-processing-obj)
    (setf signal-processing-obj (make-instance 'signal-processing
                                  :audio-data audio-data))
    signal-processing-obj))


(defmethod print-parameters-signal-processing ((signal-processing-obj signal-processing))
  (format t "~%~& sample-rate: ~A ~%" (sample-rate signal-processing-obj))
  (format t "~& frame-size:  ~A ~%" (frame-size signal-processing-obj))
  (format t "~& frame-step: ~A ~%" (frame-step signal-processing-obj))
  (format t "~& frame-length: ~A ~%" (frame-length signal-processing-obj))
  (format t "~& total-number-of-frames: ~A ~%" (total-number-of-frames signal-processing-obj))
  (format t "~& audio-length: ~A ~%" (audio-length signal-processing-obj))
  (format t "~& pad-audio-length: ~A ~%" (pad-audio-length signal-processing-obj))
  (format t "~& padding-required: ~A ~%" (padding-required signal-processing-obj)))


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
(defparameter *sig-obj* (make-signal-processing *audio-final*))

#+test
(plot-signal (emphasized-signal *signal-processing-obj*) "audio.png" :x-label "Samples"
                                                 :y-label "Amplitude"
                                                 :title "audio-data")
#+test
(print-parameters-signal-processing *signal-processing-obj*)
#+test
(defparameter *ex* (split-audio-into-frames (padded-frames *signal-processing-obj*)
                                            (frame-length *signal-processing-obj*)
                                            (frame-step *signal-processing-obj*)
                                            :padded-p t))
#+test
(defparameter *filtered* (apply-filter-to-audio *ex*))




(defun frequency-to-mel-scale (freq)
  "Function to convert value from frequency to mel-scale."
  (* 1127 (log (1+ (/ freq 700)) (exp 1))))


(defun mel-scale-to-frequency (mel)
  "Function to convert value from mel-scale to frequency."
  (* 700 (- (exp (/ mel 1127)) 1)))



;; (save-to-file "result-1.txt" (nth 56 (fft-result *signal-processing-obj*)))
;; (save-to-file "result-1.txt" (napa-fft:rfft #(0 1 2 3)))
;; (napa-fft:rfft #(1 2 3 4 5 6 7 8))


;; (plot-signal (third (pow-spectrum *signal-processing-obj*))
;;              "fft.png")

;; (plot-signal (dft-analysis-core (pad-power-of-two (third (filtered-frames *signal-processing-obj*))))
;;              "fft.png")

;; (plot-signal (napa-fft:rifft (dft-analysis-core (pad-power-of-two (third (filtered-frames *signal-processing-obj*)))))
;;              "fft.png")


(defun fbank (min-freq max-freq n-fbank)
  "Returns the Filter bank bins."
  (aops:linspace min-freq (frequency-to-mel-scale max-freq) n-fbank))


(defun mel->frequency-sequence (seq)
  "Returns the list of frequency values corresponding to Mel scale."
  (iter
    (for i in-sequence seq)
    (collect (mel-scale-to-frequency i) result-type 'vector)))


(defun frequency-bin (seq &key (nfft *nfft*) (sample-rate *sample-rate*))
  "Returns the bin containing frequencies."
  (iter
    (for elt in-sequence seq)
    (collect (floor (/ (* (1+ nfft)
                          elt)
                       sample-rate)) result-type 'vector)))


(defun triangular-window (begin middle end)
  "Returns the triangular window coordinates for the given input parameters."
  (iter
    (for k :in-sequence (range begin :stop end))
    (collect (cond
               ((or (< k begin) (> k end)) 0.0)
               ((<= k middle) (/ (- k begin)
                                 (- middle begin)))
               ((>= k middle) (/ (- end k)
                                 (- end middle)))) result-type 'vector)))


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


(defun filter-banks (bin &key (nfft *nfft*))
  "Returns the list of filter bank."
  (iter
    (for i :from 1 :below (- (length bin) 1))
    (let* ((begin (elt bin (- i 1)))
           (middle (elt bin i))
           (end (elt bin (1+ i)))
           (fbank (triangular-window begin middle end))
           (start (floor begin))
           (out (aops:zeros* 'single-float (/ nfft 2))))
      (iter
        (for i :from start :below (length out))
        (for j :in-sequence fbank)
        (setf (aref out i) j))
      (collect out))))



(defun list-to-2d-array (list)
  (make-array (list (length list)
                    (length (first list)))
              :initial-contents list))


(defun get-row (arr)
  (car (array-dimensions arr)))

(defun get-col (arr)
  (cadr (array-dimensions arr)))


(defun transpose (arr)
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


(defun matrix-multiplication (A B)
  (let* ((m (car (array-dimensions A)))
         (n (cadr (array-dimensions A)))
         (l (cadr (array-dimensions B)))
         (out (make-array `(,m ,l) :initial-element 0)))
    (iter
      (for i :from 0 :below m)
      (iter
        (for k :from 0 :below l)
        (setf (aref out i k)
              (iter
                (for j :from 0 :below n)
                (sum (* (aref A i j)
                        (aref B j k)))))))
    out))

(defun log-mel-features (filter-banks)
  "Applies log to the filter bank values."
  (with-gensyms (log-banks)
    (let ((row (get-row filter-banks))
          (col (get-col filter-banks)))
      (setf log-banks (make-array `(,row ,col) :element-type 'double-float
                                               :initial-element 0d0))
      (iter
        (for i :from 0 :below row)
        (iter
          (for j :from 0 :below col)
          (setf (aref log-banks i j)
                (* 20
                   (log (aref filter-banks i j) 10)))))
      log-banks)))


(defun apply-dct-mel-features (log-filter-banks)
  "Applies DCT-II on the filter banks and returns only the -filter-num- values."
  (let ((row (get-row log-filter-banks)))
    (iter
      (for i :from 0 :below row)
      (collect (dct:dct (aops:sub log-filter-banks i)) result-type 'vector))))


(defun dct-mel-features (log-filter-banks num-cepstrum)
  (let ((row (get-row log-filter-banks))
        (filter-banks (aops:combine log-filter-banks)))
    (iter
      (for i :from 0 :below row)
      (collect (aops:subvec (aops:sub filter-banks i) 1 (1+ num-cepstrum)) result-type 'vector))))


(defparameter *bin* (frequency-bin (mel->frequency-sequence (fbank 0 22050 42))))
(defparameter *data* (filter-banks (coerce-sequence *bin* 'single-float)))
(defparameter *arr* (list-to-2d-array *data*))
(defparameter *tra* (transpose *arr*))

(defparameter *arr1* (list-to-2d-array (pow-spectrum *sig-obj*)))
(defparameter *final* (matrix-multiplication *arr1* *tra*))
(defparameter *log-res* (log-mel-features *final*))
(defparameter *dct-res* (apply-dct-mel-features *log-res*))
(defparameter *dct-final* (aops:combine (dct-mel-features *dct-res* 12)))
(defparameter *mag* (list-to-2d-array (fft-result *sig-obj*)))
(defparameter *split* (splitted-frames *sig-obj*))
(defparameter *filt* (list-to-2d-array (filtered-frames *sig-obj*)))
(defparameter *emp* (emphasized-signal *sig-obj*))
(defparameter *pad* (padded-frames *sig-obj*))
(aops:sub *filt* 0)

(length (first (pow-spectrum *sig-obj*)))
(length (first (filtered-frames *sig-obj*)))
(length (first (splitted-frames *sig-obj*)))
(length (first (fft-result *sig-obj*)))
(length (padded-frames *sig-obj*))
(array-dimensions *arr1*)
(array-dimensions *arr*)
(length (first *data*))
(setf a '(1 2 3 4 5 6))

(defun apply-log (seq &key (base 10) (multiplier 1))
  (iter
    (for elt :in-sequence seq)
    (collect (* multiplier
                (log elt base)))))


(apply-log a :multiplier 20)
