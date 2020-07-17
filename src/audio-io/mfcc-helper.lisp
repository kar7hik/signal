(in-package #:signal)


;;; MFCC
(defun set-frequency-values (mfcc-obj)
  "Setting the maximum and minimum frequency values. Max is half of the sampling rate."
  (setf (max-freq mfcc-obj)
        (floor (/ (sample-rate (audio-obj mfcc-obj))
                  2.0)))
  (setf (min-freq mfcc-obj) 0.0))


(defun frequency-to-mel-scale (freq)
  "Function to convert value from frequency to mel-scale."
  (* 1127 (log (1+ (/ freq 700)) (exp 1))))


(defun mel-scale-to-frequency (mel)
  "Function to convert value from mel-scale to frequency."
  (* 700 (- (exp (/ mel 1127)) 1)))


(defun fbank (min-freq max-freq n-fbank)
  "Returns the Filter bank bins."
  (aops:linspace min-freq (frequency-to-mel-scale max-freq) n-fbank))


(defun mel->frequency-sequence (seq)
  "Returns the list of frequency values corresponding to Mel scale."
  (iter
    (for i in-sequence seq)
    (collect (mel-scale-to-frequency i) result-type 'vector)))


(defun get-frequency-bin (seq &key (nfft *nfft*) (sample-rate *sample-rate*))
  "Returns the bin containing frequencies."
  (iter
    (for elt in-sequence seq)
    (collect (floor (/ (* (1+ nfft)
                          elt)
                       sample-rate)) result-type 'vector)))


(defun get-filter-banks (bin &key (nfft *nfft*))
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



(defun get-log-mel-features (filter-banks)
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
          (when (zerop (aref filter-banks i j))
            (setf (aref filter-banks i j) (coerce most-negative-double-float 'double-float)))
          (setf (aref log-banks i j)
                (log (aref filter-banks i j)))))
      log-banks)))



(defun get-dct-mel-features (dct-mel-values num-cepstrum)
  (let ((row (get-row dct-mel-values))
        (reduced-dct (compute (fuse dct-mel-values))))
    (iter
      (for i :from 0 :below row)
      (collect (aops:subvec (aops:sub reduced-dct i) 1 (1+ num-cepstrum)) result-type 'vector))))



(defun set-frequency-bins (mfcc-obj)
  (setf (bins mfcc-obj)
        (coerce-sequence
         (get-frequency-bin (mel->frequency-sequence (fbank (min-freq mfcc-obj)
                                                            (max-freq mfcc-obj)
                                                            (+ 2 (num-filters mfcc-obj)))))
         'single-float)))


(defun set-filter-banks (mfcc-obj)
  (setf (filter-banks mfcc-obj)
        (list-to-2d-array (get-filter-banks (bins mfcc-obj)))))


(defun set-log-mel-features (mfcc-obj)
  (let* ((raw-mult (matrix-multiplication (list-to-2d-array (power-spectrum (audio-obj mfcc-obj)))
                                          (compute (transpose (filter-banks mfcc-obj)))))
         (log-mel (get-log-mel-features raw-mult)))
    (setf (raw-mel-features mfcc-obj) raw-mult
          (log-mel-features mfcc-obj) log-mel)))


(defun set-dct-mel-features (mfcc-obj)
  (let* ((dct-mel (apply-dct (log-mel-features mfcc-obj)))
         (reduced-mel (aops:combine (get-dct-mel-features dct-mel (num-cepstrum mfcc-obj)))))
    (setf (dct-mel-values mfcc-obj) dct-mel
          (reduced-mel-values mfcc-obj) reduced-mel)))



(defun apply-dct (log-filter-banks)
  "Applies DCT-II on the filter banks."
  (let ((row (get-row log-filter-banks)))
    (iter
      (for i :from 0 :below row)
      (collect (dct:dct (aops:sub log-filter-banks i)) result-type 'vector))))



;;; Liftering:
(defun set-lifter (mfcc-obj)
  "Returns the vector containing the liftering values as the size of num-cepstrum."
  (destructuring-bind (num-frames num-cepstrum)
      (array-dimensions (reduced-mel-values mfcc-obj))
    (declare (ignore num-frames))
    (let ((lift (range 0 :stop (1- num-cepstrum))))
      (iter
        (for i :in-vector lift)
        (collect (1+ (* (/ (cepstrum-lifter mfcc-obj) 2)
                        (sin (* pi (/ i (cepstrum-lifter mfcc-obj))))))
          result-type 'vector)))))


(defun apply-liftering-to-dct (mfcc-obj)
  "The liftering is applied on the reduced MEL values."
  (let ((lift (set-lifter mfcc-obj)))
    (compute (alpha #'*
                    lift
                    (reduced-mel-values mfcc-obj)))))


(defun get-mean-normalization (2d-array)
  "Returns the mean values of the 2d-array with axis 0."
  (let ((arr-mean (2d-mean 2d-array)))
    (compute (alpha #'- 2d-array arr-mean))))
