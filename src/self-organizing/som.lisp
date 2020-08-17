(in-package #:signal)


(defclass neuron ()
  ((x-position :initarg :x-position
               :initform 0
               :accessor x-position
               :type fixnum
               :documentation "Neuron X-location.")
   (y-position :initarg :y-position
               :initform 0
               :accessor y-position
               :type fixnum
               :documentation "Neuron X-location.")
   (weight-vector :initarg :weight-vector
           :accessor weight-vector
           :documentation "Weight-vector value"))
  (:documentation "Neuron Class. Neuron are the input data points. Each data point has x and y position values. The weight vectors contains the weight values for the number of input-vector neurons."))



(defclass som (neuron)
  ((map-size :initarg :map-size
             :accessor map-size
             :documentation "Entire SOM map size (shape). First element of the list is the row size and second is col size."
             :type list)
   (lattice-type :initarg :lattice-type
                 :accessor lattice-type
                 :initform 'rectangular
                 :documentation "Possible lattice types can be circular, rectangular and hexagonal")
   (radius-max :initarg :radius-max
               :accessor radius-max
               :documentation "Neighborhood initial radius")
   (radius-min :initarg :radius-min
               :initform 1.0
               :accessor radius-min
               :documentation "Neighborhood final radius")
   (distance-measure :initarg :distance-measure
                     :initform 'euclidean
                     :accessor distance-measure
                     :documentation "Finding shortest distance between input data and neurons. Possible measures: euclidean, manhattan."
                     :type symbol)
   (weight-initialization :initarg :weight-initialization
                          :initform 'random
                          :accessor weight-initialization
                          :documentation "Type of weight vector initialization."
                          :type symbol)
   (neurons :initarg :neurons
            :accessor neurons
            :documentation "2D array of neurons. The array shape is map-size.")
   (input-size :initarg :input-size
               :accessor input-size
               :type fixnum)
   (input-vector :initarg :input-vector
                 :accessor input-vector
                 :type vector
                 :documentation "Input vector (Raw data) for unsupervised clustering.")
   (num-iteration :initarg :num-iteration
                  :initform 100
                  :accessor num-iteration
                  :type fixnum)
   (learning-rate-start :initarg :learning-rate-start
                        :initform 0.5d0
                        :accessor learning-rate-start
                        :type double-float)
   (learning-rate-end :initarg :learning-rate-end
                        :initform 0.05d0
                        :accessor learning-rate-end
                        :type double-float)
   (neighborhood-distance-measure :initarg :neighborhood-distance-measure
                                  :initform 'pseudo-gaussian
                                  :accessor neighborhood-distance-measure
                                  :type symbol)
   (random-low :initarg :random-low
                :initform *low*
                :accessor random-low
                :documentation "Minimum value for random generators.")
   (random-high :initarg :random-high
                :initform *high*
                :accessor random-high
                :documentation "Maximum value for random generators.")
   (weight-value-type :initarg :weight-value-type
                      :initform 'double-float
                      :accessor weight-value-type
                      :documentation "Type of generated weight vectors.")
   (trained-p :initarg :trained-p
              :initform nil
              :accessor trained-p
              :type bool
              :documentation "Boolean flag to test training."))
  (:documentation "Self Organizing map Class."))



(defun make-neuron (x-pos y-pos input-vector-size &key (low *low*)
                                                    (high *high*)
                                                    (type 'double-float)
                                                    (weight-type 'random))
  "Constructor function for neuron class.
x-pos - SOM MAP, neuron x-position
y-pos - SOM MAP, neuron y-position
input-vector-size - Defines the weigh-vector size
"
  (with-gensyms (neuron)
    (when (equal weight-type 'random)
      (let ((weight-vector (create-random-vector input-vector-size :low low
                                                                   :high high
                                                                   :type type)))
        (setf neuron
              (make-instance 'neuron
                             :weight-vector weight-vector
                             :x-position x-pos
                             :y-position y-pos))))
    neuron))



(defmethod create-som-map (map-size input-vector-size &key (low *low*)
                                               (high *high*)
                                               (type 'double-float)
                                               (weight-type 'random))
  "Creates a SOM MAP."
  (unless (listp map-size)
    (error "The map size must be a list."))
  (with-gensyms (som-map)
    (let ((row (first map-size))
          (col (second map-size)))
      (setf som-map (make-array `(,row ,col)))
      ;;(format t "~&~A: ~A, ~A: ~A~&~%" "row" row "col" col)
      (dotimes (i row)
        (dotimes (j col)
          ;;(format t "~&~A: ~A, ~A: ~A~&~%" "row" i "col" j)
          (setf (aref som-map i j)
                (make-neuron i j input-vector-size :low low
                                                   :high high
                                                   :type type
                                                   :weight-type weight-type)))))
    som-map))



(defun make-som (map-size input-vector num-iteration &key (learning-rate-start 1d0)
                                                       (learning-rate-end 0.1d0)
                                                       (distance-measure 'euclidean)
                                                       (neighborhood-distance-measure 'pseudo-gaussian)
                                                       (lattice-type 'rectangular)
                                                       (weight-initialization 'random)
                                                       (random-low *low*)
                                                       (random-high *high*)
                                                       (weight-value-type 'double-float))
  "Constructor function for som class.
map-size - SOM Map row and column size.
input-vector - Raw data."
  (with-gensyms (som)
    (setf som
          (make-instance 'som
                         :map-size map-size
                         :input-vector input-vector
                         :num-iteration num-iteration
                         :learning-rate-start learning-rate-start
                         :learning-rate-end learning-rate-end
                         :distance-measure distance-measure
                         :neighborhood-distance-measure neighborhood-distance-measure
                         :lattice-type lattice-type
                         :weight-initialization weight-initialization
                         :random-low random-low
                         :random-high random-high
                         :weight-value-type weight-value-type))
    som))

(defun create-map (som-obj)
  "High level function to create a som map."
  (create-som-map (map-size som-obj)
                  (input-size som-obj)
                  :weight-type (weight-initialization som-obj)
                  :type (weight-value-type som-obj)
                  :high (random-high som-obj)
                  :low (random-low som-obj)))



(defun set-neighborhood-radius-boundary (som-obj)
  "Sets the neighborhood radius-max and radius-min of som-obj."
  (let ((map-size (map-size som-obj)))
    (setf (radius-max som-obj)
          (/ (max (first map-size)
                  (second map-size))
             2.0))
    (setf (radius-min som-obj) 1.0)))



(defmethod initialize-instance :after ((som-obj som) &key)
  (setf (input-size som-obj)
        (array-dimension (input-vector som-obj) 1))
  (setf (neurons som-obj)
        (create-map som-obj))
  (set-neighborhood-radius-boundary som-obj))


(defun print-som-object (som-obj)
  "To print the elements of the neuron-obj"
  (let ((neurons (neurons som-obj)))
    (dotimes (i (array-total-size neurons))
      (format t "~&~A: ~A" "x-location" (x-position (row-major-aref neurons i)))
      (format t "~&~A: ~A" "y-location" (y-position (row-major-aref neurons i)))
      (format t "~&Weight vector: ~& ~A" (weight-vector (row-major-aref neurons i))))))



(defun create-vector-with-value (x reference-vector)
  "Returns a vector with value x and same size as reference-vector.
example use:"
  (declare (type (simple-vector *) reference-vector))
  (make-array (array-total-size reference-vector)
              :initial-element x))



(defun vector-euclidean-distance (x-vector y-vector)
  "Returns the euclidean distance between x-vector and y-vector."
  ;(declare (type (simple-vector *) y-vector))
  (unless (vectorp x-vector)
    (setf x-vector
          (create-vector-with-value x-vector y-vector)))
  (unless (>= (length x-vector)
              (length y-vector))
    (error "~&~A length must be greater than or equal to the ~A ~%~A < ~A"
           "x-vector"
           "y-vector"
           x-vector
           y-vector))
  (let ((total-size-x (array-total-size x-vector)))
    (declare (fixnum total-size-x))
    (iter
      (for i :from 0 :below total-size-x)
      (collect (square (- (row-major-aref x-vector i)
                          (row-major-aref y-vector i))) into result result-type 'vector)
      (finally (return (sqrt (reduce #'+ result)))))))


(defun create-random-vector (vector-len &key (generator *generator*)
                                          (low *low*)
                                          (high *high*)
                                          (type 'double-float))
  "Returns a random vector with length of vector-len. 
generator - random-state:make-generator object
low - minimum value
high - maximum value"
  (with-gensyms (random-vector)
    (setf random-vector
          (make-array vector-len :initial-element 0.0))
    (ecase type
      (fixnum (iter
                (for  i :from 0 :below (array-total-size random-vector))
                (setf (row-major-aref random-vector i)
                      (random-state:random-int generator low high))))
      (double-float (iter
                      (for  i :from 0 :below (array-total-size random-vector))
                      (setf (row-major-aref random-vector i)
                            (random-state:random-float generator low high)))))
    random-vector))



(defun pick-random-sample-data (input-vector)
  "Returns the randomly chosen sample data and its corresponding location."
  (let ((random-idx (random-state:random-int
                     *generator*
                     0
                     (array-dimension input-vector 0))))
    (values
     (row-major-aref input-vector random-idx)
     random-idx)))


(defun generate-random-sample-data (data-shape &key (low *low*)
                                                 (high *high*)
                                                 (type 'fixnum))
  "Returns a random sample data with the specified data shape."
  (unless (or (listp data-shape)
              (numberp data-shape))
    (error "The data shape must be an integer or a list of integers."))
  (when (listp data-shape)
    (and (consp data-shape)
         (not (every #'numberp data-shape))
         (error "The data-shape of list must contain integers.")))
  (create-random-vector data-shape :low low :high high :type type))



(defun random-sample-data (sample-data &key (verbose nil))
    (multiple-value-bind (random-data index)
        (pick-random-sample-data sample-data)
      (when verbose
        (format t "~&~A: ~A" "random-data" random-data)
        (format t "~&~A: ~A" "index" index))
      index))





(defun decreasing-rate (start-sigma end-sigma num-iteration curr-iteration &key (mode 'exp))
  "Returns the decreasing rate."
  (cond ((equal mode 'exp)
         (exponential-decay start-sigma num-iteration curr-iteration))
        ((equal mode 'custom-exp)
         (custom-exponential-decay start-sigma end-sigma num-iteration curr-iteration))
        ((equal mode 'power)
         (power-decay start-sigma num-iteration curr-iteration))
        ((equal mode 'inverse)
         (inverse-decay start-sigma num-iteration curr-iteration))
        ((equal mode 'linear)
         (linear-decay start-sigma curr-iteration))
        (t (error "Invalid mode for decay function."))))



(defun linear-decay (start-sigma curr-iteration)
  "Linear decay."
  (/ start-sigma
     curr-iteration))


(defun exponential-decay (start-sigma num-iteration curr-iteration)
  "σ (t) = σ_0 * exp(-τ / λ)"
  (* start-sigma
     (exp  (/ (- curr-iteration)
              num-iteration))))


(defun inverse-decay (start-sigma num-iteration curr-iteration)
  ""
  (* start-sigma
     (1- (/ curr-iteration
            num-iteration))))


(defun power-decay (start-sigma num-iteration curr-iteration)
  "Power Series Decay"
  (* start-sigma
     (exp (/ curr-iteration
             num-iteration))))


(defun custom-exponential-decay (start-sigma end-sigma num-iteration curr-iteration)
  "doc"
  (* start-sigma
     (expt (/ end-sigma
              start-sigma)
           (/ curr-iteration
              num-iteration))))


(defun euclidean-distance (x-sequence y-sequence)
  "Returns the euclidean distance between x-sequence and y-sequence."
  (let ((total-size-x (array-total-size x-sequence)))
    (declare (fixnum total-size-x))
    (iter
      (for i :from 0 :below total-size-x)
      (collect (square (- (row-major-aref x-sequence i)
                          (row-major-aref y-sequence i))) into result result-type 'vector)
      (finally (return (sqrt (reduce #'+ result)))))))



(defun forbenius-norm (matrix)
  (sqrt
   (iter
     (for i :below (array-total-size matrix))
     (sum (expt (abs (row-major-aref matrix i)) 2)))))



(defun get-bmu (som-obj datapoint)
  "Returns the best matching unit for given som-obj."
  (let ((neurons-array (neurons som-obj)))
    (iter
      (for i :from 0 :below (array-total-size neurons-array))
      (collect (euclidean-distance (weight-vector (row-major-aref neurons-array i))
                                          datapoint) into result result-type 'vector)
      (finally (return (multiple-value-bind (location min-value)
                           (aops:argmin result)
                         (vector location min-value)))))))



(defun get-bmu-coordinates (som-obj bmu &key (verbose nil))
  "Returns the coordinates of the Best Matching Unit (BMU).
param:
som-obj - SOM Object
bmu-location - Result get-bmu function."
  (let ((map-size (first (map-size som-obj)))
        (bmu-location (aref bmu 0))
        (min-value (aref bmu 1)))
    (when verbose
      (format t "~&~A: ~A" "min-value" min-value))
    (multiple-value-bind (x y)
        (floor bmu-location map-size)
      (vector x y))))





(defun calculate-learning-rate (som-obj curr-iteration mode)
  ""
  (decreasing-rate (learning-rate-start som-obj)
                   (learning-rate-end som-obj)
                   (num-iteration som-obj)
                   curr-iteration
                   :mode mode))


(defun calculate-neighborhood-function (som-obj curr-iteration mode)
  "doc"
  (decreasing-rate (radius-max som-obj)
                   (radius-min som-obj)
                   (num-iteration som-obj)
                   curr-iteration
                   :mode mode))




(defun calculate-neighborhood-distance (som-obj winner-neuron-position)
  "Calculates the neighborhood distance for the winner neuron."
  (let* ((neurons-array (neurons som-obj))
         (num-neurons (array-total-size neurons-array)))
    (iter
      (for i :from 0 :below num-neurons)
      (collect (euclidean-distance 
                (vector (x-position (row-major-aref neurons-array i))
                        (y-position (row-major-aref neurons-array i)))
                (vector (aref winner-neuron-position 0)
                        (aref winner-neuron-position 1))) result-type 'vector))))



(defun calculate-neighborhood-weight (som-obj winner-neuron-position curr-iteration mode)
  "doc"
  (with-gensyms (neighborhood-distance)
    (setf neighborhood-distance
          (calculate-neighborhood-distance som-obj winner-neuron-position))
    (iter
    (for dist :in-sequence neighborhood-distance)
    (collect (exp (- (/ (expt dist 2)
                        (* 2
                           (expt
                            (calculate-neighborhood-function som-obj curr-iteration mode)
                            2)))))
      result-type 'vector))))



(defun update-weight (som-obj datapoint neighborhood-weight learning-rate)
  "doc"
  (let ((neurons-array (neurons som-obj)))
    (iter
      (for i :from 0 :below (array-total-size neurons-array))
      (setf (weight-vector (row-major-aref neurons-array i))
            (compute (α #'+
                        (weight-vector (row-major-aref neurons-array i))
                        (α #'* learning-rate
                           (α #'* (aops:sub neighborhood-weight i)
                              (elementwise-subtract datapoint
                                                    (weight-vector (row-major-aref neurons-array i)))))))))))


(defun train (som-obj)
  ""
  (let ((num-iteration (num-iteration som-obj)))
    (iter
      (for i :below num-iteration)
      (let* ((dp (random-sample-data (input-vector som-obj)))
             (datapoint (aops:sub (input-vector som-obj) dp))
             (bmu (get-bmu som-obj datapoint))
             (bmu-location (get-bmu-coordinates som-obj bmu))
             (learning-rate (calculate-learning-rate som-obj i 'exp))
             (neighborhood-weight (calculate-neighborhood-weight som-obj bmu-location i 'exp)))
        (update-weight som-obj datapoint neighborhood-weight learning-rate))
      (format t "~&~A: ~A" "Completed Iteration" i))
    (setf (trained-p som-obj) t)
    (format t "~&~A." "Completed Training")))



(defparameter *sample-data* (generate-random-sample-data '(20 39) :low 0
                                                                  :high 1
                                                                  :type 'double-float))


(defparameter *test* (make-som '(15 15) *sample-data* 100 :learning-rate-start 1.0
                                                          :learning-rate-end 0.05))



(time (train *test*))



(defparameter *res* (get-bmu *test* (aops:sub *sample-data* 1)))
(defparameter *bmu-location* (get-bmu-coordinates *test* *res*))
(print-som-object *test*)

(defparameter *ll* (calculate-learning-rate *test* 1 'custom-exp))
(calculate-neighborhood-function *test* 1 'linear)


(defparameter *dist* (calculate-neighborhood-distance *test* *bmu-location*))
(defparameter *dist-wg* (calculate-neighborhood-weight *test* *bmu-location* 1 'linear))


(defparameter *dp* (random-sample-data *sample-data*))
(defparameter *datapoint* (aops:sub *sample-data* *dp*))

(update-weight *test* *datapoint* *dist-wg* *ll*)
(print-som-object *test*)



(defun get-all-weight-vectors (som-obj)
  "doc"
  
  )
