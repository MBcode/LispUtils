;;;; A basic neural-network backpropagation algorithm. Includes a momentum
;;;; term.  Trains until all outputs within a specified epsilon of their
;;;; target values or a specified number of epochs are completed.
;;;; Uses a fully connected network with one hidden layer. By default, the
;;;; number of hidden units is 10% of the total number of binary inputs + outputs.
;;;; Includes binary encoding for feature vectors and uses one output bit per
;;;; category and classifies examples into the category with highest output.
;;;; See chapter 8 of PDP1, especially pp. 322-335.

;;;; Copyright (C) 1988 by Jude William Shavlik and Raymond Joseph Mooney
;;;;       This program may be freely copied, used, or 
;;;;       modified provided that this copyright notice 
;;;;       is included in each copy of this code and parts thereof.

(in-package :user)
(provide 'backprop)
(eval-when (compile load eval)
 (require 'data-utilities (ml-progs-file "data-utilities"))
 ;(require 'binary-encoder (ml-progs-file "binary-encoder"))
 (require 'binary-encoder (ml-progs-file "binary-encoder.lisp"))
 )

(setf (get 'backprop 'expect-training-error) t) ; Not guarantee training set correctness
(setf (get 'backprop 'parameters) '(*eta* *alpha* *initial-wt-limit* *epsilon* *bp-conflicts*
				    *max-bp-epochs* *stop-when-highest* *num-hidden*
				    *one-bit-for-two-values*))


;;;; --------------------------------------------------------------------------------------
;;;;                       Global Variables
;;;; --------------------------------------------------------------------------------------

;;; Most of these are standard backprop parameters and self-explanatory.

;;; *bp-conflicts* says how to handle examples with exactly the same features but
;;; different categorizations (noise).  If NIL, nothing special is done.
;;; If REMOVE, it removes conflicting examples, keeping only ones that agree with the
;;; majority class.  If CHECK, it checks errors to see if they are on conflicting examples
;;; and stops training as soon as all nonconflicting examples are correct.

;;; *stop-when-highest* causes training to terminate as soon as the highest output
;;; bit (using one output bit per category) is always correct and at least *epsilon* greater
;;; than the next highest output. This is usually sufficient for categorization problems.

(defparameter *eta*              0.1 "Parameter that determines the learning rate.")
(defparameter *alpha*            0.90 "Scale factor for the momentum term.")
(defparameter *initial-wt-limit* 0.3  "The maximum magnitude of the initial wts.")
(defparameter *epsilon*          0.1 "Acceptable error for correct output vector.")
(defparameter *max-bp-epochs*   1000 "Maximum number of training epochs")
(defparameter *num-hidden*       nil "Number of hidden units, defaults to 10% (#input + #output)")
(defparameter *bp-conflicts*     nil "How to handle conflicting train exs (remove, check, or nil)")
(defparameter *stop-when-highest* t "Stop training when highest output is correct")

(defparameter *trace-backprop*     nil "Produce training trace")
(defparameter *trace-backprop-exs* nil "Report i/o behavior periodically")

(defvar *network*           nil "The saved learned network")
(defvar *most-positive-exponent* (log most-positive-long-float) 
                                "Used to prevent exponentation overflow.")
(defvar *most-negative-exponent* (log long-float-epsilon)
                                "Used to prevent exponentation underflow.")
(defvar *conflict-hash-table* (make-hash-table :test #'eq) "Used to mark conflicting examples")


(defstruct (network (:print-function network-printer))
  ;;; A PDP network, with one layer of hidden units.
  ;;; Input units fully connect to all of the hidden units and the hidden
  ;;; units fully connect to all of the output units.
  ;;; There are no direct connections between input and output units.
  ;;; Besides recording the network weights for inter-node links,
  ;;; the biases, activations, and error signals of nodes are recorded.
  ;;; In addition, the most recent changes (deltas) in the weights and
  ;;; biases are recorded.  These are used to compute a "momentum" term
  ;;; when adjusting weights and biases.  Empirically, networks often
  ;;; converge faster when the momentum term is used to "smooth" weight changes.
    input-to-hidden-wts
    hidden-to-output-wts
    input-to-hidden-deltas
    hidden-to-output-deltas
    hidden-unit-biases
    output-unit-biases
    hidden-bias-deltas
    output-bias-deltas
    hidden-activations
    output-activations
    hidden-unit-errors
    output-unit-errors)

;;;----------------------------------------------------------------------
;;; Interface TO RUN WITH UNIVERSAL TESTER 
;;;----------------------------------------------------------------------

(defun train-backprop (examples)
  (if (null examples)
    (pick-one *categories*)
    (progn
      (make-encoding)
      (if (eq *bp-conflicts* 'remove) (setf examples (eliminate-conflicting-examples examples)))
      (setf examples (mapcar #'convert-to-binary-example  (make-ordered-examples examples))) 
      ;; we need multiple outputs, one for each category. So here we loop through
      ;; the examples, and for each loop through the cats.
      (setf examples
	    (loop for example in examples collect
		  (cons
		   (mapcar #'(lambda (x) (if (eq x (first example)) 1 0)) *categories*)
		   (second example))))
      (let* ((num-inputs (length (rest (first examples))))
	     (num-outputs (length *categories*))
	     (num-hidden (round (* 0.1 (+ num-inputs num-outputs)))))
        (backprop examples (or *num-hidden* num-hidden))))))


(defun test-backprop (example bp-net)
  ;;--------------------------------------------------------------------------
  ;; Here, we either find the best classification and return that category for
  ;; the example, or else we choose randomly (ie, if there is no net)
  ;;--------------------------------------------------------------------------
  (if (symbolp bp-net)
	bp-net
    (let ((best-index 0))
      ;; first, convert the example to the correct format
      (setf example
	    (convert-to-binary-example (make-ordered-example example)))
      (activate-network bp-net (second example))
      (dotimes (i (length *categories*)) 
        (if (> (aref (network-output-activations bp-net) i)
	       (aref (network-output-activations bp-net) best-index))
	    (setf best-index i)))
      ;; return the category
      (nth best-index *categories*))))


;;;; --------------------------------------------------------------------------------------
;;;;                      THE MAIN FUNCTIONS
;;;; --------------------------------------------------------------------------------------


(defun backprop (examples &optional (hidden-units *num-hidden*) randomize-examples? 
                               (report-freq 10) (max-cycles *max-bp-epochs*)
                     &aux network numb-correct (cycle 0) error
                          (numb-of-examples (length examples))
                          (input-units  (length (rest  (first examples))))
                          (output-units (length (first (first examples)))))
  ;;; Train a PDP network with these example i/o pairs.  The CAR of
  ;;; each example is the desired output when the CDR is the input.  (The
  ;;; CAR must be a list, i.e. ((1) 0 1 0) is a properly formatted example.)
  ;;; The number of hidden units desired can be specified - the number of
  ;;; input and output units is determined by the form of each example.
  ;;; If RANDOMIZE-EXAMPLES? is set, during each training cycle the training
  ;;; examples are randomly mixed-up.  Training stops when the network correctly
  ;;; learns the training examples or the maximum number of cycles is reached.
  ;;; The REPORT-FREQ variable specifies how often the system reports its performance.
    (trace-print *trace-backprop* "~%backprop initializing...")
    (setf network (build-network input-units hidden-units output-units t))
    (setf *network* network)
    (when (eq *bp-conflicts* 'check)
      (clrhash *conflict-hash-table*)
      (loop for conflict-set in (find-conflicting-examples examples) do
	    (loop for conflict in conflict-set do
		  (setf (gethash conflict *conflict-hash-table*) t))))
    (loop
     (incf cycle)
     (if (zerop (mod cycle report-freq))
         (trace-print *trace-backprop* "~%Cycle ~A~%" cycle)
	(trace-print *trace-backprop* " ~D " cycle))
     (setf numb-correct 0 error nil) 
  ;;; In each cycle, go through all of the training examples, adjusting weights
  ;;; after each example is presented.  Count the number of correct classifications
  ;;; and report the precentage correct.  If requested (by the *trace-backprop* flag),
  ;;; show the results on the training examples.  To speed things up, only report
  ;;; results if the current cycle is a multiple of the reporting frequency.
     (dolist (example (if randomize-examples? (mix-up examples) examples))
       (let ((input-vector (rest example)) (desired-output (first example)))
         ;; Activate the current network, given this input.
         (activate-network network input-vector input-units hidden-units output-units)
         (let ((correct? (output-correct? desired-output network output-units)))
	   (when (and *trace-backprop-exs* (zerop (mod cycle (* 2 report-freq))))
	     ;;; Every other time performance reporting occurs, report i/o behavior.
	     (report-current-results network input-vector desired-output output-units correct?))
           (if correct?
	       (incf numb-correct)
	       (progn ; count as an error unless in conflict check mode and this example is conflicting
		 (unless (and (eq *bp-conflicts* 'check) (gethash example *conflict-hash-table*))
		   (setf error t))
		 (perform-back-propagation network input-vector desired-output 
					 input-units hidden-units output-units))))))
     (when (and *trace-backprop* (zerop (mod cycle report-freq)))
         ;; If this is a "special" cycle, report the current results of the network.
         (format t "Percentage correct = ~5,3F%~%" (* 100 (/ numb-correct numb-of-examples))))
     (if (or (not error) (>= cycle max-cycles))
         (return))) ; All classifications correct or "enough" training, so stop.
    (trace-print *trace-backprop* "~%~%There were ~A training cycles.~%~%" cycle)
    network)


(defun activate-network (network input-vector 
                         &optional (input-units  (length input-vector))
                                   (hidden-units (length (network-hidden-unit-biases network)))
                                   (output-units (length (network-output-unit-biases network))))
  ;;; Activate this network, given this input-vector.  (For efficiency reasons,
  ;;; the number of each type of network node is provided.)
   (dotimes (h hidden-units)  ; Set the activations of the hidden units.
     (setf (aref (network-hidden-activations network) h)
           (logistic-activation-function 
              (let ((answer 0)) ; Each hidden unit gets a weighted input from each input unit.
                (dotimes (i input-units answer)
                   (incf answer (* (elt input-vector i)
                                   (aref (network-input-to-hidden-wts network) i h)))))
              (aref (network-hidden-unit-biases network) h))))
   (dotimes (o output-units)  ; Set the activations of the output units.
     (setf (aref (network-output-activations network) o)
           (logistic-activation-function 
              (let ((answer 0)) ; Each output unit gets a weighted input from each hidden unit.
                (dotimes (h hidden-units answer)
                   (incf answer (* (aref (network-hidden-activations   network) h)
                                   (aref (network-hidden-to-output-wts network) h o)))))
              (aref (network-output-unit-biases network) o)))))



(defun perform-back-propagation (network input-vector desired-output-vector
                         &optional (input-units  (length input-vector))
                                   (hidden-units (length (network-hidden-unit-biases network)))
                                   (output-units (length (network-output-unit-biases network))))
  ;;; Perform back-propagation to modify the network's weights in order to improve
  ;;; future performance.  It is assumed that the LOGISTIC activation function
  ;;; is used (see page 329 of PDP1).  For "smoothing", a momentum term is included.
  ;;; The momentum can be cancelled by setting *alpha* to zero.
  ;;    First, determine the error signals.
   (dotimes (o output-units)  ; Determine the errors of the output units.
     (setf (aref (network-output-unit-errors network) o)
        (let ((actual-output (aref (network-output-activations network) o)))
           (* (- (elt desired-output-vector o) actual-output)
              actual-output (- 1 actual-output)))))
   (dotimes (h hidden-units)  ; Determine the errors of the hidden units.
     (setf (aref (network-hidden-unit-errors network) h)
        (let ((hidden-unit-activation (aref (network-hidden-activations network) h))
              (sum 0))
          (* hidden-unit-activation (- 1 hidden-unit-activation) 
              (dotimes (o output-units sum)
                 (incf sum (* (aref (network-output-unit-errors network) o)
                              (aref (network-hidden-to-output-wts  network) h o))))))))
  ;;    Reset the weights in the network.
   (dotimes (h hidden-units)
     (dotimes (o output-units) ; Update the weights from the hidden to the output units.
       (let ((delta (delta-rule (aref (network-output-unit-errors      network) o)
                                (aref (network-hidden-activations      network) h)
                                (aref (network-hidden-to-output-deltas network) h o))))
         ; Remember the weight momentum and update the weight.
         (setf (aref (network-hidden-to-output-deltas network) h o) delta)
         (incf (aref (network-hidden-to-output-wts    network) h o) delta)))
     (dotimes (i input-units) ; Update the weights from the input to the hidden units.
       (let ((delta (delta-rule (aref (network-hidden-unit-errors     network) h)
                                (elt input-vector i)
                                (aref (network-input-to-hidden-deltas network) i h))))
         (setf (aref (network-input-to-hidden-deltas network) i h) delta)
	 (incf (aref (network-input-to-hidden-wts    network) i h) delta))))
  ;;    Reset the biases in the network.
   (dotimes (o output-units) ; Update each output unit's bias
     (let ((delta (delta-rule (aref (network-output-unit-errors network) o)
                              1
                              (aref (network-output-bias-deltas network) o))))
         ; Remember the bias momentum and update the bias.
       (setf (aref (network-output-bias-deltas network) o) delta)
       (incf (aref (network-output-unit-biases network) o) delta)))
   (dotimes (h hidden-units) ; Update each hidden unit's bias.
     (let ((delta (delta-rule (aref (network-hidden-unit-errors network) h)
                              1
                              (aref (network-hidden-bias-deltas network) h))))
       (setf (aref (network-hidden-bias-deltas network) h) delta)
       (incf (aref (network-hidden-unit-biases network) h) delta))))


;;;; --------------------------------------------------------------------------------------
;;;;                               UTILITY FUNCTIONS
;;;; --------------------------------------------------------------------------------------


(defun delta-rule (error activation previous-delta)
  ;;; Determine the weight change specified by the delta learning rule.
  ;;; Include a momentum term to reduce the chance of oscillations.
  (when (< (abs error) 1.0E-20) (setf error 0))
  (when (< (abs previous-delta) 1.0E-20) (setf previous-delta 0))
  (+ (* *eta* error activation) 
      (* *alpha* previous-delta)))


(defun logistic-activation-function (total-weighted-input bias)
  ;;; A continuous, non-linear activation function is needed.
  ;;; It must be continuous if the generalized delta rule
  ;;; is to be used.  For hidden units to be beneficial, the
  ;;; activation function must also be non-linear.
  ;;; See equation 15 on page 329 of PDP1.
   (/ 1 (+ 1 (guarded-exp (- (+ total-weighted-input bias))))))


(defun guarded-exp (x)
  ;;; Prevent overflow during exponentiation.
   (cond ((<= x *most-negative-exponent*) long-float-epsilon)
         ((>= x *most-positive-exponent*) most-positive-long-float)
         (t (exp x))))


(defun build-network (input-units hidden-units output-units &optional randomize?)
  ;;; Construct a "feed-forward" PDP network with this many input, hidden, and output units.
  ;;; Each hidden unit is connected to each of the input units and each output unit is
  ;;; connected to each hidden unit.  There are no direct connections between input and
  ;;; output units.  If randomize? is set, the weights and biases are randomized.
   (make-network 
    :output-unit-biases      (build-pdp-array (list output-units) randomize?)
    :hidden-unit-biases      (build-pdp-array (list hidden-units) randomize?)
    :output-bias-deltas      (build-pdp-array (list output-units))
    :hidden-bias-deltas      (build-pdp-array (list hidden-units))
    :output-activations      (build-pdp-array (list output-units))
    :hidden-activations      (build-pdp-array (list hidden-units))
    :output-unit-errors      (build-pdp-array (list output-units))
    :hidden-unit-errors      (build-pdp-array (list hidden-units))
    :hidden-to-output-wts    (build-pdp-array (list hidden-units output-units) randomize?)
    :input-to-hidden-wts     (build-pdp-array (list input-units  hidden-units) randomize?)
    :hidden-to-output-deltas (build-pdp-array (list hidden-units output-units))
    :input-to-hidden-deltas  (build-pdp-array (list input-units  hidden-units))))



(defun build-pdp-array (dimensions &optional randomize?)
  ;;; Construct an array of the specified size.  If requested, randomize the elements.
   (if randomize? 
       (make-array dimensions
               :element-type 'long-float
               :initial-contents (make-random-array-contents dimensions))
       (make-array dimensions
               :element-type 'long-float
               :initial-element (coerce 0 'long-float))))


(defun make-random-array-contents (dimensions &aux temp)
  ;;; Construct a list representing an array of the specified dimensions.
  ;;; The elements of the array are randomly chosen.
   (if (null dimensions)
     (coerce (bp-random-interval (- *initial-wt-limit*) *initial-wt-limit*) 'long-float)
     (dotimes (i (first dimensions) temp)
        (push (make-random-array-contents (rest dimensions)) temp))))


(defun output-correct? (desired-output network output-units)
  ;;; Return T iff all of the ouputs are within epsilon of their desired values,
  ;;; or the highest output bit is correct and beats the next best by at least epsilon
  (if *stop-when-highest*
      (let (best-index (best-output 0) difference)
	(dotimes (i output-units)
	  (setf difference (- (aref (network-output-activations network) i) best-output))
	  (cond ((> difference *epsilon*)
		 (setf best-index i best-output (aref (network-output-activations network) i)))
		((<= difference (- *epsilon*)))
		((> difference 0.0) (setf best-index nil
					  best-output (aref (network-output-activations network) i)))
		(t (setf best-index nil))))
	(and best-index (= (nth best-index desired-output) 1)))
      (let ((all-right t))
	(dotimes (o output-units all-right)
	  (unless (< (abs (- (elt desired-output o) 
			     (aref (network-output-activations network) o)))
		     *epsilon*)
	    (setf all-right nil))))))


(defun report-current-results (network input-vector desired-output output-units correct?)
  ;;; Report how well the desired output matches the actual output.
   (trace-print *trace-backprop* " Input=~A~%  Desired Output=~A~%  Actual  Output=("
           input-vector desired-output)
   (dotimes (o output-units)
       (trace-print *trace-backprop* " ~9,8F" (aref (network-output-activations network) o)))
   (if (not correct?) (trace-print *trace-backprop* ")   X~%") (trace-print *trace-backprop* ")~%"))) ; "


;(defun compute-network-values (network instance)
;  ;;; Return the list of binary output values computed by the network given the
;  ;;; instance as input.

;  (activate-network network instance)
;  (let ((output nil))
;    (dotimes (o (length (network-output-unit-biases network)) (reverse output))
;      (if (< (- 1 (aref (network-output-activations network) o))
;	     *output-epsilon*)
;	  (push 1 output)
;	  (push 0 output)))))


(defun network-printer (network stream depth
                        &aux (input-units  (first (array-dimensions
                                              (network-input-to-hidden-wts network))))
                             (hidden-units (length (network-hidden-unit-biases network)))
                             (output-units (length (network-output-unit-biases network))))
  ;; Print this PDP network in an understandable format.
   (declare (ignore depth))
   (format stream "~%Network Contents~%")
   (dotimes (o output-units)
     (format stream "~% Output Unit ~A  -  Bias =  ~9,5F~%"
       o (aref (network-output-unit-biases network) o))
     (dotimes (h hidden-units) 
       (format stream "   Wt from hidden unit ~A = ~9,5F~%"
         h (aref (network-hidden-to-output-wts network) h o))))
   (dotimes (h hidden-units)
     (format stream "~% Hidden Unit ~A  -  Bias =  ~9,5F~%"
       h (aref (network-hidden-unit-biases network) h))
     (dotimes (i input-units) 
       (format stream "   Wt from input unit ~A  = ~9,5F~%"
         i (aref (network-input-to-hidden-wts network) i h)))))


(defun bp-random-interval (a b)
  ;;; Randomly chose a value between A and B.
   (+ a (* (- b a) (/ (random 1000000) 1000000))))



