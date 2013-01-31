;;;; PERCEPTRON is a simple system for learning from examples which uses the
;;;; perceptron learning procedure to adjust a set of weights on a single
;;;; linear threshold unit until all of the training examples are correctly
;;;; classified. The perceptron convergence theorem assures that the system
;;;; will halt if the examples are linearly separable. However, the system
;;;; halts after *max-perceptron-epochs* in any case. If *loop-check* is
;;;; set it will check for repeated sets of weights which indicate an
;;;; infinite loop.  The perceptron cycling theorem guarantees this will happen
;;;; if it is not linearly separable; however, it is an expensive check.

;;;; The file BINARY-ENCODER contains the functions needed
;;;; for converting feature vector examples to bit strings

;;;; Copyright (c) 1988 by Raymond Joseph Mooney. This program may be freely 
;;;; copied, used, or modified provided that this copyright notice is included 
;;;; in each copy of this code and parts thereof.

;;;; RM: Changed 11/95 to handle instances directly in vector form.

(in-package :user)
(provide 'perceptron)
(eval-when (compile load eval)
 (require 'data-utilities (ml-progs-file "data-utilities"))
 (require 'binary-encoder (ml-progs-file "binary-encoder"))
)

(setf (get 'perceptron 'expect-training-error) t) ; Not guarantee training set correctness
(setf (get 'perceptron 'parameters) '(*tie-breaker* *max-perceptron-epochs* *loop-check*
				      *one-perceptron-for-pos* *one-bit-for-two-values*))

(defparameter *trace-perceptron* nil "Produces trace of weight updates if T")
(defparameter *trace-epoch*      nil "Just prints epoch number")
(defparameter *trace-multi-perceptron* nil "Prints output for each category")

(defparameter *max-perceptron-epochs* 5000 "Maximum number of epochs of training")
(defparameter *loop-check* nil             "Checks for repeated weight vector and terminates")
(defparameter *one-perceptron-for-pos* nil "For a pos/neg problem just make one perceptron")

(defvar *binary-encoded* nil "Remember that examples are already binary encoded")

(defstruct (perceptron (:print-function print-perceptron))
  weights threshold)

(defun train-perceptron (examples)
  (setf *binary-encoded* t)
  (unless (binary-encoded)
	  (make-encoding) (setf *binary-encoded* nil)
	  (setf examples  (mapcar #'convert-to-binary-example (make-ordered-examples examples))))
  (setf examples (eliminate-conflicting-examples examples))
  (if (and *one-perceptron-for-pos* (pos-neg?))
      (perceptron examples)
      (multi-perceptron examples)))

(defun binary-encoded ()
  (let ((result t))
    (dotimes (i (length *domains*) result)
	     (unless (equal (elt *domains* i) '(0 1))
		     (setf result nil)))))

(defun test-perceptron (example perceptron)
  (unless *binary-encoded*
	  (setf example (convert-to-binary-example (make-ordered-example example))))
  (if (perceptron-p perceptron)
      (if (compute-perceptron-output example perceptron) *positive-category* *negative-category*)
      (multi-perceptron-output example perceptron)))

(defun perceptron (examples &optional (threshold 0) (delta 1))
  ;;; Apply perceptron learning algorithm to the examples.  Iterates
  ;;; through all of the examples adjusting weights when system is wrong
  ;;; until all examples are classified correctly. Threshold and delta
  ;;; define the initial threshold of the perceptron and the learning
  ;;; increment. 

  (let* ((num-features (if (first examples)
			   (length (second (first examples)))
			   (length *binary-feature-names*)))
	 (weights (make-array (list num-features)    ; define weight vector
			      :element-type 'number  
			      :initial-element 0))   ; weights initalized to 0
	 (pos-category *positive-category*) (neg-category *negative-category*)
	 (all-correct nil) (i 0) (epoch-num 0) previous-weights instance
	 (perceptron (make-perceptron :weights weights :threshold threshold)))
    (trace-print *trace-perceptron* "~%~A" perceptron)
    ;; Loop until all examples are correct or exceed max epochs or detect loop
    (loop (cond (all-correct (trace-print *trace-perceptron* "~%~%Converged.") (return perceptron))
		((eql epoch-num *max-perceptron-epochs*)
		 (trace-print *trace-perceptron* "~%~%Exceeded maximum allowed epochs.")
		 (return perceptron))
		(*loop-check*
		 (let ((save (cons (perceptron-threshold perceptron) (copy-seq weights))))
		   (if (member save previous-weights :test #'equalp)
		     (progn (trace-print *trace-perceptron* "~%~%Loop detected, not linearly separable")
			    (return perceptron))
		     (push save  previous-weights)))))
	  (incf epoch-num)        ; Keep track of the number of trials
	  (trace-print *trace-perceptron* "~%~%Epoch ~D:" epoch-num)
	  (trace-print *trace-epoch* " ~D," epoch-num)
	  (setf all-correct t)            
	  (dolist (example examples)           ; Each trial look at all examples
	    (if (compute-perceptron-output example perceptron)
		(cond ((eq (first example) neg-category)  ; if network says + but its - example
		       (trace-print *trace-perceptron* "~%~%Classifies ~A wrong" example)
		       (setf all-correct nil)
		       (incf (perceptron-threshold perceptron) delta)	; Then increase threshold to make +
						                        ; classification harder
		       ;; and decrement weights for features present in the example
		       (setf i 0 instance (second example))
		       (if (listp instance)
			   (dolist (feature-value instance)
				   (unless (zerop feature-value)
					   (incf (aref weights i) (- (* delta feature-value)))
					   (trace-print *trace-perceptron*
							"~%Decrementing weight of feature ~A by ~,2F"
							(binary-feature-name i) (* delta feature-value)))
				   (incf i))
			 (dotimes (i num-features)
				   (unless (zerop (aref instance i))
					   (incf (aref weights i) (- (* delta (aref instance i))))
					   (trace-print *trace-perceptron*
							"~%Decrementing weight of feature ~A by ~,2F"
							(binary-feature-name i) (* delta feature-value))))))
		      (t (trace-print *trace-perceptron* "~%~%Classifies ~A right" example)))
		(cond ((eq (first example) pos-category)  ; if network says - but its +
		       (trace-print *trace-perceptron* "~%~%Classifies ~A wrong" example)
		       (setf all-correct nil)
		       (incf (perceptron-threshold perceptron) (- delta))   ; Then decrease threshold to make +
            						                    ; classification easier
		       ;; and increment weights for features present in the example
		       (setf i 0 instance (second example))
		       (if (listp instance)
			   (dolist (feature-value instance)
				   (unless (zerop feature-value)
					   (incf (aref weights i) (* delta feature-value))
					   (trace-print *trace-perceptron*
							"~%Incrementing weight of feature ~A by ~,2F"
							(binary-feature-name i) (* delta feature-value)))
				   (incf i))
			   (dotimes (i num-features)
				   (unless (zerop (aref instance i))
					   (incf (aref weights i) (* delta (aref instance i)))
					   (trace-print *trace-perceptron*
							"~%Incrementing weight of feature ~A by ~,2F"
							(binary-feature-name i) (* delta feature-value))))))
		      (t (trace-print *trace-perceptron* "~%~%Classifies ~A right" example)))))
	  (trace-print *trace-perceptron* "~%~A" perceptron))))


(defun compute-perceptron-output (example perceptron &optional analog)
  ;;; Determine value of perceptron for the given input. Return T or NIL
  ;;; instead of 0 or 1 to simply tests

  (let ((sum 0) (weights (perceptron-weights perceptron))
	(instance (second example)))
    ;; Simply sum the weight*input for all of the features
    ;; and return T if greater than threshold.
    (if (listp instance)
	(let ((i 0))
	  (dolist (feature-value instance)
		  (incf sum (* feature-value (aref weights i)))
		  (incf i)))
      (dotimes (i (length instance))
	       (incf sum (* (aref instance i) (aref weights i)))))
    (if analog
	(- sum (perceptron-threshold perceptron))
	(> sum (perceptron-threshold perceptron)))))


(defun print-perceptron (perceptron &optional (stream t) depth)
  ;; Printout the current weight vector and threshold
  (declare (ignore depth))
  (format stream "~%Weights:")
  (dotimes (i (length (perceptron-weights perceptron)))
    (format stream " ~A" (aref (perceptron-weights perceptron) i)))
  (format stream "~%Threshold: ~A" (perceptron-threshold perceptron)))

;;;---------------------------------------------------------------------------
;;;       Multi-category perceptron (1 perceptron for each category)
;;;---------------------------------------------------------------------------

(defun multi-perceptron (examples)
  (dolist (cat *categories*)
    (setf (get cat 'training-examples) nil))
  (dolist (example examples)
    (push (rest example) (get (first example) 'training-examples)))
  (mapcar #'(lambda (cat)
	      (trace-print *trace-multi-perceptron* "~%~%~A:" cat)
	      (let* ((training-examples
		       (append (category-examples cat '+)
			       (mapcan #'(lambda (other-cat)
					   (category-examples other-cat '-))
				       (remove cat *categories*))))
		     (*categories* '(+ -))
		     (*negative-category* '-) (*positive-category* '+)
		     (perceptron (perceptron training-examples)))
		(trace-print *trace-multi-perceptron* "~A" perceptron)
		(cons cat perceptron)))
	  *categories*))

(defun category-examples (cat label)
  (mapcar #'(lambda (inst) (cons label inst)) (get cat 'training-examples)))


(defun multi-perceptron-output (example perceptron-alist)
  "Pick category with maximum output above threshold"
  (let (output max-categories (max-output most-negative-fixnum))
    (trace-print *trace-perceptron* "~%")
    (dolist (pair perceptron-alist)
      (setf output (compute-perceptron-output example (cdr pair) t))
      (trace-print *trace-perceptron* "~%~A: ~A" (first pair) output)
      (cond ((> output max-output)
	     (setf max-output output)
	     (setf max-categories (list (first pair))))
	    ((= output max-output)
	     (push (first pair) max-categories))))
    (if (eq *tie-breaker* 'random)
	(pick-one max-categories)
	(dolist (cat *categories*)
	  (when (member cat max-categories)
	    (return cat))))))
