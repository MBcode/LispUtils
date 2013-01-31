;;;; Changes to universal tester to handle multi-concept problems where
;;;; each example is to be classified into the correct category for several
;;;; different concepts, such a sentence having a value for agent, patient,
;;;; instrument, etc.  Coordinates building a separate classifier for each concept
;;;; and computes both average accuracy across all concepts and percentage
;;;; of examples for which all concepts are correct.  In this case, the class
;;;; of each example (in the first of the list) is a list of categories, one
;;;; for each concept in the order listed in *concepts*.

;;;; Also handles systems that can themselves deal with multi-concept prediction
;;;; by constructing one data structure for all concepts (e.g. backprop).
;;;; This is specified by having the MULTI property of the system name set to T.
;;;; Expects system to accept multi-concept examples directly and the test
;;;; funciton to return a LIST of categories (one per concept).

;;;; Copyright (c) 1991 by Raymond Joseph Mooney. This program may be freely copied, used, or
;;;; modified provided that this copyright notice is included in each copy of this code
;;;; and parts thereof.

(eval-when (compile load eval)
 (require 'universal-tester (ml-progs-file "universal-tester"))
 )

(defparameter *concepts* nil "List of concepts in multi-concept problem")
(defparameter *data-format* '(train-accuracy test-accuracy train-time test-time concept-complexity
			       train-complete-accuracy test-complete-accuracy))

(defun run-one-trial (total-training-examples training-increment test-examples systems
		     initial-training)
  (dolist (system systems)
    (setf (get system 'result) nil)
    (funcall-if-exist (append-symbols 'prepare-trial- system) total-training-examples test-examples))
  (do* ((length-examples (length total-training-examples))
	(previous-training-length 0 training-length)
	(training-length (or initial-training length-examples)
			 (if (listp training-increment)
			     (or (pop training-increment) (1+ length-examples))
			     (+ training-length training-increment)))
	(training-examples (subseq total-training-examples 0 training-length))
	(new-training-examples training-examples)
	(result-data) (trial-results))
       ((> training-length length-examples) (nreverse trial-results))
    (setf training-examples (subseq total-training-examples 0 training-length))
    (setf new-training-examples (nthcdr previous-training-length training-examples))
    (format t "~%~80~ ~%Test results for ~A training example~:P:" training-length)
    (push (cons (if initial-training training-length *training-length*)
		(loop for system in systems do
		      (setf result-data
			    (cond ((get system 'external)
				   (funcall (append-symbols 'train-and-test- system)
					    training-examples new-training-examples test-examples))
				  ((listp (first *categories*))
				   (if (get system 'multi)
				       (multi-train-and-test1 system training-examples new-training-examples
							      test-examples)
				       (multi-train-and-test system training-examples new-training-examples
							     test-examples)))
				  (t (train-and-test system training-examples new-training-examples test-examples))))
		      (setf (get system 'result-data) result-data)
		      (format t "~%~%Training Accuracy: ~,2F%   Test Accuracy: ~,2F%"
			      (get-field result-data 'train-accuracy) (get-field result-data 'test-accuracy))
		      (format t "~%Training Time: ~,2F sec   Test Time: ~,2F sec"
			      (get-field result-data 'train-time) (get-field result-data 'test-time))
		      (format t "~%Concept Complexity: ~A~%" (get-field result-data 'concept-complexity))
		      (if (and *training-error-dump* (not *noise*)
			       (not (= (get-field result-data 'train-accuracy) 100))
			       (not (get system 'expect-training-error)))
			  (dump-data system training-examples (get system 'result)))
		      collect result-data))
	  trial-results)))

(defun multi-train-and-test (system training-examples new-training-examples test-examples)
  (let ((training-function (append-symbols 'train- system))
	(test-function (append-symbols 'test- system))
	start-time training-result
	(result-data (make-list (length *data-format*))))
    (format t "~%~%Training ~A..." system)
    (setf start-time (get-internal-run-time))
    (setf training-result
	  (loop for i from 0 to (1- (length *concepts*)) collect
		(let ((*categories* (nth i *categories*)))
		  (case (get system 'incremental)
		    ((nil) (funcall training-function (loop for example in training-examples
							    collect (cons (nth i (first example)) (rest example)))))
		    ((full-data) 
		     (funcall training-function (loop for example in training-examples
						      collect (cons (nth i (first example)) (rest example)))
			      (nth i (get system 'result))))
		    ((t) 
		     (funcall training-function (loop for example in new-training-examples
						      collect (cons (nth i (first example)) (rest example)))
			      (nth i (get system 'result))))))))
    (set-field result-data 'train-time (seconds-since start-time))
    (setf (get system 'result) training-result)
    (trace-print *print-training-result* "~%Training-result: ~%~A" training-result)
    (set-field result-data 'concept-complexity (loop for result in training-result sum
						     (or (funcall-if-exist (append-symbols system '-concept-complexity)
									   result)
							 0)))
    (format t "Testing Training Data...")
    (multiple-value-bind (accuracy complete-accuracy)
	(get-multi-percent-correct test-function training-result training-examples)
      (set-field result-data 'train-accuracy accuracy)
      (set-field result-data 'train-complete-accuracy complete-accuracy))
    (format t "Testing Test Data...")
    (setf start-time (get-internal-run-time))
    (multiple-value-bind (accuracy complete-accuracy)
	(get-multi-percent-correct test-function training-result test-examples)
      (set-field result-data 'test-time (seconds-since start-time))
      (set-field result-data 'test-accuracy accuracy)
      (set-field result-data 'test-complete-accuracy complete-accuracy))
    result-data))

(defun get-multi-percent-correct (test-function training-result training-examples)
  (if training-examples
      (let ((num-concepts-right 0) (num-concepts 0) (num-right 0) (num-examples 0) error)
	(dolist (example training-examples (values (* 100.0 (/ num-concepts-right num-concepts))
						   (* 100.0 (/ num-right num-examples))))
	  (incf num-examples) (setf error nil)
	  (loop for i from 0 to (1- (length *concepts*)) do
		(incf num-concepts)
		(if (eq (funcall test-function example (nth i training-result)) (nth i (first example)))
		    (incf num-concepts-right)
		    (setf error t)))
	  (unless error (incf num-right))))
      (values 100 100)))


(defun print-multi-results (sys)
  (loop for result in (get sys 'result) as concept in *concepts*
	do
	(format t "~%~%Concept: ~A" concept)
	(funcall (append-symbols 'print- sys '-result)
		 result)))

(defun multi-train-and-test1 (system training-examples new-training-examples test-examples)
  (let ((training-function (append-symbols 'train- system))
	(test-function (append-symbols 'test- system))
	start-time training-result
	(result-data (make-list (length *data-format*))))
    (format t "~%~%Training ~A..." system)
    (setf start-time (get-internal-run-time))
    (setf training-result (case (get system 'incremental)
			    ((nil) (funcall training-function training-examples))
			    ((full-data) (funcall training-function training-examples (get system 'result)))
			    ((t) (funcall training-function new-training-examples (get system 'result)))))
    (set-field result-data 'train-time (seconds-since start-time))
    (setf (get system 'result) training-result)
    (funcall-if-exist (append-symbols 'train- system '-output) training-result training-examples)
    (trace-print *print-training-result* "~%Training-result: ~%~A" training-result)
    (set-field result-data 'concept-complexity (funcall-if-exist (append-symbols system '-concept-complexity)
								 training-result))
    (format t "Testing Training Data...")
    (multiple-value-bind (accuracy complete-accuracy)
	(get-multi-percent-correct1 test-function training-result training-examples)
      (set-field result-data 'train-accuracy accuracy)
      (set-field result-data 'train-complete-accuracy complete-accuracy))
    (format t "Testing Test Data...")
    (setf start-time (get-internal-run-time))
    (multiple-value-bind (accuracy complete-accuracy)
	(get-multi-percent-correct1 test-function training-result test-examples)
      (set-field result-data 'test-time (seconds-since start-time))
      (set-field result-data 'test-accuracy accuracy)
      (set-field result-data 'test-complete-accuracy complete-accuracy))
    (funcall-if-exist  (append-symbols 'test- system '-output) training-result test-examples)
    result-data))

(defun get-multi-percent-correct1 (test-function training-result training-examples)
  (if training-examples
      (let ((num-concepts-right 0) (num-concepts 0) (num-right 0) (num-examples 0) error)
	(dolist (example training-examples (values (* 100.0 (/ num-concepts-right num-concepts))
						   (* 100.0 (/ num-right num-examples))))
	  (incf num-examples) (setf error nil)
	  (loop for value in (funcall test-function example training-result) as correct-value in (first example) do
		(incf num-concepts)
		(if (eq value correct-value)
		    (incf num-concepts-right)
		    (setf error t)))
	  (unless error (incf num-right))))
      (values 100 100)))


