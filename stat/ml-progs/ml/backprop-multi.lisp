;;;; Additions to backprop for handling multi-concept prediction problems.
;;;; Builds one network for predicting all concept each having one output
;;;; bit per category and assigning for each concept it's category value with
;;;; the highest value.

(in-package :user)
(setf (get 'backprop 'multi) t)

(defun train-backprop (examples)
  (if (listp (first *categories*))
      (train-backprop-multi examples)
      (train-backprop1 examples)))

(defun test-backprop (example net)
  (if (listp (first *categories*))
      (test-backprop-multi example net)
      (test-backprop1 example net)))

(defun train-backprop1 (examples)
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


(defun test-backprop1 (example bp-net)
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

(defun train-backprop-multi (examples)
  (if (null examples)
    (loop for cats in *categories* collect (pick-one cats))
    (progn
      (make-encoding)
      (if (eq *bp-conflicts* 'remove) (setf examples (eliminate-conflicting-examples examples)))
      (setf examples (mapcar #'convert-to-binary-example  (make-ordered-examples examples))) 
      ;; we need multiple outputs, one for each category. So here we loop through
      ;; the examples, and for each loop through the cats.
      (setf examples
	    (loop for example in examples collect
		  (cons
		   (mapcan #'(lambda (cats value)
			       (mapcar #'(lambda (x) (if (eq x value) 1 0)) cats))
			   *categories* (first example))
		   (second example))))
      (let* ((num-inputs (length (rest (first examples))))
	     (num-outputs (length (first (first examples))))
	     (num-hidden (round (* 0.1 (+ num-inputs num-outputs)))))
        (backprop examples (or *num-hidden* num-hidden))))))


(defun test-backprop-multi (example bp-net)
  ;;--------------------------------------------------------------------------
  ;; Here, we either find the best classification and return that category for
  ;; the example, or else we choose randomly (ie, if there is no net)
  ;;--------------------------------------------------------------------------
  (if (not (network-p bp-net))
      bp-net
      (let ((best-index 0)(i 0)(best-cat 0))
	;; first, convert the example to the correct format
	(setf example
	      (convert-to-binary-example (make-ordered-example example)))
	(activate-network bp-net (second example))
	(loop for cats in *categories* collect
	      (progn (setf best-index i best-cat (first cats))
		     (loop for cat in cats do
			   (if (> (aref (network-output-activations bp-net) i)
				  (aref (network-output-activations bp-net) best-index))
			       (setf best-index i best-cat cat))
			   (incf i))
		     best-cat)))))

(defun output-correct? (desired-output network output-units)
  ;;; Return T iff all of the ouputs are within epsilon of their desired values,
  ;;; or the highest output bit is correct and beats the next best by at least epsilon
  (if *stop-when-highest*
      (if (listp (first *categories*))
	  (let ((all-right t)(i 0))
	    (loop for cats in *categories* collect
		  (let (best-index (best-output 0) difference)
		    (loop for cat in cats do
			  (setf difference (- (aref (network-output-activations network) i) best-output))
			  (cond ((> difference *epsilon*)
				 (setf best-index i best-output (aref (network-output-activations network) i)))
				((<= difference (- *epsilon*)))
				((> difference 0.0) (setf best-index nil
							  best-output (aref (network-output-activations network) i)))
				(t (setf best-index nil)))
			  (incf i))
		    (unless (and best-index (= (nth best-index desired-output) 1))
		      (setf all-right nil))))
	    all-right)
	  (let (best-index (best-output 0) difference)
	    (dotimes (i output-units)
	      (setf difference (- (aref (network-output-activations network) i) best-output))
	      (cond ((> difference *epsilon*)
		     (setf best-index i best-output (aref (network-output-activations network) i)))
		    ((<= difference (- *epsilon*)))
		    ((> difference 0.0) (setf best-index nil
					      best-output (aref (network-output-activations network) i)))
		    (t (setf best-index nil))))
	    (and best-index (= (nth best-index desired-output) 1))))
      (let ((all-right t))
	(dotimes (o output-units all-right)
	  (unless (< (abs (- (elt desired-output o) 
			     (aref (network-output-activations network) o)))
		     *epsilon*)
	    (setf all-right nil))))))

