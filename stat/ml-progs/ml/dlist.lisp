;;;; A regular decision list learner with a Foil-like heuristic

;;;; Copyright (c) 1995 by Raymond Joseph Mooney. This program may be freely copied, used, or
;;;; modified provided that this copyright notice is included in each copy of this code
;;;; and parts thereof.

;;;; Currently only handles discrete-valued features.  Noise and missing data doesn't
;;;; cause any problems but there is no pruning.

(in-package :user)
(provide 'dlist)
(require 'data-utilities (ml-progs-file "data-utilities"))

(setf (get 'dlist 'parameters) '(*dlist-min-accuracy*))

(defparameter *trace-dlist* nil)
(defparameter *trace-dlist-gain* nil)
(defparameter *trace-dlist-local-min* nil)

(defparameter *dlist-min-accuracy* 0.8)

(defun dlist (exs)
  (setf exs (copy-list exs))
  (let (rules)
    (loop (when (null exs) (if rules 
			       (return (nreverse rules)) 
			     (return (list (list (pick-one *categories*))))))
	  (multiple-value-bind (rule exs-covered dont-covers)
	      (learn-dlist-rule exs)
	    (trace-print *trace-dlist* "~%~%Learned Rule: ~A~%"
			 (nconc  (list (first rule) '<-)
				 (mapcar #'(lambda (lit) (list (feature-name (first lit))
							       (second lit)))
					 (rest rule))))
	    (unless rule (return rules))
	    (when dont-covers
		  (let ((num-covered (length exs-covered)))
		    (if (>= (/-float num-covered (+ num-covered (length dont-covers)))
			    *dlist-min-accuracy*)
			(trace-print *trace-dlist* "~%Rule incomplete but worth keeping.")
		      (progn (setf rule nil)
			     (trace-print *trace-dlist*
			           "~%Discarding rule as too inaccurate and removing covered examples.")))))
	    (when rule (push rule rules))
	    (setf exs (nset-difference exs exs-covered))))))

(defun learn-dlist-rule (exs)
  (let* ((category (majority-category exs))
	 (dont-cover 
	  (remove-if #'(lambda (ex) (eq (example-category ex) category)) exs))
	 body)
    (setf exs (remove-if-not #'(lambda (ex) (eq (example-category ex) category)) exs))
    (loop (when (null dont-cover) (return (values (cons category (nreverse body)) exs nil)))
	  (multiple-value-bind (feature value)
	      (dlist-choose-feature exs dont-cover)
	    (cond (feature
		   (trace-print *trace-dlist* "~%Best feature: ~A=~A" (feature-name feature) value)
		   (push (list feature value) body)
		   (setf exs (delete-if-not #'(lambda (ex) (eq (feature-value feature ex) value))
					    exs))
		   (setf dont-cover (delete-if-not #'(lambda (ex) (eq (feature-value feature ex) value))
						   dont-cover)))
		  (t (trace-print *trace-dlist-local-min*
				  "~%No feature with gain.  Returning incomplete clause.")
		     (return (values (cons category (nreverse body)) exs dont-cover))))))))
			       
(defun dlist-choose-feature (pos-exs neg-exs)
  (let ((max-gain 0) best-feature best-value gain
	(pos (length pos-exs)) (neg (length neg-exs)) (match-pos 0) (match-neg 0))
    (let ((info (- (log (/ pos (+ pos neg)) 2))))
      (trace-print *trace-dlist-gain* "~%")
      (dotimes (feature (length *feature-names*))
	(dolist (value (feature-domain feature))
	  (setf match-pos 0 match-neg 0)
	  (dolist (pos-ex pos-exs)
	    (if (eq (feature-value feature pos-ex) value)
		(incf match-pos)))
	  (dolist (neg-ex neg-exs)
	    (if (eq (feature-value feature neg-ex) value)
		(incf match-neg)))
	  (when (> (setf gain (dlist-gain pos neg match-pos match-neg info))
		   max-gain)
	    (setf max-gain gain
		  best-feature feature
		  best-value value))
	  (trace-print *trace-dlist-gain* "~%~A=~A ~25TGain: ~,3F"
		       (feature-name feature) value gain)))
      (trace-print *trace-dlist-gain* "~%")
      (values best-feature best-value))))

(defun dlist-gain (pos neg match-pos match-neg &optional (info (- (log (/ pos (+ pos neg)) 2))))
  (if (= match-pos 0)
      0
      (* match-pos (+ info (log (/ match-pos (+ match-pos match-neg)) 2)))))
    
(defun train-dlist (examples)
  (setf examples (make-ordered-examples examples))
  (dlist examples))

(defun test-dlist (example dlist)
  (loop for rule in dlist 
	when (every #'(lambda (lit) (eq (feature-value (first lit) example) (second lit))) (rest rule))
	do  (return (first rule))))

(defun dlist-concept-complexity (dlist)
  (loop for rule in dlist sum (length rule)))

(defun print-dlist-result (dlist)
  (loop for rule in dlist do
	(format t "~%~A <-" (first rule))
	(if (rest rule)
	    (dolist (lit (rest rule))
		    (format t " ~A=~A" (feature-name (first lit)) (second lit))
		    (if (eq lit (first (last rule)))
			(format t ".")
		      (format t ",")))
	  (format t " TRUE."))))
  

