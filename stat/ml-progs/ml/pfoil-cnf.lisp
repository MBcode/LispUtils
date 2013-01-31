;;;; A dual propositional version of FOIL for learning CNF.
;;;; See Mooney, "Encouraging Results on Learning CNF," Machine Learning, to appear

;;;; Copyright (c) 1991 by Raymond Joseph Mooney. This program may be freely copied, used, or
;;;; modified provided that this copyright notice is included in each copy of this code
;;;; and parts thereof.

;;;; Currently only handles discrete-valued features.  Noise and missing data doesn't
;;;; cause any problems but there is no pruning.

(in-package :user)
(provide 'pfoil-cnf)
(eval-when (compile load eval)
 (require 'data-utilities (ml-progs-file "data-utilities"))
 )

(defparameter *trace-pfoil-cnf* nil)
(defparameter *trace-pfoil-cnf-gain* nil)
(defparameter *trace-pfoil-cnf-local-min* nil)

(defun pfoil-cnf (pos-exs neg-exs)
  (setf neg-exs (copy-list neg-exs))
  (let (conjuncts)
    (loop (when (null neg-exs) (return (nreverse conjuncts)))
	  (multiple-value-bind (conjunct neg-exs-removed)
	      (learn-pfoil-conjunct pos-exs neg-exs)
	    (trace-print *trace-pfoil-cnf* "~%~%Learned Conjunct: ~A~%"
			 (mapcar #'(lambda (lit) (list (feature-name (first lit))
						       (second lit)))
				 conjunct))
	    (unless conjunct (if conjuncts
				 (return (nreverse conjuncts)) ; can find no more conjuncts
				 (list nil))) ; all negative
	    (push conjunct conjuncts)
	    (setf neg-exs (nset-difference neg-exs neg-exs-removed))))))

(defun learn-pfoil-conjunct (pos-exs neg-exs)
  (setf pos-exs (copy-list pos-exs) neg-exs (copy-list neg-exs))
  (let (clause)
    (loop (when (null pos-exs) (return (values (nreverse clause) neg-exs)))
	  (multiple-value-bind (feature value)
	      (pfoil-cnf-choose-feature pos-exs neg-exs)
	    (cond (feature
		   (trace-print *trace-pfoil-cnf* "~%Best feature: ~A=~A" (feature-name feature) value)
		   (push (list feature value) clause)
		   (setf pos-exs (delete-if #'(lambda (ex) (eq (elt ex feature) value))
					    pos-exs))
		   (setf neg-exs (delete-if #'(lambda (ex) (eq (elt ex feature) value))
					    neg-exs)))
		  (t (trace-print *trace-pfoil-cnf-local-min*
				  "~%No feature with gain.  Returning incomplete clause.")
		     (return (values (nreverse clause) neg-exs))))))))
			     
(defun pfoil-cnf-choose-feature (pos-exs neg-exs)
  (let ((max-gain 0) best-feature best-value gain
	(pos (length pos-exs)) (neg (length neg-exs)) (match-pos 0) (match-neg 0))
    (let ((info (- (log (/ neg (+ pos neg)) 2))))
      (trace-print *trace-pfoil-cnf-gain* "~%")
      (dotimes (feature (length *feature-names*))
	(dolist (value (feature-domain feature))
	  (setf match-pos 0 match-neg 0)
	  (dolist (pos-ex pos-exs)
	    (if (eq (elt pos-ex feature) value)
		(incf match-pos)))
	  (dolist (neg-ex neg-exs)
	    (if (eq (elt neg-ex feature) value)
		(incf match-neg)))
	  (when (> (setf gain (pfoil-CNF-gain pos neg match-pos match-neg info))
		   max-gain)
	    (setf max-gain gain
		  best-feature feature
		  best-value value))
	  (trace-print *trace-pfoil-cnf-gain* "~%~A=~A ~25TGain: ~,3F"
		       (feature-name feature) value gain)))
      (trace-print *trace-pfoil-cnf-gain* "~%")
      (values best-feature best-value))))

(defun pfoil-CNF-gain (pos neg match-pos match-neg &optional
			   (info (- (log (/ neg (+ pos neg)) 2))))
  (let ((not-match-neg (- neg match-neg)))
    (if (= not-match-neg 0)
	0
	(*  not-match-neg (+ info (log (/ not-match-neg
					  (+ not-match-neg (- pos match-pos))) 2))))))

(defun train-pfoil-cnf (examples)
  (setf examples (make-ordered-examples examples))
  (dolist (cat *categories*)
    (setf (get cat 'training-examples) nil))
  (dolist (example examples)
    (push (second example) (get (first example) 'training-examples)))
  (let ((alist
	 (mapcar #'(lambda (cat)
		     (trace-print *trace-pfoil-cnf* "~%~%~%Category: ~A" cat)
		     (list cat (length (get cat 'training-examples))
			   (pfoil-cnf (get cat 'training-examples)
				  (mapcan #'(lambda (other-cat)
					      (copy-list (get other-cat 'training-examples)))
					  (remove cat *categories*)))))
		 (remove *negative-category* *categories*))))
    (cons (if (member *negative-category* *categories*)
	      *negative-category*
	      (maximum-category-label alist *categories*))
	  alist)))


(defun test-pfoil-cnf (example pfoil-result)
  (setf example (make-ordered-example example))
  (let ((class-counts (mapcan #'(lambda (alist-elt)
				  (if (match-pfoil-cnf (third alist-elt) (second example))
				      (list (cons (first alist-elt) (second alist-elt)))))
			      (rest pfoil-result ))))
    (if class-counts
	(maximum-label class-counts *categories*)
	(first pfoil-result))))


(defun match-pfoil-cnf (cnf inst)
  (every #'(lambda (clause) (some #'(lambda (lit) (eq (elt inst (first lit)) (second lit)))
				  clause))
	 cnf))
	
(defun maximum-category-label (count-alist &optional tie-breaker-list)
  "Returns the label in count-alist ((label count) ...)
   with the maximum count.  Break ties according to *tie-breaker*"
  (let (max-labels (max-count 0))
    (dolist (count-cons count-alist)
      (cond ((> (second count-cons) max-count)
	     (setf max-count (second count-cons))
	     (setf max-labels (list (car count-cons))))
	    ((= (second count-cons) max-count)
	     (push (first count-cons) max-labels))))
    (if (or (eq *tie-breaker* 'random) (null tie-breaker-list))
	(pick-one max-labels)
	(dolist (item tie-breaker-list)
	  (when (member item max-labels)
	    (return item))))))

(defun train-pfoil-cnfr (exs)
  (pfoil-cnf-rules (train-pfoil exs)))

(defun pfoil-cnf-rules (pfoil-cnf-result)
  (loop for x in (rest pfoil-cnf-result)
	nconc (let (new-pred-rules)
		(cons 
		 (make-brule :consequent (list (first x))
			     :antecedents
			     (loop for clause in (third x)
				   collect (if (null (rest clause))
					       (list (feature-name (first (first clause)))
						     (second (first clause)))
					       (let ((new-pred (list (gensym "F"))))
						 (loop for lit in clause do
						       (push (make-brule :consequent new-pred
									 :antecedents
									 (list (list
										(feature-name
										 (first lit))
										(second lit))))
							     new-pred-rules))
						 new-pred))))
		 (nreverse new-pred-rules)))))
		  
		
(defun pfoil-cnf-concept-complexity (foil-result)
  (let ((sum 0))
    (dolist (alist-elt (rest foil-result) sum)
      (dolist (conjunct (third alist-elt))
	(incf sum (length conjunct))))))
  
(defun print-pfoil-cnf-result (pfoil-result)
  (loop for x in (rest pfoil-result) do
	(format t "~%~%~A: (~D)~%" (first x) (loop for clause in (third x) sum (length clause)))
	(if (null (third x))
	    (format t "TRUE")
	    (loop for clause in (third x) do
		  (if (null clause)
		      (format t "FALSE")
		      (progn
			(loop for lit in clause do
			      (format t "~A=~A" (feature-name (first lit)) (second lit))
			      (unless (eq lit (first (last clause)))
				(format t " v ")))
			(unless (eq clause (first (last (third x))))
			  (format t " & ~%"))))))))


