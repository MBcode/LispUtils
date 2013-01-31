;;;; A propositional version of FOIL for learning DNF.
;;;; See Mooney, "Encouraging Results on Learning CNF," Machine Learning, to appear

;;;; Copyright (c) 1991 by Raymond Joseph Mooney. This program may be freely copied, used, or
;;;; modified provided that this copyright notice is included in each copy of this code
;;;; and parts thereof.

;;;; Currently only handles discrete-valued features.  Noise and missing data doesn't
;;;; cause any problems but there is no pruning.

(in-package :user)
(provide 'pfoil)
(eval-when (compile load eval)
 (require 'data-utilities (ml-progs-file "data-utilities"))
  )

(defparameter *trace-pfoil* nil)
(defparameter *trace-pfoil-gain* nil)
(defparameter *trace-pfoil-local-min* nil)


(defparameter *pfoil-min-examples* 2   "Minimum number of examples a clause is allowed to cover")
(defparameter *pfoil-min-accuracy* 0.7 "Minimum accuracy of a clause")

(setf (get 'pfoil 'parameters) '(*pfoil-min-examples* *pfoil-min-accuracy*))

(defun pfoil (pos-exs neg-exs)
  (setf pos-exs (copy-list pos-exs))
  (let (disjuncts)
    (loop (when (null pos-exs) (return (if (or disjuncts neg-exs) (nreverse disjuncts) (list nil))))
	  (multiple-value-bind (disjunct pos-exs-covered)
	      (learn-pfoil-disjunct pos-exs neg-exs)
	    (trace-print *trace-pfoil* "~%~%Learned Disjunct: ~A~%"
				     (mapcar #'(lambda (lit) (list (feature-name (first lit))
								   (second lit)))
					     disjunct))
	    (setf pos-exs (nset-difference pos-exs pos-exs-covered))
	    (if disjunct (push disjunct disjuncts))))))

(defun learn-pfoil-disjunct (pos-exs neg-exs)
  (if  (< (length pos-exs) *pfoil-min-examples*)
	  (return-from learn-pfoil-disjunct (values nil pos-exs)))
  (setf pos-exs (copy-list pos-exs) neg-exs (copy-list neg-exs))
  (let (term new-pos-exs new-neg-exs)
    (loop (when (null neg-exs) (return (values (nreverse term) pos-exs)))
	  (multiple-value-bind (feature value)
	      (pfoil-choose-feature pos-exs neg-exs)
	    (cond (feature
		   (trace-print *trace-pfoil* "~%Best feature: ~A=~A" (feature-name feature) value)
		   (setf new-pos-exs (delete-if-not #'(lambda (ex) (eq (elt ex feature) value))
						pos-exs))
		   (setf new-neg-exs (delete-if-not #'(lambda (ex) (eq (elt ex feature) value))
						neg-exs))
		   (if (< (length new-pos-exs) *pfoil-min-examples*)
		       (if (>= (/-float (length pos-exs) (+ (length pos-exs) (length neg-exs)))
			       *pfoil-min-accuracy*)
			   (return (values (nreverse term) pos-exs))
			 (return (values nil pos-exs)))
		     (progn (push (list feature value) term) (setf pos-exs new-pos-exs neg-exs new-neg-exs))))
		  (t (trace-print *trace-pfoil-local-min*
				  "~%No feature with gain.  Returning incomplete clause.")
		     (if (>= (/-float (length pos-exs) (+ (length pos-exs) (length neg-exs)))
			     *pfoil-min-accuracy*)
			 (return (values (nreverse term) pos-exs))
		       (return (values nil pos-exs)))))))))
			       
(defun pfoil-choose-feature (pos-exs neg-exs)
  (let ((max-gain 0) best-feature best-value gain
	(pos (length pos-exs)) (neg (length neg-exs)) (match-pos 0) (match-neg 0))
    (let ((info (- (log (/ pos (+ pos neg)) 2))))
      (trace-print *trace-pfoil-gain* "~%")
      (dotimes (feature (length *feature-names*))
	(dolist (value (feature-domain feature))
	  (setf match-pos 0 match-neg 0)
	  (dolist (pos-ex pos-exs)
	    (if (eq (elt pos-ex feature) value)
		(incf match-pos)))
	  (dolist (neg-ex neg-exs)
	    (if (eq (elt neg-ex feature) value)
		(incf match-neg)))
	  (when (> (setf gain (pfoil-gain pos neg match-pos match-neg info))
		   max-gain)
	    (setf max-gain gain
		  best-feature feature
		  best-value value))
	  (trace-print *trace-pfoil-gain* "~%~A=~A ~25TGain: ~,3F"
		       (feature-name feature) value gain)))
      (trace-print *trace-pfoil-gain* "~%")
      (values best-feature best-value))))

(defun pfoil-gain (pos neg match-pos match-neg &optional (info (- (log (/ pos (+ pos neg)) 2))))
  (if (= match-pos 0)
      0
      (* match-pos (+ info (log (/ match-pos (+ match-pos match-neg)) 2)))))
    
(defun train-pfoil (examples)
  (setf examples (make-ordered-examples examples))
  (dolist (cat *categories*)
    (setf (get cat 'training-examples) nil))
  (dolist (example examples)
    (push (second example) (get (first example) 'training-examples)))
  (let ((alist
	 (mapcar #'(lambda (cat)
		     (trace-print *trace-pfoil* "~%~%~%Category: ~A" cat)
		     (list cat (length (get cat 'training-examples))
			   (pfoil (get cat 'training-examples)
				  (mapcan #'(lambda (other-cat)
					      (copy-list (get other-cat 'training-examples)))
					  (remove cat *categories*)))))
		 (remove *negative-category* *categories*))))
    (cons (if (member *negative-category* *categories*)
	      *negative-category*
	      (maximum-category-label alist *categories*))
	  alist)))


(defun test-pfoil (example pfoil-result)
  (setf example (make-ordered-example example))
  (let ((class-counts (mapcan #'(lambda (alist-elt)
				  (if (match-pfoil (third alist-elt) (second example))
				      (list (cons (first alist-elt) (second alist-elt)))))
			      (rest pfoil-result ))))
    (if class-counts
	(maximum-label class-counts *categories*)
	(first pfoil-result))))


(defun match-pfoil (dnf inst)
  (loop for term in dnf do
	(when (every #'(lambda (lit) (eq (elt inst (first lit)) (second lit))) term)
	  (return t))))

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

(defun train-pfoilr (exs)
  (pfoil-rules (train-pfoil exs)))

(defun pfoil-rules (pfoil-result)
  (loop for x in (rest pfoil-result)
	nconc (let ((conse (list (first x))))
		(loop for clause in (third x) collect
		      (make-brule :consequent conse
				  :antecedents
				  (loop for lit in clause collect
					(list (feature-name (first lit))
					      (second lit))))))))

(defun literal-count (rules)
  (let ((sum 0))
    (loop for rule in rules do
	  (incf sum (1+ (length (brule-antecedents rule)))))
    sum))

(defun pfoil-concept-complexity (foil-result)
  (let ((sum 0))
    (dolist (alist-elt (rest foil-result) sum)
      (dolist (disjunct (third alist-elt))
	(incf sum (length disjunct))))))


(defun print-pfoil-result (pfoil-result)
  (loop for x in (rest pfoil-result) do
	(format t "~%~%~A: (~D)~%" (first x) (loop for term in (third x) sum (length term)))
	(if (null (third x))
	    (format t "FALSE")
	    (loop for term in (third x) do
		  (if (null term)
		      (format t "TRUE")
		      (progn
			(loop for lit in term do
			      (format t "~A=~A" (feature-name (first lit)) (second lit))
			      (unless (eq lit (first (last term)))
				(format t " & ")))
			(unless (eq term (first (last (third x))))
			  (format t " v ~%"))))))))

