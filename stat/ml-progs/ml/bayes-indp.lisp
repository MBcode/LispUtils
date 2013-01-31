;;;; A simple Bayes independence classification system. Uses a small epsilon instead of
;;;; zero for conditionals.  This frequently helps quite a bit.
;;;; See Weiss & Kulikowski Computer Systems that Learn book from Morgan Kaufman Pub.

;;;; Copyright (c) 1990 by Raymond Joseph Mooney. This program may be freely copied, used, or
;;;; modified provided that this copyright notice is included in each copy of this code
;;;; and parts thereof.

;;;; Modified 3/97 to include a Laplace correction for estimating parameters as recommended
;;;; by Kohavi, Becker, & Sommerfield ftp://starry.stanford.edu/pub/ronnyk/impSBC.ps.Z

(in-package user)
(provide 'bayes-indp)
(require 'data-utilities (ml-progs-file "data-utilities"))

(setf (get 'bayes-indp 'expect-training-error) t) ; expect training error with bayes 
(setf (get 'bayes-indp 'parameters) '(*bayes-laplace-correction* *bayes-epsilon*))

(defparameter *bayes-laplace-correction* t "Use Laplace correction when estimating conditional probabilities")
(defparameter *bayes-epsilon* 0.001 "To be used instead of 0 in conditional probabilities
 (alternative hack to Laplace corection)")

;;; Instance descriptions are ordered sequences of feature values (lists or arrays allowed,
;;; arrays tend to be more efficient)

(defun train-bayes-indp (examples)
  (setf examples (make-ordered-examples examples))
  (let ((num-examples (length examples))
	(class-counts (calculate-class-counts examples)))
    (list (calculate-class-priors examples num-examples class-counts)
	  (calculate-conditionals examples num-examples class-counts))))

(defun calculate-class-counts (examples)
  (let ((counts (make-array (length *categories*) :element-type 'integer :initial-element 0)))
    (dolist (ex examples counts)
      (incf (aref counts (position (first ex) *categories*))))))

(defun calculate-class-priors (examples num-examples class-counts)
  (let ((priors (make-array (length *categories*) :element-type 'single-float :initial-element 0.0)))
    (dotimes (i (length *categories*) priors)
      (setf (aref priors i) (if *bayes-laplace-correction*
				(/ (+ (aref class-counts i) (/-float 1 num-examples))
				   (+ num-examples (/-float (length *categories*) num-examples)))
				(/-float (aref class-counts i) num-examples))))))

(defun calculate-conditionals (examples num-examples class-counts)
  (let* ((num-features (length *feature-names*))
	 (conditionals (make-array num-features :element-type 'list)))
    (dotimes (feature-num num-features)
      (setf (aref conditionals feature-num)
	    (mapcar #'(lambda (val) (declare (ignore val))
			(make-array (length *categories*) :element-type 'single-float :initial-element 0.0))
		    (feature-domain feature-num))))
    (dolist (ex examples conditionals)
      (dotimes (feature-num num-features)
	(let* ((val (elt (second ex) feature-num))
	       (val-seq (elt conditionals feature-num))
	       (cat-seq (unless (eq val *missing-value*)
			  (elt val-seq (position val (feature-domain feature-num))))))
	  (when cat-seq (incf (aref cat-seq (position (first ex) *categories*)))))))
    (dotimes (f num-features conditionals)
     (let ((num-vals (length (aref conditionals f))))
      (dolist (ca (aref conditionals f))
	(dotimes (c (length *categories*))
	  (setf (aref ca c) 
		(if *bayes-laplace-correction*
		    (/ (+  (aref ca c) (/-float 1 num-examples))
		       (+  (aref class-counts c) (/-float num-vals num-examples)))
		    (if (and (zerop (aref ca c)) (zerop (aref class-counts c)))
			0.0
		      (/-float (aref ca c) (aref class-counts c)))))))))))

(defun test-bayes-indp (example train-result)
  (setf example (make-ordered-example example))
  (let* ((priors (first train-result))
	 (conditionals (second train-result)))
    (maximum-category-label
      (mapcar #'(lambda (cat)
		  (list cat (* (aref priors (position cat *categories*))
			       (multiply-conditionals (second example) cat conditionals))))
	      *categories*)
      *categories*)))

(defun maximum-category-label (count-alist &optional tie-breaker-list)
  "Returns the label in count-alist ((label . count) ...)
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


(defun multiply-conditionals (instance cat conditionals)
  (let ((product 1))
    (setf product (coerce product 'double-float))
    (dotimes (feature-num (length instance) product)
	(let* ((val (elt instance feature-num))
	       (val-seq (elt conditionals feature-num))
	       (cat-seq (unless (eq val *missing-value*)
			  (elt val-seq (position val (feature-domain feature-num)))))
	       (p (when cat-seq (aref cat-seq (position cat *categories*)))))
	  (when p (setf product (* product (if (= p 0) *bayes-epsilon* p))))))))


(make-variant bayes-indp0 bayes-indp ((*bayes-laplace-correction* nil)(*bayes-epsilon* 0)))
(make-variant bayes-indp-nlc bayes-indp ((*bayes-laplace-correction* nil)))
