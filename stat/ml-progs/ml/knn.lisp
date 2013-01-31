;;;; A simple version of a k nearest neighbor algorithm. Handles linear
;;;; features (by normalizing values to [0,1]) and missing features (dist is 0.5)

;;;; Copyright (c) 1991 by Raymond Joseph Mooney. This program may be freely 
;;;; copied, used, or modified provided that this copyright notice is included 
;;;; in each copy of this code and parts thereof.

(in-package :user)
(provide 'knn)
(require 'data-utilities (ml-progs-file "data-utilities"))

(setf (get 'knn 'parameters) '(*neighbors*))
(setf (get 'knn 'expect-training-error) t)

(defparameter *neighbors* 1)
(defparameter *trace-knn* nil)

(defvar *current-knn-domains* nil)

(defun train-knn (examples)
  (compute-linear-feature-spans)
  (make-ordered-examples examples))

(defun test-knn (example training-examples)
  (setf example (make-ordered-example example))
  (if (null training-examples)
      (pick-one *categories*)
      (let (dist k-closest temp)
	(dotimes (i *neighbors*) (push (cons nil most-positive-fixnum) k-closest))
	(dolist (training-example training-examples)
	  (setf dist (example-distance training-example example))
	  (when (< dist (rest (first k-closest)))
	    (setf temp (first k-closest))
	    (setf (first temp) (first training-example))
	    (setf (rest temp) dist)
	    (do ((k-rest k-closest (rest k-rest)))
		((or (null (rest k-rest))
		     (>= dist (rest (second k-rest))))
		 (setf (first k-rest) temp))
	      (setf (first k-rest) (second k-rest)))))
	(majority-class k-closest))))

(defun majority-class (k-closest)
  (let (counts count (max-count 0) max-category)
    (dolist (pair (nreverse k-closest))
      (setf count (assoc (first pair) counts))
      (if count
	  (incf (rest count))
	  (push (cons (first pair) 1) counts)))
    (dolist (pair counts max-category)
      (when (>= (rest pair) max-count)
	(setf max-count (rest pair))
	(setf max-category (first pair))))))

(defun example-distance (ex1 ex2)
  (let ((dist 0))
    (map nil #'(lambda (feature value1 value2)
		 (incf dist (feature-distance feature value1 value2)))
	 *feature-names* (second ex1) (second ex2))
    dist))

(defun feature-distance (feature value1 value2)
  (let ((span (get feature 'span)))
    (cond ((or (eq value1 *missing-value*) (eq value2 *missing-value*))
	   0.5)
	  (span (/-float (abs (- value1 value2)) span))
	  (t (if (eq value1 value2) 0 1)))))

(defun compute-linear-feature-spans ()
  (unless (eq *current-knn-domains* *domains*)
    (doseq (feature *feature-names*)
      (if (linear-feature-p feature)
	  (progn (unless (get feature 'range) (make-ranges (compute-ranges (list feature))))
		 (setf (get feature 'span) (- (second (get feature 'range))
					      (first (get feature 'range)))))
	  (remprop feature 'span)))
    (setf *current-knn-domains* *domains*)))


;;;; ==========================================================================================
;;;; Variant systems
;;;; ==========================================================================================

(make-variant nn  knn ((*neighbors* 1)))
(make-variant 3nn knn ((*neighbors* 3)))
(make-variant 5nn knn ((*neighbors* 5)))


