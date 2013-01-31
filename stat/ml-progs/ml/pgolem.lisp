;;;; A propositional version of GOLEM for learning DNF.

;;;; Copyright (c) 1996 by Raymond Joseph Mooney. This program may be freely copied, used, or
;;;; modified provided that this copyright notice is included in each copy of this code
;;;; and parts thereof.

;;;; Currently only handles discrete-valued features.  Noise and missing data doesn't
;;;; cause any problems but there is no pruning other than a minimum clause coverage parameter.

;;;; Missing data really needs to be handled better in LGGs

;;;; "Memorizes" lone positive examples with very specific disjunct unless min-coverage
;;;; prevents it.

;;;; Includes partial match function for use in testing.  Category chosen with highest partial
;;;; score in this case. If there is a negative category for which rules are not learned,
;;;; then a threshold is determined for each non-negative category, if the highest category
;;;; does not reach it's threshold then negative is assigned by default.  Threshold is
;;;; determined as midway between lowest scoring positive and highest scoring negative for
;;;; that category with midpoint-thresholder.

(in-package :user)
(provide 'pgolem)
(eval-when (compile load eval)
 (require 'data-utilities (ml-progs-file "data-utilities"))
 )

(defparameter *trace-pgolem* nil)

(defparameter *pgolem-sample-size* 10 "Size of random sample used to generate generalizations")
(defparameter *pgolem-min-examples* 1   "Minimum number of examples a clause is allowed to cover")

;;; Partial match parameters
(defparameter *pgolem-closest-match* t   "Use closest match instead of logical match")
(defparameter *pgolem-matcher* 'max-avg-term   "Partial match function to use")
(defparameter *pgolem-thresholder* 'midpoint-thresholder  "Determine thresholder")

(setf (get 'pgolem 'parameters) '(*pgolem-sample-size* *pgolem-min-examples* *pgolem-closest-match* *pgolem-matcher* 
				  *pgolem-thresholder* ))

(defun pgolem (pos-exs neg-exs)
  (setf pos-exs (copy-list pos-exs))
  (let (disjuncts)
    (loop (when (null pos-exs) (return (if (or disjuncts neg-exs) (nreverse disjuncts) (list nil))))
	  (multiple-value-bind (disjunct pos-exs-covered)
	      (learn-pgolem-disjunct pos-exs neg-exs)
	    (if (or disjunct (> *pgolem-min-examples* 1))
		(trace-print *trace-pgolem* "~%~%Learned Disjunct: ~A~%" (format-term disjunct))
	      (progn 
		(trace-print *trace-pgolem* "~%~%Memorizing uncovered examples:~%")
		(loop for ex in pos-exs-covered do 
		      (push (example-to-term ex) disjuncts))))
	    (setf pos-exs (nset-difference pos-exs pos-exs-covered))
	    (if disjunct (push disjunct disjuncts))))))

(defun learn-pgolem-disjunct (pos-exs neg-exs)
  (if  (< (length pos-exs) *pgolem-min-examples*)
      (return-from learn-pgolem-disjunct (values nil pos-exs)))
  (setf pos-exs (copy-list pos-exs))
  (let* (term
	 (terms (loop for pair in (pick-pairs pos-exs *pgolem-sample-size*)
		    do (setf term (plgg (first pair) (rest pair)))
		    unless (some #'(lambda (ex) (cover-term term ex)) neg-exs)
		    collect term)))
    (unless terms
      (return-from learn-pgolem-disjunct (values nil pos-exs)))
    (let* ((best-term (argmax terms #'(lambda (term) (example-coverage term pos-exs))))
	   (covered (remove-if-not #'(lambda (ex) (cover-term best-term ex)) pos-exs))
	   (newly-covered covered))
      (trace-print *trace-pgolem* "~%Initial disjunct (covers ~D): ~A" (length covered) (format-term best-term))
      (setf pos-exs (nset-difference pos-exs covered))
      (loop (unless newly-covered (return (if (< (length covered) *pgolem-min-examples*)
					      (values nil pos-exs)
					    (values best-term covered))))
	(setf terms (loop for example in (pick-n pos-exs *pgolem-sample-size*)
			do (setf term (plgg-term-example best-term example))
			unless (some #'(lambda (ex) (cover-term term ex)) neg-exs)
			collect term))
	(if terms
	    (progn
	      (setf best-term (argmax terms #'(lambda (term) (example-coverage term pos-exs))))
	      (setf newly-covered (remove-if-not #'(lambda (ex) (cover-term best-term ex)) pos-exs))
	      (setf pos-exs (nset-difference pos-exs newly-covered))
	      (setf covered (nconc newly-covered covered))
	      (trace-print *trace-pgolem* "~%Generalized disjunct (covers ~D): ~A" (length covered) 
			   (format-term best-term)))
	  (setf newly-covered nil))))))

(defun example-coverage (term exs)
    (count-if #'(lambda (ex) (cover-term term ex)) exs))
  
(defun cover-term (term example)
      (every #'(lambda (lit) (eql (elt example (first lit)) (second lit))) term))
			    
(defun pick-pairs (list n)
  (let* ((l (length list))
	 (max (/ (* l (- l 1)) 2)))
    (if (>= n max)
	(loop for tlist on list nconc (loop for elt in (rest tlist) collect (cons (first tlist) elt)))
      (let (pair pairs)
	(loop for i from 1 to n do
	      (loop (setf pair (cons (pick-one list) (pick-one list)))
		(unless (or (eq (first pair) (rest pair))
			    (some #'(lambda (p) (or (and (eq (first pair) (first p))
							 (eq (rest pair) (rest p)))
						    (and (eq (rest pair) (first p))
							 (eq (first pair) (rest p)))))
				  pairs))
		  (push pair pairs)(return nil)))
	    finally (return pairs))))))

(defun pick-n (list n)
  (if (>= n (length list))
      list
    (let (elt elts)
      (loop for i from 1 to n do
	    (loop (setf elt (pick-one list))
	      (unless (member elt elts)
		(push elt elts) (return nil)))
	    finally (return elts)))))

;(defun pick-n1 (list n)
;  (setf list (copy-list list))
;  (loop for i from 1 to (min n (length list)) collect
;	(multiple-value-bind (item new-list)
;	    (delete-one list)
;	  (setf list new-list)
;	  item)))
;      
;(defun delete-one (list)
;  "Pick an item randomly from the list"
;  (let ((n (1- (random (length list))))
;	(item))
;    (if (= n -1)
;	(values (first list) (rest list))
;      (loop for sublist on list as i from 0 to most-positive-fixnum do
;	    (when (= i n)
;	      (setf item (second sublist))
;	      (setf (rest sublist) (rest (rest sublist)))
;	      (return (values item list)))))))

(defun plgg (example1 example2)
  (let (term value)
    (dotimes (i (length *domains*) term)
      (when (eql (setf value (elt example1 i)) (elt example2 i))
	(push (list i value) term)))))


(defun plgg-term-example (term example)
  (remove-if-not #'(lambda (literal) (eql (elt example (first literal))
					  (second literal)))
		 term))

(defun example-to-term (example)
  (let (term)
    (dotimes (i (length *domains*) term)
	(push (list i (elt example i)) term))))
  
(defun format-term (term)
  (mapcar #'(lambda (lit) (list (feature-name (first lit))
				(second lit)))
	  term))


;;;; ==========================================================================================
;;;; Train and test interface
;;;; ==========================================================================================

(defun train-pgolem (examples)
  (setf examples (make-ordered-examples examples))
  (dolist (cat *categories*) (setf (get cat 'training-examples) nil))
  (dolist (example examples)
    (push (second example) (get (first example) 'training-examples)))
  (let ((alist
	 (mapcar #'(lambda (cat)
		     (trace-print *trace-pgolem* "~%~%~%Category: ~A" cat)
		     (let ((result (pgolem (get cat 'training-examples)
					   (mapcan #'(lambda (other-cat)
						       (copy-list (get other-cat 'training-examples)))
						   (remove cat *categories*)))))
		       (list cat 
			     (if (and *pgolem-closest-match* (member *negative-category* *categories*))
				 (funcall *pgolem-thresholder* result (get cat 'training-examples) 
					  (get *negative-category* 'training-examples))
				 (length (get cat 'training-examples)))
			   result)))
		 (remove *negative-category* *categories*))))
    (cons (if (member *negative-category* *categories*)
	      *negative-category*
	      (maximum-category-label alist *categories*))
	  alist)))


(defun test-pgolem (example pgolem-result)
  (setf example (make-ordered-example example))
  (if *pgolem-closest-match*
      (test-pgolem-closest example pgolem-result)
    (test-pgolem-strict example pgolem-result)))

(defun test-pgolem-closest (example pgolem-result)
  (multiple-value-bind (max-class max-val)
      (argmax (rest pgolem-result) #'(lambda (c) (funcall *pgolem-matcher* (third c) (second example))))
    (if (not (member *negative-category* *categories*))
	(first max-class)
      (if (> max-val (second max-class))
	  (first max-class)
	*negative-category*))))

(defun test-pgolem-strict (example pgolem-result)
  (let ((class-counts (mapcan #'(lambda (alist-elt)
				  (if (match-pgolem (third alist-elt) (second example))
				      (list (cons (first alist-elt) (second alist-elt)))))
			      (rest pgolem-result ))))
    (if class-counts
	(maximum-label class-counts *categories*)
      (first pgolem-result))))

(defun match-pgolem (dnf inst)
  (loop for term in dnf do
        (when (every #'(lambda (lit) (eq (elt inst (first lit)) (second lit))) term)
          (return t))))

(defun midpoint-thresholder (dnf pos-exs neg-exs)
  (let ((min-pos-val (loop for ex in (remove-if-not #'(lambda (ex) (match-pgolem dnf ex)) pos-exs)
			 minimize (funcall *pgolem-matcher* dnf ex)))
	(max-neg-val (loop for ex in neg-exs maximize (funcall *pgolem-matcher* dnf ex))))
      (if (> max-neg-val min-pos-val)
          max-neg-val
        (/ (+ min-pos-val max-neg-val) 2))))

(defun lowpoint-thresholder (dnf pos-exs neg-exs)
  (declare (ignore pos-exs))
  (loop for ex in neg-exs maximize (funcall *pgolem-matcher* dnf ex)))

(defun max-avg-term (dnf inst)
  (let ((term-score 0))
    (loop for term in dnf maximize
	  (progn (setf term-score 0)
		 (loop for lit in term when (eq (elt inst (first lit)) (second lit)) do (incf term-score))
		 (/-float term-score (length term))))))

(defun avg-avg-term (dnf inst)
  (let ((term-score 0) (score 0))
    (loop for term in dnf do
	  (progn (setf term-score 0)
		 (loop for lit in term when (eq (elt inst (first lit)) (second lit)) do (incf term-score))
		 (incf score (/-float term-score (length term)))))
    (/-float score (length dnf))))
  
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

;(defun train-pgolemr (exs)
;  (pgolem-rules (train-pgolem exs)))
;
;(defun pgolem-rules (pgolem-result)
;  (loop for x in (rest pgolem-result)
;	nconc (let ((conse (list (first x))))
;		(loop for clause in (third x) collect
;		      (make-brule :consequent conse
;				  :antecedents
;				  (loop for lit in clause collect
;					(list (feature-name (first lit))
;					      (second lit))))))))

(defun literal-count (rules)
  (let ((sum 0))
    (loop for rule in rules do
	  (incf sum (1+ (length (brule-antecedents rule)))))
    sum))

(defun pgolem-concept-complexity (golem-result)
  (let ((sum 0))
    (dolist (alist-elt (rest golem-result) sum)
      (dolist (disjunct (third alist-elt))
	(incf sum (length disjunct))))))


(defun print-pgolem-result (pgolem-result)
  (loop for x in (rest pgolem-result) do
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


;;;; ==========================================================================================
;;;; Utilities
;;;; ==========================================================================================

(defun argmax (list function)
  (let (maxelt (maxval most-negative-short-float) val)
    (dolist (elt list (values maxelt maxval))
      (setf val (funcall function elt))
      (when (> val maxval)
	(setf maxelt elt maxval val)))))

(defun argmin (list function)
  (let (minelt (minval most-positive-short-float) val)
    (dolist (elt list (values minelt minval))
      (setf val (funcall function elt))
      (when (< val minval)
	(setf minelt elt minval val)))))

;;;; ==========================================================================================
;;;; Variant systems
;;;; ==========================================================================================

(make-variant pgolem-max-mid pgolem ((*pgolem-closest-match* t) (*pgolem-matcher* #'max-avg-term)  
				     (*pgolem-thresholder* #'midpoint-thresholder)))

(make-variant pgolem-max-low pgolem ((*pgolem-closest-match* t) (*pgolem-matcher* #'max-avg-term)  
			             (*pgolem-thresholder* #'lowpoint-thresholder)))

(make-variant pgolem-avg-mid pgolem ((*pgolem-closest-match* t) (*pgolem-matcher* #'avg-avg-term)  
				     (*pgolem-thresholder* #'midpoint-thresholder)))

(make-variant pgolem-avg-low pgolem ((*pgolem-closest-match* t) (*pgolem-matcher* #'avg-avg-term)  
			             (*pgolem-thresholder* #'lowpoint-thresholder)))

(make-variant pgolem-plain pgolem ((*pgolem-closest-match* nil)))
