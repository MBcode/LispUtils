;;;; Copyright (c) 1990 by Raymond Joseph Mooney. This program may be freely copied, used, or
;;;; modified provided that this copyright notice is included in each copy of this code
;;;; and parts thereof.

;;;; This code is a version of the ID3 decision tree induction algorithm modified to
;;;; handle many things simulataneously: 1) Multiple classses, 2) Noise using either
;;;; no-pruning or chi-squared pruning, 3) Missing features using either most-common
;;;; replacement or fractioning examples for test evaluation and partitioning;
;;;; and token summing during classification.  4) Linear features using the standard technique. 
;;;; See Quinlan's papers in Machine Learning 1:1 and ML-89 workshop.

;;;; Compiling this version took advantage of previous code written by  Wan Yik Lee 
;;;; (noise for 2 category case), Rita Duran (missing), Alan Gove (multiple category
;;;; version for noise and missing), and Goeff Towell (switch to arrays and misc).

(in-package :user)
(eval-when (compile load eval)
 (provide 'id3)
 (require 'data-utilities (ml-progs-file "data-utilities"))

 (proclaim '(optimize (speed 3) (safety 0)(compilation-speed 0)))
 (proclaim '(inline square /-float feature-name feature-domain feature-value linear-feature-p
	     change-weight example-weight))
 )

(setf (get 'id3 'parameters) '(*noise* *tie-breaker* *missing-method* *negative-bias*))

;;; Instance descriptions are ordered sequences of feature values (lists or arrays allowed,
;;; arrays tend to be more efficient)

;;; If the *noise-error* parameter is set, inconsistent data will cause an error.
;;; If *noise* is set, pre-pruning of the decision tree with the  chi-square test will be performed 
;;; If neither is set, inconsistent data will be classified using the majority class.
(defparameter *noise-error* nil "Cause error if find conflicting data. For finding unexpected noise.")
(defparameter *trace-id3* nil       "Produces a trace if set to T")
(defparameter *trace-id3-detail* nil "Produces detail in trace if set to T")
(defparameter *indent-increment* 3  "Number of spaces for tabbing")
(defparameter *missing-method*  'fraction-example "How to handle missing data (most-common or fraction-example)")
(defparameter *negative-bias* t "Prefer negative as the class in breaking category ties to model simplicity bias")

(defvar *decision-tree* nil "Used to permanently save the decision-tree produced")
(defvar *indent* 0 "Stored the current indent (dynamically scoped and reset)")

(defvar *chi-table*
   ;; Chi-square values for degree of freedom (dof) 1-30.  Values for
   ;; higher dof's are approximated by the function compute-chi.
   ;; All values are for 99% confidence.
  (make-array 
	 '(30) 
	 :initial-contents
	 '(6.64 9.21 11.34 13.28 15.09 16.81 18.48 20.09 21.67 23.21 24.72 26.22 27.69 29.14
	   30.58 32.00 33.41 34.80 36.19 37.57 38.93 40.29 41.64 42.98 44.31 45.64 46.96 48.28
           49.59 50.89)))

;;;; ==========================================================================================
;;;; Structure Definitions and Access Functions
;;;; ==========================================================================================

;; A decision tree is either a leaf represented by a symbol for the class or a structure.
;; The structure has a feature, a threshold, and a list of branches.  The feature is the
;; feature number (starting from 0) for the split. The threshold is NIL if the feature is
;; nominal or a real-valued threshold if it is linear.
(defstruct (decision-tree (:print-function print-decision-tree))
  feature threshold branches)

;; Each branch has a value, a ratio, and a subtree.  The value is < or >= if the feature
;; is linear.  The ratio is the fraction of training examples with a value for this feature
;; that went down this branch. Subtree is a decision tree.
(defstruct branch
  value ratio subtree)

(defun instance-value (feature-num instance)
  "Returns feature-value for an unclassified instance"
  (elt instance feature-num))

(defun print-decision-tree (tree stream depth &optional (indent 1))
  "Print decision tree in a nice indented form. Use feature names if available"
  (cond ((symbolp tree)
	 (format stream "~%~vTClass is: ~A" indent tree))
	(t (format stream "~%~vTFeature: ~A" indent 
		   (feature-name (decision-tree-feature tree)))
	   (mapc #'(lambda (branch)
		     (if (decision-tree-threshold tree)
			 (format stream "~%~vT ~A ~,3F (~,3F)" indent (branch-value branch)
				 (decision-tree-threshold tree)(branch-ratio branch))
			 (format stream "~%~vT ~A (~,3F)" indent (branch-value branch) (branch-ratio branch)))
		     (print-decision-tree (branch-subtree branch) stream depth (+ indent *indent-increment*)))
	     (decision-tree-branches tree)))))

(defun example-weight (example)
  (let ((weight (third example)))
    (or weight 1)))

(defun change-weight (example fraction)
  (list (first example) (second example)
	(* fraction (example-weight example))))

(defmacro id3-trace-print (ctl-string &rest args)
  `(if *trace-id3*
       (format t ,(concatenate 'string "~%~vT" ctl-string) *indent* ,@args)))

(defmacro id3-detail-print (ctl-string &rest args)
  `(if *trace-id3-detail*
       (format t ,(concatenate 'string "~%~vT" ctl-string) *indent* ,@args)))

;;;; ==========================================================================================
;;;; Basic ID3 code
;;;; ==========================================================================================

(defun train-id3 (training-examples)
  "Function to be used by UNIVERSAL-TESTER.  Handles feature ordered and alist example formats"
  (id3 (make-ordered-examples training-examples)))

(defun test-id3 (example id3-tree)
  "Function to be used by UNIVERSAL-TESTER.  Handles feature ordered and alist example formats"
  (classify (second (make-ordered-example example)) id3-tree))

(defun id3 (examples)
;;; This function takes a list of examples where an example is a list consisting of a class name
;;; and an instance and produces a decision tree which classifies instances into the classes present
;;; in the examples.
  (if (eq *missing-method* 'most-common)
      (setf examples (fill-in examples)))
  (setf *decision-tree*
	(if (null examples)
	    (pick-one *categories*)
	    (build-decision-tree examples (let ((features nil)) 
					    (dotimes (i (length *domains*) (nreverse features))
					      (push i features))))))
  (id3-trace-print "") *decision-tree*)

(defun build-decision-tree (examples features &optional parent-majority-class (*indent* 1))
;;; This function produces a decision tree for the given set of examples by choosing one of the
;;; given features (features are indicated by a number giving its position in the vector) as the
;;; root of the tree and recursively making trees for each of the resulting categories.
  (let* ((class-counts (compute-class-counts examples)) ; determine # of examples in each class
	 (majority-class (and examples (maximum-label class-counts *categories*))))
    (cond ((null examples)
           (cond ((and *negative-bias*
		       (member *negative-category* *categories*))
		  (id3-trace-print "No examples.  Using negative bias and labelling ~A" *negative-category*)
		  *negative-category*)
		 (t (id3-trace-print "No examples.  Using most common class of parent node ~A" parent-majority-class) 
		    parent-majority-class)))
	  ((null (rest class-counts))
	   ;; if all examples are in the same class then make a leaf indicating this class
	   (id3-trace-print "All examples in class ~A" (first (first class-counts)))
	   (first (first class-counts)))
	  ((all-equal examples)  ; If all examples equal but not in same class then there is noisy data
	   (id3-trace-print "All examples equal but not in same class")
	   (if (not *noise-error*)
	       (progn (id3-trace-print "Using majority class: ~A" majority-class)
		      majority-class)  ; If allowing for noise, return a leaf of majority class
	       (error "Inconsistent training set")))
	  (t (let (split-nodes split-node chi-not-applicable? fraction-of-example?
		   (E 0) (min-E 1e10) (IR 0) branches (bigI (info class-counts)))
	       ;; Otherwise find the feature which maximizes information gain (minimizes E)
	       ;; and make it the root of the decision tree (i.e. make it the "split feature")
	       (dolist (feature features)
		 (dolist (threshold (if (linear-feature-p feature)
					(thresholds-for-feature feature examples)
					'(NIL)))
		   (multiple-value-setq (E IR branches chi-not-applicable? fraction-of-example?)
		     (expected-info feature threshold examples class-counts))
		   (when (and branches  ; must have branches (i.e. all examples must not be missing value)
			      (<= E min-E)
			      (> bigI E)     ;Must have some info gain.
			      (or (not *noise*)
				  chi-not-applicable?
				  (> IR (compute-chi feature)))      ;feature must be relevant
			      (not fraction-of-example?))            ; Must not give only fraction
		     (setf split-node (make-decision-tree :feature feature
							  :threshold threshold
							  :branches branches))
		     (if (< E min-E)
			 (progn (setf split-nodes (list split-node)) (setf min-E E))
			 ;; If randomly breaking tie, then save all ties and pick one randomly
			 (if (eq *tie-breaker* 'random)
			     (push split-node split-nodes))))))
	       (setf split-node (and split-nodes (pick-one split-nodes)))
	       (if (rest split-nodes) (id3-trace-print "Gain Tie- Randomly choosing"))
	       (if split-node
		   (progn
		     (when *trace-id3*
		       (id3-trace-print "")
		       (if (decision-tree-threshold split-node)
			   (id3-trace-print "Splitting on ~A using threshold ~,3F"
					    (feature-name (decision-tree-feature split-node))
					    (decision-tree-threshold split-node))
			 (id3-trace-print "Splitting on ~A" (feature-name (decision-tree-feature split-node)))))
		     ;; Recursively create subtrees for each branch
		     (dolist (branch (decision-tree-branches split-node) split-node)
		       (when *trace-id3*
			 (id3-trace-print "")
			 (if (decision-tree-threshold split-node)
			     (id3-trace-print "  Looking at ~A ~A ~,3F"
					      (feature-name (decision-tree-feature split-node))
					      (branch-value branch)
					      (decision-tree-threshold split-node))
			     (id3-trace-print "  Looking at ~A = ~A"
					      (feature-name (decision-tree-feature split-node))
					      (branch-value branch))))
		       (setf (branch-subtree branch)
			     (build-decision-tree (branch-subtree branch)
						  (if (decision-tree-threshold split-node)
						      features
						      (remove (decision-tree-feature split-node) features))
						  majority-class
						  (+ *indent* *indent-increment*)))))
		   ;;If no relevant features, label leaf with biggest class
		   (progn (id3-trace-print "Features irrelevant. Using majority class: ~A" majority-class)
			  majority-class)))))))

(defun all-equal (examples)
  "Return true if all of the examples have the same instance description"
  (let ((first-instance (second (first examples))))
    (dolist (example (rest examples) T)
      (unless (equalp (second example) first-instance) ; Use equalp to catch arrays
	(return nil)))))

(defun compute-class-counts (examples)
;;; Counts the number of examples in each class represented in examples and returns an A-list of 
;;; the form ((class1 . #in-class1)(class2 . #in-class2) ... )
  (let ((class-counts nil) (class-count nil))
    (dolist (example examples class-counts)
      (setf class-count (assoc (first example) class-counts))
      (if class-count
	  (incf (cdr class-count) (example-weight example))
	  (push (cons (first example) (example-weight example)) class-counts)))))

(defun thresholds-for-feature (feature examples)
  "Return list of possible thresholds for given feature by taking midpoint between each pair of
   ordered values present in the examples"
  (do ((sorted-values (sort (delete *missing-value* (mapcar #'(lambda (ex) (feature-value feature ex)) examples))
			    #'<)
			   (rest sorted-values))
       (result nil))
      ((null (rest sorted-values)) (nreverse result))
    (unless (= (first sorted-values) (second sorted-values))
      (push (/-float (+ (first sorted-values) (second sorted-values)) 2)
	    result))))

(defun expected-info (feature threshold examples class-counts)
;;; Compute the expected amount of information needed for the subtrees created by
;;; splitting on the given feature. This is simply a weighted sum of the information
;;; needed for each subtree.  Also calculate the relevancy R of feature.  this is
;;; a straightforward generalization of quinlan's 2-category method. 
    (let ((E 0) 
	(R 0) 
	(chi-not-applicable? nil) ; set if sample to small for chi-square test
	(fraction-of-example? nil) ; set if any branch has only a fraction of 1 example
	(num-examples 0) 
	 branches)
    (dolist (class-count class-counts) (incf num-examples (cdr class-count)))
    (if *trace-id3-detail*
	(if threshold
	    (id3-detail-print "Considering splitting on ~A using threshold ~,3F"
			     (feature-name feature) threshold)
	    (id3-detail-print "Considering splitting on ~A" (feature-name feature))))
    (setf branches (make-branches feature threshold examples num-examples))
    (dolist (branch branches)
      (let* ((examples-with-value (branch-subtree branch))
	     (class-counts-i (compute-class-counts examples-with-value))
	     (sum-p-i 0)
	     (sum-weights 0))
	(dolist (example examples-with-value)
	  (incf sum-weights (example-weight example)))
	(if (and examples-with-value (< sum-weights 1))
	    (setf fraction-of-example? t))
	(dolist (cc class-counts-i) (incf sum-p-i (cdr cc)))
	(if *noise*
	    (dolist (cc class-counts) ;;compute relevancy
	      (let* ((p-prime-i (/-float (* (cdr cc) sum-p-i) num-examples)) ;;expected value of p-i
		     (cc-i (assoc (car cc) class-counts-i)) 
		     (p-i (if cc-i (cdr cc-i) 0)))	;actual value of p-i
		(when (< p-prime-i 4)
		  (setf chi-not-applicable? T))
		(incf R (if (zerop p-prime-i) 0 (/-float (square (- p-i p-prime-i))
							 p-prime-i))))))
	(incf E (* (/-float sum-p-i num-examples)
		   (info class-counts-i)))))
    (if branches
	(id3-detail-print "   Gain = ~5,3F Chi square stat = ~7,5F (~A) ~A"
		     (- (info class-counts) E) R
		     (if chi-not-applicable?
			 'SAMPLE-TOO-SMALL
			 (if (> R (compute-chi feature))
			     'OK 'NOGOOD))
		     (if fraction-of-example? "(examples < 1)" ""))
	(id3-detail-print "   No examples with value.  Feature no good."))
    (values E R branches chi-not-applicable? fraction-of-example?)))

(defun make-branches (feature threshold examples num-examples)
  "Create branches for spliting on this feature by partitioning examples approriately"
  (let* ((num-with-value num-examples)
	 examples-with-missing
	 (all-missing? t)
	 (branches (mapcar #'(lambda (val)
			       (make-branch :value val
					    :ratio 0
					    :subtree nil))
			   (if threshold
			       '(< >=)
			       (feature-domain feature)))))
    (dolist (example examples)
      (let* ((value (feature-value feature example))
	     branch)
	(if (equal value *missing-value*) 
	    (progn (decf num-with-value (example-weight example))
		   (push example examples-with-missing))
	    (progn
	      (setf all-missing? nil)
	      (if threshold
		  (setf value (if (< value threshold) '< '>=)))
	      (setf branch (find value branches :key #'branch-value))
	      (if (not branch)
		  (error "Invalid value for feature ~A=~A" (feature-name feature) value)
		  (progn (incf (branch-ratio branch) (example-weight example))
			 (push example (branch-subtree branch))))))))
    (when all-missing? (setf branches nil))  ;when all examples missing, can't determine branches
    (dolist (branch branches)
      (setf (branch-ratio branch)
	    (/-float (branch-ratio branch) num-with-value)))
    (if (eq *missing-method* 'fraction-example) ; Assign fraction of missing examples to each branch
	(dolist (branch branches)               ; based on ratios
	  (unless (zerop (branch-ratio branch))
	    (dolist (example examples-with-missing)
	      (push (change-weight example (branch-ratio branch)) (branch-subtree branch))))))
     branches))

(defun compute-chi (feature)
;;; look it up in a table if the dof is low enough
;;; otherwise use law of large numbers to assume normal distribution
;;; and compute using formula (chi-value)
;;;     x-offset = (sqrt 2 * chi-value) - (sqrt 2 * dof - 1)
;;; solving for chi-alpha 
;;;     chi-value = 0.5 * (expt ((sqrt 2 * dof - 1) + x-offset) 2)
;;; NOTE:  this code only handles 99% confidence 
;;;        that is for 99% confidence x-offset=2.3856 and the lookup table is good
  (let ((dof (* (1- (length *categories*))
		(1- (if (linear-feature-p feature) 2 
		      (length (feature-domain feature)))))))
    (if (< dof 31) (aref *chi-table* (1- dof))
      (* 0.5 (expt (+ 2.3856 (sqrt (1- (* 2 dof)))) 2)))))

(defun info (class-counts)
;;; Compute the amount of information needed to distinguish among the various
;;; classes given the number of instances for each class specified in the
;;; class-counts alist

  (let ((num-examples 0) (I 0))
    (dolist (class-count class-counts) (incf num-examples (cdr class-count)))
    (dolist (class-count class-counts I)
      (incf I  (- (* (/-float (cdr class-count) num-examples)
		     (log (/-float (cdr class-count) num-examples) 2)))))))

;;;; ==========================================================================================
;;;; Filling in missing values
;;;; ==========================================================================================

(defun fill-in (examples)
  ;;; Replaces missing discrete features with most common value given the category
  ;;; Replaces missing linear features with average for the category
  (setf examples (mapcar #'(lambda (example) (list (first example) (copy-seq (second example))))
			 examples))
  (let* ((num-categories (length *categories*))
	 (num-features (length *domains*))
	 (array (make-array (list num-categories num-features))))
    ;; Initialize replacement array
    (dotimes (category num-categories)
      (dotimes (feature num-features)
	    (setf (aref array category feature)
		  (if (linear-feature-p feature)
		      (cons 0 0)   ; Linear stores pair of sum and number examples to compute average
		      (mapcar #'(lambda (value) (cons value 0)) ; Discrete stores a-list of counts for
			      (feature-domain feature))))))     ; for each value
    ;; Change counts and sums to account for each non-missing value for each example
    (dolist (example examples)
      (dotimes (feature num-features)
	(let ((value (feature-value feature example))
	      (cell (aref array (category-number (first example)) feature)))
	  (unless (eq value *missing-value*)
	    (if (linear-feature-p feature)
		(progn (incf (car cell) value)
		       (incf (cdr cell)))
		(let ((pair (assoc value cell)))
		  (if pair
		      (incf (cdr pair))
		      (error "Unknown value: ~A=~A" (feature-name feature) value))))))))
    ;; Compute final replacement value from the stored information
    (dotimes (category num-categories)
      (dotimes (feature num-features)
	(let ((cell (aref array category feature)))
	  (setf (aref array category feature)
		(let ((replacement-value (if (linear-feature-p feature)
					     (/-float (car cell) (cdr cell))
					     (maximum-label cell (feature-domain feature)))))
		  (id3-trace-print "Replacement value for ~A in ~A examples is: ~A" (feature-name feature)
				   (elt *categories* category) replacement-value)
		  replacement-value)))))
    ;; Replace missing values with the appropriate replacement value
    (dolist (example examples)
      (dotimes (feature num-features)
	(if (eq (feature-value feature example) *missing-value*)
	    (setf (elt (second example) feature)
		  (aref array (category-number (first example)) feature))))))
  examples)

(defun category-number (category)
  (position category *categories*))


;;;; ==========================================================================================
;;;; Stuff for Classifying New Instances
;;;; ==========================================================================================

(defun classify (instance tree)
  "Return the class of the instance assigned by the decision tree"
   (maximum-label (sum-tokens (decide instance tree)) *categories*))

(defun decide (instance tree &optional (token 1))
  "Classifies an instance using a decision tree returning a list of
   (class . token-value) pairs where each pair is evidence for a particular
   class"
  ;;; See Quinlan ML journal article for discussion of this approach to
  ;;; handling missing values during classification
  (if (symbolp tree)
      (list (cons tree token))
      (let ((value (instance-value (decision-tree-feature tree) instance)))
	(if (eql value *missing-value*)
	    (mapcan #'(lambda (branch)
			(decide instance (branch-subtree branch)
				(* token (branch-ratio branch))))
		    (decision-tree-branches tree))
	    (progn
	      (when (decision-tree-threshold tree)
		  (setf value (if (< value (decision-tree-threshold tree))
				  '< '>=)))
	      (let ((branch (find value (decision-tree-branches tree) :key #'branch-value)))
		(if branch
		    (decide instance (branch-subtree branch) token)
		    (error "Unexpected value"))))))))

(defun sum-tokens (token-alist)
  "Takes an a-list of category-token pairs and sums up the total score
   for each category and returns total score a-list"
  (let (score-alist)
    (dolist (token-cons token-alist score-alist)
      (let ((score-cons (assoc (car token-cons) score-alist)))
	(if score-cons
	    (incf (cdr score-cons) (cdr token-cons))
	    (push token-cons score-alist))))))

;;;; ==========================================================================================
;;;; Complexity of tree stuff
;;;; ==========================================================================================

(defun id3-concept-complexity (decision-tree)
  (count-leaves decision-tree))

(defun count-leaves (tree &optional leaf-type)
  "Returns the number of leaves of a decision-tree (with a given label if specified"
  (cond ((decision-tree-p tree)
	 (let ((sum 0))
	   (dolist (branch (decision-tree-branches tree) sum)
	     (incf sum (count-leaves (branch-subtree branch) leaf-type)))))
        ((or (null leaf-type) (eq tree leaf-type)) 1)
	(t 0)))

(defun count-nodes (tree &optional (count-leaves? t))
  "Returns the number of nodes of a decision-tree"
  (cond	((symbolp tree) (if count-leaves? 1 0))
	((decision-tree-p tree)
	 (let ((sum 1))
	   (dolist (branch (decision-tree-branches tree) sum)
	     (incf sum (count-nodes (branch-subtree branch) count-leaves?)))))
	(t 0)))

(defun tree-depth (tree)
  "Returns the depth of the tree"
  (if (symbolp tree)
      0
      (1+ (apply #'max
		 (mapcar #'(lambda (branch)
			     (tree-depth (branch-subtree branch)))
			 (decision-tree-branches tree))))))


;;;; ==========================================================================================
;;;; Variant systems
;;;; ==========================================================================================

(make-variant id3-prune id3 ((*noise* t)))
(make-variant id3-full id3 ((*noise* nil)))
