;;;; This is a simple implementation of the COBWEB Incremental Conceptual Clustering algorithm
;;;; (cf. "Knowledge Acquisition Via Incremental Conceptual Clustering" D.H Fisher, ML 2 139-172)

;;;; Copyright (c) 1993 by Raymond Joseph Mooney. This program may be freely 
;;;; copied, used, or modified provided that this copyright notice is included
;;;; in each copy of this code and parts thereof.

;;;; See COBWEB-ANIMAL-DATA (from ML article) and COBWEB-ANIMAL2-DATA for simple sample data files.
;;;; Try (cobweb-train *raw-examples*) after loading either file.
;;;; Also try COBWEB-SOYBEAN-DATA and COBWEB-SOYBEAN2-DATA (also discussed in ML article)
;;;; Examples are assumed to be ordered lists of feature values of the form:
;;;; (example-name (feature-1-value feature-2-value ... feature-n-value))

;;;; -------------------------------------------------------------------------------------------------------------
;;;; Data structures and global variables
;;;; -------------------------------------------------------------------------------------------------------------

(in-package :user)
(provide 'cobweb)
(require 'data-utilities (ml-progs-file "data-utilities"))

(setf (get 'cobweb 'expect-training-error) T)
(setf (get 'cobweb 'incremental) T)

(defparameter *print-category* 'instance-names "Controls printing of categories (instance-names, name, all)")
(defparameter *trace-cobweb* nil  "Produces trace of COBWEB's decisions")
(defparameter *trace-utility* nil "Produces trace of category utility values for each option considered")

(defstruct (category (:print-function print-category)(:copier nil))
  (name (gensym "C-"))     ; Unique category name
  (num-instances 0)        ; number of instances in category
  (instances nil)          ; list of instances in category
  (counts (init-counts))   ; feature counts of the form
                           ; ((feature1-value1-count feature1-value2-count ...)(feature2-value1-count ...) ...)
                           ; where features and their values are in the order given in *domains*
  (subs nil))              ; list of sub-categories 

;;; A potential is a data structure used to temporarily store potential changes
;;; to the subs of a category that incorporate a new example.  Several
;;; potentials are constructed and evaluated and one is chosen as the actual update
(defstruct potential
  subs                ; list of adjusted sub-categories for this potential change
  host                ; category where new example is assigned (counts adjusted)
  old-host            ; host category before new example is added
  (utility 0))            ; category utility of this potential change


;;;; -------------------------------------------------------------------------------------------------------------
;;;; Top-level functions
;;;; -------------------------------------------------------------------------------------------------------------

(defun cobweb-train (examples &optional root-category)
  "Incrementally add each of the examples to the category structure with the given root"
  (unless root-category				; Initialize root to single instance if not given
    (setf root-category (make-category))
    (add-example root-category (pop examples))
    (trace-print *trace-cobweb* "~%~%Intializing root category with first instance. ~%~A" root-category))
  ;; Incrementally process each example and print entire hierarchy after each for trace
  (dolist (example examples)
    (trace-print *trace-cobweb* "~%~%~%Incorporating instance: ~A" (first example))
    (cobweb example root-category)
    (when *trace-cobweb*
      (terpri)(print-hierarchy root-category)))
  (when *trace-cobweb* (terpri)(terpri))
  root-category)

(defun cobweb (example category &optional (level 0))
  "Incorporate example into existing category structure"
  (if (= level 0) (add-example category example))  ; Except at root, example is already added by parent
  (if (null (category-subs category))
      ;; If leaf category, then just add example as new leaf
      (progn (trace-print *trace-cobweb* "~%~vTLevel ~D: Growing leaf category" (1+ (* 2 level)) level)
	     (grow-leaf category))
      ;; Otherwise check following potential changes and pick the one resulting in best category utility
      ;; 1) Adding to each existing sub-category
      ;; 2) Putting in separate category
      ;; 3) Merging two best existing hosts and adding to it  
      ;; 4) Splitting best host and adding to best child of best host    
      (let ((existing-category-potentials   ; sorted in decreasing order of utility
	      (existing-category-potentials category example))
	    (new-category-potential (new-category-potential category example)))
	(when *trace-utility*
	  (dolist (potential existing-category-potentials)
	    (format t "~%~vTCategory utility ~,3F when put with ~A"
		    (1+ (* 2 level)) (potential-utility potential) (potential-old-host potential)))
	  (format t "~%~vTCategory utility ~,3F when by itself"
		  (1+ (* 2 level)) (potential-utility new-category-potential)))
	(cond
	;; If new-category better than best existing one then use it
	  ((> (potential-utility new-category-potential)
	      (potential-utility (first existing-category-potentials)))
	   (trace-print *trace-cobweb* "~%~vTLevel ~D: putting instance by itself" (1+ (* 2 level)) level)
	   (use-potential category new-category-potential) (potential-host new-category-potential))
	;; Next checks option of merging two best hosts and adding example to the new merged category
	  ((let* ((sub1 (potential-old-host (first existing-category-potentials)))
		  (sub2 (potential-old-host (second existing-category-potentials)))
		  (merge-potential (merge-potential sub1 sub2 category example)))
	     (trace-print *trace-utility* "~%~vTCategory utility ~,3F when merge ~A and ~A"
			  (1+ (* 2 level)) (potential-utility merge-potential) sub1 sub2)
	     (when (> (potential-utility merge-potential)
		      (potential-utility (first existing-category-potentials)))
	       (trace-print *trace-cobweb* "~%~vTLevel ~D: Merging 2 best hosts ~A and ~A"
			    (1+ (* 2 level)) level sub1 sub2)
	       (use-potential category merge-potential)
	       ;; After merging incorporate example recursively to the merged host
	       (cobweb example (potential-host merge-potential) (1+ level)))))
	;; Next check option of spliting best host and adding example to it's best child 
	  ((let* ((split-category (potential-old-host (first existing-category-potentials)))
		  (split-potential (if (category-subs split-category)
				       (split-potential split-category category example))))
	     (when split-potential
	       (trace-print *trace-utility* "~%~vTCategory utility ~,3F when split ~A and put in ~A"
			    (1+ (* 2 level)) (potential-utility split-potential)
			    split-category (potential-old-host split-potential))
	       (when (> (potential-utility split-potential)
			(potential-utility (first existing-category-potentials)))
		 (trace-print *trace-cobweb* "~%~vTLevel ~D: Splitting best host"
			      (1+ (* 2 level)) level split-category)
		 (use-potential category split-potential)
		 ;; After splitting incorporate example recursively into its host
		 (cobweb example (potential-host split-potential) (1+ level))))))
	;; Otherwise incorporate example into the best existing subcategory
	  (t (trace-print *trace-cobweb* "~%~vTLevel ~D: putting instance with ~A" (1+ (* 2 level)) level
			  (potential-old-host (first existing-category-potentials)))
	     (use-potential category (first existing-category-potentials))
	     (cobweb example (potential-host (first existing-category-potentials)) (1+ level)))))))


;;;; -------------------------------------------------------------------------------------------------------------
;;;; Functions for generating different potential changes
;;;; -------------------------------------------------------------------------------------------------------------

(defun existing-category-potentials (category example)
  "Returns sorted list (highest utility first) of potentials for adding example
   to each existing subcategory"
  (sort (mapcar #'(lambda (sub-category)
		    (let ((host (copy-category sub-category)))  ; make copy so don't effect category structure yet
		      (add-example host example)                ; try adding to this sub-category
		      (let ((subs (substitute host sub-category (category-subs category))))
			(make-potential :subs subs :host host :old-host sub-category
					:utility (category-utility category subs)))))
		(category-subs category))
	#'> :key #'potential-utility))

(defun new-category-potential (category example)
  "Returns potential for adding example to its own category at the current level"
  (let ((new-category (make-category)))
    (add-example new-category example)
    (let ((subs (cons new-category (category-subs category))))  ; add as a new sub
      (make-potential :subs subs :host new-category
		      :utility (category-utility category subs)))))

(defun merge-potential (sub-category1 sub-category2 category example)
  "Returns potential for merging two existing sub-categories"
  (let ((new-category (merge-categories sub-category1 sub-category2)))
    (add-example new-category example)    ; add example to new merged category
    ;; Replace two exsiting subs with new merged one
    (let ((subs (cons new-category (remove sub-category1 (remove sub-category2 (category-subs category))))))
      (make-potential :subs subs :host new-category
		      :utility (category-utility category subs)))))

(defun merge-categories (category1 category2)
  "Returns new category formed by merging two existing ones"
  (make-category :num-instances (+ (category-num-instances category1)(category-num-instances category2))
		 :counts (mapcar #'(lambda (f1 f2) (mapcar #'+ f1 f2))  ; add feature counts together
				 (category-counts category1)(category-counts category2))
		 :subs (list category1 category2)        ; make existing categories subs of merged one
		 :instances (append (category-instances category1) (category-instances category2))))

(defun split-potential (split-category category example)
  "Returns potential for splitting existing category and adding example to its best sub"
  (let* ((sub-potentials (existing-category-potentials split-category example))
	 ; replace existing category with its new subs 
	 (subs (append (potential-subs (first sub-potentials))  
		       (remove split-category (category-subs category)))))
      (make-potential :subs subs :host (potential-host (first sub-potentials))
		      :old-host (potential-old-host (first sub-potentials))
		      :utility (category-utility category subs))))

;;;; -------------------------------------------------------------------------------------------------------------
;;;; Functions for calculating Category Utility
;;;; -------------------------------------------------------------------------------------------------------------

(defun category-utility (category &optional subs)
  "Return category utility of breaking category into given sub-categories (or its existing subs)"
  ;; See equation in COBWEB article
  (unless subs (setf subs (category-subs category)))
  (let ((exp-incr 0)  ; Stores sum of total increase in expected number of correct features inferred
	(cat-PV2 (prob-values-squared category))) ; Expected number of correct inferences without sub-category
    (dolist (sub-category subs)
      (incf exp-incr
	    (* (/ (category-num-instances sub-category)  ; prior probability of sub-category
		  (category-num-instances category))
	       (- (prob-values-squared sub-category)     ; increase in expected number of correct
		  cat-PV2))))                            ; features guessed given sub-category
    (/ exp-incr (length subs))))        ; average over number of sub-categories

(defun prob-values-squared (category)
  "Computes sum of squares of probability of each feature value for category, i.e.
   expected number of correct feature values inferred given category"
  (let ((result 0)
	(n (category-num-instances category)))
    (dolist (value-counts (category-counts category) result)
      (dolist (count value-counts)
	(incf result (expt (/ count n) 2))))))


;;;; -------------------------------------------------------------------------------------------------------------
;;;; Low-level functions for actually changing category structure
;;;; -------------------------------------------------------------------------------------------------------------

(defun use-potential (category potential)
  "Change category structure to actually use the sub-categories in the potential"
  (setf (category-subs category) (potential-subs potential)))

(defun grow-leaf (leaf-cat)
  "Add new leaves for each of the two examples in the current leaf category"
  (let ((cat1 (make-category))
	(cat2 (make-category)))
    (add-example cat1 (first (category-instances leaf-cat)))
    (add-example cat2 (second (category-instances leaf-cat)))
    (setf (category-subs leaf-cat) (list cat1 cat2))
    cat1))

(defun add-example (category example)
  "Updates instances, instance count and feature counts for adding example to category"
  (push example (category-instances category))
  (incf (category-num-instances category))
  (mapc #'(lambda (value-counts value domain)
	    (unless (eq value *missing-value*)     ; missing values don't count
	      (incf (nth (position value domain) value-counts)))) ; destructively modify counts
	(category-counts category) (second example) *domains*))


;;;; -------------------------------------------------------------------------------------------------------------
;;;; Functions for initializing, copying, and printing data structures & misc utilities
;;;; -------------------------------------------------------------------------------------------------------------

(defun init-counts ()
  "Produces initial feature counts of all zeros for new category"
  (mapcar #'(lambda (domain)
	      (mapcar #'(lambda (value) (declare (ignore value)) 0)
		      domain))
	  *domains*))

(defun copy-category (category &optional recursive-p)
  "Make copy of a category and recursively all subs if flag set"
  (make-category :name (category-name category)
		 :num-instances (category-num-instances category)
		 :instances (category-instances category)
		 :counts (copy-tree (category-counts category)); also copy because counts changed destructively
		 :subs (if recursive-p
			   (mapcar #'copy-category (category-subs category))
			   (category-subs category))))
  
(defun print-category (category stream depth)
  "Print category either just as name (name), with names of instances (instance-names), or
   also with counts (all) depending on value of *print-category*"
  (declare (ignore depth))
  (princ (category-name category) stream)
  (when (member *print-category* '(instance-names all))
    (princ "{" stream)
    (dolist (instance (category-instances category))
      (unless (eq instance (first (category-instances category)))
	(princ " " stream))
      (princ (first instance) stream))
    (princ "}" stream))
  (when (eq *print-category* 'all)
    (format t " :num-instances ~D :counts ~A" (category-num-instances category)
	    (category-counts category))))

(defun print-hierarchy (category &optional (stream t) (level 0))
  "Pretty-print the complete hierarchy under the category"
  (format stream "~%~vT~A" (1+ (* 2 level)) category)
  (dolist (sub-category (category-subs category))
    (print-hierarchy sub-category stream (1+ level))))

;;;; -------------------------------------------------------------------------------------------------------------
;;;; Running tests and evaluating predictive accuracy
;;;; -------------------------------------------------------------------------------------------------------------

(defun run-cobweb-test (examples num-train test-interval predict-features)
  "Run a learning curve for predicting values of given feature names using a maximum
   of num-train training instances and testing after every test-interval'th example
   (should divide evenly)"
  (setf examples (mix-up examples))        ; randomize because COBWEB is order dependent
  (let* ((train-examples (subseq examples 0 num-train))
	 (test-examples  (subseq examples num-train))
	 root-category)
    (dotimes (batch (/ num-train test-interval))
      (format t "~%~%Trained on ~D examples" (* (1+ batch) test-interval))
      (setf root-category (cobweb-train (subseq train-examples
						(* batch test-interval) (* (1+ batch) test-interval))
					root-category))
      (cobweb-test test-examples root-category predict-features))))
  
(defun cobweb-test (examples category predict-features)
  "Test ability to infer missing values for predict-features.
   Prints out and returns overall accuracy of inference for these features."
  (let ((num-correct 0) percent-correct)
    (dolist (example examples)
      ;; Traverse category structure to find closest training example
      ;; and use it to infer values for the missing features.
      (let* ((leaf-category  (cobweb-classify (make-missing example predict-features) category))
	     (closest-example (first (category-instances leaf-category))))
	(mapc #'(lambda (value real-value feature)
		  (if (and (member feature predict-features)
			   (eq value real-value))
		      (incf num-correct)))
	      (second closest-example) (second example) *feature-names*)))
    (setf percent-correct (* 100 (/ num-correct (* (length examples) (length predict-features)))))
    (format t "~%Correct Prediction: ~,2F% on ~D examples" percent-correct (length examples))
    percent-correct))
				       
(defun make-missing (example features)
  "Change example to have missing values for the given feature names"
  (list (first example)
	(mapcar #'(lambda (value feature)
		    (if (member feature features)
			*missing-value*
			value))
		(second example) *feature-names*)))

(defun cobweb-classify (example category &optional (level 0))
  "Classify example down category structure and return the final leaf category"
  (if (null (category-subs category))   ; if leaf, stop and return
      category
      ;; Otherwise use sorted list of best existing hosts for example to determine which
      ;; existing sub-category to follow.
      (cobweb-classify example (potential-old-host (first (existing-category-potentials category example)))
		(1+ level))))


;;;; -------------------------------------------------------------------------------------------------------------
;;;; COBWEB as a supervised system
;;;; -------------------------------------------------------------------------------------------------------------


(defun train-cobweb (examples &optional root-category)
  (when examples
    (setf examples (make-ordered-examples examples nil))
    (let ((*domains* (cons *categories* *domains*))
	  (*feature-names* (cons 'category *feature-names*)))
      (cobweb-train (mapcar #'(lambda (ex) (list (first ex) (cons (first ex) (second ex)))) examples)
	           root-category))))

(defun test-cobweb (example root-category)
  (if (null root-category)
      (pick-one *categories*)
      (progn
	(setf example (make-ordered-example example nil))
	(let* ((*domains* (cons *categories* *domains*))
	       (*feature-names* (cons 'category *feature-names*)))
	  (first (first (category-instances
			  (cobweb-classify (list (first example)
						 (cons *missing-value* (second example))) root-category))))))))
