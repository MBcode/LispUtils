;;;; Standard utilities for dealing with feature-vector categorization data.
;;;; Contains a lot of miscellaneous stuff that needs to be cleaned up.

;;;; Copyright (c) 1991 by Raymond Joseph Mooney. This program may be freely copied, used, or
;;;; modified provided that this copyright notice is included in each copy of this code
;;;; and parts thereof.

;;;; Standard data file:  A standard example data file sets a set of standard
;;;; global variables. The UNIVERSAL-TESTER only crucially relies on *RAW-EXAMPLES* but the rest are
;;;; standard for many systems. 
;;;; *** Learning systems should not alter the values of these variables or destructively modify them ***

;;;; *CATEGORIES*:     A list of all categories (classes) present in the data.

;;;; *FEATURE-NAMES*:  An ordered list of names for the features used to describe examples.

;;;; *DOMAINS*:        An ordered list of domains for each feature where a domain is either a list
;;;;                   of possible values or the symbol LINEAR to indicate a real-valued feature.

;;;; *RAW-EXAMPLES*:   A list of examples where the first element of an example is its class.
;;;;                   The two standard formats for examples assumed by many systems are:
;;;;                        Ordered example: (<class> <ordered-feature-value-sequence>)  
;;;;                                    e.g. (+ (big red square))
;;;;                        Alist example:   (<class> (<feature> <value>) ... )  
;;;;                                    e.g. (+ (size big) (color red) (shape square))
;;;;                   ID3-ALL works with both where a sequence of feature values can be a list or a vector.
;;;;                   Vectors are usually more efficient.
;;;;                   UNIVERSAL-TESTER only assumes that the first element is the class and as long as
;;;;                   the learners are happy with the example format it is too.

(in-package :user)
(provide 'data-utilities)

;(defparameter *ml-progs-directory* "~mooney/ml-progs/")
(defparameter *ml-progs-directory* "/Users/bobak/Documents/downloads/lang/lsp/ai/ml/ml-progs/")
(defun ml-progs-file (file-name) (merge-pathnames file-name *ml-progs-directory*))

;;; Global variables used in data files

(defvar *raw-examples* nil    "List of examples")
(defvar *feature-names* nil   "A list of names for each feature")
(defvar *domains* nil         "A list defining the domain of each feature in the vector")
(defvar *categories* '(positive negative)   "A list of all categories in the data")
(defvar *positive-category* 'positive "The name of the category representing positive")
(defvar *negative-category* 'negative "The name of the category representing negative")

(defvar *missing-value* '?    "Feature value representing missing")
(defvar *noise*         nil   "Perform extra processing to handle noise and/or prevent over-fitting")
(defvar *tie-breaker* 'random "How to break ties (random or ordered)")

;;; Macros

(defmacro trace-print (test-var &rest format-form)
  ;;; When test-var (usually a *-trace variable) is set then use formated print
  `(if ,test-var
       (format t ,@format-form)))

(defmacro make-variant (new-system system parameters)
  "Define a variant of a system with certain parameter settings in a LET format"
  `(progn (defun ,(append-symbols 'train- new-system) (examples)
	    (let , parameters
	      (, (append-symbols 'train- system) examples)))
	  (defun ,(append-symbols 'test- new-system) (example result)
	    (let , parameters
	      (, (append-symbols 'test- system) example result)))
	  (if (fboundp (quote , (append-symbols 'train- system '-output)))
	      (defun ,(append-symbols 'train- new-system '-output) (result examples)
		(let , parameters
		  (, (append-symbols 'train- system '-output) result examples))))
	  (if (fboundp (quote ,(append-symbols 'test- system '-output)))
	      (defun ,(append-symbols 'test- new-system '-output) (result examples)
		(let , parameters
		  (, (append-symbols 'test- system '-output) result examples))))
	  (if (fboundp (quote ,(append-symbols system '-concept-complexity)))
	      (defun ,(append-symbols new-system '-concept-complexity) (result)
		(let , parameters
		  (, (append-symbols system '-concept-complexity) result))))
	  (setf (get (quote , new-system) 'parent-systems) (quote (, system)))
	  (setf (get (quote , new-system) 'expect-training-error) (get (quote , system) 'expect-training-error))
	  (setf (get (quote , new-system) 'incremental) (get (quote , system) 'incremental))))


(defmacro doseq (s &rest body)
  `(let ((seq ,(second s)))
     (if (listp seq)
	 (dolist (,(first s) seq ,(third s)) ,@ body)
       (let (,(first s))
	 (dotimes (i (length seq) ,(third s))
		  (setf ,(first s) (aref seq i))
		  ,@ body)))))
  
;;;;-------------------------------------------------------------------------------------------------------------
;;;;  Access functions for various info about data
;;;;-------------------------------------------------------------------------------------------------------------

(defun example-category  (example)
  (first example))

(defun example-instance (example)
  (second example))

(defun feature-name (feature-num)
  "Returns the name of a feature given its number"
  (or (elt *feature-names* feature-num) feature-num))

(defun feature-domain (feature)
  "Returns the domain of a feature given its number"
  (if (numberp feature)
      (elt *domains* feature)
      (elt *domains* (position feature *feature-names*))))

(defun feature-number (feature)
  (position feature *feature-names*))

(defun feature-value (feature example)
      (elt (second example)   (if (numberp feature)
				  feature
				  (feature-number feature))))

(defun binary-feature-p (feature)
  (member (feature-domain feature) '((true false)(false true)(f t)(t f)(no yes)(yes no)(0 1)(1 0))
	  :test #'equal))

(defun linear-feature-p (feature)
  "Returns T is given feature is linear"
  (eq (feature-domain feature) 'linear))

(defun pos-neg? ()
  "Returns T if categories represent positives and  negatives"
  (and (eq (length *categories*) 2)
       (or (member *negative-category* *categories*)
	   (let ((neg-categories (intersection *categories* '(- negative neg nil))))
	     (and neg-categories (null (rest neg-categories))
		  (setf *negative-category* (first neg-categories)))))
       (or (member *positive-category* *categories*)
	   (setf *positive-category* (first (remove *negative-category* *categories*))))))
   

;;;;-------------------------------------------------------------------------------------------------------------
;;;; Functions for checking data for consistency
;;;;-------------------------------------------------------------------------------------------------------------

(defun check-data (&optional (examples *raw-examples*))
  "Check for various errors in data file"
  (unless (eq (length *domains*)(length *feature-names*))
    (format t "~%~%Length of *domains* not same as *feature-names*"))
  (dolist (feature-name *feature-names*)
    (unless (symbolp feature-name)
      (format t "~%~%Illegal entry in *feature-names*: ~A" feature-name)))
  (dolist (domain *domains*)
    (unless (or (eq domain 'linear) (and domain (listp domain) (every #'atom domain)))
      (format t "~%~%Illegal entry in *domains**: ~A" domain)))
  (cond ((alist-example-p (first examples))
	 (format t "~%~% Looks like a-list examples")
	 (mapc #'check-alist-example examples))
	(t (format t "~%~%Looks like ordered examples")
	   (mapc #'check-ordered-example examples)))
  nil)


(defun eliminate-conflicting-examples  (examples)
  "Eliminate examples that have same features but different class,
   keeping ones that conform to the majority"
  (let ((conflicts-list (find-conflicting-examples examples))
	discards)
    (dolist (conflicts conflicts-list)
      (let ((majority-category (majority-category conflicts)))
	(dolist (example conflicts)
	  (unless (eq (first example) majority-category)
	    (push example discards)))))
    (set-difference examples discards)))

(defun majority-category (examples)
  "Return majority class of examples"
  (let (class-counts class-count)
    (dolist (example examples)
      (setf class-count (assoc (first example) class-counts))
      (if class-count
	  (incf (cdr class-count))
	  (push (cons (first example) 1) class-counts)))
    (maximum-label class-counts *categories*)))


(defun check-alist-example (example)
  (unless (member (first example) *categories*)
    (format t "~%~%Unknown class for: ~A" example))
  (dolist (pair (rest example))
    (unless
      (and (listp pair) (member (first pair) *feature-names*) (null (rest (rest pair)))
	   (or (and (rest pair) (or (and (numberp (second pair))
					 (linear-feature-p (first pair)))
				    (member (second pair) (feature-domain (first pair)))
				    (eq (second pair) *missing-value*)))
	       (binary-feature-p (first pair))))
      (format t "~%~%Illegal feature: ~A in ~%~A" pair example))))

(defun check-ordered-example (example)
  (unless (member (first example) *categories*)
    (format t "~%~%Unknown class for: ~A" example))
  (unless (and (or (consp (second example)) (arrayp (second example)))
	       (= (length (second example)) (length *domains*)))
    (format t "~%~%Illegal example: ~A" example))
  (dotimes (i (length *domains*))
    (let ((value (elt (second example) i))
	  (domain (elt *domains* i)))
      (unless
	(or (eq value *missing-value*)
	    (and (numberp value)
		 (eq domain 'linear))
	    (and (listp domain) (member value domain)))
	(format t "~%~%Illegal feature: ~A = ~A in ~%~A" (feature-name i) value example)))))

(defun find-conflicting-examples (&optional (examples *raw-examples*))
  "Return list of lists of examples that have all same features but
   not all same class"
     (when examples
       (let (conflict matches (test (first examples)))
	 (dolist (ex (rest examples))
	   (when (equalp (rest test) (rest ex))
	     (push ex matches)
	     (if (not (equal (first test) (first ex)))
		 (setf conflict t))))
	 (if conflict
	     (cons (cons (first examples) matches)
		   (find-conflicting-examples (set-difference (rest examples) matches)))
	     (find-conflicting-examples (rest examples))))))

(defun miss-conflicting-examples (&optional (examples *raw-examples*))
  "Return list of lists of examples that have all same features but
   not all same class"
     (when examples
       (let (conflict matches (test (first examples)))
	 (dolist (ex (rest examples))
	   (when (or (as-undefined (second test) (second ex))
		     (as-undefined (second ex) (second test)))
	     (push ex matches)
	     (if (not (eq (first test) (first ex)))
		 (setf conflict t))))
	 (if conflict
	     (cons (cons (first examples) matches)
		   (miss-conflicting-examples (set-difference (rest examples) matches)))
	     (miss-conflicting-examples (rest examples))))))

(defun as-undefined (inst1 inst2)
  (loop for x in inst1 as y in inst2 always
	(or (eq x *missing-value*) (eq x y))))
  

;;;;-------------------------------------------------------------------------------------------------------------
;;;; Functions for computing information about data
;;;;-------------------------------------------------------------------------------------------------------------

(defun compute-domains (&optional (features *feature-names*) (examples *raw-examples*))
  "Compute feature domains from examples"
  (mapcar #'(lambda (feature) (compute-domain feature examples)) features))

(defun compute-domain (feature &optional (examples *raw-examples*))
  (let (domain)
    (if (alist-example-p (first examples))
	(dolist (ex examples domain)
	  (let ((pair (assoc feature (rest ex))))
	    (if pair
		(pushnew (second pair) domain))))
	(dolist (ex examples domain)
	  (pushnew (feature-value feature ex) domain)))))

(defun compute-ranges (&optional (features *feature-names*) (examples *raw-examples*))
  "For each feature in features with undefined range, compute it from the examples.
   Return list of (feature-name min max)"
  (setf examples (make-ordered-examples examples))
  (let (ranges)
    (dolist (feature features (nreverse ranges))
      (when (and (linear-feature-p feature) (null (get feature 'range)))
	(let ((min most-positive-fixnum)(max most-negative-fixnum) value)
	  (dolist (example examples (push (list feature min max) ranges))
	    (setf value (feature-value feature example))
	    (unless (eq value *missing-value*)
	      (when (< value min) (setf min value))
	      (when (> value max) (setf max value)))))))))


(defun describe-data (&optional count-missing (examples *raw-examples*))
  (let* ((groups (group-examples examples))
	 (num-examples (length examples))
	 (num-categories (length groups))
	 (unrep-cats (set-difference *categories* (mapcar #'first groups)))
	 (num-feature-values (reduce #'+ (map 'list #'(lambda (d) (if (atom d)
								   0
								   (length d)))
						  *domains*)))
	 (num-features (length *feature-names*))
	 (num-linear-features (count-if #'(lambda (d)
					    (eq d 'linear))
					*domains*))
	 (num-binary-features (count-if #'binary-feature-p
					*feature-names*))
	 (num-nominal-features (- num-features num-linear-features num-binary-features)))
    (format t "~%Number of examples: ~A" num-examples)
    (format t "~%Number of features: ~A" num-features)
    (format t "~%Number of linear features: ~A (~,2F%)" num-linear-features (* 100 (/ num-linear-features num-features)))
    (format t "~%Number of binary features: ~A (~,2F%)" num-binary-features (* 100 (/ num-binary-features num-features)))
    (format t "~%Number of nominal features: ~A (~,2F%)" num-nominal-features (* 100 (/ num-nominal-features num-features)))
    (format t "~%Number of categories: ~A (~,2F% random guess)" num-categories (* 100 (/ 1 num-categories)))
    (format t "~%Unrepresented categories: ~A" unrep-cats)
    (format t "~%Number of examples per category:")
    (dolist (group (sort groups #'(lambda (g1 g2) (member (first g2) (member (first g1) *categories*)))))
      (let ((n (length (rest group))))
	(format t "~%  ~A: ~A (~,2F%)" (first group) n (* 100 (/ n num-examples)))))
    (format t "~%Average number of examples per category: ~,2F" (/ num-examples num-categories))
    (unless (zerop (- num-features num-linear-features))
      (format t "~%Average number of feature values: ~,2F"
	      (/ num-feature-values (- num-features num-linear-features))))
    (if count-missing (count-missing examples))))
  
(defun group-examples (examples)
  (let (alist)
    (dolist (example examples alist)
      (let ((set (assoc (first example) alist)))
	(if set
	    (nconc set (list example))
	    (push (list (first example) example) alist))))))

(defun count-missing (&optional (examples *raw-examples*))
  "Give info about missing values"
  (setf examples (make-ordered-examples examples))
  (let ((value-count 0) (example-count 0) (num-examples (length examples))
	(num-features (length (second (first examples)))))
    (dolist (example examples)
      (let ((count (count *missing-value* (second example))))
	(unless (zerop count)
	  (incf value-count count)
	  (incf example-count))))
    (format t "~%There are ~A missing values (~,2F%)" value-count (* 100 (/ value-count (* num-examples num-features))))
    (format t "~%There are ~A examples with missing values (~,2F%)" example-count (* 100 (/ example-count num-examples)))))

(defun delete-examples-with-missing (&optional (examples *raw-examples*))
  "Delete examples with missing values for ordered examples"
  (delete-if #'(lambda (ex) (member *missing-value* (second ex))) (make-ordered-examples examples)))
  
(defun remove-categories (categories &optional (examples *raw-examples*))
  "Delete examples in these categories"
  (remove-if #'(lambda (ex) (member (first ex) categories)) examples))

;;;;-------------------------------------------------------------------------------------------------------------
;;;; Simple Function for Testing a training result of a system  (see universal-tester for more testing software)
;;;;-------------------------------------------------------------------------------------------------------------

(defun test-system (system training-result test-examples &optional (print-results t))
  (let ((test-function (append-symbols 'test- system))
	(num-examples (length test-examples))
	(num-correct 0)
	answer)
    (dolist (example test-examples)
      (setf answer (funcall test-function example training-result))
      (when (equal answer (first example)) (incf num-correct))
      (trace-print print-results "~%~AReal category: ~A; Classified as: ~A"
		 (if (equal answer (first example)) "  " "**")  (first example) answer))
    (format t "~%~%~A classified ~,2F% of the ~D test cases correctly."
	    test-function (* 100 (/ num-correct num-examples)) num-examples)))


;;;;-------------------------------------------------------------------------------------------------------------
;;;; Functions for converting between different data formats
;;;;-------------------------------------------------------------------------------------------------------------

(defun alist-example-p (example)
  "Return T if example is in alist form"
  (or (rest (rest example))
      (null (rest example))
      (let ((second-elt (second example)))
	(and (consp second-elt)
	     (null (rest (rest second-elt)))
	     (member (first second-elt) *feature-names*)))))

(defun make-ordered-examples (examples &optional (array-flag 'prefer))
  (cond ((alist-example-p (first examples))
	 (mapcar #'(lambda (ex) (convert-to-ordered-example ex array-flag)) examples))
	((and (null array-flag) (arrayp (second (first examples))))
	 (mapcar #'(lambda (ex) (list (first ex) (coerce (second ex) 'list))) examples))
	((and (eq array-flag t) (listp (second (first examples))))
	 (mapcar #'(lambda (ex) (list (first ex) (coerce (second ex) 'array))) examples))
	(t examples)))

(defun make-ordered-example (example &optional (array-flag 'prefer))
  (cond ((alist-example-p example)
	 (convert-to-ordered-example example array-flag))
	((and (null array-flag) (arrayp (second example)))
	 (list (first example) (coerce (second example) 'list)))
	((and (eq array-flag t) (listp (second example)))
	 (list (first example) (coerce (second example) 'array)))
	(t example)))

(defun convert-to-ordered-example (example &optional (array-flag t))
  "Convert an example from alist form to ordered feature form (an array
   if flag set)"
  (let ((counter 0)
	output-example
	(array (if array-flag (make-array (length *domains*)))))
    (dolist (feature-name *feature-names* (list (first example) (if array-flag
								    array
								    (reverse output-example))))
      (let* ((item (find feature-name (cdr example) :key #'car))
	     (value (if (and item (second item))
			 (second item)
			 (let ((domain (elt *domains* counter)))
			   (if (and (consp domain)(member 'true domain))
			       (if item 'true 'false)
			       (if item (error "No value for ~A" item) *missing-value*))))))
	(if array-flag
	    (setf (aref array counter) value)
	    (push value output-example))
	(incf counter)))))

(defun make-alist-examples (examples &optional falses-present)
  (if (not (alist-example-p (first examples)))
      (mapcar #'(lambda (ex) (convert-to-alist-example ex falses-present)) examples)
      examples))

(defun make-alist-example (example &optional falses-present)
  (if (not (alist-example-p example))
      (convert-to-alist-example example falses-present)
      example))

(defun convert-to-alist-example (example &optional falses-present)
  (let (instance)
    (dotimes (i (length *feature-names*) (cons (first example) (nreverse instance)))
      (let ((value (elt (second example) i)))
	(unless (or (eq value *missing-value*) (and (not falses-present)(eq value 'false)))
	  (push (if (and (not falses-present)(eq value 'true))
		    (list (elt *feature-names* i))
		    (list (elt *feature-names* i) value))
		instance))))))

(defun convert-category-examples ()
  (mapcan #'(lambda (cat)
	      (mapcar #'(lambda (instance) (list cat instance))
		      (eval cat)))
	  *categories*))

;;;;-------------------------------------------------------------------------------------------------------------
;;;; Accessory functions for data
;;;;-------------------------------------------------------------------------------------------------------------

(defun make-domains (levels-list)
  ;;; If features values are simply integers 0 to n then the number of values of a
  ;;; feature is sufficient for determining its domain.  This function creates a
  ;;; list suitable for *domains* given a list of the number of values for each feature.
  ;;; See the file SOYBEAN-RDATA for a sample use.

  (mapcar #'(lambda (levels) (let ((domain nil)) 
			       (dotimes (i levels domain)
				 (setf domain (nconc domain (list i))))))
	  levels-list))

(defun make-examples (pos-instances neg-instances)
  ;;; Converts lists of positive and negative instances into a list of examples
  ;;; suitable for ID3.

  (append (mapcar #'(lambda (instance) (list '+ instance)) pos-instances)
	  (mapcar #'(lambda (instance) (list '- instance)) neg-instances)))

(defun make-ranges (range-list)
  (dolist (range-element range-list)
    (setf (get (first range-element) 'range) (rest range-element))))


(defun binary-domains ()
  (make-array (list (length *feature-names*)) :initial-element '(0 1)))

(defun index-features ()
  (dotimes (i (length *feature-names*))
       (setf (get (elt *feature-names* i) 'number) i)))
		      
(defun expand-examples (examples)
  (index-features)
  (loop for cat in examples nconc
	(loop for ex in (rest cat)
	      collect (list (first cat)
			    (expand-example ex)))))

(defun expand-example (ex)
  (let ((vector (make-array (length *feature-names*) :element-type 'bit
			     :initial-element 0)))
    (dolist (stem ex vector)
	    (setf (aref vector (get stem 'number)) 1))))

(defun label-category-examples (examples)
  (loop for cat in examples nconc
	(loop for ex in (rest cat)
	      collect (list (first cat) ex))))

;;;;-------------------------------------------------------------------------------------------------------------
;;;;  Miscellaneous functions
;;;;-------------------------------------------------------------------------------------------------------------


(defun maximum-label (count-alist &optional tie-breaker-list)
  "Returns the label in count-alist ((label . count) ...)
   with the maximum count.  Break ties according to *tie-breaker*"
  (let (max-labels (max-count 0))
    (dolist (count-cons count-alist)
      (cond ((> (cdr count-cons) max-count)
	     (setf max-count (cdr count-cons))
	     (setf max-labels (list (car count-cons))))
	    ((= (cdr count-cons) max-count)
	     (push (first count-cons) max-labels))))
    (if (or (eq *tie-breaker* 'random) (null tie-breaker-list))
	(pick-one max-labels)
	(dolist (item tie-breaker-list)
	  (when (member item max-labels)
	    (return item))))))

(defun random-subseq (list length &optional (destructive nil))
  (unless (or (zerop length) (null list))
    (unless destructive (setf list (copy-list list)))
    (loop for i from 1 to length as len from (length list) downto 1
	  with position and tail and pretail and result
	  do (setf position (random len))
	     (if (zerop position)
		 (progn (setf tail list)
			(setf list (rest list)))
		 (progn (setf pretail (nthcdr (1- position) list))
			(setf tail (rest pretail))
			(setf (rest pretail) (rest tail))))
	     (setf (rest tail) result)
	     (setf result tail)
	   finally (return (values result list)))))


(defun mix-up (list)
  "Randomize the order of elements in this list."
  (mapcar #'(lambda (pair) (rest pair))
	  (sort (mapcar #'(lambda (item) (cons (random 1.0) item)) list)
		#'(lambda (a b) (> (first a) (first b))))))

(defun pick-one (list)
  "Pick an item randomly from the list"
  (nth (random (length list)) list))

(defun read-file (file-name)
  (with-open-file (input file-name :direction :input)
    (read  input nil nil)))

(defun write-data-file (filename &optional (pretty-print t)
			(var-list '(*feature-names* *domains* *categories* *raw-examples*)))
  (unless (equal (pathname-type filename) "lisp") (setf filename (concatenate 'string filename ".lisp")))
  (let ((*print-pretty* pretty-print) (*print-array* t))
    (with-open-file (file filename :direction :output :if-exists :new-version)
      (dolist (var var-list)
	(if (eq var '*raw-examples*)
	    (progn (format file "~%(setf *raw-examples* '(")
		   (dolist (ex (eval var))
		     (format file "~%  ~A" ex))
		   (format file "~%))"))
	    (format file "~%(setf ~A~%  ~A)~%" var (list 'quote (eval var))))))))


(defun read-line-list (stream)
  (read-from-string (concatenate 'string "(" (read-line stream nil) ")")))

(defun square (x)
  (* x x))

(defun /-float (a b)
  "Division forcing a floating point output"
   (/ (coerce a 'single-float) (coerce b 'single-float)))

(defun append-symbols (&rest symbols)
  (intern (format nil "~{~A~}" symbols)))

(defun seconds-since (time)
   ;;; Return seconds elapsed since given time (initially set by get-internal-run-time)
  (float (/ (- (get-internal-run-time) time)
	    internal-time-units-per-second)))

(defun set-equal (set1 set2 &key (test #'eql))
  "Return T if two sets are equal (assumes sets contains no duplicates)"
  (let ((temp set2)(flag t))
    (dolist (elt1 set1)
      (unless (member elt1 set2 :test test)
	(setf flag nil))
      (setf temp (rest temp)))
    (and flag (null temp))))

(defun find-duplicates (list &key (test #'eql) (key #'identity))
  "Return lists of duplicates"
     (when list
       (let (matches (item (first list)))
	 (dolist (elt (rest list))
	   (when (funcall test (funcall key item) (funcall key elt))
	     (push elt matches)))
	 (if matches
	     (cons (cons (first list) matches)
	       (find-duplicates (set-difference (rest list) matches) :test test :key key))
	     (find-duplicates (rest list) :test test :key key)))))

(defun delete-feature (feature &optional (examples *raw-examples*))
  (unless (numberp feature) (setf feature (feature-number feature)))
  (setf *feature-names* (delete-nth feature *feature-names*))
  (setf *domains* (delete-nth feature *domains*))
  (loop for example in examples do
	(setf (second example) (delete-nth feature (second example)))))

(defun delete-nth (n list)
  (if (= n 0)
      (rest list)
      (progn (setf (rest (nthcdr (1- n) list))
		   (nthcdr (1+ n) list))
	     list)))

(defvar train nil)
(defvar test nil)
(defun data-sample (train-size &optional test-size (randomize t))
  (cond (randomize
	 (multiple-value-setq (train test) (random-subseq *raw-examples* train-size))
	 (if test-size (setf test (random-subseq test test-size))))
	(t (setf train (subseq *raw-examples* 0 train-size))
	   (setf test (subseq *raw-examples* train-size (if test-size (+ train-size test-size))))))
  nil)


;;;;-------------------------------------------------------------------------------------------------------------
;;;;           Make a data file from a C4.5 type data file
;;;;-------------------------------------------------------------------------------------------------------------

(defparameter *white-space* '(#\space #\tab #\newline #\return #\linefeed))

(defparameter *comment-char* #\|)

(defparameter *line-terminators* '(#\newline #\return #\linefeed))

(defparameter *item-terminators* '(#\, #\newline #\return #\linefeed))

(defun translate-c4.5-data-file (file-stem &optional output-file)
  (setf *feature-names* nil *domains* nil *raw-examples* nil
	*categories* nil)
  (let (item char domain ignore-features (feature-num 0))
    (with-open-file (input (format nil "~A.names" file-stem) :direction :input)
      (read-white-space input)
      (loop 
       (multiple-value-setq (item char)  (read-until-char *item-terminators* input))
       (push item *categories*)
       (if (member char *line-terminators*) (return nil)))
      (setf *categories* (nreverse *categories*))
      (loop 
       (unless (read-white-space input)
	 (setf *feature-names* (nreverse *feature-names*)
	       *domains* (nreverse *domains*))
	 (return nil))
       (setf item (read-until-char '(#\:) input))
       (push item *feature-names*)
       (incf feature-num)
       (setf domain nil)
       (loop
	(multiple-value-setq (item char)  (read-until-char *item-terminators* input))
	(push item domain)
	(if (member char *line-terminators*) (return nil)))
       (if (and (eq (first domain) 'ignore)
		(null (rest domain)))
	   (progn (push feature-num ignore-features)
		  (pop *feature-names*))
	   (push (if (and (eq (first domain) 'continuous)
			  (null (rest domain)))
		     'linear
		     (nreverse domain))
		 *domains*))))
    (setf *raw-examples* (nconc (read-c4.5-data (format nil "~A.data" file-stem) ignore-features)
				(read-c4.5-data (format nil "~A.test" file-stem) ignore-features))))
  (when output-file (write-data-file output-file))
  )


(defun read-c4.5-data (file &optional ignore-features &aux item char example examples feature-num)
  (with-open-file (input file :direction :input :if-does-not-exist nil)
    (loop 
     (unless (read-white-space input)
       (return (nreverse examples)))
     (setf example nil feature-num 0)
     (push (loop
	    (incf feature-num)
	    (multiple-value-setq (item char)  (read-until-char *item-terminators* input))
	    (if (member char *line-terminators*)
		(return (list item (coerce (nreverse example) 'array)))
		(unless (member feature-num ignore-features)
		  (push item example))))
	   examples))))
    

(defun read-until-char (char-list stream)
  (let (char chars)
    (loop (setf char (read-char stream))
	  (cond ((member char char-list) (read-white-space stream)
		 (return (values (symbolize-string (chars-to-string chars)) char)))
		((char= char *comment-char*) (read-line stream) (unread-char #\newline stream))
		(t (push char chars))))))

(defun chars-to-string (char-list)
  (let* ((length (length char-list))
	 (string (make-string length)))
    (loop for char in char-list as i from (1- length) downto 0
	  do (setf (char string i) char))
    string))
	
    
(defun read-white-space (stream)
  (let (char)
    (loop (setf char (peek-char nil stream nil nil))
	  (cond ((null char)(return nil))
		((member char *white-space*)
		 (read-char stream))
		((char= char *comment-char*) (read-line stream))
	        (t (return t))))))


(defun symbolize-string (string)
  (read-from-string (nsubstitute #\_ #\Space (string-trim (cons #\. *white-space*) string))))
