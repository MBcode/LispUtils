;;;; This is a UNIVERSAL TESTER that can be used to generate learning curve, n-fold cross-validation, leave-one-out
;;;; comparisons between multiple learning systems and generate a standard output file that can be used
;;;; to generate graphs (currently has facilities for generating input for UNIX X-GRAPH) and statistical
;;;; testing (see the file T-TEST which runs a t-test on all pairs of systems).

;;;; Copyright (c) 1991 by Raymond Joseph Mooney. This program may be freely copied, used, or
;;;; modified provided that this copyright notice is included in each copy of this code
;;;; and parts thereof.

;;;; Standard data file:  A standard example data file (possibly with a theory) sets a set of standard
;;;; global variables. The UNIVERSAL-TESTER only crucially relies on *RAW-EXAMPLES* but the rest are
;;;; standard for many systems. 
;;;; *** Learning systems should not alter the values of these variables or destructively modify them ***
;;;; *** Changes should only be made by using dynamic scoping to temporarily rebind them              ***

;;;; *CATEGORIES*:     A list of all categories (classes) present in the data.
;;;;                   One of these categories can be NEGATIVE which is the default.

;;;; *FEATURE-NAMES*:  An ordered list of names for the features used to describe examples.

;;;; *DOMAINS*:        An ordered list of domains for each feature where a domain is either a list
;;;;                   of possible values or the symbol LINEAR to indicate a real valued feature.

;;;; *RAW-EXAMPLES*:   A list of examples where the first element of an example is its class.
;;;;                   The two standard formats for examples assumed by many systems are:
;;;;                        Ordered example: (<class> <ordered-feature-value-sequence>)  
;;;;                                    e.g. (+ (big red square))
;;;;                        Alist example:   (<class> (<feature> <value>) ... )  
;;;;                                    e.g. (+ (size big) (color red) (shape square))
;;;;                   ID3-ALL works with both where a sequence of feature values can be a list or an array.
;;;;                   UNIVERSAL-TESTER only assumes that the first element is the class and as long as
;;;;                   the learners are happy with the example format it is too.
;;;;                   Most learning systems should be written to handle both data formats by using
;;;;                   conversion routines in DATA-UTILITIES to convert to correct format if needed.
;;;; *THEORY*:         For theory revision problems a list of rules suitable for DEDUCE.

;;;; Standard for systems: A standard learning system X should have a couple of interface functions 
;;;; to be used by UNIVERSAL-TESTER 
;;;; (TRAIN-X <examples>):  Takes a list of training examples (taken from *raw examples*)
;;;;                        and returns a data structure representing the learned definition.
;;;;                        It normally makes use of the information in the variables described above.
;;;; (TEST-X <example> 
;;;;    <training-result>): Takes an example (with category) and the result returned by the trainer and
;;;;                        returns a category (i.e. if this is EQ to (first <example>) it is correct).

;;;; If the system is incremental and can incorporate new examples into an evolving definition,
;;;; then the INCREMENTAL property of the system name should be set (e.g. (get 'X 'incremental) -> T).
;;;; The TRAIN-X function for an incremental system should take the existing learned structure
;;;; (result of previous training) as a second argument. A value of NIL is given as the initial
;;;; learned structure at the begining of each trial. A normal incremental system is only given
;;;; the new training examples added since the last increment.  If the INCREMENTAL property value is
;;;; the special symbol FULL-DATA, then it is given all of the training data at each increment.

;;;; Optionally one can define the following additional functions
;;;; (TRAIN-X-OUTPUT <training-result>  : Called after training X to print out any information desired.
;;;;              <training-examples>) 
;;;; (TEST-X-OUTPUT <training-result>   : Called after testing X to print out any informtation desired.
;;;;                  <test-examples>)
;;;; (X-CONCEPT-COMPLEXITY
;;;;        <training-result>) :Returns a number representing the complexity of the training result
;;;;                            This data is also stored in the output file.

;;;;  Properties that can be attached to a system name include:
;;;;
;;;; INCREMENTAL:            Set to T if system is incremental (set to FULL-DATA if require all previous data).
;;;; EXPECT-TRAINING-ERROR:  Set to T if system not guaranteed to fit training data even when there is no noise.
;;;; PARAMETERS:             Set to a list of global parameter variable names whose values should be included
;;;;                         in the comments of an output file.                      
;;;; PARENT-SYSTEMS:         Set to a list of systems upon which the current system relies.  The parameters of
;;;;                         all "ancestor" systems are included in the comments
;;;; ALWAYS-TRAIN-CORRECT:   Set to T if you do just want the training accuracy of this system always assumed to
;;;;                         be 100% even without checking.

;;;; Systems defined outside of LISP must specify their own train-and-testing function.
;;;; The EXTERNAL property of the system name should be set to T to indicate this.
;;;; In this case the following functions are expected to be defined.  See c4.5.lisp as an example.

;;;; (PREPARE-X)             Called at the very begining of a test to set up data files.
;;;; (PREPARE-TRIAL-X        Called at the begining of every trial to set up test examples.
;;;;   <total-training-examples>
;;;;   <test-examples>)
;;;; (TRAIN-AND-TEST-X       Called at each plotted point, should return a list of results
;;;;   <training-examples>   of the form specified in *data-format*.
;;;;   <new-training-examples>
;;;;   <test-examples>)

;;;; Standard for output file:  UNIVERSAL-TESTER generates an output file in a standard format.
;;;; There are a number of comments at the beginning listing information about the run gathered
;;;; from the arguements to the testing function. After that the format is:
;;;; <list-of-system-names>
;;;; <trial-result>
;;;; <trial-result>
;;;; ...
;;;; Where a trial result is a list of data items for each point along a single learning curve.
;;;; ((<num-training> <results-for-system1> <results-for-system2>...)
;;;;  (<num-training> <results-for-system1> <results-for-system2>...)
;;;;  ...)
;;;; Where the system results are ordered as they appear in <list-of-system-names>
;;;; and each result is of the form:
;;;; (<training-set--correctness> <training-time> <test-set-correctness> <test-time> <concept-complexity>)
;;;; The format of a result is kept in *data-format* to allow for easy change to the code to add more items.

;;;; MAKE-PLOT-FILE is used to generate a plot file for X-GRAPH from this output file.
;;;; T-TEST-FILE in the file T-TEST can be used to run statistical t-tests on the results in this file.

;;;; The standard function used to run tests are RUN-STANDARD-TESTS, RUN-LEAVE-ONE-OUT
;;;; and RUN-CV-TESTS (n-fold cross validation)

;;;; In order to allow running different systems at different times, one can generate and save a set of
;;;; training-set/test-set splits of a given set of data in a file using MAKE-SAVED-TESTS.
;;;; After making such a file of stored train/test trials, RUN-SAVED-TESTS can be used to run a system on 
;;;; these trials.  The function COMBINE-TEST-RESULTS combines two results files for the same saved
;;;; tests into one result file.  There is also MAKE-SAVED-CV-TESTS and RUN-SAVED-CV-TESTS

(in-package :user)
(provide 'universal-tester)

;(defparameter *ml-progs-directory* "~mooney/ml-progs/")
(defparameter *ml-progs-directory* "/Users/bobak/Documents/downloads/lang/lsp/ai/ml/ml-progs/")
(defun ml-progs-file (file-name) (merge-pathnames file-name *ml-progs-directory*))

(eval-when (compile load eval)
 (require 'data-utilities (ml-progs-file "data-utilities"))
 ;(require 't-test         (ml-progs-file "t-test"))
 (require 't-test         (ml-progs-file "t-test.lisp"))
 )

;;;; Global parameters
;;;;------------------------------------------------------
(defparameter *training-error-dump* nil "Dumps data to a file if not get 100% correct on training data")
;; Will not create dump if either the *noise* flag is set or the EXPECT-TRAINING-ERROR property of
;; the system name is set for a particular system that does not insure training-set consistency.
;; The training data with the error will be saved in a -BUG file for debugging purposes.

(defparameter *data-format* '(train-accuracy test-accuracy train-time test-time concept-complexity))
(defparameter *print-training-result* nil "Print item representing result of training")

;;;; Variables used in the processing of examples. 
(defvar *current-data-file* nil)
(defvar *output-file* nil)
(defvar *systems* nil)
(defvar *current-result-file* nil)

;;;;----------------------------------------------------------------------------------------------------------
;;;; Macros for Universal Tester
;;;;----------------------------------------------------------------------------------------------------------

(defmacro  append-file (output-file &rest format-forms)
  `(when ,output-file
     (with-open-file (output ,output-file :direction :output :if-exists :append
			      :if-does-not-exist :create)
     (format output ,@format-forms))))

(defmacro funcall-if-exist (function &rest args)
  `(let ((function ,function))
     (if (fboundp function)
	 (funcall function ,@args))))

;;;;----------------------------------------------------------------------------------------------------------
;;;; Basic Testing Functions
;;;;----------------------------------------------------------------------------------------------------------

(defun run-standard-tests (systems number-trials number-training &optional (training-increment 10)
			   number-test data-file output-file (initial-training 0))
  
  "Run standard learning curves comparing a number of systems and generate universal data file.
   systems: List of system names. Each system should have functions 
         (train-<system> training-examples) and (test-<system> test-example data-structure)
         where data-structure represents the learned definition returned by the trainer.
   number-of-trials: Number of runs to average over.
   number-training: Maximum number of training instances in curve.
   training-increment: Number of examples between plotted points on curve, can be list of points
   number-test: Number of examples kept aside for testing performance (leave NIL to get what's left).
   data-file:  Standard data file used for input (leave NIL to get already loaded data file).
   output-file: Name of output file for collected data (leave NIL if don't want an output file).
   initial-training: Number of training examples to start learning curve at."

  (load-data data-file)
  (run-all-tests systems number-trials 
		 (list 'example-generator number-training number-test)
		 training-increment  output-file initial-training
		 (format nil "; Data file: ~A~%"  (namestring *current-data-file*))))

  
(defvar *saved-splits* nil "Stored saved splits for use by saved-example-generator")

(defun run-saved-tests (systems saved-splits-file  &optional output-file (start-trial 1)
			training-increment  initial-training)
  
  "Run standard learning curves comparing a number of systems and generate universal data file.
   Gets training/test splits from a special file generated by MAKE-SAVED-TESTS so that
   exact same splits can be saved and rerun later.
   systems: List of system names. Each system should have functions 
         (train-<system> training-examples) and (test-<system> test-example data-structure)
         where data-structure represents the learned definition returned by the trainer.
   saved-test-file: Name of file storing the saved train/test splits (made by make-saved-tests)
   training-increment: Number of examples between plotted points on curve
                       (uses one stored in split file if not given).
   output-file: Name of output file for collected data (leave NIL if don't want an output file).
   initial-training: Number of training examples to start learning curve at
                     (uses one stored in split file if not given)."

  (let (data-file)
    (with-open-file (input saved-splits-file :direction :input)
      (setf data-file (read input))
      (if training-increment  (read input) (setf training-increment (read input)))
      (if initial-training (read input) (setf initial-training (read input)))
      (setf *saved-splits* (nthcdr  (1- start-trial) (read input))))
    (load-data data-file))
  (run-all-tests systems (length *saved-splits*)  (list 'saved-example-generator)
		 training-increment output-file  initial-training 
		 (format nil "; Saved splits file: ~A" (namestring (probe-file saved-splits-file)))
		 start-trial))

(defvar *previous-examples* nil)
(defvar *remaining-examples* nil)

(defun run-leave-one-out (systems &optional data-file output-file (start-number 1))
  "Runs a standard leave-one-out test on the data.  See run-standard-test for arg descriptions
   Starts with leaving out <start-number> example in *raw-examples*"

  (load-data data-file)
  (let ((num-examples (length *raw-examples*)))
    (setf *previous-examples* (subseq *raw-examples* 0 (1- start-number)))
    (setf *remaining-examples* (nthcdr (1- start-number) *raw-examples*))
    (run-all-tests systems (- num-examples (1- start-number))
		 '(leave-one-out) 1 output-file (1- num-examples)
		 (format nil "; Data file: ~A~%; Leave one out"
			 (namestring *current-data-file*))
		 start-number)))

(defun run-one-point (systems number-trials number-training &optional number-test 
		      data-file output-file)
  (run-standard-tests systems number-trials number-training 1 number-test data-file
		      output-file number-training))

(defvar *training-length* nil) ; This is a hack to get around the fact that training set size
                               ; can actually vary by one in cross-validation.  This is the
                               ; calculated to be the more frequent length to use as training
                               ; set size in the result file.  To indicate this, initial-training
                               ; is set to nil in CV tests and run-all-tests triggers on this to
                               ; do something special.

(defun run-cv-tests (systems num-folds data-file output-file)
  "Runs standard n-fold cross-validation"
  (load-data data-file)
  (setf *previous-examples* nil)
  (setf *remaining-examples* (segment-examples *raw-examples* num-folds))
  (multiple-value-bind (size remainder)
      (floor (length *raw-examples*) num-folds)
    (setf *training-length* (+ (* (1- num-folds) size) remainder ; calculate most common size of training
			       (if (> remainder (/ num-folds 2)) ; set since can vary by 1
				   -1 0))))
  (run-all-tests systems num-folds '(cross-validation-generator)
		1 output-file nil
		(format nil "; ~D-fold cross-validation run. Data file: ~A~%"
			    num-folds (namestring *current-data-file*))))

(defun run-saved-cv-tests (systems saved-test-file &optional output-file (start-trial 1))
  "Runs standard n-fold-cross validation where the specific splits used are from a
   file of saved-splits created by make-saved-cv-tests"
  (let (data-file segments num-folds)
    (with-open-file (input saved-test-file :direction :input)
      (setf data-file (read input))
      (setf segments (read input)))
    (load-data data-file)
    (loop for segment in segments do  ; replace example numbers in saved segments with actual examples
	  (loop for tail on segment do (setf (first tail) (elt *raw-examples* (first tail)))))
    (setf num-folds (length segments))
    (setf *previous-examples* (apply #'nconc (subseq segments 0 (1- start-trial))))
    (setf *remaining-examples* (nthcdr (1- start-trial) segments))
    (multiple-value-bind (size remainder)
      (floor (length *raw-examples*) num-folds)
      (setf *training-length* (+ (* (1- num-folds) size) remainder  ; calculate most common size of training
				 (if (> remainder (/ num-folds 2))  ; set since can vary by 1
				     -1 0))))
    (run-all-tests systems (length *remaining-examples*) '(cross-validation-generator)
		   1 output-file nil
		   (format nil "; Saved ~D-fold cross-validation test file: ~A~%"
			   num-folds (namestring (probe-file saved-test-file)))
		   start-trial)))


(defun load-data (data-file &optional always-load)
  (when data-file
    (let ((pathname (probe-file data-file)))
      (unless pathname
	(unless (setf pathname (probe-file (concatenate 'string data-file ".lisp")))
	  (error "Data file: ~A not found" data-file)))
      (unless (and (not always-load)  *current-data-file*
		   (equal pathname *current-data-file*))
	(setf *feature-names* nil *domains* nil *raw-examples* nil *categories* '(positive negative)
	      *positive-category* 'positive *negative-category* 'negative)
	(setf *current-data-file* pathname)
	(load pathname)
	(pos-neg?)  ; check if positive and negative dataset 
	nil))))

;;;;----------------------------------------------------------------------------------------------------------
;;;; Internal Testing stuff
;;;;----------------------------------------------------------------------------------------------------------

(defun run-all-tests
       (systems number-trials example-generator-form training-increment
	&optional (output-file nil) (initial-training 0) output-comment
	          (start-trial 1))
  (let* ((existing-file (probe-file output-file))
	 (*output-file*  (if (and existing-file (= start-trial 1)
				  (not (y-or-n-p "Append data to existing file: ~A (no means overwrite)?"
						 existing-file)))
			     (progn (delete-file existing-file)(setf existing-file nil) output-file)
			     (or existing-file output-file))))
    (setf *systems* systems)
    (dolist (system systems)
      (funcall-if-exist (append-symbols 'prepare- system)))
    (dotimes (index number-trials)
      (multiple-value-bind (total-training-examples test-examples)
	  (eval example-generator-form)
	;; If first trial and new output file print out header information
	(when (and (zerop index) (not existing-file))
	  (if output-comment (append-file *output-file* "~A" output-comment))
	  (append-file *output-file*
		       "~%~%; Number of trials: ~A,~%; Training increment: ~A,~%; Training start: ~A,"
		       number-trials training-increment initial-training)
	  (append-file *output-file* "~%; Maximum training examples: ~A,~%; Number of test examples: ~A."
		       (length total-training-examples) (length test-examples))
	  (mapc #'note-parameters (remove-duplicates
				    (mapcan #'(lambda (system) (cons system (ancestor-systems system)))
					    systems)))
	  (append-file *output-file* "~%~%~A~%" systems))
	(format t "~%~%~80~")
	(format t "~%Results for trial ~A:" (+ start-trial index))
	(let ((trial-results (run-one-trial total-training-examples training-increment test-examples
				    systems initial-training)))
	  (append-file *output-file* "~%~A" trial-results))))
    (format t "~%~%~80~~%")))


(defun note-parameters (system)
;; Write to output file the parameters of this particular system
    (when (get system 'parameters)
      (append-file *output-file* "~%~%; ~A parameters:" system)
      (dolist (var (get system 'parameters))
	(append-file *output-file* "~%; ~A = ~A" var (eval var)))))

(defun print-parameters (system)
  "Printout parameters for system and all its parents"
    (when (get system 'parameters)
      (format t "~%~%~A parameters:" system)
      (dolist (var (get system 'parameters))
	(format t "~%~A = ~A" var (eval var))))
    (mapc #'print-parameters (get system 'parent-systems)))

(defun ancestor-systems (system)
  "Return all parents of a system"
  (when (get system 'parent-systems)
    (mapcan #'(lambda (parent) (cons parent (ancestor-systems parent))) (get system 'parent-systems))))


(defun run-one-trial (total-training-examples training-increment test-examples systems
		     initial-training)
  (dolist (system systems)
    (setf (get system 'result) nil)
    (funcall-if-exist (append-symbols 'prepare-trial- system) total-training-examples test-examples))
  (do* ((length-examples (length total-training-examples))
	(previous-training-length 0 training-length)
	(training-length (or initial-training length-examples)
			 (if (listp training-increment)
			     (or (pop training-increment) (1+ length-examples))
			     (+ training-length training-increment)))
	(training-examples (subseq total-training-examples 0 training-length))
	(new-training-examples training-examples)
	(result-data) (trial-results))
       ((> training-length length-examples) (nreverse trial-results))
    (setf training-examples (subseq total-training-examples 0 training-length))
    (setf new-training-examples (nthcdr previous-training-length training-examples))
    (format t "~%~80~ ~%Test results for ~A training example~:P:" training-length)
    (push (cons (if initial-training training-length *training-length*)
		(loop for system in systems do
		      (setf result-data
			    (if (get system 'external)
				(funcall (append-symbols 'train-and-test- system)
					 training-examples new-training-examples test-examples)
				(train-and-test system training-examples new-training-examples test-examples)))
		      (setf (get system 'result-data) result-data)
		      (format t "~%~%Training Accuracy: ~,2F%   Test Accuracy: ~,2F%"
			      (get-field result-data 'train-accuracy) (get-field result-data 'test-accuracy))
		      (format t "~%Training Time: ~,2F sec   Test Time: ~,2F sec"
			      (get-field result-data 'train-time) (get-field result-data 'test-time))
		      (format t "~%Concept Complexity: ~A~%" (get-field result-data 'concept-complexity))
		      (if (and *training-error-dump* (not *noise*)
			       (not (= (get-field result-data 'train-accuracy) 100))
			       (not (get system 'expect-training-error)))
			  (dump-data system training-examples (get system 'result)))
		      collect result-data))
	  trial-results)))


(defun train-and-test (system training-examples new-training-examples test-examples)
  (let ((training-function (append-symbols 'train- system))
	(test-function (append-symbols 'test- system))
	start-time training-result
	(result-data (make-list (length *data-format*))))
    (cond ((null training-examples)  ; With no training examples always assume guess category at random
	   (set-field result-data 'train-time 0) (set-field result-data 'test-time 0)
	   (set-field result-data 'concept-complexity (if (fboundp (append-symbols system '-concept-complexity)) 0))
	   (set-field result-data 'train-accuracy 100)
	   (set-field result-data 'test-accuracy  (* 100 (/ 1 (length *categories*)))))
	  (t (format t "~%~%Training ~A..." system)
	     (setf start-time (get-internal-run-time))
	     (setf training-result
		   (case (get system 'incremental)
			 ((nil) (funcall training-function training-examples))
			 ((full-data) (funcall training-function training-examples (get system 'result)))
			 ((t) (funcall training-function new-training-examples (get system 'result)))))
	     (set-field result-data 'train-time (seconds-since start-time))
	     (setf (get system 'result) training-result)
	     (funcall-if-exist (append-symbols 'train- system '-output) training-result training-examples)
	     (trace-print *print-training-result* "~%Training-result: ~%~A" training-result)
	     (set-field result-data 'concept-complexity (funcall-if-exist (append-symbols system '-concept-complexity)
									  training-result))
	     (cond ((get system 'ALWAYS-TRAIN-CORRECT)(set-field result-data 'train-accuracy 100))
		   (t (format t "Testing Training Data...")
		      (set-field result-data 'train-accuracy
				 (get-percent-correct test-function training-result training-examples))))
	     (format t "Testing Test Data...")
	     (setf start-time (get-internal-run-time))
	     (set-field result-data 'test-accuracy
			(prog1
			    (get-percent-correct test-function training-result test-examples)
			  (set-field result-data 'test-time (seconds-since start-time))))
	     (funcall-if-exist  (append-symbols 'test- system '-output) training-result test-examples)))
    result-data))

  
(defun get-percent-correct (test-function training-result training-examples)
  (if training-examples
      (let ((number-right 0) (num-examples 0))
	(dolist (example training-examples (* 100.0 (/ number-right num-examples)))
	  (incf num-examples)
	  (when (eq (funcall test-function example training-result) (first example))
	    (incf number-right))))
      100))

(defvar *bug-file-num* 0 "To number versions of bug files")

(defun dump-data (system training-examples &optional training-result)
  (format t "~%Unexpected error on training data!  Dumping data into -BUG file~%")
  (incf *bug-file-num*)
  (with-open-file (bug-file (format nil "~A-BUG~D" (namestring *output-file*) *bug-file-num*)
			    :direction :output)
    (format bug-file ";;;Buggy examples for ~A~%~%(setf *buggy-examples* ~%  '~A)" system training-examples)
    (if training-result
	(format bug-file ";;;Buggy training-result for ~A~%~%(setf *training-result* ~%  '~A)"
		system training-result))))


;;;;----------------------------------------------------------------------------------------------------------
;;;; Stuff for saving splits and combining seperate runs from saved splits
;;;;----------------------------------------------------------------------------------------------------------
 
(defun make-saved-tests (output-file number-trials number-training  &optional
        		 (training-increment 10)  number-test data-file  (initial-training 0))

  "Make a file that permanently stores a set of train/test splits for this data file.
   See RUN-STANDARD-TESTS for description of args"
  ;; File stores in order: data-file, training-increment, initial-training, and list
  ;; of splits each of form (<training-examples> <test-examples>)
  ;; Examples are simply stored as a number indicating their position in *raw-examples*
  (load-data data-file)
  (let (splits training-examples test-examples)
    (dotimes (i number-trials)
      (multiple-value-setq (training-examples test-examples)
			   (example-generator number-training number-test))
      (push (list (use-position-number training-examples)
		  (use-position-number test-examples))
	    splits))
    (with-open-file (output output-file :direction :output)
      (format output ";;;; ~D stored data splits with ~D training-examples and ~D test examples" 
	      number-trials number-training (or number-test (length test-examples)))
      (print (namestring *current-data-file*) output)
      (print training-increment output)
      (print initial-training output)
      (format output "~%~%(~:{ ~%(~A~% ~A) ~}~%)" splits))))

(defun make-saved-cv-tests (output-file num-folds &optional data-file)
  (load-data data-file)
  (let ((segments (segment-examples *raw-examples* num-folds)))
    (loop for segment in segments do  ; replace examples in segments with position numbers
	  (loop for tail on segment do (setf (first tail) (position  (first tail) *raw-examples*))))
    (with-open-file (out output-file :direction :output)
      (format out ";;;; Saved ~D-fold cross-validation split" num-folds)
      (print (namestring *current-data-file*) out)
      (print segments out))
    nil))

(defun use-position-number (examples)
  (mapcar #'(lambda (example) (position example *raw-examples*))
	  examples))

(defun append-test-results  (old-file new-file)
  "Append the results in new-file to old-file"
  (combine-test-results old-file new-file "temp")
  (if (equalp *current-result-file* (probe-file old-file))
      (setf *current-result-file* nil))
  (rename-file "temp" old-file))

(defun combine-test-results (input-file1 input-file2 output-file)
  "Combines runs from two separate result files from same saved splits into a new
   result file as if they were run at the same time"
    (with-open-file (in1 input-file1 :direction :input)
      (with-open-file (in2 input-file2 :direction :input)
	(let ((comment1 (read-line in1))(comment2 (read-line in2)))
	  (when (or (string-equal comment1 comment2)
		    (y-or-n-p "~%Files not from same saved tests, combine anyway?"))
		(with-open-file (out output-file :direction :output)
		  (loop (if (not (string-equal comment1 comment2))
			    (return nil))
			(format out "~A~%" comment1)
			(cond ((equal (peek-char nil in1) #\()
			       (setf comment1 "") (return nil))
			      (t (setf comment1 (read-line in1))))
			(cond ((equal (peek-char nil in2) #\()
			       (setf comment2 "") (return nil))
			      (t (setf comment2 (read-line in2)))))
		  (loop (format out "~A~%" comment1)
			(if (equal (peek-char nil in1) #\()
			    (return nil)
			    (setf comment1 (read-line in1))))
		  (loop (format out "~A~%" comment2)
			(if (equal (peek-char nil in2) #\()
			    (return nil)
			    (setf comment2 (read-line in2))))
		  (format out "; Combined results of ~A and ~%;   ~A~%"
			  (pathname (probe-file input-file1))
			  (pathname (probe-file input-file2)))
		  (format out "~%~A~%" (append (read in1) (read in2)))
		  (let (run1 run2)
		    (loop (setf run1 (read in1 nil nil)
				run2 (read in2 nil nil))
			  (cond ((and (null run1) (null run2)) (return nil))
				((or (null run1) (null run2))
				 (break "Unequal number of runs in two files")
				 (return nil))
				(t (format out "~%~A"
					   (mapcar #'(lambda (set1 set2)
						       (if (eql (first set1) (first set2))
							   (append set1 (rest set2))
							   (break "Unequal training set sizes")))
						   run1 run2))))))))))))
			   

;;;;----------------------------------------------------------------------------------------------------------
;;;; Sample Example Generators for Generating Training and Test sets
;;;;----------------------------------------------------------------------------------------------------------


(defun example-generator (number-training &optional number-test)
  "Standard example generator randomly picks training and test
   disjointly"
  (multiple-value-bind (training-examples test-examples)
      (random-subseq *raw-examples* number-training)
    (if number-test
	(setf test-examples (random-subseq test-examples number-test t)))
    (values training-examples test-examples)))

(defun saved-example-generator ()
  (let ((split (pop *saved-splits*)))
    (values (mapcar #'(lambda (position) (elt *raw-examples* position)) (first split))
	    (mapcar #'(lambda (position) (elt *raw-examples* position)) (second split)))))

(defun leave-one-out ()
  (if (null *remaining-examples*)
      (values nil nil)
      (let (training-examples test-examples)
	(setf test-examples (list (pop *remaining-examples*)))
	(setf training-examples (append *previous-examples* *remaining-examples*))
	(push (first test-examples) *previous-examples*)
	(values training-examples test-examples))))

(defun cross-validation-generator ()
  (let (training-examples test-examples)
    (setf test-examples (pop *remaining-examples*))
    (setf training-examples (append *previous-examples*
				    (apply #'append *remaining-examples*)))
    (setf *previous-examples*
	  (append test-examples *previous-examples*))
    (values training-examples test-examples)))
  
(defun segment-examples (examples num-segments)
  (let ((length (length examples)))
    (setf examples (random-subseq examples length))  ; randomize order
    (multiple-value-bind (size remainder)
	(floor length  num-segments)
      (loop for i from 1 to num-segments
	    with tail and batch
	    do (setf tail (nthcdr (if (<= i remainder) size (1- size)) examples))
	       (setf batch examples)
	       (setf examples (rest tail))
	       (setf (rest tail) nil)
	    collect batch))))
  
;;;;----------------------------------------------------------------------------------------------------------
;;;; Accessory and Utility Functions
;;;;----------------------------------------------------------------------------------------------------------

(defun get-field (item field-name &optional (field-list *data-format*))
  "Returns element of item in the position that field-name occupies in the field-list"
  (nth (position field-name field-list) item))

(defun set-field (item field-name value &optional (field-list *data-format*))
  (setf (nth (position field-name field-list) item) value))

;;;;----------------------------------------------------------------------------------------------------------
;;;; X-graph and Gnuplot File Generator
;;;;----------------------------------------------------------------------------------------------------------

(defun make-plot-files (input-file &optional systems)
  (dolist (data-item *data-format*)
    (make-plot-file input-file data-item systems)))

(defun make-plot-file (input-file &optional (data-item 'test-accuracy) systems)
  "Create an X-GRAPH file from a universal data file. Puts output
   in fname.plot. Data-item can be train-accuracy, test-accuracy, train-time, test-time
   train-error, test-error, concept-complexity"
  (let (assoc-list system-list plot-error)
    (with-open-file (stream input-file :direction :input)
      (setf system-list (read stream))
      (let (run)
	(setf run (read stream))
	(setf assoc-list (mapcar #'(lambda (point) (list (first point) (rest point)))
				 run))
	(loop (if (null (setf run (read stream nil nil)))
		  (return nil)
		  (progn (dolist (point run)
			   (let ((point-list (assoc (first point) assoc-list)))
			     (if point-list
				 (push (rest point) (rest point-list))
				 (error "Unknown point: ~A" (first point))))))))))
    (with-open-file (output (concatenate 'string input-file "." (string-downcase (symbol-name data-item)) "-plot")
			    :direction :output :if-exists :supersede)
      (format output "TitleText: ~A ~A~%XUnitText: Train Exs~%YUnitText: ~A~%Markers: true~%~%"
	      (remove #\SPACE (file-namestring (pathname input-file)))
	      (format-symbol data-item)
	      (case data-item
		(train-accuracy "% Correct")
		(test-accuracy "% Correct")
		(train-error   "% Error")
		(test-error    "% Error")
		(train-time    "Seconds")
		(test-time     "Seconds")
		(concept-complexity "Literals")))
      (case data-item
	(train-error (setf data-item 'train-accuracy) (setf plot-error t))
	(test-error  (setf data-item 'test-accuracy)  (setf plot-error t))
	(otherwise nil))
      (dolist (system (or systems system-list))
	(format output "\"~A" system)
	(dolist (point-list assoc-list)
	  (format output "~%~D ~F" (first point-list)
		  (let ((sum 0))
		    (dolist (set (rest point-list))
		      (incf sum
			    (get-field (get-field set system system-list) data-item)))
		    (let ((average (/ sum (length (rest point-list)))))
		      (if plot-error (- 100 average) average)))))
	(format output "~%~%")))))

(defun format-symbol (symbol)
 (string-capitalize (substitute #\Space #\- (symbol-name symbol))))

(defun make-gnuplot-files (input-file &optional systems)
  (dolist (data-item *data-format*)
    (make-gnuplot-file input-file data-item systems)))

(defun make-gnuplot-file (input-file &optional (data-item 'test-accuracy) systems)
  "Create GNUPLOT files from a universal data file. Puts output
   in fname.data-item-gnuplot. Data-item can be train-accuracy, test-accuracy, train-time, test-time,
   train-error, test-error, concept-complexity"
  (let (assoc-list system-list plot-error graphs x-points y-points data-item1)
    (with-open-file (stream input-file :direction :input)
      (setf system-list (read stream))
      (let (run)
	(setf run (read stream))
	(setf assoc-list (mapcar #'(lambda (point) (list (first point) (rest point)))
				 run))
	(loop (if (null (setf run (read stream nil nil)))
		  (return nil)
		  (progn (dolist (point run)
			   (let ((point-list (assoc (first point) assoc-list)))
			     (if point-list
				 (push (rest point) (rest point-list))
				 (error "Unknown point: ~A" (first point))))))))))
    (case data-item
	  (train-error (setf data-item1 'train-accuracy) (setf plot-error t))
	  (test-error  (setf data-item1 'test-accuracy)  (setf plot-error t))
	  (otherwise (setf data-item1 data-item)))
    (setf graphs
	  (loop for system in (or systems system-list)
		collect (cons  (list system
				 (concatenate 'string input-file "-" (string-downcase (symbol-name system))
					      "." (string-downcase (symbol-name data-item)) "-gnuplot"))
			      (loop for point-list in assoc-list collect
				    (list (first point-list)
					  (let ((sum 0))
					    (dolist (set (rest point-list))
						    (incf sum
							  (get-field (get-field set system system-list) data-item1)))
					    (let ((average (/-float sum (length (rest point-list)))))
					      (if plot-error (- 100 average) average))))))))
    (loop for graph in graphs do
	  (loop for point in (rest graph) do (push (first point) x-points) (push (second point) y-points)))
    (with-open-file (output (concatenate 'string input-file "." (string-downcase (symbol-name data-item)) "-gnuplot")
			    :direction :output :if-exists :supersede)
      (format output "set terminal postscript portrait color~%set size 0.75,0.75~%")
      (format output  "~%set xlabel \"Training Examples\"~%set ylabel \"~A\"~%" 
	      (case data-item
		(train-accuracy "% Correct")
		(test-accuracy "% Correct")
		(train-error   "% Error")
		(test-error    "% Error")
		(train-time    "Seconds")
		(test-time     "Seconds")
		(concept-complexity "Literals")))
      (if plot-error
	  (format output "~%set data style linespoints~%")
	  (format output "~%set data style linespoints~%set key ~,2F,~,2F~%"
	      (interpolate 0.75 (apply #'min x-points) (apply #'max x-points))
	      (interpolate 0.25 (apply #'min y-points) (apply #'max y-points))))
      (format output "~%plot ")
      (dolist (graph graphs)
	      (format output "'~A' title \"~A\"" (second (first graph)) (first (first graph)))
	      (unless (eq graph (first (last graphs)))
		      (format output ", "))))
    (dolist (graph graphs)
	    (with-open-file (output (second (first graph)) :direction :output :if-exists :supersede)
			    (dolist (point (rest graph))
				    (format output "~&~A ~A" (first point) (second point)))))))
      

(defun interpolate (fraction min max)
  (+ min (* fraction (- max min))))
