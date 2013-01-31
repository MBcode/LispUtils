;;;; LISP interface to C4.5 that handles multi-concept problems where
;;;; each example is to be classified into the correct category for several
;;;; different concepts, such a sentence having a value for agent, patient,
;;;; instrument, etc.  Coordinates building a separate tree for each concept
;;;; and computes both average accuracy across all concepts and percentage
;;;; of examples for which all concepts are correct.

;;;; Copyright (c) 1993 by Raymond Joseph Mooney. This program may be freely 
;;;; copied, used, or modified provided that this copyright notice is included
;;;; in each copy of this code and parts thereof.

;;;; Makes separate files of data and results for each concept.  Requires
;;;; my modified version of C4.5 C code that uses the -i flag to show
;;;; list of 0's and 1's indicating incorrect/correct on each test example
;;;; so that complete accuracy of all concepts of an example can be determined.

;;;; See c4.5.lisp for additional comments.

(in-package :user)
(provide 'c4.5-multi)

(setf (get 'c4.5-multi 'external) t)   ; declare C4.5 to be defined outside LISP
(setf (get 'c4.5-multi 'parameters) '(*c4.5-prune-tree* *c4.5-min-examples* *c4.5-prune-confidence*
				      *c4.5-group-values*))
(setf (get 'c4.5-multi 'expect-training-error) t)   

(defparameter *c4.5-prune-tree* t        "Use pruned decision tree")
(defparameter *c4.5-prune-confidence* 25 "Percent confidence factor for pruning")
(defparameter *c4.5-min-examples* 2      "Minimum number of examples at some branch for a split")
(defparameter *c4.5-group-values* nil    "Tries to create groups of values on same branch")

(defun prepare-c4.5-multi ()
  (loop for concept in *concepts* as categories in *categories* do
	(with-open-file (output (multi-fname concept "names") :direction :output)
	  (format output "~A" (first categories))
	  (dolist (category (rest categories))
	    (format output ", ~A" category))
	  (format output ".~%~%")
	  (dolist (feature *feature-names*)
	    (format output "~A:~20T" feature)
	    (if (linear-feature-p feature)
		(format output "continuous.~%")
		(progn
		  (let ((domain (feature-domain feature)))
		    (format output "~A" (first domain))
		    (dolist (value (rest domain))
		      (format output ", ~A" value))
		    (format output ".~%"))))))))

(defun multi-fname (concept &optional extension (file-stem *output-file*))
  (concatenate 'string (namestring file-stem) "-" (symbol-name concept)
	       (if extension "." "") (or extension "")))
    
(defun prepare-trial-c4.5-multi (total-training-examples test-examples)
  (declare (ignore total-training-examples))
  (loop for concept in *concepts* do
	(write-c4.5-multi-datafile nil concept)              ; empty out training data file
	(write-c4.5-multi-datafile (make-ordered-examples test-examples) concept t))) ; write test data file
  

(defun train-and-test-c4.5-multi (training-examples new-training-examples test-examples)
  (let ((num-train (length training-examples))
	(num-test (length test-examples))
	(result-data (make-list (length *data-format*))))
    (format t "~%~%Training C4.5...")
    (train-c4.5-multi new-training-examples)
    (set-field result-data 'train-time (extract-c4.5-multi-time))
    (format t "Testing Training Data...")
    (multiple-value-bind (tree-size train-errors train-complete-errors)
	(test-c4.5-multi t)
      (set-field result-data 'train-accuracy (percent-correct train-errors (* num-train (length *concepts*))))
      (set-field result-data 'train-complete-accuracy (percent-correct train-complete-errors num-train))
      (set-field result-data 'concept-complexity tree-size))
    (format t "Testing Test Data...")
    (multiple-value-bind (tree-size test-errors test-complete-errors)
	(test-c4.5-multi)
      (declare (ignore tree-size))
      (set-field result-data 'test-accuracy (percent-correct test-errors (* num-test (length *concepts*))))
      (set-field result-data 'test-complete-accuracy (percent-correct test-complete-errors num-test))
      (set-field result-data 'test-time (extract-c4.5-multi-time)))
    result-data))

(defun percent-correct (errors total)
  (if (zerop total)
      100
      (* 100.0 (/ (- total errors) total))))

(defun train-c4.5-multi (new-examples)
  (loop for concept in *concepts* do
	(write-c4.5-multi-datafile (make-ordered-examples new-examples) concept nil t)
	(unix-run-c4.5 (concatenate 'string "c4.5 -x"
				    (unless (= *c4.5-min-examples* 2)
				      (format nil " -m ~A" *c4.5-min-examples*))
				    (unless (= *c4.5-prune-confidence* 25)
				      (format nil " -c ~A" *c4.5-prune-confidence*))
				    (if *c4.5-group-values* " -s"))
		       (multi-fname concept))))


(defun test-c4.5-multi (&optional eval-training)
  (let ((total-errors 0)(bit-list nil)(total-tree-size 0))
    (loop for concept in *concepts* do
	  (unix-run-c4.5 (concatenate 'string "c4.5test -i"
				      (if eval-training " -t")
				      (unless *c4.5-prune-tree* " -u"))
			 (multi-fname concept))
	  (multiple-value-bind (bits tree-size errors)
	      (extract-c4.5-multi-results concept)
	    (incf total-errors errors) (incf total-tree-size tree-size)
	    (setf bit-list (log-and-bits bit-list bits))))
    (values total-tree-size total-errors (count 0 bit-list))))

(defun log-and-bits (list1 list2)
  (if (null list1)
      list2
      (loop for rest1 on list1 as bit in list2 do
	    (setf (first rest1)
		  (if (and (= (first rest1) 1) (= bit 1)) 1 0))
	    finally (return list1))))

(defun concept-category (example concept)
  (nth (position concept *concepts*) (first example)))
  
(defun write-c4.5-multi-datafile (examples concept &optional as-test append?)
    (with-open-file (output (multi-fname concept (if as-test "test" "data"))
			    :direction :output
			    :if-exists (if append? :append :supersede)
			    :if-does-not-exist :create)
      (let ((feature-num (length *feature-names*)))
	(dolist (example examples)
	  (dotimes (i feature-num)
	    (format output "~A, " (elt (second example) i)))
	  (format output "~A.~%" (concept-category example concept))))))


(defun unix-run-c4.5 (command &optional (file-stem (namestring *output-file*)))
  (shell (concatenate 'string "echo `time "
		      command " -f " file-stem " > " file-stem ".out` > "
                      file-stem ".time")))

(defun extract-c4.5-multi-time ()
  "Take the first number up to the letter u from the time file to indicate user time expended"
  (loop for concept in *concepts* sum
	(let ((last-line (last-file-line (multi-fname concept "time"))))
	  (read-from-string (subseq last-line 0 (position #\u last-line))))))

(defun extract-c4.5-multi-results (concept)
  "Return the first two numbers on the last line of the output file for c4.5test as the tree-size
   and error"
  (let (line next-line bits)
    (with-open-file (in (multi-fname concept "out") :direction :input)
      (loop while (setf next-line (read-line in nil nil))
	    do 
	    (setf line next-line)
	    (if (member line '("0" "1") :test #'string-equal)
		(push (read-from-string line) bits))))
    (multiple-value-bind (tree-size index)
	(read-from-string line)
      (values bits tree-size (read-from-string (subseq line index))))))

(defun last-file-line (file)
  (with-open-file (in file :direction :input)
    (let (line next-line)
      (loop while (setf next-line (read-line in nil nil))
	    do (setf line next-line)
	    finally (return line)))))

;;;;--------------------------------------------------------------------------
;;; Allow running unpruned version by stealing training from pruned version

(setf (get 'c4.5-multi-unpruned 'external) t)   ; declare C4.5 to be defined outside LISP
(setf (get 'c4.5-unpruned-multi 'parameters) '(*c4.5-min-examples* *c4.5-prune-confidence*
					       *c4.5-group-values*))
(setf (get 'c4.5-multi-unpruned 'expect-training-error) t)   

(defun c4.5-multi-run-before (system)
  (member system (member 'c4.5-multi *systems*)))

(defun prepare-c4.5-multi-unpruned ()
  (unless (c4.5-multi-run-before 'c4.5-multi-unpruned)
    (prepare-c4.5-multi)))

(defun prepare-trial-c4.5-multi-unpruned (total-training-examples test-examples)
    (unless (c4.5-multi-run-before 'c4.5-multi-unpruned)
    (prepare-trial-c4.5-multi total-training-examples test-examples)))


(defun train-and-test-c4.5-multi-unpruned (training-examples new-training-examples test-examples)
  (let ((*c4.5-prune-tree* nil))
    (if (c4.5-multi-run-before 'c4.5-multi-unpruned)
	(let ((result-data (copy-list (get 'c4.5-multi 'result-data)))
	      (num-train (length training-examples))
	      (num-test (length test-examples)))
	  (format t "~%~%C4.5-UNPRUNED taking training from C4.5...Testing Training Data...")
	  (multiple-value-bind (tree-size train-errors train-complete-errors)
	      (test-c4.5-multi t)
	    (set-field result-data 'train-accuracy (percent-correct train-errors (* num-train (length *concepts*))))
	    (set-field result-data 'train-complete-accuracy (percent-correct train-complete-errors num-train))
	    (set-field result-data 'concept-complexity tree-size))
	  (format t "Testing Test Data...")
	  (multiple-value-bind (tree-size test-errors test-complete-errors)
	      (test-c4.5-multi)
	    (declare (ignore tree-size))
	    (set-field result-data 'test-accuracy (percent-correct test-errors (* num-test (length *concepts*))))
	    (set-field result-data 'test-complete-accuracy (percent-correct test-complete-errors num-test))
	    (set-field result-data 'test-time (extract-c4.5-multi-time)))
	  result-data)
	; else
	(train-and-test-c4.5-multi training-examples new-training-examples test-examples))))
    

;;;;--------------------------------------------------------------------------
;;; Rule version of C4.5-Multi. Allows stealing tree training from pruned version

(setf (get 'c4.5-multi-rules 'external) t)   ; declare C4.5 to be defined outside LISP
(setf (get 'c4.5-multi-rules 'parameters) '(*c4.5-rules-prune-confidence* *c4.5-rules-fisher-test*
				      *c4.5-rules-redundancy*))
(setf (get 'c4.5-multi-rules 'expect-training-error) t)

(defparameter *c4.5-rules-prune-confidence* 25 "Confidence level for pruning rules")
(defparameter *c4.5-rules-fisher-test* nil "Use Fisher exact test to prune rules")
(defparameter *c4.5-rules-redundancy* 1  "Data redundancy for minimal encoding")


(defun prepare-c4.5-multi-rules ()
  (unless (c4.5-multi-run-before 'c4.5-multi-rules)
    (prepare-c4.5-multi)))


(defun prepare-trial-c4.5-multi-rules (total-training-examples test-examples)
    (unless (c4.5-multi-run-before 'c4.5-multi-rules)
    (prepare-trial-c4.5-multi total-training-examples test-examples)))


(defun train-and-test-c4.5-multi-rules (training-examples new-training-examples test-examples)
  (declare (ignore training-examples test-examples))
  (let ((train-time
	  (if (c4.5-multi-run-before 'c4.5-multi-rules)
	      (progn (format t "~%~%C4.5-RULES taking training from C4.5...")
		(get-field (get 'c4.5-multi 'result-data) 'train-time))
	      (progn (format t "~%~%Training C4.5...")
		     (train-c4.5-multi  new-training-examples)
		     (extract-c4.5-multi-time))))
	(num-train (length training-examples))
	(num-test (length test-examples))
	(result-data (make-list (length *data-format*))))
    (format t "Extracting Rules...")
    (extract-c4.5-multi-rules)
    (set-field result-data 'train-time (+ train-time (extract-c4.5-multi-time)))
    (format t "Testing Training Data...")
    (multiple-value-bind (num-lits train-errors train-complete-errors)
	(test-c4.5-multi-rules t)
      (set-field result-data 'train-accuracy (percent-correct train-errors (* num-train (length *concepts*))))
      (set-field result-data 'train-complete-accuracy (percent-correct train-complete-errors num-train))
      (set-field result-data 'concept-complexity num-lits))
    (format t "Testing Test Data...")
    (multiple-value-bind (num-lits test-errors test-complete-errors)
	(test-c4.5-multi-rules)
      (declare (ignore num-lits))
      (set-field result-data 'test-accuracy (percent-correct test-errors (* num-test (length *concepts*))))
      (set-field result-data 'test-complete-accuracy (percent-correct test-complete-errors num-test))
      (set-field result-data 'test-time (extract-c4.5-multi-time)))
    result-data
    ))

(defun extract-c4.5-multi-rules (&optional print-rules)
  (loop for concept in *concepts* do
	  (unix-run-c4.5 (concatenate 'string "c4.5rules" (unless print-rules " -x")
				      (unless (= *c4.5-rules-prune-confidence* 25)
					(format nil " -c ~A" *c4.5-rules-prune-confidence*))
				      (if *c4.5-rules-fisher-test*
					  (format nil " -F"))
				      (unless (= *c4.5-rules-redundancy* 1)
					(format nil " -r ~A" *c4.5-rules-redundancy*)))
			 (multi-fname concept))))

(defun test-c4.5-multi-rules (&optional eval-training)
  (let ((total-errors 0)(bit-list nil)(total-num-lits 0))
    (loop for concept in *concepts* do
	  (unix-run-c4.5 (concatenate 'string "c4.5rtest -i"
				      (if eval-training " -t"))
			 (multi-fname concept))
	  (multiple-value-bind (bits num-lits errors)
	      (extract-c4.5-multi-rules-results concept)
	    (incf total-errors errors) (incf total-num-lits num-lits)
	    (setf bit-list (log-and-bits bit-list bits))))
    (values total-num-lits total-errors (count 0 bit-list))))

(defun extract-c4.5-multi-rules-results (concept)
  (let (line next-line bits prev-line)
    (with-open-file (in (multi-fname concept "out") :direction :input)
      (loop while (setf next-line (read-line in nil nil))
	    do
	    (setf prev-line line)
	    (setf line next-line)
	    (if (member line '("0" "1") :test #'string-equal)
		(push (read-from-string line) bits))))
    (let ((list (read-from-string (concatenate 'string "(" (delete #\, line) ")"))))
      (values bits (read-from-string prev-line) (fourth list)))))
