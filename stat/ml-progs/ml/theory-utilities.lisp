;;;; Standard utilities for dealing with data.  Contains a lot of miscellaneous stuff that
;;;; needs to be cleaned up.

;;;; Copyright (c) 1991 by Raymond Joseph Mooney. This program may be freely copied, used, or
;;;; modified provided that this copyright notice is included in each copy of this code
;;;; and parts thereof.

(in-package :user)
(provide 'theory-utilities)

(eval-when (compile load eval)
 (require 'data-utilities (ml-progs-file "data-utilities"))
 ;(require 'deduce         (ml-progs-file "deduce"))
 (require 'deduce         (ml-progs-file "deduce.lisp"))
 )

(defvar *theory* nil          "Initial domain theory")

;;;;-------------------------------------------------------------------------------------------------------------
;;;; Functions for checking theory
;;;;-------------------------------------------------------------------------------------------------------------

(defvar  *augmented-feature-names* nil)

(defun check-theory-antecedents (&optional (theory *theory*))
  (setf *augmented-feature-names*
	(append *feature-names* '(< <= = >= >)))
  (let* (bad-rules-and-antecedents bad-consequents good-consequents
	 (answer
	   (dolist (rule theory (list (reverse bad-rules-and-antecedents) (reverse bad-consequents)))
	     (dolist (antecedent (brule-antecedents rule))
	       (when (eq (first antecedent) 'not) (setf antecedent (second antecedent)))
	       (let ((predicate (first antecedent)))
		 (if (member predicate *augmented-feature-names*)
		     (when (and (= (length antecedent) 2)
				(not (eq (feature-domain predicate) 'linear)))
		       (let ((value (second antecedent)))
			 (unless (or (pcvar-p value)
				     (member value (feature-domain predicate)))
			   (format t "~%The value for predicate ~A 
is not within the domain of its attribute in rule ~%~1T~A" predicate rule)
			   (setf bad-rules-and-antecedents (cons (list rule antecedent)
								 bad-rules-and-antecedents)))))
		     (unless (find antecedent (remove rule theory) :key
				   'consequent
				   :test 'equal)
		       (format t "~%Antecedent ~A 
is not implied by any rule and is not a feature in rule ~%~A."
			       antecedent rule)
		       (setf bad-rules-and-antecedents (cons (list rule antecedent)
							     bad-rules-and-antecedents))))))
	     (let ((consequent (brule-consequent rule)))
	     (or (member consequent good-consequents :test 'equal-or-av)
		 (member consequent bad-consequents :test 'equal-or-av)
		 (if (find-consequent-in-theory consequent theory)
		     (setf good-consequents (cons consequent good-consequents))
		     (setf bad-consequents (cons consequent bad-consequents))))))))
    (if  (and (null (first answer))
		 (null (second answer)))
      (format t "~%Theory is O.K.")
      answer)))

(defun find-consequent-in-theory (consequent theory)
  (if (or (member (first consequent) *categories*)
	  (dolist (rule theory)
	    (when (member consequent (brule-antecedents rule) :test 'equal-or-av)
	      (return t))))
      consequent
      (let ((rules (find-rules consequent theory)))
      (format t "~%~%The rule~P ~{~%~1T~A ~} ~%~[is ~;are ~] not used by the theory." 
	      (length rules)
	      rules
	      (if (> (length rules) 1)
		  1 0)))))

(defun find-rules (consequent theory)
  (remove-if-not #'(lambda (rule) (equal consequent (brule-consequent rule)))
		 theory))
  
(defun equal-or-av (list1 list2)
  (or (equal list1 list2)
      (and (consp list1)
	   (consp list2)
	   (> (length list1) 1)
	   (> (length list2) 1)
	   (set-equal list1 list2 :test
		      #'alphabetic-variant))))


(defun alphabetic-variant (literal1 literal2)
  (or (equal literal1 literal2)
      (and (consp literal1)
	   (consp literal2)
	   (eq (first literal1) (first literal2))
	   (pcvar-p (second literal1))
	   (pcvar-p (second literal2))
	   (if (and (third literal1)(third literal2))
	       (= (third literal1) (third literal2))
	       (and (null (third literal1)) (null (third literal2)))))))   ;using
					   ;fact that (third '(a b))
					   ;=> nil.


;;;;-------------------------------------------------------------------------------------------------------------
;;;; Functions for describing theory
;;;;-------------------------------------------------------------------------------------------------------------


(defun describe-theory (&optional (theory *theory*))
  (let* ((groups (group-rules theory))
	 (num-rules (length theory))
	 (num-conses (length groups))
	 (num-antes  (count-antecedents theory)))
    (format t "~%~%Number of rules: ~A" num-rules)
    (format t "~%Number of consequents: ~A" num-conses)
    (format t "~%Number of symbols: ~D" (+ num-antes num-rules))
    (format t "~%Average number of disjuncts: ~,2F" (/ num-rules num-conses))
    (format t "~%Average number of antecedents: ~,2F" (/ num-antes num-rules))))
    
(defun count-antecedents (rules)
  (let ((sum 0))
    (dolist (rule rules sum)
      (incf sum (length (brule-antecedents rule))))))

(defun group-rules (rules)
  (let (alist)
    (dolist (rule rules alist)
      (let ((set (assoc (brule-consequent rule) alist :test #'equal)))
	(if set
	    (nconc set (list rule))
	    (push (list (brule-consequent rule) rule) alist))))))

(defun print-theory (&optional (theory *theory*))
  (dolist (rule theory)
    (format t "~%~(~A~) <-~{ ~(~A~)~}" (brule-consequent rule) (brule-antecedents rule)))
  (format t "~%~%Observable features:~{ ~(~A~)~}" *feature-names*)
  (format t "~%~%Categories: ~{ ~(~A~)~}" *categories*))


;;;;-------------------------------------------------------------------------------------------------------------
;;;; Functions for Testing  Theories
;;;;-------------------------------------------------------------------------------------------------------------


(defvar *number-nils* 0)
(defvar *number-overgeneral* 0)
(defvar *number-exactly-right* 0)

(defun test-theory (&optional (theory *theory*) (examples *raw-examples*) (print-results t))
  "Test the accuracy of a theory on a set of examples"
  (setf *number-nils* 0) (setf *number-overgeneral* 0) (setf *number-exactly-right* 0)
  (let* ((num-exs (length examples))
	 (accuracy (if examples
		      (let ((number-right 0))
			(dolist (example examples (* 100.0 (/ number-right num-exs)) )
			  (setf number-right (+ number-right (test-theory-example example theory print-results)))))
		      100)))
  (format t "~%~%Theory classified ~,2F% of the ~D test cases correctly." accuracy num-exs)
  (format t "~%Number not classified in any category: ~D (~,2F%)" *number-nils* (* 100 (/ *number-nils* num-exs)))
  (format t "~%Number classified exactly right: ~D (~,2F%)" *number-exactly-right*
	  (* 100 (/ *number-exactly-right* num-exs)))
  (format t "~%Number classified overly-general: ~D (~,2F%)" *number-overgeneral* (* 100 (/ *number-overgeneral* num-exs)))
  (format t "~%Number classified overly-specific: ~D (~,2F%)" (- num-exs *number-overgeneral* *number-exactly-right*)
	   (* 100 (/ (- num-exs *number-overgeneral* *number-exactly-right*) num-exs)))
  ))

(defun test-theory-example (example &optional (theory *theory*) (print-results t))
  "Return probability that example is classified correctly by theory"
  (setf example (make-alist-example example))
  (let ((provable-categories (provable-example-categories example theory *categories* print-results)))
    (cond ((null provable-categories)
	   (incf *number-nils*)
	   (if (member *negative-category* *categories*)
	       (if (eq (first example) *negative-category*)
		   1 0)
	       (/ 1 (length *categories*))))
	  ((member (first example) provable-categories)
	   (if (rest provable-categories)
	       (incf *number-overgeneral*)
	       (incf *number-exactly-right*))
	   (/ 1 (length provable-categories)))
	  (t 0))))

(defun provable-example-categories (example &optional (theory *theory*) (categories *categories*) (print-results t))
  "Reutrn the list of categories example is provable in"
  (if (or (equal categories '(+ -)) (equal categories '(- +)))
      (if (prove theory example) '(+) '(-))
      (let (proved-categories)
	(dolist (category categories)
	  (if (prove theory example (list category))
	      (push category proved-categories)))
	(trace-print print-results
		     "~%~AReal category: ~A; Proved categories: ~A"
		     (if (or (and (eq (first example) (first proved-categories))
				  (null (rest proved-categories)))
			     (and (eq (first example) *negative-category*) (null proved-categories)))
			 "  " "**")
		     (first example) proved-categories)
	proved-categories)))

(defvar *current-theory* nil)
(defvar *current-facts* nil)

(defun prove (theory example goal)
  (unless (eq theory *current-theory*)
    (setf *current-theory* theory)
    (clear-brules)
    (index-brules theory))
  (unless (eq (cdr example) *current-facts*)
    (setf *current-facts* (cdr example))
    (clear-facts)
    (index-facts (cdr example)))
  (let ((answer (retrieve goal)))
    (gfirst answer)))

(defun make-deduce-rules (theory)
  (mapcar #'(lambda (rule)
	      (cons '<- (cons (first rule) (rest (rest rule)))))
	  theory))


