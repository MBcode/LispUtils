;;; This is a version of the AQ algorithm for learning from examples which uses
;;; beam search to generate bounded stars. 

;;;; Copyright (c) 1993 by Raymond Joseph Mooney. This program may be freely 
;;;; copied, used, or modified provided that this copyright notice is included
;;;; in each copy of this code and parts thereof.

;;;; This version of AQ assumes that events are represented as ordered lists of
;;;; nominal feature values and that complexes are represented similarly with
;;;; ?'s for values which are not constrained. Consequently, its language does
;;;; not include negation, internal disjunction, linear or structural features.
;;;; It has a general LEF function but does not support tolerances.

(in-package :user)
(provide 'aq)
(require 'data-utilities (ml-progs-file "data-utilities"))
 
(setf (get 'aq 'parameters) '(*max-star* *lef*))

(defparameter *trace-aq* nil  "AQ produces a trace when set to T")
(defparameter *print-with-feature-names* nil  "Print out complexes with feature names")

;; The LEF (Lexicographic Evaluation Functional) determines which complexes
;; are more preferable. A LEF is a list of criteria functions. A criteria
;; function takes two arguements, a complex and the current list of uncovered
;; + events and returns a value, where a lesser value indicates more preferable.
;; The current LEF function first maximizes coverage of + events in order to
;; minimize disjuncts and with in that minimizes the number of selectors.
(defparameter *lef* '(count-coverage count-selectors)) 

(defparameter *max-star* 1 "The beam width which controls the maximum size stars may achieve.")

(defun train-aq (examples)
  (setf examples (make-ordered-examples examples nil))
  (if (pos-neg?)
      (aq examples)
      (multi-aq examples)))

(defun test-aq (example aq-result)
  (setf example (make-ordered-example example nil))
  (if (pos-neg?)
      (if (cover-match aq-result (second example)) *positive-category* *negative-category*)
      (multi-aq-test example aq-result)))

(defun aq-concept-complexity (aq-result)
    (if (pos-neg?)
	(cover-complexity aq-result)
	(let ((sum 0))
	  (dolist (alist-elt (rest aq-result) sum)
	    (incf sum (cover-complexity (third alist-elt)))))))
  
(defun aq (examples)
  ;;; AQ takes a list of examples where an example is a list of either + or -
  ;;; (indicating the class) and an event description. It returns a cover
  ;;; (a list of complexes) which covers all of the postive events and none
  ;;; of the negative ones.

  ;; First separate into + and - events and call AQ1
  (let ((pos-instances nil)(neg-instances nil)
	(pos-category *positive-category*) (neg-category *negative-category*))
    (dolist (example examples)
      (cond ((eq (first example) pos-category)
	     (push (second example) pos-instances))
	    ((eq (first example) neg-category)
	     (push (second example) neg-instances))
	    (t (error "Not + or -"))))
    (aq1 pos-instances neg-instances)))


(defun aq1 (pos-instances neg-instances)
  ;;; AQ1 takes a lists of positive and negative events and returns a cover
  ;;; (a list of complexes) which covers all of the postive events and none
  ;;; of the negative ones.

  (let ((star nil)(cover nil)(seed nil)(best-complex nil)(delta 0))
    ;; Use seeds which are not covered by any previous star until there
    ;; are no more such seeds.
    (do ((seed-instances pos-instances))
	((null seed-instances))
      (setf seed (pop seed-instances))     ; pick a seed from the available set
      (setf pos-instances (remove seed pos-instances))
      (trace-print *trace-aq* "~%~%Seed: ~A" seed)
      ;; Generate a bounded star covering this seed but not any of the - events
      (setf star (generate-star seed neg-instances pos-instances))
      ;; Since generate-star returns a list of complexes sorted from best to
      ;; worst according to the LEF, the best is the first element of the star
      (setf best-complex (first star))
      (trace-print *trace-aq* "~%Best complex: ~A" best-complex)
      (push best-complex cover)            ; add the best complex to the cover
      ;; Remove from the set of possible seeds those which are covered by any
      ;; complex in the star
      (setf seed-instances (remove-if #'(lambda (instance)
					 (dolist (complex star)
					   (when (match complex instance)
					     (return t))))
				     seed-instances))
      ;; Remove from the set of positive events those covered by the chosen
      ;; complex
      (setf pos-instances (remove-if #'(lambda (instance)
					 (match best-complex instance))
				     pos-instances)))
    ;; Pick seeds and generate bounded stars until all + events are covered
    (do nil
	((null pos-instances))
      (setf seed (pop pos-instances))
      (trace-print *trace-aq* "~%~%Seed: ~A" seed)
      (setf star (generate-star seed neg-instances pos-instances))
      (setf best-complex (first star))
      (trace-print *trace-aq* "~%Best complex: ~A" best-complex)
      (push best-complex cover)
      (setf pos-instances (remove-if #'(lambda (instance)
					 (match best-complex instance))
				     pos-instances))
      (incf delta))                 ; delta is an estimate of distance from
                                    ; the minimal # of complexes
    (trace-print *trace-aq* "~%~%Cover minimal within ~D complexes" delta)
    cover))


(defun generate-star (seed neg-instances pos-instances)
  ;;; Generate a star which covers seed but not any of the neg-instances
  ;;; If star ever gets larger than *max-star* complexes then trim it the
  ;;; best *max-star* complexes as judged by the LEF. The star is
  ;;; represented as a list of "evaled-complexes" which are cons-cells with a
  ;;; complex in their CAR and a list of LEF values in their CDR.  This prevents
  ;;; unnecessary recalculating LEF values. 

  ;; Initialize the star to an evaled-complex for the most general complex
  (let ((star (compute-lef-values (initialize-star) pos-instances))
	new-star)
    ;; For each negative event specialize each complex in the star which matches
    ;; the negative event just enough so it doesn't cover it.
    (dolist (neg-instance neg-instances)
      ;; if |star|>*max-star* then trim it to the best complexes by sorting it
      ;; according to the LEF and taking the best *max-star* complexes
      (when (> (length star) *max-star*)
	(trace-print *trace-aq* "~%Trimming star to best ~D" *max-star*)
	(setf star (subseq (sort star #'lef-less-than :key #'rest)
			     0 *max-star*)))
      (trace-print *trace-aq* "~%Current star: ~A ~%~%Processing neg event ~A"
		   star neg-instance)
      (setf new-star (update-star star seed neg-instance pos-instances))
      ;; check if star has went empty due to limited search or noise (should never happen otherwise)
      (if (null new-star)
	  (when *trace-aq*
	    (if (equalp seed neg-instance)
		(format t "~%~%Star empty due to noise.~% Not consistent with: ~A" neg-instance)
		(format t "~%~%Star empty due to limited search.~% Not consistent with: ~A" neg-instance)))
	  (setf star new-star)))
    ;; Before returning the star, sort it, trim it if necessary, and return only
    ;; complexes, not evaled complexes
    (setf star (sort star #'lef-less-than :key #'rest))
    (when (> (length star) *max-star*)
      (trace-print *trace-aq* "~%Trimming star to best ~D" *max-star*)
      (setf star (subseq star 0 *max-star*)))
    (trace-print *trace-aq* "~%Final star: ~A" star)
    (mapcar #'(lambda (evaled-complex) (first evaled-complex))	star)))


(defun update-star (star seed neg-instance pos-instances)
  ;;; Specializes complexes in the star so that none match the given instance 
  ;;; for a negative example but each still covers the seed.  Calculates new
  ;;; LEF values for any new complexes to get evaled-complexes

  (setf star (mapcan #'(lambda (evaled-complex) 
			(if (match (first evaled-complex) neg-instance)
			    (compute-lef-values
			      (specializations-against (first evaled-complex)
						       neg-instance seed)
			      pos-instances)
			    (list evaled-complex)))
		    star))
  ;; Remove from the star those complexes which are more specific than some other
  (dolist (evaled-complex1 star)  
    (dolist (evaled-complex2 (rest (member evaled-complex1 star)))
      (cond ((more-general? (first evaled-complex1) (first evaled-complex2))
	     (setf star (remove evaled-complex2 star)))
	    ((or (more-general? (first evaled-complex2) (first evaled-complex1))
		 (equal (first evaled-complex1) (first evaled-complex2)))
	     (setf star (remove evaled-complex1 star))))))
  star)


(defun specializations-against (complex neg-instance seed)
  ;;; Specialize the given complex just enought so it doesn't cover the
  ;;; negative event but still covers the seed. For each feature in
  ;;; complex which is "?" change it to the value in seed to obtain a
  ;;; specialization unless the value in seed and neg-instance are the same.

  (do ((complex-rest complex (rest complex-rest))
       (neg-rest neg-instance (rest neg-rest))
       (complex-bef  nil (append complex-bef (list (first complex-rest))))
       (seed-rest seed (rest seed-rest))
       (specializations nil))
      ((null complex-rest) specializations)
    (if (and (eq (first complex-rest) '?)
	     (not (eq (first neg-rest) (first seed-rest))))
	(push (append complex-bef (list (first seed-rest)) (rest complex-rest))
	      specializations))))


(defun match (generalization instance)
  ;;; Match function for a simple feature vector representation where "?" is a 
  ;;; wildcard
  
  (do ((gen generalization (rest gen))
       (inst instance (rest inst)))
      ((null gen) t)
    (unless (or (eq (first gen) '?)
		(eq (first gen) (first inst)))
      (return nil))))

(defun more-general?  (x y)
  ;;; Returns T iff generalization x is strictly more general than 
  ;;; generalization y for a simple feature vector representation. 
  ;;; For x to be more general than y,  they must match and x must 
  ;;; have a "?" where y has a specific value; however y must never 
  ;;; have a "?" where x has a specific value

  (cond ((or (null x)(null y)) nil)
	((and (eq (first x) '?) (not (eq (first y) '?))
	      (or (equal (rest x)(rest y)) 
		  (more-general? (rest x) (rest y)))) 
	 t)
	((equal (first x) (first y)) 
	 (more-general? (rest x) (rest y)))))


(defun initialize-star ()
  ;;; Initialize G to a set containing the all "?" feature vector

  (list (mapcar #'(lambda (feature) (declare (ignore feature)) '?)  *domains*)))


(defun compute-lef-values (star pos-instances)
  ;;; Calculate LEF values for each complex and return a list of evaled 
  ;;; complexes: (complex lef-value1 lef-value2 ...)

  (mapcar #'(lambda (complex)
	      (cons complex
		    (mapcar #'(lambda (lef-fn)
				(funcall lef-fn complex pos-instances))
			    *lef*)))
	  star))


(defun lef-less-than (value-list1 value-list2)
  ;;; Returns T iff the first list of LEF values is less than the second
  ;;; Interprets values lexicographically

  (or (< (first value-list1) (first value-list2))
      (and (rest value-list1)
	   (equal (first value-list1)(first value-list2))
	   (lef-less-than (rest value-list1)(rest value-list2)))))


(defun count-coverage (complex pos-instances)
  ;;; Counts the number of positive events covered by the complex
  ;;; and returns the negation of this value so less is better

  (let ((match-count 0))
    (dolist (pos-instance pos-instances (- match-count))
      (if (match complex pos-instance)
	  (incf match-count)))))


(defun count-selectors (complex pos-instances)
  ;;; Counts the number of selectors in a complex, assuming less is better

  (declare (ignore pos-instances))
  (let ((selector-count 0))
    (dolist (feature complex selector-count)
      (unless (eq feature '?) (incf selector-count)))))


(defun cover-match (cover instance)
  ;;; If cover covers instance then return t
  (dolist (complex cover)
    (when (match complex instance)(return t))))

(defun cover-complexity (cover)
  (let ((sum 0))
    (dolist (complex cover sum)
      (incf sum (count-selectors complex nil)))))

(defun print-cover (cover)
  ;;; Print cover in a nice format
  (dolist (complex cover)
    (format t "~%~A" (format-complex complex))
    (unless (eq complex (first (last cover)))
      (format t " or"))))


(defun format-complex (complex)
  ;;; Format a complex into a prettier form for output
  ;;; ( (feature-name value) ...) for features with constrained values
  (if *print-with-feature-names*
      (do ((complex-rest complex (rest complex-rest))
	   (feature-rest *feature-names* (rest feature-rest))
	   (formated-complex nil))
	  ((null complex-rest) (nreverse formated-complex))
	(unless (eq (first complex-rest) '?)
	  (push (list (first feature-rest) (first complex-rest)) formated-complex)))
      complex))


;;;; ==========================================================================================
;;;; Multiple category stuff
;;;; ==========================================================================================


(defun multi-aq (examples)
  (dolist (cat *categories*)
    (setf (get cat 'training-examples) nil))
  (dolist (example examples)
    (push (second example) (get (first example) 'training-examples)))
  (let ((cover-alist
	  (mapcar #'(lambda (cat)
		      (list cat (length (get cat 'training-examples))
			    (aq1 (get cat 'training-examples)
				 (mapcan #'(lambda (other-cat) (copy-list (get other-cat 'training-examples)))
					 (remove cat *categories*)))))
		  (remove *negative-category* *categories*))))
    (cons (if (member *negative-category* *categories*)
	      *negative-category*
	      (maximum-category-label cover-alist *categories*))
	  cover-alist)))


(defun multi-aq-test (example cover-alist)
  (let ((class-counts (mapcan #'(lambda (alist-elt)
				  (if (cover-match (third alist-elt) (second example))
				      (list (cons (first alist-elt) (second alist-elt)))))
			      (rest cover-alist))))
    (if class-counts
	(maximum-label class-counts *categories*)
	(first cover-alist))))


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

(defun print-multi-aq-result (cover-alist)
  (let ((*print-with-feature-names* t))
    (format t "~%Most common class: ~A" (first cover-alist))
    (loop for elt in (rest cover-alist) do
	  (format t "~%~%~A (~D)" (first elt) (second elt))
	  (loop for complex in (third elt) do
		(format t "~%  ~A" (format-complex complex))))))

(defun train-aqr (exs)
  (convert-aq-result-to-rules (train-aq exs)))

(defun convert-aq-result-to-rules (cover-alist)
  (let ((*print-with-feature-names* t))
    (loop for elt in (rest cover-alist) nconc
	  (loop for complex in (third elt) collect
		`(<- , (list (first elt))
		  ,@ (format-complex complex))))))
	
