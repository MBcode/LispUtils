;;; -*- Mode:Common-Lisp; Package:USER; Base:10; Fonts:(COURIER TR12I COURIER COURIER TR12B) -*-      

(eval-when (compile load eval)
 ;(require 'theory-utilities (ml-progs-file "theory-utilities"))
 (require 'theory-utilities (ml-progs-file "theory-utilities.lisp"))
)

(defvar *correct-theory* nil)
(defvar *conflicting-categories* nil)

(defun make-examples-of-categories (&optional (num-per-category 30)
				    (num-negs-per-cat 0) (theory *correct-theory*)
				    (conflicting-cats *conflicting-categories*))
  (mapcan #'(lambda (category)
	      (let* ((core-examples (make-core-examples category theory))
		     (core-negatives
		       (unless (zerop num-negs-per-cat)
			 (make-core-negatives core-examples theory conflicting-cats))))
		  (nconc
		    (complete-examples core-examples num-per-category theory conflicting-cats)
		    (complete-examples core-negatives num-negs-per-cat theory conflicting-cats))))
	  (remove 'negative *categories*)))

(defun make-core-examples (category theory)
  (mapcar #'(lambda (example)
	      (cons category example))
       (remove-if #'inconsistent-example
		  (mapcar #'(lambda (ex) (remove-duplicates ex :test #'equal))
			  (generate-category-examples category theory)))))

(defun inconsistent-example (example)
  (if (null (rest example))
      nil
      (or (assoc (first (first example)) (rest example))
	  (inconsistent-example (rest example)))))

(defun generate-category-examples (category theory)
  (mapcan
      #'(lambda (rule)
	  (let* ((antecedents (antecedents rule))
		   (observables (observables (antecedents rule)))
		   (remainder (set-difference antecedents observables)))
;	    (format t "~%observables ~A" observables)
;	    (format t "~%remainder ~A" remainder)
	    (combine-proofs 
	      (if remainder
		  (let ((remainder-proofs (remainder-proofs remainder theory)))
;		    (format t "~%remainder ~A" remainder)
;		    (format t "~%remainder-proofs ~A" remainder-proofs)
		    (append (antecedent-values observables) remainder-proofs))
		  (antecedent-values observables)))))
      (sublist category theory :key #'caadr)))

(defun combine-proofs (item-list)
  (if (null item-list) '(())
      (mapcan #'(lambda (item)
		  (mapcar #'(lambda (list)
			      (append item list))
			  (combine-proofs (cdr item-list))))
	      (car item-list))))

(defun remainder-proofs (remainder theory )
  (mapcar
    #'(lambda (antecedent)
;	(format t "~%antecedent ~A" antecedent)
	(generate-category-examples (car antecedent) theory))
    remainder))

(defun observables (antecedents)
  (mapcan #'(lambda (feature-name)
	     (let ((antecedent (find feature-name antecedents
				      :key #'antecedent-feature)))
		(when antecedent
		  (or
		    (when (= (length antecedent) 2)
		      (let ((item (second antecedent)))
			(when (pcvar-p item)
			  (sublist item
				   antecedents
				   :key 'second
				   :test #'my-var-eq))))
		    (list antecedent)))))
	  *feature-names*))

(defun antecedent-values (antecedents)
  (mapcan #'(lambda (feature-name)
	      (let ((antecedent (find feature-name antecedents
				      :key 'antecedent-feature)))
		(when antecedent
		  (or
		    (when (linear-feature-p (antecedent-feature antecedent))
		      (let ((item (second antecedent)))
			(when (pcvar-p item)
			  (list (linear-feature-values (car antecedent)
						 (sublist item
							  antecedents
							  :key 'second
							  :test #'my-var-eq))))))
		    (list (list (list (if (eq (first antecedent) 'not)
				    (list (antecedent-feature antecedent) 'false)
				    antecedent))))))))
	  *feature-names* ))

(defun my-var-eq (var1 var2)
  (and (and (pcvar-p var1) (pcvar-p var2))
       (var-eq var1 var2)))

(defun linear-feature-values (feature-name interval-list  &optional (eps 1e-3))
  (when interval-list
    (let (min max)
      (dolist (interval (sortcar (copy-list interval-list) 'string-lessp))
	(let ((predicate (first interval))
	      (value (third interval)))
	  (case predicate
	    (> (setf min (+ value eps)))
	    (>= (setf min value))
	    (= (return value))
	    (<= (setf max value))
	    (< (setf max (- value eps))))))
      (list (list (list feature-name min max))))))

(defun make-core-negatives (core-examples theory conflicting-cats)
  (mapcan #'(lambda (core-example)
	      (remove-if #'(lambda (core-neg) (conflicting-categories core-neg theory conflicting-cats))
			 (core-negatives core-example)))
	  core-examples))

(defun core-negatives (core-example)
  (setf core-example (cons 'negative (rest core-example)))
  (mapcan #'(lambda (pair)
	      (mapcar #'(lambda (new-pair)
			  (subst new-pair pair core-example))
		      (if (linear-feature-p (first pair))
			  (invert-interval pair)
			  (mapcar #'(lambda (val)
				      (list (first pair) val))
				  (remove (or (second pair) 'true)
					  (feature-domain (first pair)))))))
          (rest core-example)))

(defun invert-interval (pair &optional (eps 1e-3))
  (let* ((feature (first pair))
	 (min (second pair))
	 (max (third pair))
	 (feature-range (linear-feature-range feature)))
    (cond ((or (null min) (= min (first feature-range)))
	   (list (list feature (+ max eps) nil)))
	  ((or (null max) (= max (second feature-range)))
	   (list (list feature nil (- min eps))))
	  (t (list
	       (list feature (+ max eps) nil)
	       (list feature nil (- min eps)))))))

(defun complete-examples (examples number-to-make theory conflicting-cats)
  (let (new-examples (num-new-examples 0))
    (loop
      (if (= num-new-examples number-to-make)
	  (return new-examples))
      (let* ((example (fill-in-linear (pick-one examples)))
	     (needed-features (remove-if #'(lambda (feature-name)
					     (member feature-name (rest example) :key #'first))
					 *feature-names*))
	     (new-features
	       (mapcar #'(lambda (feature)
			   (list feature
				 (if (linear-feature-p feature)
				     (random-linear-value feature)
				     (pick-one (feature-domain feature)))))
		       needed-features))
	     (new-example (subst-binary (nconc example new-features))))
	;;(format t "~% Ex: ~A" new-example)
	(unless (or (member new-example new-examples :test #'equal)
		    (conflicting-categories new-example theory conflicting-cats))
	  (push new-example new-examples)
	  (incf num-new-examples)
	  )))))

(defun conflicting-categories (example theory conflicting-cats)
  (let ((provable-categories (provable-example-categories example theory)))
    (or (if (eq (first example) 'negative)
	    provable-categories
	    (not (equal provable-categories (list (first example)))))
	(dolist (cats conflicting-cats nil)
	  (if (rest (provable-example-categories example theory cats))
	      (return t))))))

(defun fill-in-linear (example)
  (cons (first example)
	(mapcar #'(lambda (pair)
		    (if (= (length pair) 3)
			(list (first pair)
			      (random-linear-value (first pair) (rest pair)))
			pair))
		(rest example))))
	      
(defun random-linear-value (feature &optional interval)
  (let* ((range (linear-feature-range feature))
	 (min (or (and interval (first interval))
		  (first range)))
	 (max (or (and interval (second interval))
		  (second range))))
    (+ min (random (float (- max min))))))

(defun linear-feature-range (feature)
  (or (get feature 'range) '(0 100)))

(defun subst-binary (example)
  (cons (first example)
	(mapcan #'(lambda (feature)
		    (cond ((eq (second feature) 'false) nil)
			  ((eq (second feature) 'true) (list (list (first feature))))
			  (t (list feature))))
		(rest example))))


(defun antecedent-feature (antecedent)
  (if (eq (first antecedent) 'not)
      (first (second antecedent))
      (first antecedent)))

(defun antecedents (rule) (cddr rule))

(defun sublist (value list &key (test #'eql) (key 'identity))
  (remove-if-not #'(lambda (elt) (funcall test value elt)) list
		 :key key))



