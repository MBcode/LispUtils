;;;; This file adds functions to ID3-ALL to create a version for that builds a separate decision-tree for
;;;; each category, runs a test example through each tree, and puts in the most frequent class whose tree
;;;; classifies it as a member.

(in-package :user)
(eval-when (compile load eval)
 (provide 'id3-multi)
 ;(require 'id3  (ml-progs-file "id3-all"))
 (require 'id3  (ml-progs-file "id3-all.lisp"))
)

(setf (get 'id3-multi 'parent-systems) '(id3))

(defun train-id3-multi (examples)
  (setf examples (make-ordered-examples examples))
  (dolist (cat *categories*)
    (setf (get cat 'training-examples) nil))
  (dolist (example examples)
    (push (rest example) (get (first example) 'training-examples)))
  (let ((tree-alist
	  (mapcar #'(lambda (cat)
		      (let ((training-examples
			      (append (category-examples cat '+)
				      (mapcan #'(lambda (other-cat)
						  (category-examples other-cat '-))
					      (remove cat *categories*))))
			    (*categories* '(+ -)))
			(list cat (length (get cat 'training-examples))
			      (train-id3 training-examples))))
		  *categories*)))
    (cons (maximum-category-label tree-alist *categories*)
	  tree-alist)))

(defun category-examples (cat label)
  (mapcar #'(lambda (inst) (cons label inst)) (get cat 'training-examples)))


(defun test-id3-multi (example tree-alist)
  (setf example (make-ordered-example example))
  (let ((class-counts (mapcan #'(lambda (alist-elt)
				  (if (eq (let ((*categories* '(- +))
						(*negative-category* '-) (*positive-category* '+))
					    (test-id3 example (third alist-elt)))
					  '+)
				      (list (cons (first alist-elt) (second alist-elt)))))
			      (rest tree-alist))))
    (if class-counts
	(maximum-label class-counts *categories*)
	(if (member *negative-category* *categories*)
	    *negative-category*
	    (first tree-alist)))))


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

(defun id3-multi-concept-complexity (tree-alist)
  (let ((sum 0))
    (dolist (alist-elt (rest tree-alist) sum)
      (incf sum (id3-concept-complexity (third alist-elt))))))

