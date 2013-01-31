;;; This file contains functions for encoding multi-value feature vectors into
;;; binary.  Handles linear features by normalizing to [0,1].  Handles missing
;;; by using 1/N for all N bits for a feature with N values.

(in-package user)
(provide 'binary-encoder)

(defparameter *print-encoding* nil          "Prints the binary encoding of values if non-NIL")
(defparameter *one-bit-for-two-values*  t   "Encode two-valued features using one bit")

(defvar       *current-encoded-domains* nil "*domains* whose encoding is currently saved in *encoding-alists*")
(defvar       *encoding-alists* nil         "Binary encodings for feature values")
(defvar       *binary-feature-names* nil)

(defun convert-to-binary-example (example)
  ;;; Encode examples with n-valued features as bit strings using n bits/feature
  ;;; unless short-encode-flag is set in which case log(n) bits are used
  ;;; Return the encoded examples.

  ;; Replace values with their coresponding bit strings in each example
  (list (first example)
	(encode-instance (second example))))

(defun make-encoding ()
  (unless (eq *current-encoded-domains* *domains*)
    (setf *encoding-alists* (mapcar #'encode-domain *domains* *feature-names*))
    (setf *current-encoded-domains* *domains*)
    (setf *binary-feature-names* (make-binary-feature-names))
    (trace-print *print-encoding* "~%~%Encoding: ~A" *encoding-alists*)))

(defun encode-domain (domain feature)
  ;; Assign an n bit string for each value in domain with the one bit for that
  ;; value set to 1
  (cond ((eq domain 'linear)
	 (let ((range (get feature 'range)))
	   ;; Store min and span of linear features in encoding alist for scaling during encoding
	   (when (null range) (make-ranges (compute-ranges)) (setf range (get feature 'range)))
	   (list (first range) (- (second range) (first range)))))
	((and *one-bit-for-two-values* (= (length domain) 2))
	 ;; encode two-valued features using only 1 bit instead of two
	 (if (member (first domain) '(true yes t))
	     (list (list (first domain) 1) (list (second domain) 0) (list *missing-value* 0.5))
	     (list (list (first domain) 0) (list (second domain) 1) (list *missing-value* 0.5))))
	(t (nconc
	     (mapcar #'(lambda (value)
		       (cons value (mapcar #'(lambda (v) (if (eq v value) 1 0))
					   domain)))
		   domain)
	     ;; If missing value give 1/N to every feature value
	     (list (cons *missing-value* (make-list (length domain) :initial-element (/-float 1 (length domain)))))))))


(defun encode-instance (instance)
  ;;; Given the encoding for each feature value, encode an instance into binary
  (if (listp instance)
      (mapcan #'(lambda (feature-value encoding-alist)
		  (if (listp (first encoding-alist))
		      (copy-list (rest (assoc feature-value
					      encoding-alist)))
		      (if (eq feature-value *missing-value*)
			  (list 0.5)                        ; Missing linear features get 0.5
		      ;;; Otherwise scale value between 0 and 1 using min and span stored in encoding alist
			  (list (/-float (- feature-value (first encoding-alist)) (second encoding-alist))))))
	      instance *encoding-alists*)
      (let ((vector (make-array (list (length *binary-feature-names*))))
	    (i 0) feature-value)
	(loop for j from 0 to (1- (length *feature-names*)) as encoding-alist in *encoding-alists* do
	      (setf feature-value (aref instance j))
	      (cond ((listp (first encoding-alist))
		     (loop for bit in (rest (assoc feature-value encoding-alist)) do
			   (setf (aref vector i) bit) (incf i)))
		    ((eq feature-value *missing-value*)
		     (setf (aref vector i) 0.5) (incf i))
		    (t (setf (aref vector i) (/-float (- feature-value (first encoding-alist))
						      (second encoding-alist)))
		       (incf i))))
	vector)))

(defun make-binary-feature-names ()
  (mapcan #'(lambda (feature)
	      (let ((domain (feature-domain feature)))
		(cond ((eq domain 'linear) (list feature))
		      ((and *one-bit-for-two-values* (= (length domain) 2))
		       (if (member (first domain) '(true yes t))
			   (list (append-symbols feature '- (first domain)))
			   (list (append-symbols feature '- (second domain)))))
		      (t (mapcar #'(lambda (value) (append-symbols feature '- value))
				 domain)))))
	  *feature-names*))

(defun binary-feature-name (num)
  (if *binary-feature-names*
      (elt *binary-feature-names* num)
      num))
