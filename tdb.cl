;;;a collection of some code using sqlite
;;;==> db.cl <==
;get a few base fncs into utils, eg. count-items/count-a
;==============> di2.cl <==
;bobak@balisp.org have a doc-term matrix, with docid,term,count; given2docid's return overlap cnt
(ql 'sqlite)
(lu)
;(in-package :sqlite)
(defvar *c* (sqlite:connect "reuters.db"))
(defun ft2alst (t3)
  "freq table, docid term count -> term,count alist"
  (cons (second t3) (third t3)))
;should set up for any doc
;(defvar *l1* (sqlite:execute-to-list *c* "select * from frequency where docid='17035_txt_earn'"))
;(defvar *l2* (sqlite:execute-to-list *c* "select * from frequency where docid='10080_txt_crude'"))
(defun di-rows (di)
  "rows for a docid"
  (sqlite:execute-to-list *c* (str-cat "select * from frequency where docid='" di "'")))

(defun di_rows (di)
  "alst of term count, from di"
  (mapcar #'ft2alst (di-rows di)))

(defun int2docs (&optional (d1 "17035_txt_earn") (d2 "10080_txt_crude"))
  "could cosine overlap, but only asked for simple sum of count*count for overlap terms"
  (let* ((a1 (di_rows d1))
         (a2 (di_rows d2))
         (int1 (intersection a1 a2 :key #'car :test #'equal)))
    ;int1 is from a1, so now need to find in a2,  and mult the vals, and sum
   (sum-l
    (mapcar #'(lambda (li) 
                (let ((li2 (assoc (car li) a2 :test #'equal)))
                  (* (cdr li) (cdr li2))))
            int1))))

;==============> count-qry.cl <==

;http://compgroups.net/comp.lang.lisp/increment-hash-value-2/703023
(defun count-items (lst)
  "sum up lst occurances &print"
  (let ((ht (make-hash-table)))
    (loop :for item :in lst :do
       (incf (gethash item ht 0))
       :finally
       (maphash #'(lambda (k v) (format t "~A: ~A~%" k v)) ht)))) 

(defun count-alst (lst)
  "sum up alst vals &print"
  (let ((ht (make-hash-table)))
    (loop :for item :in lst :do
          ;maximizing 
       (incf (gethash (car item) ht 0) (cdr item))
          ;into mx
       :finally
       (maphash #'(lambda (k v) (format t "~A: ~A~%" k v)) ht)
       ;mx
       ))) ;get a loop max in there too ;have a version that does it

(ql 'sqlite)
;(in-package :sqlite)
(defvar *c* (sqlite:connect "reuters.db"))
(defvar *qs* "select * from v where term='washington' union select * from v where term='taxes' union select * from v where term='treasury' group by docid")

(defun f-dc2alst (t3)
  "freq table, docid term count -> term,count alist"
  (cons (first t3) (third t3)))

(defvar *l1* (mapcar #'f-dc2alst (sqlite:execute-to-list *c*  *qs*)))

(defun count-qry (&optional (f2alst #'f-dc2alst) (qs *qs*) (db *c*))
  "qry db convert2alst and count values"
  (count-alst  (mapcar f2alst (sqlite:execute-to-list db qs))))

;==============> cnt-mx.cl <==
;bobak@balisp.org   doing a group-by sum of values
;(defun count-items (lst)
;  (let ((ht (make-hash-table)))
;    (loop :for item :in lst :do
;       (incf (gethash item ht 0))
;       :finally
;       (maphash #'(lambda (k v) (format t "~A: ~A~%" k v)) ht)))) 

;rename this, or pass in fnc or ..
#+ignore ;already above now
(defun count-alst (lst)
  (let ((ht (make-hash-table)))
    (loop :for item :in lst :do
      ;(let ((iv
       (incf (gethash (car item) ht 0) (cdr item))
      ;    ))
      ;   maximizing iv into mx
      ;   )
       :finally
       (let ((mx 0))
      (maphash #'(lambda (k v) 
                   (when (> v mx)
                     (setf mx v)
                   (format t "~A: ~A~%" k v)) 
                   )
               ht)
      )
      ;mx
       ))) 
;get a loop max in there too

;(ql 'sqlite)
;;(in-package :sqlite)
;(defvar *c* (sqlite:connect "reuters.db"))
;(defvar *qs* "select * from v where term='washington' union select * from v where term='taxes' union select * from v where term='treasury' group by docid")
;;use one below if another view named v hasn't been constructed
;;(defvar *qs* "select * from frequency where term='washington' union select * from frequency where term='taxes' union select * from frequency where term='treasury' group by docid")

;(defun f-dc2alst (t3)
;  "freq table, docid term count -> term,count alist"
;  (cons (first t3) (third t3)))
;
;(defvar *l1* (mapcar #'f-dc2alst (sqlite:execute-to-list *c*  *qs*)))
;
;(defun count-qry (&optional (f2alst #'f-dc2alst) (qs *qs*) (db *c*))
;  "qry db convert2alst and count values"
;  (count-alst  (mapcar f2alst (sqlite:execute-to-list db qs))))
 

;;;==> cnt-mx.cl <==
;bobak@balisp.org   doing a group-by sum of values
#+ignore ;already above now
(defun count-items (lst)
  (let ((ht (make-hash-table)))
    (loop :for item :in lst :do
       (incf (gethash item ht 0))
       :finally
       (maphash #'(lambda (k v) (format t "~A: ~A~%" k v)) ht)))) 

#+ignore ;already above now
(defun count-alst (lst)
  (let ((ht (make-hash-table)))
    (loop :for item :in lst :do
      ;(let ((iv
       (incf (gethash (car item) ht 0) (cdr item))
      ;    ))
      ;   maximizing iv into mx
      ;   )
       :finally
       (let ((mx 0))
      (maphash #'(lambda (k v) 
                   (when (> v mx)
                     (setf mx v)
                   (format t "~A: ~A~%" k v)) 
                   )
               ht)
      )
      ;mx
       ))) 
;get a loop max in there too

(ql 'sqlite)
;(in-package :sqlite)
(defvar *c* (sqlite:connect "reuters.db"))
(defvar *qs* "select * from v where term='washington' union select * from v where term='taxes' union select * from v where term='treasury' group by docid")
;use one below if another view named v hasn't been constructed
;(defvar *qs* "select * from frequency where term='washington' union select * from frequency where term='taxes' union select * from frequency where term='treasury' group by docid")

(defun f-dc2alst (t3)
  "freq table, docid term count -> term,count alist"
  (cons (first t3) (third t3)))

(defvar *l1* (mapcar #'f-dc2alst (sqlite:execute-to-list *c*  *qs*)))

(defun count-qry (&optional (f2alst #'f-dc2alst) (qs *qs*) (db *c*))
  "qry db convert2alst and count values"
  (count-alst  (mapcar f2alst (sqlite:execute-to-list db qs))))
 
