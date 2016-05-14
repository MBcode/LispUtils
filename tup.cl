;getting some xls/.. into a format for reasoning/learning over, incl joins/conversions/.. ;mike.bobak@gmail
;csv are confidential
(lu) ;assumed
(defun val=dot-p (c) (equal (cdr c) "."))
(defun assoc-lists (keys vals)
  (mapcar #'cons keys vals))
(defun assoc-col-names (lol)
  (let ((cnames (first lol))
        (vals (rest lol))) ;ListOfLists
    (remove-if #'val=dot-p
        (mapcar #'(lambda (vl) (assoc-lists cnames vl)) vals)
    )
    ))
(ql 'cl-csv)
;could map over the files but keep seperate4a bit
;(cl-csv:read-csv "m-cc.csv")
;(cl-csv:read-csv "m-cp.csv")
;(cl-csv:read-csv "m-pp.csv")
;(trace cl-csv:read-csv)
(defun read-msg-csv (typ)
  (cons (intern typ) 
        (cl-csv:read-csv 
          (make-pathname :name (str-cat "m-" typ ".csv")))))
(defvar *mesgs* (mapcar #'read-msg-csv '("cc" "cp" "pp")))
;typ= '|cc| etc
(defun get_m (typ) (assoc_v typ *mesgs*))
(defun get-m (typ) (assoc typ *mesgs*))
(defvar *pd* (assoc-col-names (cl-csv:read-csv (make-pathname :name "pd.csv"))))
(defvar *leg* (cl-csv:read-csv (make-pathname :name "pleg.csv")))
;can also try: ;works but in arrays/use if want re ops
;(ql 'cl-simple-table)
;(defvar *st* (simple-table:read-csv "pd.csv"))

;w/assoc-cal-names alists json would be very easy, but old protege frames xmltab is 1st target
;so find simple xml writer that doesn't need fancy format 1st; just an alist
