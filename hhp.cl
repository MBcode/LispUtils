;try loading some kaggle hhp data, as R had some problems w/it bobak@balisp.org
#+ignore ;fix here&in utils
(defun read-from-csv-str (str &key (start 0) (separator #\,))
    (if (>= start (length str)) nil
      (let ((pn (position separator str)))
        (if (not pn) nil
          (cons (read-from-csv-str str t nil :start start)
            (read-from-csv-str str :start (+ start pn)))))))
;do this for first-lv etc
(defgeneric first- (la))
(defgeneric second- (la))
(defmethod first- ((l LIST)) (first l))
(defmethod second- ((l LIST)) (second l))
(defmethod first- ((a ARRAY)) (aref a 0))
(defmethod second- ((a ARRAY)) (aref a 1))

(defun numstr_ (ns) 
  "convert nil->0"
  (if (null ns) 0 (numstr- ns)))

(defun prs-csv_nums (str)
    (mapcar- #'numstr_ (prs-csv str)))

(defun rcf (fn f)
  "read csv file"
  (with-open-file (in fn :direction :input)
    (print (parse-comma-separated-header (read-line in)))
    (loop for line = (read-line in nil nil) do
     (when line
       (funcall f 
          ;(csv_parse-str line)
          ;(read-from-csv-str line)
          ;(mapcar- #'numstr (csv-parse-string line :separator #\t))
          (prs-csv_nums line)
          )))))

(defun rcf-n (fn f  &optional (n nil))
  "read csv file, upto line n"
;  (prs-csv_nums (apply-lines-n fn f n))
  (apply-lines-n fn f n))
;(trace prs-csv prs-csv_nums tokens)

(defun rt0 () 
  (rcf ;"t.csv" 
       "data/HHP_release3/Target.csv" 
       ;#'print
       #'(lambda (l) (if (eq (second- l) 1) (print (first- l)) (print  l)))
       ;#'(lambda (l) (when (eq (second- l) 1) (print (first- l))))
       ;#'(lambda (l) (when (eq (second l) 1) (print (first l))))
       ))
(defun rtt ()  ;don't think i want to save in a list as much as assert it
  (read-csv "data/HHP_release3/Target.csv" ))

(defun tokens- (s) (tokens s :separators (list #\space #\return #\linefeed #\tab #\,)))
(defun tokens-n (s) 
  (mapcar- #'numstr_
           (tokens s :separators (list #\space #\return #\linefeed #\tab #\,))))

(defun rt () 
  (rcf-n
       ;"t.csv" 
       "data/HHP_release3/Target.csv" 
       ;#'print
       ;#'(lambda (ln) (let ((l (prs-csv_nums ln)))
       #'(lambda (ln) (let ((l (tokens-n ln)))
                        (when (eq (second- l) 1) (print (first- l)))))
                     ;  (if (eq (second- l) 1) (print (first- l)) (print  l))))
       ;#'(lambda (l) (when (eq (second- l) 1) (print (first- l))))
       ;#'(lambda (l) (when (eq (second l) 1) (print (first l))))
      ;99
       ))
;now do for rest of files, and assert values to proper(km)structs along the way 
(defvar *basedir* "data/HHP_release3")
(defvar *filelst* '("Claims" "DaysInHospital_Y2" "DaysInHospital_Y3" "DrugCount" "LabCount"
                     "Lookup PrimaryConditionGroup" "Lookup ProcedureGroup" "Members" "Target"))

;(rt) ;in end of ld.cl
