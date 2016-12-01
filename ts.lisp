(lu)
;-
;(ql 'cl-cwd)
(require 'cl-cwd)
(defun cwd (p) (cl-cwd:cwd p))
;(load "/home/bobak/lsp/ts.cl") ;trivial-shell (tsh str)
;-
;(ql 'trivial-shell)
(require :trivial-shell)
;(lu)
(defun tsh (str)
 (trivial-shell:shell-command (to-str str)))
 
(defun run-sh (&rest strs)  
  "easier replacement for run-ext"
  (tsh (str-cat strs))) 

;-
;==use tsh for some quick scripting
;-
;-tsv/csv distribution of unique vals per col

(defun split-header (fn &optional (splt #\tab))
  (split-string (tsh (format nil "head -1 ~a" fn)) splt))

(defun cut-fn-nth-u (fn n)
  (numstr (tsh (format nil "cut -f~d ~a|sort -u|wc" n fn)))) ;or 'uniq' vs sort -u

(defun sv-unique-alst (fn &optional (splt #\tab)) ;tsv or #\, for csv
  (let* ((colNames (split-header fn splt))
         (numUnique (loop for i from 1 upto (len colNames) collect (cut-fn-nth-u fn i))))
    (mapcar #'cons colNames numUnique)))
;-
;-save url text in file named after url

(defun str2file (str fn)
   (with-open-file (strm fn :direction :output) 
     (write-string str strm)))

(defun url2txt (url)
  (tsh (format nil "lynx -dump ~a" url)))

(defun rm-http (url)
  (subseq (second-lv (split_at1 url ":")) 2))

(defun url2fn (url)
  (replace-substrings (rm-http url) "\/" "_")) 

(defun url2txt-file (url)
  (let ((fn (str-cat (url2fn url) ".txt")))
    (str2file (url2txt url) fn)))
;- 
