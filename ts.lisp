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
;use tsh for some quick scripting
(defun split-header (fn &optional (splt #\tab))
  (split-string (tsh (format nil "head -1 ~a" fn)) splt))

(defun cut-fn-nth-u (fn n)
  (numstr (tsh (format nil "cut -f~d ~a|sort -u|wc" n fn))))

(defun sv-unique-alst (fn &optional (splt #\tab)) ;tsv or #\, for csv
  (let* ((colNames (split-header fn splt))
         (numUnique (loop for i from 1 upto (len colNames) collect (cut-fn-nth-u fn i))))
    (mapcar #'cons colNames numUnique)))
