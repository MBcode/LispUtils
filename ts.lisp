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
