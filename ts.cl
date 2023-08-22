(ql 'trivial-shell)
(require :trivial-shell)
;defun tshell-command (str)
;defun tshell (str)
;(lu)
(defun tsh (str)
 (trivial-shell:shell-command (to-str str)))

(defun tshe (str)
  (rm-nils (explode- (remove #\newline (tsh str)))))

(defun pdfgrep (fstr fn)
  (tshe (str-cat "pdfgrep " fstr " " fn)))

       ;(rl (tshe (str-cat "pdfgrep " fstr " " fn)))
(defun pdfgrep-val (fstr fn)
  (let ((args (explode- fstr))
        (rl (pdfgrep fstr  fn)))
    (set-difference rl (append args (list (str-cat fn ":"))) :test #'equal)))
 
