(require :asdf) ;this is old, load updates, so fix again

(setf asdf:*central-registry* 
      '( *default-pathname-defaults* 
        #p"/usr/local/lib/sbcl/site-systems/"
        #p"/home/bobak/dwn/lang/lsp/code/project/"
        ))
(defun ldql ()
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))
      (t2 (merge-pathnames "quicklisp/t.lsp" (user-homedir-pathname))))
    (if (probe-file quicklisp-init)
      (progn
          (load quicklisp-init)
          (if (probe-file t2) (load t2 :print t)
            (warn "no qa/ql"))
         ;(eval-when (:load-toplevel :execute)
         ;      (use-package :ql)
         ;      (defun ql (a) (quickload  a))
         ;      (defun qa (a) (system-apropos  a))
         ;      )
          ) ;t2 has fncs below
      (warn "no quicklisp")))
)
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
				       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init))) 

;(lql) ;just do it 
;#+quicklisp (defun ql (a) (ql:quickload  a))
#+quicklisp (defun ql (a) (ql:quickload  a :verbose t :explain t))
#+quicklisp (defun qa (a) (ql:system-apropos  a))
#+quicklisp (defun qd (a) (ql:who-depends-on  a))
;=might try breaking rest out here
;(ql 'trivial-shell)
;(require :trivial-shell)
;(defun tsh (str) (trivial-shell:shell-command (to-str str)))
(defun tsh (str) (run-shell-command (to-str str)))
;(defun cu () (compile-file "/home/bobak/lsp/util_mb.lisp"))
;(defun lu () (load "/home/bobak/lsp/util_mb"))
;(lu)
;-
(defun to-str (s)  ;could have symbolp then symbol-name, but this ok
  (if (stringp s) s
    (format nil "~a" s)))  ;or just use 'string'
;-
(defun str-cat2 (a b)
    (format nil "~a~a" a b))
(defun str-cat (&rest args)
    (reduce #'str-cat2 args))
;-
(defun rsc (s) (asdf:run-shell-command s))
 
(defun run-sh (&rest strs)  
    "easier replacement for run-ext"
      (tsh (str-cat strs))) 
(defun run-ext (&rest strs)  
      (run-shell-command (str-cat strs)))  ;acl

(defun ex () 
  (print "byE")
  (exit))

;;; The following lines (should have been) added by ql:add-to-init-file:
;(defun run-ext- (cmd &rest args)
;  (let ((str (make-string-output-stream)))
;   ;(sb-ext:run-program cmd args :search t :output str)
;    (run-program cmd args :search t :output str)
;    (let ((s (get-output-stream-string str))) s ))) 
;(defun date () (run-ext "date"))
(defun date () (rsc "date"))
(defvar *uptime* (date)) ;start-time
(defun uptime () *uptime*)
(defun lo () (format t "~&start:~a to-now:~a" (uptime) (date)) (ex))
