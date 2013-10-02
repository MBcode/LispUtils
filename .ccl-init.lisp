(require :asdf) ;this is old, load updates, so fix again
;(declaim (optimize (debug 3) (safety 3) (speed 0) (space 0))) 
;(sb-ext:restrict-compiler-policy 'debug 3)
;(require' sb-posix)
;(require 'swank)
;(swank-loader:init :load-contribs t)

;#-:asdf (load "/Users/bobak/Documents/downloads/lang/lsp/asdf/asdf-install/load-asdf-install")
(setf asdf:*central-registry* 
      '( *default-pathname-defaults* 
        #p"/usr/local/lib/sbcl/site-systems/"
;       #p"/Users/bobak/quicklisp/local-projects/" same as:
        ;#p"/Users/bobak/Documents/downloads/lang/lsp/code/project/"
        #p"/Users/bobak/dwn/lang/lsp/code/project/"
        #p"/usr/share/common-lisp/systems/" ;ln to below
))
        ;#p"/Users/bobak/.sbcl/systems/"
         ;#p"/Users/bobak/lsp/y"
        ;#p"/Users/bobak/Documents/downloads/lang/lsp/code/project/src/clbuild_0/systems/" 
;(defun pv () (princ sb-ext:*debug-print-variable-alist*))
;#+ignore
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
;;; The following lines (should have been) added by ql:add-to-init-file:
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
				       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init))) 

(defun lut () (load "/Users/bobak/lsp/util_mb"))
(defun lrsm () (load "/Users/bobak/lsp/rsm"))
(defun lql- () (load "/Users/bobak/lsp/ql.lisp"))
(defun lql () (load #P"/Users/bobak/quicklisp/setup.lisp"))
;(lql) ;just do it 
;#+quicklisp (defun ql (a) (ql:quickload  a))
#+quicklisp (defun ql (a) (ql:quickload  a :verbose t :explain t))
#+quicklisp (defun qa (a) (ql:system-apropos  a))
#+quicklisp (defun qd (a) (ql:who-depends-on  a))
;#<SYSTEM km / km-2-5-33 / quicklisp 2012-09-09>
(defun lkm () (load "/Users/bobak/lsp/km_2-5-33"))
;(defun lkm () (ql 'km))
(defun lkm2 () (lkm) (load "/Users/bobak/lsp/u2"))
;#p"/usr/share/common-lisp/source/" ; #p"/Users/bobak/Documents/downloads/lang/lsp/asdf/registry"
(defun apr- (s) (apropos s :cl))
(defun apr (s) (apropos s))
;--load-preferences
(defun ui () (in-package :usr))
(defun u () (use-package :usr))
;(defun inp (pkg) (in-package pkg))
;--
;;; If the first user-processable command-line argument is a filename, 
;;; disable the debugger, load the file handling shebang-line and quit. 
#-CCL
(let ((script (and (second *posix-argv*) 
                     (probe-file (second *posix-argv*))))) 
    (when script 
       ;; Handle shebang-line 
       (set-dispatch-macro-character #\# #\! 
                                      (lambda (stream char arg) 
                                         (declare (ignore char arg)) 
                                         (read-line stream))) 
       ;; Disable debugger 
       (setf *invoke-debugger-hook* 
             (lambda (condition hook) 
                (declare (ignore hook)) 
                ;; Uncomment to get backtraces on errors 
                ;; (sb-debug:backtrace 20) 
                (format *error-output* "Error: ~A~%" condition) 
                (exit))) 
       (load script) 
       (terpri) 
       (exit)))  
;--
;xach suggest also core-ing in (require 'sb-aclrepl)  ;which I have below
(defun lp (f) (load f :print t))
(defun lp- (l) 
 "load w/print of symbol"
  (load (format nil "~A" l) :print t))
(defun lsp (l) 
  (load (format nil "~A.lsp" l) :print t))
(defun lt () (load "t.cl" :print t)) 
(defun ltb () (load "t")) 
(defun lt2 () (load "t2.cl" :print t)) 
(defun lu () (load "/Users/bobak/lsp/util_mb"))
(defun lkm3 () (lu) (lkm2))
(defun lq () "https://github.com/pnathan/cl-linq"
  (ql 'alexandria) (load "/Users/bobak/lsp/cl-linq.lisp"))
(defun cu () (compile-file "/Users/bobak/lsp/util_mb.lisp"))
(defun cu2 () (compile-file "/Users/bobak/lsp/u2.lisp"))
(defun cgl () (load "/Users/bobak/lsp/cg.cl")) ;load cg callgraph
(defun ct () (compile-file "t.cl")) 
(defun lt- () (load "t.fasl" :print t)) 
(defun l-1 () (load "ld1.cl" :print t)) 
(defun l1 () (load "ld.cl" :print t)) 
(defun l2 () (load "ld2.cl" :print t)) 
(defun l3 () (load "ld3.cl" :print t)) 
(defun l4 () (load "ld4.cl" :print t)) 
(defun l5 () (load "ld5.cl" :print t)) 
(defun l6 () (load "ld6.cl" :print t)) 
;(defun getcwd () (sb-unix:posix-getcwd/))
;(defun pwd () (sb-unix:posix-getcwd))
(defun al (l) 
 "asdf load"
  (asdf:oos 'asdf:load-op l))
(defun altst (l) 
 "asdf load"
  (asdf:oos 'asdf:test-op l))
(defvar *clocc-root* "/Users/bobak/Documents/downloads/lang/lsp/code/clocc/clocc/") 
(defun loc ()
   "load clocc"
 ;(setq *clocc-root* "/Users/bobak/Documents/downloads/lang/lsp/code/clocc/clocc/") ; or whatever ...
  (load (concatenate 'string *clocc-root* "clocc"))
  (load (translate-logical-pathname "clocc:src;cllib;base")) ; or whatever ...
)
;defun sai ()
(defun lai ()
   "load asdf install"
 (load #p"/usr/share/common-lisp/inst.cl" :print t)) 
(defun lai2 () (load #p"/usr/share/common-lisp/inst2.cl" :print t))
; (progn
;  (require :asdf-install)
;  (pushnew '(
;    #p"/usr/share/common-lisp/source/"  
;    #p"/usr/share/common-lisp/systems/" 
;    "shared-space" 
;      ) asdf-install:*locations*) 
; (defun aii (l) 
;  "asdf load"
;   (asdf-install:install l))
;)
;now in cdf's ll.cl
(defun ll () ;&opt f
  (load "load.cl" :print t)
  ;(when f (apply f))
  )
(defun pwd- ()
 (progn *default-pathname-defaults*))
(defun ex () 
  (print "byE")
  ;(sb-ext:quit)
  (quit) ;(sb-ext:exit) ;v1.1
  )
(defun ex0 () (ex));for typos
#-CCL
(defun run-ext (cmd &rest args)
  (let ((str (make-string-output-stream)))
    (sb-ext:run-program cmd args :search t :output str)
    (let ((s (get-output-stream-string str))) s ))) 
(defun to-str (s) (coerce s 'string))
(defun ls () (ccl:run-program "ls" '() :output *standard-output*))
(ql 'trivial-shell)
;defun tshell-command (str)
(defun run-ext (str)
     (trivial-shell:shell-command (to-str str)))
(defun date () (run-ext "date"))
(defvar *uptime* (date))
(defun uptime () *uptime*)
(defun lo () (format t "~&start:~a to-now:~a" (uptime) (date)) (ex))
;;; Make SBCL act like Allegro REPL. It's optional but fun to do for productivity.
;;; If you don't want it, just delete it.

;(al 'acl-compat) ;new
;(ignore-errors (require 'sb-aclrepl))

(when (find-package 'sb-aclrepl)
  (push :aclrepl cl:*features*))
#+aclrepl  ;http://www.sbcl.org/manual/sb_002daclrepl.html
(progn
        (setq sb-aclrepl:*max-history* 100)
        (setf (sb-aclrepl:alias "asdc")
        #'(lambda (sys) (asdf:operate 'asdf:compile-op sys)))
        (sb-aclrepl:alias "l" (sys) (asdf:operate 'asdf:load-op sys))
        (sb-aclrepl:alias "t" (sys) (asdf:operate 'asdf:test-op sys))
        ;; The 1 below means that two characaters ("up") are required
        (sb-aclrepl:alias ("up" 1 "Use package") (package) (use-package package))
        ;; The 0 below means only the first letter ("r") is required,
        ;; such as ":r base64"
        (sb-aclrepl:alias ("require" 0 "Require module") (sys) (require sys))
        (setq cl:*features* (delete :aclrepl cl:*features*))) 
;
;(defun bt (&optional (n 7)) (sb-debug:backtrace n)) ;bt
;;;=easier.cl
;Easier ASDF?
;Currently ASDF is not very user-friendly. I can think about at least two improvements: a separate function instead of (asdf:oos 'asdf:load-op :foo), and asking for path when some library is not found. I tried to write a function that solves both of these problems: 
(defun load-lib (library) 
  "Loads a library using ASDF" 
  (handler-bind ((asdf:missing-component (lambda (c) (print-object c *standard-output*) 
                                           (format t "~%~%Enter the path of its possible location:") 
                                           (let ((path (read-line))) 
                                             (if (zerop (length path)) 
                                               (invoke-restart 'abort-request) 
                                               ;or (abort) ;in SLIME-less ;environment 
                                               (progn (push path asdf:*central-registry*) 
                                                      (invoke-restart 'asdf:retry))))))) 
    (asdf:oos 'asdf:load-op library)))
;Unfortunately it doesn't work, because there is no RETRY restart available at the time of searching through *central-registry*. At the time the missing-component error occurs there is no way to fix it, without patching up asdf.lisp. There must be another way, for example through *system-definition-search-functions*. Or maybe it's better to encourage ASDF developers to provide retry restart for the missing-component error? 
(defun al2 (l) (load-lib l)) 
;-
(defun add-package-nickname (name nickname)
 (let ((p (find-package name)))
   (rename-package p (package-name p)
        (cons nickname (package-nicknames name)))))

(add-package-nickname :cl-user :user) 
(add-package-nickname :common-lisp-user :lisp) ;new 
;(add-package-nickname :sb-ext :extensions) 
;(add-package-nickname :sb-ext :ext) 
;https://groups.google.com/group/comp.lang.lisp/msg/6e91e20f2f371b52?&noredirect
;;; More shortcuts & conveniences: 
(defun ap- (string &optional package)    ; be more liberal about 2nd arg 
     (apply #'apropos string (when package (list (find-package package))))) 
(defun de (&rest rest) (apply #'describe rest)) 
(defun dis (&rest rest) (apply #'disassemble rest))   
(defun mxp (&rest rest) (pprint (apply #'macroexpand rest))) 
(defun mxp0 (&rest rest) (apply #'macroexpand rest))     ; same, but w/o PP 
(defun mxp1 (&rest rest) (apply #'macroexpand-1 rest)) 
;(defun mxp* (&rest rest) (apply #'walker:macroexpand-all rest)) ; CMUCL only 

;;; For REPL compiles of functions with non-NIL lexical environments, 
;;; e.g, (COMPILE* (let ((y 5)) (defun add5 (x) (+ x y)))). 
(defmacro compile* (&body body) 
     `(funcall (compile nil (lambda () ,@body)))) 

;;; I don't know why I find myself needing this so often, but I do... 
(defun hash-table-alist (hash-table &key (test (constantly t))) 
     (loop for key being each hash-key in hash-table using (hash-value value) 
       when (funcall test key value) 
         collect (cons key value)))  
 
;=====
#-CCL
(progn ;http://jsnell.iki.fi/blog/archive/2007-12-19-pretty-sbcl-backtraces.html
(defun backtrace-with-extra-info (&key (start 1) (end 20))
  (swank-backend::call-with-debugging-environment
   (lambda ()
     (loop for i from start to (length (swank-backend::compute-backtrace
                                        start end))
           do (ignore-errors (print-frame i))))))
;-or try: http://tehran.lain.pl/stuff/pretty-sbcl-backtrace.lisp
#-CCL
(defun print-frame (i)
  (destructuring-bind (&key file position &allow-other-keys)
      (apply #'append
             (remove-if #'atom
                        (swank-backend:frame-source-location-for-emacs i)))
    (let* ((frame (swank-backend::nth-frame i))
           (line-number (find-line-position file position frame)))
      (format t "~2@a: ~s~%~
                   ~:[~*~;~:[~2:*    At ~a (unknown line)~*~%~;~
                             ~2:*    At ~a:~a~%~]~]~
                   ~:[~*~;    Local variables:~%~{      ~a = ~s~%~}~]"
              i
              (sb-debug::frame-call (swank-backend::nth-frame i))
              file line-number
              (swank-backend::frame-locals i)
              (mapcan (lambda (x)
                        ;; Filter out local variables whose variables we
                        ;; don't know.
                        (unless (eql (getf x :value) :<not-available>)
                          (list (getf x :name) (getf x :value))))
                      (swank-backend::frame-locals i))))))
#-CCL
(defun find-line-position (file char-offset frame)
  ;; It would be nice if SBCL stored line number information in
  ;; addition to form path information by default. Since it doesn't
  ;; we need to use Swank to map the source path to a character
  ;; offset, and then map the character offset to a line number.
  (ignore-errors
   (let* ((location (sb-di::frame-code-location frame))
          (debug-source (sb-di::code-location-debug-source location))
          (line (with-open-file (stream file)
                  (1+ (loop repeat char-offset
                            count (eql (read-char stream) #\Newline))))))
     (format nil "~:[~a (file modified)~;~a~]"
             (= (file-write-date file)
                (sb-di::debug-source-created debug-source))
             line)))) 
(defun bt2 (&optional (n 20))
  (backtrace-with-extra-info :end n))
)

;change  *default-pathname-defaults* to do a cd
;(defun cd (path)
;  (when (pathnamep path) (setf *default-pathname-defaults* path)))
(defgeneric cd- (p))
(defmethod cd- ((path pathname))
  (setf *default-pathname-defaults* path))
(defmethod cd- ((path string))
  (cd- (pathname path)))
(defmethod cd- (path)
  (princ path))
;I have a util that does it now

(defun lac ()
;(load "/Users/bobak/Documents/downloads/lang/lsp/asdf/asdf-config.lisp")
 (load "/Users/bobak/Documents/downloads/lang/lsp/asdf/asdf-config")
 ;(eval-when (compile load eval)
 ;(defun alc (l) (asdf:operate 'asdf-config:initialize-op l)) )
)

;(defun pwd () *default-pathname-defaults*)
(defun defaulted-source-pathname () *default-pathname-defaults*)
;#+ignore
(defun ldlspy ()
 (progn
  ;(load #p"/Users/bobak/Documents/downloads/lang/lsp/asdf/lispy-all-0.x/asdf-config.lisp")
  ;(load #p"/Users/bobak/Documents/downloads/lang/lsp/asdf/lispy-all-0.x/asdf-cfg.lisp")
  (load #p"/Users/bobak/Documents/downloads/lang/lsp/code/project/src/lispy-all-0.4/asdf-config.lisp")
  (print ASDF:*CENTRAL-REGISTRY*)
  (al 'lispy)
  (eval "(defun all (s) (lispy:install (lispy:module-by-name s)))")
))

;(defun getenv (var) ;close (sb-ext:posix-getenv var))

(defun lsm (&optional (package :cl-user)) 
  (let ((functions (list)) 
        (macros (list))) 
    (do-symbols (symbol package) 
      (when (fboundp symbol) 
        (if (macro-function symbol) 
            (push symbol macros) 
            (push symbol functions)))) 
    (values functions macros)) )   
;;; If a fasl was stale, try to recompile and load (once).
#-CCL
(defmethod asdf:perform :around ((o asdf:load-op)
                              (c asdf:cl-source-file))
  (handler-case (call-next-method o c)
   ;; If a fasl was stale, try to recompile and load (once).
   (sb-ext:invalid-fasl ()
      (asdf:perform (make-instance 'asdf:compile-op) c)
      (call-next-method)))) 

(defun ltilde ()
  (load "/Users/bobak/lsp/tilde.lisp"))

#-CCL
(defmacro without-package-variance-warnings (&body body)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
            (handler-bind (#+sbcl(sb-int:package-at-variance #'muffle-warning))
                     ,@body)))

#-CCL
 (sb-alien:alien-funcall ;http://xach.livejournal.com/208882.html
       (sb-alien:extern-alien "disable_lossage_handler" (function sb-alien:void)))
 ;-vim:
;(require :sb-introspect)

;(define-alien-routine system int (command c-string))

#-CCL
(defun namestring-for-vim (thing)
  (when thing
    (typecase thing
      (pathname
       (native-namestring (translate-logical-pathname thing) :as-file t))
      (string
       thing)
      (t
       (let* ((source
               (sb-introspect:find-definition-source (fdefinition thing)))
              (pathname
               (sb-introspect:definition-source-pathname source))
              (offset
               (or (sb-introspect:definition-source-character-offset source)
                   0)))
         (unless pathname
           (error "Don't know where the definition of ~S is, sorry." thing))
         (format nil "-c \"goto ~A\" ~A"
                 offset
                 (namestring-for-vim pathname)))))))

;(defun ed-in-vim (thing) (system (format nil "vim~@[ ~A~]" (namestring-for-vim thing))))
;(defun vim (thing) (system (format nil "vim~@[ ~A~]" (namestring-for-vim thing))))

;(push 'ed-in-vim *ed-functions*) 
;;--
;(defun gc-full () (sb-ext:gc :full t))
;;--
;;/Users/bobak/Documents/downloads/lang/lsp/code/project/src/poiu
;(require :sb-posix)
;; Simple heuristic: if we have allocated more than the given ratio
;; of what is allowed between GCs, then trigger the GC.
;; Note: can possibly modify parameters and reset in sb-ext:*after-gc-hooks*
(defparameter *prefork-allocation-reserve-ratio* .50) ; default ratio: 50%

#-CCL
(defun should-i-gc-p ()
  (let ((available-bytes (- (sb-alien:extern-alien "auto_gc_trigger" sb-alien:long)
                            (sb-kernel:dynamic-usage)))
        (allocation-threshhold (sb-ext:bytes-consed-between-gcs)))
    (< available-bytes (* *prefork-allocation-reserve-ratio* allocation-threshhold))))

#-CCL
(defun posix-fork ()
  (when (should-i-gc-p)
    (sb-ext:gc))
  (sb-posix:fork))

;(defun posix-close (x) (sb-posix:close x)) 
;(defun posix-setpgrp () (sb-posix:setpgrp)) 
;(defun posix-wait () (sb-posix:wait)) 
;(defun posix-wexitstatus (x) (sb-posix:wexitstatus x)) 
;(defun posix-pipe () (sb-posix:pipe))
 
#+IGNORE ;even though it works
(defun make-communicating-subprocess (data continuation cleanup)
  (multiple-value-bind (read-fd write-fd) (posix-pipe)
    ;; Try to undo problems caused by sb-ext:run-program. XXX: hack.
    ;; Will still cause a race condition if an ASDF op calls run-program at load-time.
    ;; But this work-around makes it is safe to call run-program before to invoke poiu
    ;; (it is of course safe after). The true fix to allow run-program to be invoked
    ;; at load-time would be to define and hook into an exported interface for process interaction.
    #+sbcl
    (sb-sys:default-interrupt sb-unix:sigchld) ; ignore-interrupt is undefined for SIGCHLD.
    (let* ((pid (posix-fork))
           (proc (make-instance 'communicating-subprocess
                    :pid pid
                    :cleanup cleanup
                    :data data)))
      (cond ((zerop pid)
             ;; don't receive the parent's SIGINTs
             (posix-setpgrp)
             ;; close the read end, set the write end to be the status reporter.
             (posix-close read-fd)
             (setf (status-pipe proc)
                   ;; XXX: something's breaking at padis-access. consistently.
                   ;; WTF.
                   #+sbcl
                   (sb-sys:make-fd-stream write-fd
                                          :output t
                                          :name (format nil "write FD of pid ~A"
                                                        (sb-posix:getpid)))
                   #+clozure
                   (ccl::make-fd-stream write-fd :direction :output))
             (when (find-package :sb-sprof)
               (funcall (intern "STOP-PROFILING" :sb-sprof)))
             (let ((*current-subprocess* proc))
               #+sbcl (sb-ext:disable-debugger)
               #+clozure (setf ccl::*batch-flag* t)
               (unwind-protect (funcall continuation data)
                 (close (status-pipe proc))
                 #+clozure
                 (ccl:quit 0)
                 #+sbcl ;exit in v1.1 was quit
                 (sb-ext:exit :recklessly-p t ; don't cause
                                              ; compilation unit error
                                              ; msg.
                              :unix-status 0))))
            (t
             ;; close the write end, set up the read end
             (posix-close write-fd)
             (setf (status-pipe proc)
                   #+clozure
                   (ccl::make-fd-stream read-fd :direction :input)
                   #+sbcl
                   (sb-sys:make-fd-stream read-fd :input t
                                                  :name (format nil "read FD of pid ~A"
                                                                pid)))
             proc)))))
;http://groups.google.com/group/comp.lang.lisp/browse_thread/thread/408831c3a2057190?hl=en# 
;need installed 1st
; (defun asdf-install::recklessly-install (&rest packages) 
;    (handler-bind 
;        ((error (lambda (condition) 
;                  (declare (ignore condition)) 
;                  (or (continue) 
;                      (and (find-restart 'asdf-install::skip-gpg-check) 
;                           (invoke-restart 'asdf-install::skip-gpg-check)))))) 
;      (apply 'asdf-install:install packages))) 
;  (export 'asdf-install::recklessly-install 'asdf-install)  
;http://tehran.lain.pl/stuff/sbcl-asdf-install
;(defun %install (&rest packages))
;  (handler-bind ((asdf-install::key-not-found
;                  (lambda (r)
;                    (declare (ignore r))
;                    (invoke-restart 'asdf-install::skip-gpg-check))))
;    (with-input-from-string (*standard-input* (format nil "2~%"))
;      (apply #'asdf-install:install packages)))
;  (values))
;(values) 

(defgeneric %ap (thing &optional package))

(defmethod %ap ((thing string) &optional package)
  (let ((*package* (or (and package (find-package package))
           *package*)))
    (apropos-list thing package)))

(defmethod %ap ((thing symbol) &optional package)
  (%ap (symbol-name thing) package))

(defmethod %ap ((thing list) &optional package)
  (cond ((null thing) nil)
  ((null (rest thing)) (%ap (first thing) package))
  (t
   (let ((current (%ap (first thing) package)))
     (dolist (next (rest thing))
       (setf current (intersection current (%ap next package))))
     current)))) 

(defun ap (&rest args)
  (let ((package nil)
  (search-list nil))
    (loop for arg in args do
   (if (and (symbolp arg) (find-package arg))
       (setf package (find-package arg))
       (push arg search-list)))
    (%ap (nreverse search-list) package)))
;; Need to define these to get "Lispdoc" to work
(defun section (&rest rest) (declare (ignore rest))) 
(defun subsection (&rest rest) (declare (ignore rest)))
(defun subsubsection (&rest rest) (declare (ignore rest)))
(defun to-do (&rest rest) (declare (ignore rest)))
(defun todo (&rest rest) (declare (ignore rest)))
 
#-CCL
(defun dldb ()
 (sb-alien:alien-funcall ;http://xach.livejournal.com/208882.html
    (sb-alien:extern-alien "disable_lossage_handler" (function sb-alien:void))))
 
     (defmacro define-constant (name value &optional doc)
       `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
                           ,@(when doc (list doc)))) 
;#+ignore ;https://sites.google.com/site/sabraonthehill/home/exploring-quicklisp-packages
(defun list-current-systems ()
  "Just return a list of all the current systems."
  (let ((system-list ()))
    (flet ((push-pkg-to-system-list 
            (pkg)
            (push (asdf:component-name pkg) system-list)))
      (asdf:map-systems #'push-pkg-to-system-list))
    (sort system-list #'string<))) 

(defun lcs () (list-current-systems))
;=use old prompt until newprompt.cl has something more interesting
(defvar *last-package* nil)
(defvar *cached-prompt* nil)
  
(defun package-prompt (stream)
  (unless (eq *last-package* *package*)
    (setf *cached-prompt*
          (concatenate 'string (or (first (package-nicknames *package*))
                                   (package-name *package*))
                       "> "))
    (setf *last-package* *package*))
  (terpri)
  (princ *cached-prompt* stream))

;(setf sb-int:*repl-prompt-fun* #'package-prompt) 
;(sb-ext:set-sbcl-source-location "/Users/bobak/dwn/lang/lsp/lisps/sbcl/sbcl") 

;https://github.com/Hexstream/hexstream-sbcl-init/blob/master/.sbclrc us&cp..
; wish these had comments /add them@some pnt
(defun us (symbols &optional (package *package*))
  (if (not (listp symbols))
      (setf symbols (list symbols)))
  (unexport symbols package)
  (dolist (symbol symbols)
    (unintern symbol package)))

(defun cp (package)
  (do-symbols (symbol package)
    (if (not (symbol-package symbol))
	(us symbol package)))) 

(defun unexport-all (package)
  (do-external-symbols (symbol package)
    (unexport symbol package)))

(defun unintern-all (package)
  (let ((package (find-package package)))
    (do-symbols (symbol package)
      (if (eq (symbol-package symbol) package)
	  (unintern symbol package))))) 

(defun char-apropos (string &key (start 0) (end char-code-limit))
  (let ((string (string string)))
    (loop for code from start below end
          for name = (char-name (code-char code))
          when (search string name)
          do (format t "~&; ~S = ~C~%" (code-char code) (code-char code))))) 
;-
;/Users/bobak/dwn/lang/lsp/doc/book/HyperSpec/Body/m_do_sym.htm 
(defun pkg-symbs (&optional (pkg cl:*package*)) ;can (describe pkg) as well
  (let ((lst ()))
   (do-symbols (s (find-package pkg)) (push s lst))
   lst))
;or
(defun fs (&optional (pkg :cl-user))
 (let ((p (find-package pkg)))
  (when p
   (loop
     for s being the symbols in p
     when (eq p (symbol-package s))
     do (format t "~a~%" s)))))
