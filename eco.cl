
; exec-and-collect-output now in eco.cl(
; working in osx but not on linux
;http://www.mail-archive.com/stumpwm-devel@nongnu.org/msg01195.html
;(in-package :stumpwm-user)
#+sbcl ;or #-acl
(require :sb-posix)
;;; XXX This is only a workaround for SBCLs with a unreliable
;;; run-program implementation (every version at least until
;;; 1.0.21). If someone makes run-program race-free, this should be
;;; removed! - Julian Stecklina (Oct 23th, 2008)
#+sbcl
(progn
  (defun exec-and-collect-output (name args env)
    "Runs the command NAME with ARGS as parameters and return everything
the command has printed on stdout as string."
    (flet ((to-simple-strings (string-list)
             (mapcar (lambda (x)
                       (coerce x 'simple-string))
                     string-list)))
      (let ((simplified-args (to-simple-strings (cons name args)))
            (simplified-env (to-simple-strings env))
            (progname (sb-impl::native-namestring name))
            (devnull (sb-posix:open "/dev/null" sb-posix:o-rdwr)))
        (multiple-value-bind (pipe-read pipe-write)
            (sb-posix:pipe)
          (unwind-protect
               (let ((child 
                      ;; Any nicer way to do this?
                      (sb-sys:without-gcing 
                        (sb-impl::with-c-strvec (c-argv simplified-args)
                          (sb-impl::with-c-strvec (c-env simplified-env)
                            (sb-impl::spawn  progname c-argv devnull 
                                             pipe-write ; stdout
                                             devnull 1 c-env 
                                             nil ; PTY
                                             1 ; wait? (seems to do nothing)
                                             ))))))
                 (when (= child -1)
                   (error "Starting ~A failed." name))
                 ;; We need to close this end of the pipe to get EOF when the child is done.
                 (sb-posix:close pipe-write)
                 (setq pipe-write nil)
                 (with-output-to-string (out)
                   ;; XXX Could probably be optimized. But shouldn't
                   ;; make a difference for our use case.
                   (loop 
                      with in-stream = (sb-sys:make-fd-stream pipe-read :buffering :none)
                      for char = (read-char in-stream nil nil)
                      while char
                      do (write-char char out))
                   ;; The child is now finished. Call waitpid to avoid
                   ;; creating zombies.
                   (handler-case
                       (sb-posix:waitpid child 0)
                     (sb-posix:syscall-error ()
                       ;; If we get a syscall-error, RUN-PROGRAM's
                       ;; SIGCHLD handler probably retired our child
                       ;; already. So we are fine here to ignore this.
                       nil))))
            ;; Cleanup
            (sb-posix:close pipe-read)
            (when pipe-write
              (sb-posix:close pipe-write))
            (sb-posix:close devnull))))))

 ;(defun stumpwm::run-prog-collect-output (prog &rest args)
 ;  "run a command and read its output."
 ;  #+sbcl (exec-and-collect-output prog args (cons 
 ;	(stumpwm::screen-display-string (current-screen))
 ;    (remove-if (lambda (str)
 ;		 (string= "DISPLAY=" str :end2 (min 8 (length str)))) 
 ;(sb-ext:posix-environ)))))
 (defun run2 (name args)
   "compare w/run-2 etc"
  #+sbcl (exec-and-collect-output name args nil))
  #-sbcl (run-shell-command name args)
  ) 
 
