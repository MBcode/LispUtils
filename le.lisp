
;https://gist.github.com/bdsatish/5403002
;;; Check for --no-linedit command-line option.
(if (member "--no-linedit" sb-ext:*posix-argv* :test 'equal)
    (setf sb-ext:*posix-argv* 
	  (remove "--no-linedit" sb-ext:*posix-argv* :test 'equal))
    (when (interactive-stream-p *terminal-io*)
      (require :sb-aclrepl)
      (require :linedit)
      (funcall (intern "INSTALL-REPL" :linedit) :wrap-current t)))
;
;; Don't break into debugger, but just display error message and return to top-level loop (REPL)
       (setf *debugger-hook* #'(lambda (c h) (declare (ignore h)) (print c) (abort))) 
;http://www.cliki.net/linedit
;Another way is instead to say in your .sbclrc
#+IGNORE
(when (and (interactive-stream-p *terminal-io*)
           (null (find-package 'swank)))
  (require :linedit)
  (funcall (intern "INSTALL-REPL" :linedit) :wrap-current t)) 
;http://stackoverflow.com/questions/5379110/enabling-autocomplete-in-sbcl 
;so w/o --no-linedit get autocomplete but loose history between runs in same shell
;maybe http://weitz.de/completions.html &/or http://www.cliki.net/rlwrap /?
;linedit to emacs-y, if I could get tab-complete&hist-search w/rlwrap I'd use it.
;(defvar *histlen* nil) ;mv up, &use linedit::*history* len during lo /later
;  so can do an append to .sbcl_history of the last (- newlen initlen) lines
;try for evil like edit binding, but completions alone are very nice, so:
;https://github.com/nikodemus/linedit/issues/2 
(defun load-sbcl-history ()
  (with-open-file (f "~/.sbcl_history" :direction :input :if-does-not-exist nil)
    ;when f
    (when (and f (find-package 'linedit)) ;look@more in case of: --no-linedit
      (let ((history
              (or linedit::*history*
                  (setf linedit::*history* (make-instance 'linedit::buffer)))))
        (loop for line = (read-line f nil)
              while line
              do (linedit::buffer-push line history))
        ;(when (listp history) (setf *histlen* (length history)) ;need linedit len
        ; (format t "~%hl=~a~%" *histlen*))
        )))) 
;then can just load it, ;though would like to maybe just dump out new cmnds@end
(load-sbcl-history)
; so could cp-list or just keep length& dump from that pnt on after a lo
;Of course I should move on to: https://common-lisp.net/project/slime/
;(ql 'quicklisp-slime-helper) ;https://gist.github.com/bdsatish/5403002 
; slime/swank &even nrepl(for other interop too)/?
;also look@ http://quickdocs.org/prepl/ & http://quickdocs.org/repl-utilities/
;&see how sb-aclrepl is used by others, as I was already loading that/hist/etc
;&dbg w/http://quickdocs.org/conium/ &more
