;=netcat send/recv   ;simple enough that abcl can do this
;inspired by seeing: @climagic  nc -q1 -lvp 1234 < file.txt # poor man's file serve. 
; Use nc serverhost 1234 > output.txt to retrieve file from remote host. NAT bugs this.
;had nc.cl&nc2.cl seperate for sbcl&abcl till it worked for both, then:
;> cp nc2.cl nc1.cl   ;so can work for either abcl or sbcl, in 1 file:
;nlp.lisp has: ;   (rsc (format nil "echo \"~a\"|cat> text" s))

;from (lu), assume loaded for sbcl (of late)
#+abcl
(defun rsc (txt) (asdf:run-shell-command txt)) ;not using for recv, could skip for send/or not
; need more from utils ;really should get/re-find largest set off utils that abcl will load
;-
#+abcl
(defun run-ext2 (s) ;abcl has this
  "input1str&it breaks4you"
  (ext:run-shell-command s))

;#+sbcl
;(defun run-ext2- (s) ;since abcl won't use this, most of the utils just above can go away (in next commit)or not
;  "input1str&it breaks4you"
;  (apply #'run-ext (tokens s)))
;-

(defun nc-s (s n)
  "netcat send"
    (rsc 
      (format nil "echo \"~a\"|nc -q1 -lvp ~a" s n)))

(defun nc-r (n)
  "netcat recv"
    (run-ext2 ;rsc 
      (format nil "nc localhost ~a" n)))

(trace nc-s nc-r)

(defun tn (&optional (n 2134))
  ;ran w/rsc, but w/tsh ncs won't exit until ncr
  ;so threads or diff lsp instance
  (nc-s "hi" n)
  (nc-r n))

;calling t1, then t2 from diff lisp instance works
(defun tn1 (&optional (n 2135))
  (nc-s 
#+abcl "hi from abcl" 
#+sbcl "hi from sbcl" 
        n))
(defun tn2 (&optional (n 2135))
  (nc-r n))

;next start wrapping interesting(slow loading)java libs w/abcl(&can call/minip 
; more than given api),as server even if not intented that way
 
