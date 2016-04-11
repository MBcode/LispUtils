;http://hyperpolyglot.org/lisp  bobak@balisp.org a few compat fncs, to ease transferring code2cl
(defun nil? (n) (null n))
(defun symbol? (n) (symbolp n))
(defun list? (n) (listp n))
(defun number? (n) (numberp n))
(defun integer? (n) (integerp n))
(defun rational? (n) (rationalp n))
(defun float? (n) (floatp n))
(defun string? (n) (stringp n))
(defun meta (n) (quote n)) ;so (get (meta foo) :prop) -> (get 'foo :prop)
(defconstant false nil)
(defun quot (a b) (truncate a b))
(defun Math/pow (a b) (expt a b))
(defun Math/sqrt (a b) (sqrt a b))
(defun Math/exp (a b) (exp a b))
(defun Math/log (a b) (log a b))
(defun Math/sin (a) (sin a))
(defun Math/cos (a) (cos a))
(defun Math/tan (a) (tan a))
(defun Math/asin (a) (asin a))
(defun Math/acos (a) (acos a))
(defun Math/atan (a) (atan a))
(defun Math/atan2 (a) (atan a))
(defun Math/round (a &optional b) (round a b))
(defun Math/floor (a &optional b) (floor a b))
(defun Math/ceil (a &optional b) (ceiling a b))
(defun bit-shift-left (a b) (ash a b))
;(logbitp j (ash n k)) ==  (and (>= j k) (logbitp (- j k) n))
(defun bit-and (a b) (logand a b))
(defun bit-or (a b) (logior a b))
(defun bit-xor (a b) (logxor a b))
(defun bit-not (a b) (lognot a b))
(defun .charAt (s n) (char s n))
(defun .indexOf (a b) (search b a))
(defun .substring (a b n) (subseq a b n))
(defun .length (a) (length a))
(defun count (a) (length a))
(defgeneric .equals (a b)) ;b-> &rest
(defgeneric .compareTo (a b)) ;b-> &rest
(defmethod .equals ((a String) (b String)) (string= a b))
(defmethod .compareTo ((a String) (b String)) (string< a b))
(defun .toLowerCase (a) (string-downcase a))
(defun str (&rest args)
    ;(reduce #'(lambda (a b) (format nil "~a~a" a b)) args)
    (concatenate 'string args))
(defun Integer/parseInt (a) (parse-integer a))
(defun Float/parseFloat (a) (read-from-string a))
(require :cl-ppcre)
(defun .split (a b) (cl-ppcre:split b a)) ;seq 
(defun .re-seq  (a b) (cl-ppcre:all-matches a b)) 
(defun .replaceAll  (a b) (cl-ppcre:regex-replace-all a b)) 
(defun next (l) (rest l))
(defun concat (a b) (append a b))
;filter remove-if-not
;right fold (reverse l)
(defun drop (v l) (nthcdr v l))
;(defun replace (a b v) (setf (aref v a) b))
;multimethod get on hash-table
;multimethod count on hash-table
(defun seq (a) (coerce a 'list))
(defun vec (a) (coerce a 'vector))
(defun seq? (a) (typep a 'sequence))
;will also try a cl.clj for fun as well, then look at some past clj code, now incl:
; https://github.com/runa-dev/riemann & https://github.com/andrew-nguyen/titan-clj
; & probably a little program first in cl then clj
;https://play.google.com/store/apps/details?id=com.sattvik.clojure_repl&hl=en on my phone now
;might start calling out to: https://github.com/halgari/clojure-py ;could still go mq comm route too 
(defun re-seq (a b) (cl-ppcre:all-matches a b))
;nth&sort args switched
;reduce fnc iv l -> (reduce #'fnc l :initial-value iv)
;doseq -> dolist  ;have work on hash too
;write: (defun take (n l) ;loop n &remove )
;reader/macro stuff (for non parens) if I don't want to change that/but probably would
;write range
(defun range (start end)
    (when  (and  (numberp start)  (numberp end))
              (loop for i from start to end collect i)))
;assoc -> gethash
;dissoc -> remhash
;doseq -> maphash
;all lets are let*s
;& -> &optional
;doc -> describe
;apropos ->
;.exec -> run-program
;*posix-argv* -> *command-line-args*
;=Look@ running https://github.com/takeoutweight/clojure-scheme from a scheme w/in lisp like
;  http://mumble.net/~jar/pseudoscheme/ which unfortunately needs some help to load.
;  can build clojure-scheme-0.1.0-SNAPSHOT.jar w/nightcode, but it might not keep up w/jvm ver/?
;Still curious about abcl clj interaction too.
;
;http://lispm.dyndns.org/sbcl (april-1-2014  I wish;)  ;Curious std clj libs java lib dependance
 
;Clojure OSS @oss_clj  5h 
;land-of-lisp-in-clojure - Code from Barski's Land of Lisp book done in Clojure https://github.com/quux00/land-of-lisp-in-clojure …
 
;http://www.meetup.com/ClojureUserGroupHH/events/189615172/ clj clos comparison
 
;http://chriskohlhepp.wordpress.com/metacircular-adventures-in-functional-abstraction-challenging-clojure-in-common-lisp
;(ql '(fset folio  cl-stmx  chanl calispel  cl-actors   optima cl-unification cl-algebraic-data-type))
 
;https://github.com/demonview/disclojure to deal w/some sytax diffs/..
 
;j16_:Rich Hickey will discuss Transit at @LambdaJam, a little something-something we've been cooking up over at Cognitect. http://a.confui.com/-39FAMF8L 
;Yes, we love our programming languages. But what’s the most important language in a polyglot system? Hint: it’s not a programming language. It’s the language used between programming languages. Options are many, and tradeoffs abound. This talk will introduce Transit, a new solution to the problem of sending values between applications written in different languages.
;
;http://www.pixelmonkey.org/2014/11/02/clojonic .. clj-s in py  hy/etc
 
;Look into using MakeALisp https://github.com/kanaka/mal/tree/master/clojure
 
;http://www.meetup.com/The-Bay-Area-Clojure-User-Group/events/220972863/ had some pre conj talks
;I was reminded of some work from http://cliki.net/concurrency eg. lparallel's pmap,preduce...  (&lfarm)
;also cl-future, cl-async, chanl, ..  ;pcall, patron ..                   ; http://www.cliki.net/distributed
;http://quickdocs.org/search?q=concurrency 
;persistance vivace-graph-v3 .. &re rdf stores
;http://www.cliki.net/current%20recommended%20libraries

;datasci: https://twitter.com/ds_ldn/status/588764440146870273 https://github.com/henrygarner/clojure-data-science/blob/master/talk.org 
; http://clojure-datascience.herokuapp.com/ cl-ml clj-vw  ;sw: grafter loom semantic-csv
;https://github.com/liebke/incanter.git
;Drew Conway ‏@drewconway Data science in Clojure, nice talk by @sorenmacbeth https://www.youtube.com/watch?v=omSTAwSjYc8 … 
; @sorenmacbeth hadoop(cacalog);to storm & yieldbot/marceline on trident;to spark clj-spark yieldbot/flambo ; cubert web-repl data explore(ipy like);  use s3, parquet 
 
;Reddit Lisp @reddit_lisp  ;#lisp Cloje: A Clojure dialect on top of Scheme | http://redd.it/3fevo8 ; https://gitlab.com/cloje/cloje

;would rather focus on clj2cl(or a bridge, w/even abcl)but good to keep tabs on: @LispDaily  Porting Common #Lisp code to Clojure - http://bit.ly/1Ibg8by 

;@thelittlelisper How to control your iPhone over telnet with Lisp http://blog.fikesfarm.com/posts/2015-11-11-ambly-and-socket-repl.html … via @mfikes

;pyjure is an implementation (still incomplete) of a pure dialect of Python, written in Clojure. Its main repository is currently on github: https://github.com/qitab/pyjure.
