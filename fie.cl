;file InfoExtraction bobak@balisp.org  ;put in LispUtils at some point/generalize 1st
;e.g. have an incoming csv and say which column has the fn-path,&a list of IE fncs to use
; all of which will depend on a list-lines version of the file being available &ret 1csv-able item
(require :trivial-shell)
(lu)
(defun ts  (str)
 (trivial-shell:shell-command  (to-str str)))
;---
(defun save-lines- (l filename)
 (when (fulll l)
  (with-open-file (stream filename :direction :output
                                :if-exists :overwrite :if-does-not-exist :create)
    (mapcar #'(lambda (x) (write-line x stream)) l))))  
;---
;(defun fie (csvfn fncl) ) ;eg (fie "p.csv" '(h1+ ...)) ;but h1+ working on list-lines
;(defun fie (csvfn fcn fncl) ) ;eg (fie "p.csv" '(h1+ ...)) ;but h1+ working on list-lines
;  need fcn ==file columen number,  but need it above, as fie should be for just one file
;h1+ is really for title, though since other inputs could be used, name via fnctionality
; which is first-full-line fn or fl  ;fn then h1+   w/list-o-lines, write that now:
; just  ~like below:  (first-lv (collect-if #'(lambda (l) (len-gt ln)) (head- lol)))
;(defun first-full-head   (first-lv (collect-if #'(lambda (l) (len-gt ln)) (head- lol)))
;first-full-line w/a depth restriction would be the same,  could skip collect&do more of find/search
;so like defun find-len-gt but no item
(defun first-len-gt (loln len)  ;not exactly but start w/  ;should look4list end vs -lv but..
  "look down list of lengthable things, &ret 1st gt len"
   (if (len-gt (first-lv loln) len) (first-lv loln)
       (first-len-gt (rest loln) len)))
;---as I might want all the first full lines, for paper/file-IE, also try a collect&continue to use
;defun all-len-gt (loln &optional (len 5) (ln 10)) 
(defun head-len-gt (loln &optional (len 5) (ln 10)) 
  "look down list of lengthable things, ln long, &ret all gt len"
   (collect-if #'(lambda (l) (len-gt l len)) (head loln ln)))
;eg. (head-len-gt '("0123" "0123456" "123" "1234567"))
;("0123456" "1234567")
;---
;defun find-w (lot)) ;like map-search
;defun map-search (lot)) 
(defun collect-search (fs lot)  
  "list of txt lines, ret lines w/ .."
  (collect-if #'(lambda (s) (when (search fs s) s)) lot))
;eg. (collect-search "012" '("0123" "0123456" "123" "1234567"))
;("0123" "0123456")
;-might want an egrep | version, so send in list &collect if any match ;should stop after 1st find
;so 1st make a multi/rec search
(defun searchl (lfs s)
 "list-find-strs over a str"
  (when (full lfs) 
    (if (search (first lfs) s) s
      (searchl (rest lfs) s))))

(defun collect-searchs (lfs lot)  
  "list of txt lines, ret lines w/ .."
  (collect-if #'(lambda (s) (searchl lfs s)) lot))
;eg. (collect-searchs '("0" "7") '("0123" "0123456" "123" "1234567"))
;("0123" "0123456" "1234567")
;---
;(defun fie  (csvfn fncs)
; "InfoExtraction for all files"
; (let ((lol (list-lines csvfn)))
;   (mapcar #'(lambda (fn) (funcall (function fn) lol)) fncs) ;fix, might not be simple2start, eg all-len-gt then reuse
; ))
;focus on doing one txt paper file, then generalize
;;go from f+ft to first-len-gt if just want something like title, but then all-len-gt and get more than title
(defun tp1  (fn)
  "process one txt paper to get title/etc"
  (let* ((lot (list-lines fn)) ;list of txt 
         (tfl (head-len-gt lot)) ;top full list o txt
         (titl (first-lv tfl))
         (eml (collect-search '("@" ".edu" ".com") tfl)))
    (list titl eml)))
    
;(trace head-len-gt)
;---
;(load "p.l" :print t) ;p.csv in *p*
(ql 'cl-csv)
(defvar *p* 
;(head ;in testing only look@a few files 
  (with-open-file (s "p.csv" :direction :input) (cl-csv:read-csv s)))
;)
;(load "l6.l" :print t) ;*files* == (mapcar #'third *p*)
(defvar *files*  (mapcar #'third *p*))
(defvar *tp1* (mapcar #'tp1 *files*))
;---
(defun h1  (fn)  (ts  (str-cat "head -1 " fn)))
(defun head-  (fn)  (ts  (str-cat "head  " fn)))
;(defun break-by-newline  (s) (explode- s #\\Newline)) ;no use break2lines
(defun hf+  (fn &optional (ln 5))  
  "1st line gt .."
  (let ((tl (break2lines (head- fn))))
    (first-lv (collect-if #'(lambda  (tl)  (len-lt tl ln)) tl))
  ))
(trace hf+)
(defun h1+  (fn &optional (ln 5))  
  "1st line of file over ln in length" ;hoping it is pdf's text's title
 (str-trim  (rm-commas 
  (let ((l1 (h1 fn)))
    (if (len-gt l1 ln) l1 ;get luck&it's the 1st
      (hf+ fn ln))))))
;there is a head on list, could defgeneric between lists & str ;do this soon
(defun hml (l) (mapcar #'head- l))
(defun hml1 (l) (mapcar #'h1 l))
(defun hml1+ (l) (mapcar #'h1+ l))
;test over *files*
(defun gres (f fn)
  (let ((s (read-file-to-string fn)))
     (search f s)
  ))
(defun gred  (f fn)  (ts  (str-cat "grep --color  " f " " fn)))
;now that I'm using *p* might want to map a map, but could compose as well
; one way being having an accessor fnc, or an actual more lambda compose (fnc)
;can start out quick&dirty though
(defun hml1+3 (l) (mapcar #'h1+ (third l)))
(defun f+ft (tr) (append tr (list (h1+ (third tr))))) ;test (adding title) over *p*
;so far using h1 only sometimes going further but will want to get a lot out &prob worth list-lines
;&have several IE fncs goin over it

;could use csv read&write fncs, incl map-csv, if just append save map-line txt&just append csv out
 
;======now can test it here
(defvar *ft* (mapcar #'f+ft *p*))
(mapcar #'(lambda  (tr)  (print  (last tr))) *ft*)
;might get rid of string-trim &just get upto the 1st Newline
;save-lines ;after lists turned to txt line, want to insert commas when imploding
;(with-open-file (strm "out.csv" :direction :output :if-exists :overwrite :if-does-not-exist :create)
;   ;(write-csv strm *ft*)
;    (mapcar #'(lambda (l) (write-line strm (apply #'str-cat l))) *ft*))
(trace implode-l)
;(save-lines- (mapcar #'(lambda (l)  (substitute #\, #\Space  (implode-l l))) *ft*) "out.csv")
(save-lines- (mapcar #'(lambda (l)  (format nil "~{~a, ~}" l)) *ft*) "out.csv")
;use: https://github.com/mark-watson/lisp_practical_semantic_web/blob/master/knowledgebooks_nlp/summarize.lisp 
;but don't see use of interestingness eg.tf-idf http://cs.unm.edu/~eschulte/data/tfidf.lisp.html
;instead of mac-service look@ http://libots.sourceforge.net/ 
