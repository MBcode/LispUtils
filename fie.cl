;file InfoExtraction bobak@balisp.org  ;put in LispUtils at some point/generalize 1st
;e.g. have an incoming csv and say which column has the fn-path,&a list of IE fncs to use
; all of which will depend on a list-lines version of the file being available &ret 1csv-able item
(require :trivial-shell)
(lu)
(defun ts  (str)
 (trivial-shell:shell-command  (to-str str)))
;---
;read-csv to *p* &depending on column of filenames to InfoExtract might fill up *files* for dbg
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
(defun all-len-gt (loln &optional (len 5) (ln 10))
  "look down list of lengthable things, ln long, &ret all gt len"
   (collect-if #'(lambda (l) (len-gt l len)) (head loln ln))) 
 
;---
(defun fie  (csvfn fncs)
 "InfoExtraction for one file"
 (let ((lol (list-lines csvfn)))
   (mapcar #'(lambda (fn) (funcall (function fn) lol)) fncs) ;fix
 ))
;---
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
 (str-trim 
  (let ((l1 (h1 fn)))
    (if (len-gt l1 ln) l1 ;get luck&it's the 1st
      (hf+ fn ln)))))
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
;can start out quick&dirty though  ;1st test case w/file column#=3
(defun hml1+3 (l) (mapcar #'h1+ (third l)))
(defun f+ft (tr) (append tr (list (h1+ (third tr))))) ;test (adding title) over *p*
;so far using h1 only sometimes going further but will want to get a lot out &prob worth list-lines
;&have several IE fncs goin over it

;go from f+ft to first-len-gt if just want something like title, but then all-len-gt and get more than title

;could use csv read&write fncs, incl map-csv, if just append save map-line txt&just append csv out
 
;==testing
;(load "fie.cl" :print t)
;now can test it here
(defvar *ft* (mapcar #'f+ft *p*))
(mapcar #'(lambda  (tr)  (print  (last tr))) *ft*)
;might get rid of string-trim &just get upto the 1st Newline
;save-lines ;after lists turned to txt line, want to insert commas when imploding
;(with-open-file (strm "out.csv" :direction :output :if-exists :overwrite :if-does-not-exist :create)
;   ;(write-csv strm *ft*)
;    (mapcar #'(lambda (l) (write-line strm (apply #'str-cat l))) *ft*))
(save-lines (mapcar #'(lambda (l)  (substitute #\, #\Space  (implode-l l))) *ft*) "out.csv") 
