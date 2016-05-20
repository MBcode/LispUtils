;getting some xls/.. into a format for reasoning/learning over, incl joins/conversions/.. ;mike.bobak@gmail
;csv are confidential
(lu) ;my load-utils, sometimes assumed
(defun val=dot-p (c) (equal (cdr c) ".")) ;no value given, so skip
(defun snumstr (s) (if (> (len s) 9) s (numstr s)))
(defun assoc-lists (keys vals)
 ;(mapcar #'cons keys vals)
  (mapcar #'(lambda (k v) (cons k (num-str v))) keys vals)
  )
(defun assoc-col-names (lol)
  (let ((cnames (mapcar #'underscore (first lol))) ;get rid of spaces in colnames
        (vals (rest lol))) ;ListOfLists
   ;(remove-if #'val=dot-p
        (mapcar #'(lambda (vl) (assoc-lists cnames vl)) vals)
   ;)
    ))
(ql 'cl-csv)
(defun csv2alst (fn) (assoc-col-names (cl-csv:read-csv (make-pathname :name fn))))
;replace 3msg types w/ascii version of all3files in ma.csv ;as type is a col only
;(defvar *ma* (assoc-col-names (cl-csv:read-csv (make-pathname :name "ma.csv"))))
;(defvar *pd* (assoc-col-names (cl-csv:read-csv (make-pathname :name "pd.csv"))))
 ;(defvar *ma* (csv2alst "ma.csv")) ;found multiline text slot that messed up csv
;consider libs to read xls &/or open equivalents
(defvar *ma-cc* (csv2alst "m-cc.csv"))
(defvar *ma-cp* (csv2alst "m-cp.csv"))
(defvar *ma-pp* (csv2alst "m-pp.csv"))
(defvar *pd* (csv2alst "pd.csv"))
(defvar *leg* (cl-csv:read-csv (make-pathname :name "pleg.csv"))) ;might use
;when I used parts of a csv lib, I used to turn colname spaces to underscores;do again

(ql 'xml-emitter)
(defun st (pr) (xml-emitter:simple-tag (car pr) (cdr pr)))
(defun xo (al ;&optional (strm *standard-output*)
              );replace xml version line w/people or msg tags
  (xml-emitter:with-xml-output (*standard-output*) ;strm
                   (mapcar #'st al)))
(defun lxo (lal &optional (fn "out.xml"))
  (with-open-file (strm fn :direction :output :if-exists :supersede) 
    (let ((*standard-output* strm))
      (mapcar #'xo lal))))
(defun tox ()
  "test output of xml"
  (lxo *pd* "out-p.xml") ;works &only small chage to import2protege frames
 ;(lxo *ma* "out-m.xml") ;iconv -c -f utf8 -t ASCII  keeps away hand edits like utf8 version
  (lxo *ma-cc* "out-m-cc.xml") 
  (lxo *ma-cp* "out-m-cp.xml") 
  (lxo *ma-pp* "out-m-pp.xml") 
  ) ;could deal w/multibytechars or ignore
;;by using: alias iconv8 'iconv -c -f UTF-8 -t ISO-8859-1 '
;still missed some emojii/etc. Will have to automate/use multibyte-chars
;protege xml-tab can load these w/only a little clean up
;-Did each ppl/msg dump to seperate xml file; might want to split out msgs, otherwise
;  might programatically take convestaion-type slot to split them into subclasses.
;Had msg xls sheets seperate at start, might want to wait for sql or ..;mv on2txt/# analysis

;Started to look@Jambalaya-tab, and realize it would be nice to have inter instance links vs integer-ids
; so can viz not just isa but hasa
;If I could just get the table schema,&load my test data somplace, I could play w/programatically doing it all;still can.

(defun but-ext (s) (first-str2by-end s #\.))
;USER(1): (len (remove-duplicates (mapcar #'(lambda (x) (but-ext (car x))) (first *pd*)) :test #'equal))
; 62 ;Will also need slot defn of these to have the rest be subslots of
(defvar *clp0* "~%(single-slot ~a~%  (type INTEGER)~%;+    (cardinality 0 1)~% (create-accessor read-write))")
;(mapcar #'(lambda (s0) (format nil *clp0* s0)) (remove-duplicates (mapcar #'(lambda (x) (but-ext (car x))) (first *pd*)) :test #'equal))
;now for clp pont &maybe km, get others slots to be subslots of related
(defun str-ext (s) (last-str2by-end s #\.))
(defvar *clp* "~%(single-slot ~a~%  (type INTEGER)~%;+    (cardinality 0 1)~%;+    (subslot-of ~a)~%   (create-accessor read-write))")
;could alter 1col w/_ instead of ., or deal w/below
(defun print-s-re (s &optional (frmt *clp*) ;(frmt "~%~a -> ~a") 
                                 (strm nil)) 
  (let ((base (but-ext s)))
    (when (and base 
               (len< base s)) ;(format strm frmt base s)
                               (format strm frmt s base)
      )))
(defun slot-hier (lalst) ;list of alsits
  (remove-duplicates
    (mapcar #'(lambda (x) (print-s-re (car x))) (first lalst))
    :test #'equal))
;(slot-hier *pd*) ;not needed for *ma*

;can also try: ;works but in arrays/use if want re ops
;(ql 'cl-simple-table)
;(defvar *st* (simple-table:read-csv "pd.csv"))

;w/assoc-col-names alists json would be very easy, but old protege frames xmltab is 1st target
;so find simple xml writer that doesn't need fancy format 1st; just an alist
 ;could also just dump alists in pins format, after making a class for a table
;if get use of mysql tables, can use protege Datamaster&skip this(for this part)
;could aslo use one of my db bridges in lisp

;the lisp ML code that can read protege(frames)files,also allows viewing data&running the ML algos on it
 ;algernon-tab ask/tell (via wire) could speed things up ;esp if other lisp code can't do it easily

;lots of cols that are related; might want slot hierarchy (a KM/OWL thing),but also in frames(view)
; nowadays I might skip protege if I could just get a good KM gui; (power)LOOM also possible
 ;might be an excuse to use DL in old or newer version of protege &the new tabs, incl spreadsheet import

;some slots end in #w for which week meansured, instead of further branch in slot heirarchy, could array/multislot but
; the week measures are not all the same so would need an alist or a multislot of measure instances, that have the date-time

;utils.clp has a km-tax that might dump for KM; but also want reorg for ML algorithms, ;might look at data-table/cl-ana etc too
 ;subslots that partition by time might be workable, would be nice to have multislot view at higher level, but regular/fitted ;sparklines

;Other than viz off of R-stat/studio &re-viz/explore(ipy/juypyter/etc),I might use xlispstat's vista, as imput is similar to arff files*
 ;*which that ML code has IO for,  (as well as many envs incl weka/...)

;having a sharable data-table would be nice, maybe even pass via(feather when more in use),though arff,etc ok now too

;end goal somewhat similar to:
;"Natural Language Processing for Mental Health: Large Scale Discourse Analysis of Counseling Conversations": arxiv.org/abs/1605.04462
;How do you make someone feel better? NLP to promote #mentalhealth. See TACL paper at http://stanford.io/1XrwOjL . With @jure & @stanfordnlp
