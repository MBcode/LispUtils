;getting some xls/.. into a format for reasoning/learning over, incl joins/conversions/.. ;mike.bobak@gmail
;csv are confidential
(lu) ;assumed
(defun val=dot-p (c) (equal (cdr c) "."))
(defun assoc-lists (keys vals)
  (mapcar #'cons keys vals))
(defun assoc-col-names (lol)
  (let ((cnames (first lol))
        (vals (rest lol))) ;ListOfLists
   ;(remove-if #'val=dot-p
        (mapcar #'(lambda (vl) (assoc-lists cnames vl)) vals)
   ;)
    ))
(ql 'cl-csv)
;replace 3msg types w/ascii version of all3files in ma.csv ;as type is a col only
(defvar *ma* (assoc-col-names (cl-csv:read-csv (make-pathname :name "ma.csv"))))
(defvar *pd* (assoc-col-names (cl-csv:read-csv (make-pathname :name "pd.csv"))))
(defvar *leg* (cl-csv:read-csv (make-pathname :name "pleg.csv")))

(ql 'xml-emitter)
(defun st (pr) (xml-emitter:simple-tag (car pr) (cdr pr)))
(defun xo (al ;&optional (strm *standard-output*)
              );replace xml version line w/people or msg tags
  (xml-emitter:with-xml-output (*standard-output*) ;strm
                   (mapcar #'st al)))
(defun lxo (lal)
  (with-open-file (strm "out.xml" :direction :output :if-exists :append) 
    (let ((*standard-output* strm))
      (mapcar #'xo lal))))
(defun tox ()
  "test output of xml"
 ;(lxo *pd*) ;works &only small chage to import2protege frames
  (lxo *ma*) ;iconv -c -f utf8 -t ASCII  keeps away hand edits like utf8 version
  ) ;could deal w/multibytechars or ignore
;;by using: alias iconv8 'iconv -c -f UTF-8 -t ISO-8859-1 '
;still missed some emojii/etc. Will have to automate/use multibyte-chars

(defun but-ext (s) (first-str2by-end s #\.))
;USER(1): (len (remove-duplicates (mapcar #'(lambda (x) (but-ext (car x))) (first *pd*)) :test #'equal))
; 62 
;now for clp pont &maybe km, get others slots to be subslots of related
(defun str-ext (s) (last-str2by-end s #\.))
(defvar *clp* "~%(single-slot ~a~%  (type INTEGER)~%;+    (cardinality 0 1)~%;+    (subslot-of ~a)~%   (create-accessor read-write))")
;could alter 1col w/_ instead of ., or deal w/below
(defun print-s-re (s &optional (frmt *clp*) ;(frmt "~%~a -> ~a") 
                                 (strm nil)) 
  (let ((base (but-ext s)))
    (when (len< base s) (format strm frmt base s))))
(defun slot-hier (lalst) ;list of alsits
  (remove-duplicates
    (mapcar #'(lambda (x) (print-s-re (car x))) (first lalst))
    :test #'equal))


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
