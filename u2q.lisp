(ql 'km) ;mike.bobak@gmail   
;(in-package :km)
(use-package :km)
(defvar *dbg* t)
(defun quote_str (s) (rm-colon (quote-str s)))
;;make so ins can also be [] vs just * (by convention) so easier to interoperate w/clips/pprj:pins
(defvar *clp* nil) ;t ;use the clips ins version [] ;as soon as make sure the old behavior is the same
(defun insCon (n &optional (pre "") (clp *clp*))
  (if clp (str-cat "[" pre n "]")
    (str-cat "*" pre n)))
 ;work w/*nostar*  &w/sym worry about intern/?
;Might not be a bad idea to have something to switch between representations
(defun rm-brakets (s) (remove #\[ (remove #\] s)))
(defun star2clp (n) (ki (rm-star n)))
(defun clp2star (n) (ki (rm-brakets n)))
(defun insCon2 (n &optional (clp *clp*))
  "convert (even already loaded?) ins conventions"
  (let ((*clp* clp)) ;so all other calls below will use tmp changed global
  (if clp 
    (if (prefixp "*" n) (star2clp n)
      n)
    (if (prefixp "*" n) n
      (clp2star n)))))

;some missing things: ;oh just txt accessor, but want more general;or not/see use
;nlm/mmkm.lisp:(defun ktxt (i) (let ((txt (kmtxt i))) (if txt txt (to-str i))))
;nlm/mmkm.lisp:(defun ktxt+ (i) (gv+ i "txt")) ;2diff from txt method
;nlm/mmkm.lisp:(defun kmtxt (i) (gv i "txt")) ;2diff from txt method
(defun ktxt (i) (let ((txt (kmtxt i))) (if txt txt (to-str i))))
(defun ktxt+ (i) (gv+ i "txt")) ;2diff from txt method
(defun kmtxt (i) (gv i "txt")) ;2diff from txt method
;
;bobak parts of utkm
;todo: give full path from a class all the way to the top
;
;only completely generic thing taken out of mmkm.lisp
;it should build on more generic work, some of which will be in ../c/tools ..
(defun eval-str2km (s)  ;now ka below as well, starting to prefer ka-, even through sv/gv
  (when (stringp s)
      (eval-str (str-cat "(km::km '#$" ;"(km '#$" 
                         (rm_comma s) ")"))))

;consider a eval2km that could also take a list,  which could be tricky
;well at least get a :set formatting4assert:
;defun ids2km-set (idl &optional (add-star t))
; "take a set of str/sym and turn into km :set of ins IDs"
 
;defun get-cui-slot (c sn)
(defun get-ins-slot (i sn)  ;(gv i sn)
     (km (append '(|the|) (list sn) '(|of|) (list i))))

(defun  is-a-p (ins cls)
  (first-lv ;was full , should ki/symbol
   ;(km (list ins '|&+| (list '|a| cls)))
    (km (list (kin ins) '|&+| (list '|a| cls))) ;dec
    ))
;consider keeping nlp bits, but take out for now; &make unkm if re-introduced
(defgeneric type_of (a))
(defmethod type_of (a)  (gv a "instance-of"))
(defmethod type_of ((l List))
    (mapcar #'type_of l))
(defun type_of+ (i)
    "assure list+"
      (list+ (type_of i))) 
(defun type_of1 (i) (first-lv (type_of i)))
(defgeneric txt_type (i))
(defmethod txt_type (i) (when i (str-cat (ktxt i) "_" (type_of1 i))))
(defmethod txt_type ((l List)) (mapcar #'txt_type l))
;type_of could also be gv ins "classes"
(defun atom-p2 (i) (is-a-p (kin i) '|atomPhrase|))

(defun is_a-p (i cls)
  (member cls (type_of+ i)))
;-del-more nlp
;consider alst2km ((:pair k1 v1) (:pair k2 v2))
;already: defun affix-pair-prefix(input) in controller-miscellaneous.lisp
;
;generic/macro: the-of sn ins (km0 `(|the| sn |of| (|a| ,concept))
;  (car (km0 `(|the| |text-gen| |of| ,inst))))
;;;Issues a KM slot lookup
;;;(defun ps-slot-lookup(query-frame query-slot)
;;;  (ps-km-query `(|the| ,QUERY-SLOT |of| ,QUERY-FRAME)))
;;;  from "controller-km-bridge.lisp" 58L, 1913C                         30,3-10       All
;;;--
;Might be interesting to turn any alist of (s v)'s into an ins, of either &/or clos/km
;look@ *rins* construction right now, might go off that?
;
;-might pull in bits about class/ins naming ;want some generic things to assure even ins names
;-del-more tax/nlp
;-
(defun km-name2 (i)
    (string-upcase (km-name i)))
;-
;generalize the pre-append if not prefixp ;KM ID
(defgeneric ki (s &optional pre)) ;"make sure km ins always starts w/*"  ;keep original type
;I could make an s-cat to call str|symbol&havein 1fnc, but would have to decide about mixed inputs
(defvar *nostar* nil)
(defmethod ki ((s String) &optional (pre ""))
  (if (or *nostar* (prefixp "*" s)) s
    (insCon pre s)))
;assume keep the same, but..
(defmethod ki ((s Symbol) &optional (pre ""))
 ;(intern (ki (symbol-name s) pre))
  (intern (ki (underscore (safe-trim (symbol-name s))) pre) :km)
 ;;if (prefixp "*" s) s
 ;(if (or *nostar* (prefixp "*" (symbol-name s))) s
 ;  (sym-cat "*" pre s))
 )
(defmethod ki ((s List) &optional (pre ""))
  (mapcar #'ki s))
;
(defmethod ki ((n Number) &optional (pre ""))
  (ki (to-str n) pre))
;
(defmethod ki (n  &optional (pre "")) (ki (to-str n) pre)) ;new
;
(defun ki_ (s) ;str  ;from/to #s left along, but for ins-names, still want it
  "clean up 2 KM id str_" ;like txt2kmId
  ;(ki (under_ (trim-punct2 s)))
  (let ((str  (rm-space (underscore ;under_ 
		  (trim-punct2 (clean4echo (to-str s)))))))
    (if (digit_prefixp str) ;(numberp (numstr str)) 
      str
      (ki str))) ;so #'s not *ed
  ) ;if numberp numstr, then skip ki,  ;in :arg might mix ins &#s
(defun k_i (s) ;str  ;from/to #s left along, but for ins-names, still want it
  "km id"
  (ki (under_ (trim-punct2 (to-str s)))))
;
(defun kin (s)
  "km id  interned"
  (let ((i (ki s)))  ;2bad can't make where cls&relation names not *ed
    (if (symbolp i) i 
      (intern i :km))))
;
(defun ins-of-p (i cls)
  "is ins member of the class"
    (member (kin i) (ins-of cls)))
;
(defun show- (s)
  "w/o*"
  (km::showme (intern s :km)))
;
(defun show2 (s)
  "show ins then cls"
  (show s)
  (show- s))
;
(defun words-of (id) (gv id "words-of"))  ;probably get rid of
;
(defun show-all (s)
  (mapcar #'show (explode- s)))
(defun show_all (s)
  (mapcar #'show (explode- (clean-se s)))) ;clean more agressively/finish
(defun show-c (s)
  "print out our word ins like the columbia format"
  (let* ((wo (words-of s))
	 (wc (when wo (first-lv (type_of wo))))  ;pos1 also get _pos part at least, via txt_type
	 (wa (when wc (gv- wc "abbrev"))))
    (format nil "~&~a~a/~a" s wa (type_of s))))
(defun show-c1 (s)
  "print out our word ins like the columbia format" ;little less like it but better print
  (let* ((wo (words-of s))
	 (tt (txt_type s))
	 (wc (when wo (first-lv (flatten- (type_of wo)))))  ;pos1 get _pos part , via txt_type
	 (wa (when wc (gv- wc "abbrev"))))
    (format nil "~&~a~a" tt wa))) ;not like columbia any longer
(defun gv-abbrev (wc) (gv- wc "abbrev"))
(defun show-cl (s) ;maybe rename
  "get parts for cmp w/columbia version of a word/phrase" ;which will be done in colu.lisp
  (let* ((wo (words-of s))
	 (tt (txt_type s))
	 (wca (when wo (flatten- (type_of wo))))  ;pos1 get _pos part , via txt_type
	 (wc (first-lv wca))
	 (wa (when wc (gv- wc "abbrev"))) ;should mapcar to get them all -finish
	 (waa (mapcar-  #'gv-abbrev wca))
	 ) 
    (list wo tt wca waa)))
;(defun cmp-cw (s) "cmp my&columbia parse for a word") ;diff version in colu.lisp 2cmp w/cea-parts lst
(defun clean_se (s)
   (simple-replace-string "(. " "( "
    (rm-strs '("(. .)" "(, ,)" "(: -)" "(: ;)"  "," ":" "(" ")" ";" "\\" "\"" "'"
               "\/") s)))
(defun show_c (s)
  (mapcar #'show-c1 (explode- (clean_se s)))) ;
(defgeneric show (s))
;defun show (s)
(defmethod show (s)
  "km showme for sym|str even w/o *"
  (km::showme (kin s)))
(defmethod show ((l List))
  (mapcar #'show l)) ;was show already a fnc?check/looks ok
;
(defun txt2kmId (txt &optional (idfnc #'under_f)) ;txt2ki, ki_
  "txt->km *id"
  (ki (funcall idfnc txt)))
;
(defun strl (s)
  "str about 2eval to (km slt) lst should have () around it"
  (format nil "(~a)" s))
(defun quote-strl (s)
  "str about 2eval to lst should have '() around it"
  ;(format nil "'(~a)" s)
  (str-cat "'" (strl s)))
;
(defgeneric ka (sl)) ;assert string or list
;KM assert
;(defmethod ka ((s String))  ;like eval-str2km above
;  "KM assert string"
;  (let ((sl (if (prefixp "(" s) s
;	      (strl s))))
;    (eval-str (str-cat "(km-unique '#$" (rm_comma sl) ")"))))
;could of sent in optional for getMulti, but just get back &if just one ret first-lv of it
(defun ka1 (sl) "for a single value" ;needs parens
  (eval-str (str-cat "(km-unique '#$" sl ")")))
(defun full1 (l) (and (fulll l) (eq (len l) 1)))
(defmethod ka ((s String))  ;like eval-str2km above
  "KM assert string"
 (when (full s) ;so no ""
  (let* ((sl (if (prefixp "(" s) s
	      (strl s)))
	 (ret (eval-str (str-cat "(km::km '#$" ;"(km '#$" 
                             (rm_comma (rm-bslash sl) ;(safe-trim sl) ;sl
                                         ) ")"))))
    (if (full1 ret) (first-lv ret) 
      ret)))) ;if 1thing get it, else get list, ka+ 
(defmethod ka ((s List))
  "assert list2 km"
 (when (fulll s)
  (ka (implode-l s))))
;
(defun ka- (&rest args) ;fix
  "km assert w/o a list, just args"
  (when *dbg* (format t "~&ka:~a,~a"  args (type-of args)))
 (when (fulll args)
  (funcall #'ka args)))
  ;had apply, but ka is a fnc that works on 1list, not takes n args
;
;2.4.0 - The semantics of KM expressions:
;   (the instances of ...)
;   (the all-instances of ...)
;   and the equivalent Lisp functions:
;        (all-instances <class>)
;	 (immediate-instances <class>)
(defun ins-of (cls) ;maybe rename as could be short for instance-of; or might write as 'class'
  (ka- "the instances of " cls))
(defun inss-of (cls) (list+ (ins-of cls))) ;new
(defun ins_of (cls) (all-instances cls)) ;2.4.0
(defun ins_of_only (cls) (immediate-instances cls)) ;2.4.0
(defun an-ins-of (cls) (first-lv (ins-of cls)))
(defun tmp-ins-of (cls) ;try2avoid
      (ka- "an instance of " cls))
(trace tmp-ins-of)
(defun an_ins-of (cls)
  (let ((try (an-ins-of cls)))
    (if try try
      ;(ka- "an isntance of " cls)
      (tmp-ins-of cls))))
(defun gvs (cls sn) ;gv-cls
  "get-value cls slot"
  (let ((c (if (symbolp cls) (symbol-name cls)
	     (to-str cls))))
   ;(gv (an_ins-of cls) sn)) ;found err n25
    (gv (an_ins-of c) sn)))
;
(defun isa-p (i cls)
  (ka- i '|&+| (list '|a| cls)))
;
(defun subcls-p (a b)
  (find a (mapcar_ #'symbol-name (gv- b "subclasses")) :test #'equal))
;
(defun cmp-p- (i) ;check on
  (isa-p i "numericComparator"))
;
(defun gv- (i sn)  ;GetValue could be a cls/relation, so don't add *
  "get of cls/relation"
  (ka- "the" sn "of" i))
(defun gv-+ (i sn) (ka- "the+" sn "of" i)) ;new: make it if not there
(defun gv-cls (cls sn) ;gv-cls instead of gvs, by skipping an ins
  (let ((c (if (symbolp cls) (symbol-name cls)
	     (to-str cls))))
    (gv- c sn)))
;defun gv (i sn)  ;GetValue
(defgeneric gv (i sn))
(defmethod gv (i sn)  ;GetValue
  "get km value"
 ;(ka- "the" sn "of" (ki i))
  (gv- (ki i) sn))
(defmethod gv ((l List) sn)
  (mapcar #'(lambda (i) (gv i sn)) l))
(defun gv+ (i sn) "GetValue always as a list" (list+ (gv i sn))) ;as ka returns firs-lv if only 1
(defun gv+2 (i sn) "GetValue args/etc as a list" (list+2 (gv i sn))) ;as ka returns firs-lv if only 1
(defun gva (i sn) "get-value for :args" (mapcar- #'rest (gv+2 i sn)))
(defgeneric gv1 (i sn))
(defmethod gv1 (i sn)  (ka- "the1" sn "of" (ki i)))
(defmethod gv1 ((l List) sn) (mapcar #'(lambda (i) (gv1 i sn)) l))
(defun gv2 (i sn)  (ka- "the2" sn "of" (ki i)))
(defun gv3 (i sn)  (ka- "the3" sn "of" (ki i)))
(defun gv4 (i sn)  (ka- "the4" sn "of" (ki i)))
;
(defun gv-1 (i sn) (first-lv (gv i sn)))
;
(defun class_of (i)
  (gv i "instance-of"))
;
(defun ins_of0 (i) ;fix ;use ins-of
  (gv-cls i "instances of"))
;
(defun has-star (s) (prefixp "*" (if (symbolp s) (symbol-name s) s)))
;(defvar *quote-all* t)
(defun quoteable_p (s) ;str
  "if space then put quotes before print/assert"
  (when (stringp s) (or (position #\Space s) (position #\: s))))
;defun quoteable=p (s) ;str
(defun quoteable-p (s) ;str
  "more stringent version, that also does anything not a #|ins"
  (or (quoteable_p s) (not (or (numericp s) ;(numberp s) 
                               (has-star s)))))
;(defun quoteable-p (s) ;str
;  (if *quote-all* (quoteable=p s)
;    (quoteable_p s)))
 
;(defun quoteable-p (s) ;str
; "if space then put quotes before print/assert"
; (when (stringp s) (position #\Space s)))
(defun snvs (sn val &optional (qtval 'auto))
  "slotname value string for km"
 ;let ((sval (if (and (eq qtval 'auto) (quoteable-p val)) (quote-str val) val)))
 (let ((sval (if (or (not qtval) (and (eq qtval 'auto) (not (quoteable-p val))))  val
	       (quote_str val)))) ;was quote-str
  (format nil "(~a (~a))" sn sval)))
(defun sv! (i sn v) (sv i sn v 'auto 'now)) ;replaces old value
;-add something for quoting strings ;&optional (qtval t) ;or the quoateable-p
(defun sv (i sn val &optional (qtval 'auto) (also nil))  ;SetValue   ;if also='now then replaces
  "set km value"
 ;let ((has (if also "also-has" "has")))
 (let ((has (if also (if (eq also 'now) "now-has" "also-has") "has")))
  (ka- (ki i) has (snvs sn val qtval))))   ;consider "also-has", so multislot but not unified&&
(defun svif (i sn val &optional (qtval 'auto) (also nil))  ;Set if Value 
  (when val (sv i sn val qtval also)))
(defun sv-from (i sn fnc &optional (qtval 'auto) (also nil))   ;SetValue s from a fnc
  "set km values from a fnc on i"
  (sv i sn (funcall fnc i) qtval also))
(defun sv_from (i sn &optional (qtval 'auto) (also nil))   ;SetValue s from a fnc
  "set km values from a fnc on i, w/same name as sn"
  (let ((idfnc (symbol-function sn)))
    (sv-from i sn idfnc qtval also)))
;sval, sv using an alist
(defun sv_ida (ida sn &optional (qtval t))   ;SetValue s from a cons pair ;id.val
   (sv (car ida) sn (cdr ida) qtval)) 
(defun sv-pr (i pr &optional (qtval t))   ;SetValue s from a cons pair ;sn.val
   (sv i (car pr) (cdr pr) qtval)) ;can't use below, as need to lambda in the i anyway
(defun sv-al (i al)   ;SetValue s from alist
  "set km values from alst"
  (mapcar #'(lambda (pr) (sv i (car pr) (cdr pr)))
	  al))
;	make sure there is a gv-cls to get a class value, by not putting a *in ins name; it is: gvs
;(defun sv-cls (i cls) (sv i "instance-of" cls))
(defun sv-cls (i cls-) 
  "set class(es) of instance"
  (let ((cls (implode-l (list+ cls-))))
    (sv i "instance-of" cls nil))) ;don't quote
;(defun sv-cls (i cls) 
;  (when (fulll cls) (mapcar- #'(lambda (c) (sv-cls i c)) cls))
;  (sv i "instance-of" cls 'auto t)) ;also

;;			add-star=ki_ instead of just under_
;;defun ids2km-seq (idl &optional (add-star t) (add-quote nil))
(defun ids2km-seq (idl &optional (add-star t) (add-quote nil) (idfnc #'under_f))
  "take a seq of str/sym and turn into km :seq of ins IDs" ;used in words-seq and put-slot-cui
  (when (fulll idl)
    (let* ((idls (mapcar #'to-str idl))  ;add remove-duplicates
	   ;(idls (mapcar #'to-str (remove-duplicates idl)))  ;add remove-duplicates
	   (id-l (append '(":seq ") (if add-star (mapcar #'(lambda (i) (txt2kmId i idfnc)) idls)
				  (mapcar idfnc idls))))
	   (bare-l (implode-l id-l)))
      (if add-quote (quote-strl bare-l)
	(strl bare-l))))) 
;
(defgeneric words-seq (w))
(defmethod words-seq ((txt String))
              (words-seq (explode- txt)))
(defmethod words-seq ((wl List))
              (ids2km-seq wl))
;
(defun svs (i sn vals)  ;also -has
  "sv :seq"
  (sv i sn (words-seq vals) nil t))

;defun svs2 (i sn vals)  ;also -has
(defun svs2 (i sn vals &optional (rd nil))  ;also -has
  (sv i sn (ids2km-seq 
	     (if rd (remove-duplicates vals) vals)
	     t nil #'ki) nil t))

;also a test of making above more simple
(defun km-args (al &optional (add-quote nil))
  "list to go to :args"
  (let* ((als (mapcar #'to-str (list+ al)))
	 (a-l (append '(":args ") (mapcar #'ki_ als)))
	 (bare-l (implode-l a-l)))
      (if add-quote (quote-strl bare-l) ;see how oft used
	(strl bare-l)))) 
;setVal :args
(defun sva (i sn vals)  ;also -has
  "sv :args"
 ;(ka- (ki i) "has" (km-args vals)) ;dwn a ()s , &didn't have sn
  (sv i sn (km-args vals) nil t) ;was auto
  )

(defun subclasses-of (cls) (gv- cls "subclasses"))
(defun ins-of2 (cls) (flat1 (mapcar_ #'ins-of (cons cls (subclasses-of cls)))))
;=newer:
;-
(defgeneric safe_v (s))
;(defmethod safe_v ((s String)) (rm-colon (safe-trim s)))
(defmethod safe_v ((s String)) (num-str (rm-colon (safe-trim s)))) ;want nums if there
(defmethod safe_v (a) (safe_v (to-str a)))
;(defmethod safe_v (sy) (intern (safe_v (symbol-name sy))))
(defmethod safe_v ((sy symbol))  ;already in utr2.lisp
  (let* ((s (symbol-name sy))
     (p (position ":" s :test #'equal)))
    (if (and (numberp p) (> p 1)) (intern (safe_v s) :km) ;
      sy)))
(defmethod safe_v ((c cons))
  (when *dbg* (warn "do not send safe_v a cons"))
  (cons (safe_v (car c)) (safe_v (cdr c))))

(defun safe_al (a) ;already in utr2.lisp
  "test if no lst in (b . c)"
  (if (and (consp a) (not (consp (car a))) (not (consp (cdr a)))) a
    (when *dbg* (warn "not safe alt_elt:~a" a))))

(defun sv_al (i al)   ;SetValue s from alist
  "set km values from alst"
;(first-lv
  (mapcar
    ;#'(lambda (pr) (sv i (safe_v (car pr)) (safe_v (cdr pr))))
    #'(lambda (pr) (when (safe_al pr) (sv i (safe_v (car pr)) (safe_v (cdr pr)))))
    ;#'(lambda (pr) (sv i (safe_v (car pr)) (cdr pr))) ;see if can just insert value
          al))
;);maybe ret first-lv of al instead
(defun sv_al_ (i al)   ;SetValue s from aything
  (mapcar
    ;#'(lambda (pr) (when (consp pr) (sv i (safe_v (first-lv pr)) (safe_v (rest-lv pr))))) 
    #'(lambda (pr) (when (consp pr) (sv i (safe_v (car-lv pr)) (safe_v (cdr-lv pr)))))
    al))
(defun sv_al_f (i al)   ;SetValue s from aything
    #'(lambda (pr) (when (consp pr) (let ((prl (flat1 pr))) ;don't use
                      ;(sv i (safe_v (first-lv prl)) (safe_v (rest-lv prl)))
                      (sv_al_ i prl)
                      ))))
(defun sv_al_1 (l) (sv_al_ (first-lv l) (rest-lv l))) ;map over c2q     ;ins only no cls
;-
;defun get_id_ (dl &optional (n nil))  ;xmls-idp
;-
 
(defun sv_al_gi (al &optional (c nil))   ;SetValue s from aything
  ;let ((i (get_id_ al)))
  (let ((i (assoc-v :id al)))
    (when c (sv-cls i c))
    (sv_al_ i al)))

(defun sv_al_c (l)  ;same but sets cls, and gensyms it for insname ;close but squished ins together
  (let* ((f (rm-colon (to-str (first-lv l))))
     (i (gensym f))
     ;(i (get_id_ (rest-lv l))) ;now need2map over parts of lst
     )
    (format t "~%~a"  f) ;dbg
    (sv-cls i f)
    ;(sv_al_f i (rest-lv l))
    (sv_al_ i (flat1 (rest-lv l)))
    )) ;map over c2q     ;could flat1 here
;take c2q top type as cls, and pull :ID for each as insname &rest info the ins
;  might str-cat cls _ id  for insname
(defun sv_al_c2 (l &optional (c nil))  ;same but sets cls, and gensyms it for insname
    ;(mapcar_ #'(lambda (p) (sv_al_gi p c)) l)
    (sv_al_gi l c)
    )
(defun sv_al_c1 (l)  ;same but sets cls, and gensyms it for insname
  (let* ((f (rm-colon (to-str (first-lv l)))))
    (mapcar_ #'(lambda (p) (sv_al_c2 p f)) (rest-lv l))))
;turn c1&c2 to sv_al_c_

(defun sv_al_c_ (l)  ;same but sets cls, and gensyms it for insname
  (let* ((f (rm-colon (to-str (first-lv l)))))
    (mapcar_ #'(lambda (p) (sv_al_gi p f)) (rest-lv l))))
;_ 
;(defun starts_with (lst x)  ;km has starts-with
;    "Is x a list whose first element is x?"
;    (and (consp lst) (eql (first lst) x)))
;- 

;=newer2:
(defun s-ki (n) (when n (ki n)))
(defun s-sn (sn)  (if (equal sn "format")  (str-cat sn "_") sn)) ;might expand
;(defun s-sv (i s v) (when (and i s) (sv i (s-sn s) (safe_v v)))) ;might set v=nil
(defun s-sv (i s v) (when (and i s)
              (let ((s-v (safe_v v)))
            (sv i (s-sn s) s-v) s-v))) ;ret v vs ins in this one, ....

(defun sv-dl (i dl)   ;SetValue s from ~alist
    "set km values from alst"
    (mapcar #'(lambda (pr) (if (len-eq pr 2) (s-sv i (first pr) (second pr))
                 (warn "not-dl:~a" pr)))
        dl))

(defun dl2insc (ins dl &optional (cls nil))
  "ins w/cls" ;like the al&pl versions
  (when cls (sv-cls ins cls))
  (when dl (sv-dl ins dl)))

(defun get_id-n (dl n)  ;find from avl an a w/n/cls w/in it, and ret cls+v
  (let ((a (find n dl :key #'first :test #'search)))
    (when a (str-cat n (second-lv a)))))
(defun get_id (dl &optional (n nil))  ;xmls-idp
  "get id value" ;from avl, len=2 lists
  (let* ((i (find "id" dl :key #'first :test #'equal))
         (id (if (fulll i) (second i) ;len-eq i 2 ,instead of fulll
           ;(when n (find n dl :key #'first :test #'search))
           (when n (get_id-n dl n))
           )))
     (if id id (when n (gensym n))) ;last resort
    ))
;-
(defun map-ins-of (fn cls) (mapcar_ fn (ins-of cls)))
(defun map-ins-of_ (fn cls) (mapcar_ fn (ins-of2 cls))) 
;-
(defun parts2ins (ins parts)
  "save all the slot-value pairs into the instance"
  (let ((al (list2alst parts)))
    (when al (sv_al ins al))))
(defun al2ins (ins al)
  "save all the slot-value pairs into the instance"
  (when (listp al) (sv_al ins al)))

(defun json2ins (n)
  "for ldj.cl version for now"
  ;(parts2ins n (get-jas n))
  (al2ins n (get-jas n))
  ) 
;
(defun pl2ins (ins pl)
  "ins w/s,v from pl"
  (al2ins ins (plist-to-alist pl)))
