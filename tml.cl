;new use for km-utils, where want to step toward more generalized json->triples, bobak@balisp.org

;- ;these bits ref my github, re:km/etc  ;&/or will be added soon to u2.lisp
(defun sv_al_nn (i al)   ;SetValue s from aything ;no-nils   
  (mapcar
    ;#'(lambda (pr) (when (consp pr) (sv i (safe_v (car-lv pr)) (safe_v (cdr-lv pr)))))
    #'(lambda (pr) (when (and (consp pr) (cdr pr)) 
                     (sv i (safe_v (car-lv pr)) (safe_v (cdr-lv pr)))))
    al)
  i) ;hope alst wasn't expected anyplc in sv_al ;but want insname now, so can eval emed ins&just store it's name

;-don't assert slot-val of a long alst, that should have been another instance
(defun strlst (s)
  "a str that wants to be a list"
  (when (and (stringp s) (prefixp "(" s))
    (eval-str (str-cat "'" s))))

(defun sv_al_nn2 (i al)   ;SetValue s from aything ;no-nils  ;allow4slotVals that are alst-turnIt2->ins
  (mapcar  ;when val a list, try to assert it, &use it's insname as the value  ;1deeper now
    #'(lambda (pr) (when (and (consp pr) (cdr pr)) 
                     (let* ((sval (strlst (safe_v (cdr-lv pr)))) ;lst will be in a str/so trans b4 listp
                            (val (if (listp sval) (sv_al_nn (gentemp (to-str i)) sval)
                                   sval)))
                       (when val
                         (sv i (safe_v (car-lv pr)) val)))))
    al))

(trace sv_al_nn2)
(trace sv_al_nn)
;defmethod ka ((s String))  ;&thing-p trying in tc.cl
;-
(defun n-ins (in cls)
  "if num for ins-name pre-append cls-name"
  (let ((n (s2num in)))
    (if (numberp n) (str-cat cls n)
      in)))

(defun sv-i-al (i-al)
  "ins-name alst assertion" 
  (sv_al_nn (first i-al) (rest i-al)))
 ;(sv_al_nn (n-ins (first i-al) cls) (rest i-al))
 ;if going to pass in something that needs the cls, might as well set that2ins name@same time
;=use mk-i-al instead of these2, if ok2also set cls, in one go 
(defun sv-cls-inum (in cls)
  "ins-name is a num, so pre-append w/cls-name"
  (let ((i (n-ins in cls)))
    (sv-cls i cls)
    i))

(defun mk-i-al (i-al cls)
  "set cls then alst values" ;where check2for safe insnames
  (sv-cls-inum (first i-al) cls)
  (sv_al_nn ;try sv_al_nn2  ;started fix in ts.cl, but need2fix new fnc b4trying *
    (n-ins (first i-al) cls) (rest i-al)))

(defun mk-i-al_s (i-al_s cls)
  "lst of (insnaem alst) ->assert" ;set cls then alst values
  (mapcar #'(lambda (i-al) (mk-i-al i-al cls)) i-al_s))

;fix deeper ins /in net ;if long-value is in parens, check if it's an i-al and recursively set

;write something so if slotname is also a taxonomy classname, to make the value an instance
(defun thing-p (cls)
  "is this symbol the name of a km class?" ;makes tmp qry so works on anything
  (eq '|Thing| (first-lv (gvs cls "superclasses"))))  ;or (member '|Thing| (list+ ...))
;just give a list of str/sym of slotnames (that are clses) that you should turn there vals, into *insnames
;* and above just have a list of cls/slotnames that might have json which would cause an embedded ins /2start

;test w/cl-json output that has (insname alst of slotname/value pairs)
;(mk-i-al_s *nets* "net")
