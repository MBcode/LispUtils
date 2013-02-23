;new use for km-utils, where want to step toward more generalized json->triples, bobak@balisp.org
(defvar *ips* (rest (cadr *annot*)))
(defvar *nets* (cdr *net*))

;- ;these bits ref my github, re:km/etc  ;&/or will be added soon
(defun sv_al_nn (i al)   ;SetValue s from aything ;no-nils   ;add to u2.lisp
  (mapcar
    ;#'(lambda (pr) (when (consp pr) (sv i (safe_v (car-lv pr)) (safe_v (cdr-lv pr)))))
    #'(lambda (pr) (when (and (consp pr) (cdr pr)) (sv i (safe_v (car-lv pr)) (safe_v (cdr-lv pr)))))
    al)) 

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

(defun sv-cls-inum (in cls)
  "ins-name is a num, so pre-append w/cls-name"
 ;(sv-cls (str-cat cls (s2num in)) cls)
  ;let ((i (str-cat cls (s2num in))))
  (let ((i (n-ins in cls)))
    (sv-cls i cls)
    i))

(defun mk-i-al (i-al cls)
  "set cls then alst values" ;where check2for safe insnames
  (sv-cls (first i-al) cls)
  (sv_al_nn (n-ins (first i-al) cls) (rest i-al)))

(defun mk-i-al_s (i-al_s cls)
  "lst of (insnaem alst) ->assert" ;set cls then alst values
  (mapcar #'(lambda (i-al) (mk-i-al i-al cls)) i-al_s))

(mk-i-al_s *ips* "ip")
(mk-i-al_s *nets* "net")
;-if work dump both below-
(defun mk-ip (&optional (ips *ips*))
  "assert ip info"  ;set cls then alst values
  (mapcar #'(lambda (ip) (sv-cls (first ip) "ip")) ips)
  (mapcar #'sv-i-al ips))
;defun mk-net (&optional (nets *nets*))
