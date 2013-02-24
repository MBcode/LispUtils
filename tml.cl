;new use for km-utils, where want to step toward more generalized json->triples, bobak@balisp.org

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
;=use mk-i-al instead of these2, if ok2also set cls, in one go 
(defun sv-cls-inum (in cls)
  "ins-name is a num, so pre-append w/cls-name"
  (let ((i (n-ins in cls)))
    (sv-cls i cls)
    i))

(defun mk-i-al (i-al cls)
  "set cls then alst values" ;where check2for safe insnames
  (sv-cls-inum (first i-al) cls)
  (sv_al_nn (n-ins (first i-al) cls) (rest i-al)))

(defun mk-i-al_s (i-al_s cls)
  "lst of (insnaem alst) ->assert" ;set cls then alst values
  (mapcar #'(lambda (i-al) (mk-i-al i-al cls)) i-al_s))

;fix deeper ins /in net ;if long-value is in parens, check if it's an i-al and recursively set

;write something so if slotname is also a taxonomy classname, to make the value an instance

;test w/cl-json output that has (insname alst of slotname/value pairs)
;(mk-i-al_s *nets* "net")

