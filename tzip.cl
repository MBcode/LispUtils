;try out https://github.com/bhyde/zipcode-distance-api.git, mike.bobak@gmail 
;uses my utils,            & (ql 'zipcode-distance-api.git)
(use-package :ZIPCODE-DISTANCE-API)
;(setf (get :zipcodedistanceapi.redline13.com :token)
;    "eqnvqIjAK---you-actual-key-here---B33LUUpt0u");have2get your own from: http://www.zipcodeapi.com/
(load ".redline.cl" :print t)

(defun zip-dist (a b) 
  (distance-between (to-str a) (to-str b)))
(defun td (&optional (a 94110) (b 60622))
  (zip-dist 94110 60622))

(defun zips4city (s)
  (let* ((cs (split-str2by s #\,))
         (city (rm-space (car cs)))
         (state (rm-space (cdr cs))))
    (zipcodes-for-city city state)))
(defun tc (&optional (cs "berkeley, ca"))
  (zips4city cs))

(defun zipinfo (ns)
  (zipcode-info (to-str ns)))
(defun ti (&optional (a 94110))
  (zipinfo a))

;could: (defun zipinfos4city (s) (mapcar #'zipinfo (zips4city s)))
; then use take the alists,&dump w/a json lib, like cl-json
;Prob could get1lookup table w/zip2info list&calc the rest w/o srvc
;+have a zipcodes.json w/city,state,zip,lat,lon,..
(ql 'cl-json)
(defun decode-file (path)
  (with-open-file (stream path :direction :input)
    (json:decode-json-strict stream)))
(defvar *z* (decode-file "zipcodes.json"))
;-or
;+have prefuse-beta/data/zipcode.txt similar info
(ql 'cl-csv)
(defvar *z2* (cl-csv:read-csv #P"zipcode.txt"))
;zip,lat,lon,.,city,state,. ;create that alist again&go from there
;(defun za2alst (a) (macar #'(lambda (num nm) (cons nm (aref num a))) '(5 0 1 2) '(state zip lat lon)))
;(defvar *za* (mapcar #'(lambda (a) (cons (aref 4 a) (za2alst a))))) 
;(defun citylst (c) "list of cities by that name" (assoc c *za* :test #'equal))
;(defun citystate2alst (c s) (assoc s (citylst c))) ;might need accessfnc..finish ;zips4city-
;Could also process to have an alst of zips w/rest of info for zipinfo ;then could skip storing extra info above(2lookup-tables)
;go through list once to assoc w/the city name, then when 
; access can cull by the state.  Give all slots or just lat/lon 4dist calc
;- nearby-zipcodes might want another indexing frmt/geo even/mcclim might have something re
;- zipcode-ok.. stay w/service for now; in fact can replace/augment in waves
;=https://rosettacode.org/wiki/Haversine_formula#Common_Lisp 
(defparameter *earth-radius* 6372.8)
 
(defparameter *rad-conv* (/ pi 180))
 
(defun deg->rad (x)
  (* x *rad-conv*))
 
(defun haversine (x)
  (expt (sin (/ x 2)) 2))
 
(defun dist-rad (lat1 lng1 lat2 lng2)
  (let* ((hlat (haversine (- lat2 lat1)))
         (hlng (haversine (- lng2 lng1)))
         (root (sqrt (+ hlat (* (cos lat1) (cos lat2) hlng)))))
    (* 2 *earth-radius* (asin root))))
 
(defun dist-deg (lat1 lng1 lat2 lng2)
  (dist-rad (deg->rad lat1)
            (deg->rad lng1)
            (deg->rad lat2)
            (deg->rad lng2))) 
;=
;For mapping below the zipcode granulatrity we are back to a service; 
; as mentioned in: http://franz.com/support/tech_corner/usgs-011207.lhtml
; http://franz.com/support/tech_corner/usgs.tar.gz
;-well unless openstreetmap or similar incl street addresses

;could probably just use: https://github.com/kraison/cl-geocode
