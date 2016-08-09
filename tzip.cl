;try out https://github.com/bhyde/zipcode-distance-api.git, mike.bobak@gmail 
;uses my utils,            & (ql 'zipcode-distance-api.git)
(use-package :ZIPCODE-DISTANCE-API)
(setf (get :zipcodedistanceapi.redline13.com :token)
    "y2keUOOev6RiiAcLjUhPO3emTrWR0bDzeYO9MCoiJfB1QON9J4Yy7dU1FRSG2wfh")

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
  (zips4city "berkeley, ca"))

(defun zipinfo (ns)
  (zipcode-info (to-str ns)))
(defun ti (&optional (a 94110))
  (zipinfo a))