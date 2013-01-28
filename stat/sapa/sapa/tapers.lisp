;;;-*- Mode: LISP; Package: :SAPA; Syntax: COMMON-LISP -*-
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;
;  tapers.lisp
;
;  a collection of Lisp functions to taper a time series ...
;  Note:  before compiling and loading tapers.lisp,
;         you should compile and load (in the order listed)
;            sapa-package.lisp, utilities.lisp and basic-statistics.lisp
;
;  6/3/93
;
;  SAPA, Version 1.0; Copyright 1993, Donald B. Percival, All Rights Reserved
;
;  Use and copying of this software and preparation of derivative works
;  based upon this software are permitted.  Any distribution of this
;  software or derivative works must comply with all applicable United
;  States export control laws.
; 
;  This software is made available AS IS, and no warranty -- about the
;  software, its performance, or its conformity to any
;  specification -- is given or implied. 
;
;  Comments about this software can be addressed to dbp@apl.washington.edu
;-------------------------------------------------------------------------------
;;; (compile-file "ccl:SAPA;tapers.lisp")
;;; (load "ccl:SAPA;tapers.fasl")
;-------------------------------------------------------------------------------
(if (not (find-package :SAPA))
  (defpackage :SAPA (:USE :COMMON-LISP)))

(in-package :SAPA)

(export '(;;; functions to compute data tapers discussed in the SAPA book ...
          cosine-data-taper!
          Hanning-data-taper!
          dpss-data-taper!

          ;;; function to taper a time series using a supplied vector ...
          supplied-data-taper!
          
          ;;; function to center and taper a time series ...
          center&taper-time-series
          ))

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;;  The functions  cosine-data-taper!
;;;                 Hanning-data-taper!
;;;                 dpss-data-taper!
;;;  implement tapering of a time series (see Section 6.4 of the SAPA book).
;;;  In all cases, the time series that is supplied to these functions
;;;  are altered (hence the ``!'' is attached to all three names)
;;;  UNLESS the user supplies a value for the keyword parameter result.
;;;  All these functions return two values, namely, a vector containing
;;;  the tapered time series (same as the vector used as input to the
;;;  function unless the keyword parameter result is specified) and
;;;  C_h, the variance inflation factor due to tapering (see Equation (251b)
;;;  in the SAPA book.
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
(defun cosine-data-taper!
       (a-time-series
        &key
        (taper-parameter 1.0)
        (normalization :unity)
        (result a-time-series))
  "given
   [1] a-time-series (required)
       <=> a vector containing a time series;
           this vector is modified UNLESS keyword result
           is bound to a different vector
   [2] taper-parameter (keyword; 1.0, i.e., Hanning data taper)
       ==> a number >= 0.0 and <= 1.0 that specifies the degree
           of tapering (parameter p of Equation (209)).
   [3] normalization (keyword; :unity)
       ==> if :unity, taper normalized such that
           its sum of squares is equal to unity (Equation (208a));
           if :N, taper normalized such that
           its sum of squares is equal to the number of points
           in the time series
   [4] result (keyword; a-time-series)
       <=> a vector to contain time series multiplied
           by cosine data taper
returns
   [1] a vector containing the tapered time series
   [2] C_h, the variance inflation factor
       computed using Equation (251b) in the SAPA book"
  (let* ((n (length a-time-series))
         (n-i-1 (1- n))
         (sum-of-squares 0.0)
         (C_h-top 0.0)
         ;;; function ``1-'' converts low-cutoff to 0 based indexing
         (low-cutoff (1- (/ (floor (* taper-parameter n)) 2)))
         ;;; function ``1+'' converts low-cutoff to SAPA convention;
         ;;; function ``1-'' converts high-cutoff to 0 based indexing
         (high-cutoff (1- (- (1+ N) (1+ low-cutoff))))
         (two-pi/denom (/ (* 2 pi) (1+ (floor (* taper-parameter n))))))
    (dotimes (i (truncate n 2))
      (let ((factor
             (cond ((<= 0 i low-cutoff)
                    (* 0.5 (- 1.0 (cos (* two-pi/denom (1+ i))))))
                   ((< low-cutoff i high-cutoff)
                    1.0)
                   (t
                    (error
                     "fatal error in cosine-data-taper!")))))
        (incf sum-of-squares (* 2 (expt factor 2)))
        (incf C_h-top (* 2 (expt factor 4)))
        (setf (aref result i) (* factor (aref a-time-series i))
              (aref result n-i-1) (* factor (aref a-time-series n-i-1)))
        (decf n-i-1)))
    (if (oddp n)
      (let ((factor
             (cond ((<= 0 n-i-1 low-cutoff)
                    (* 0.5 (- 1.0 (cos (* two-pi/denom (1+ n-i-1))))))
                   ((< low-cutoff n-i-1 high-cutoff)
                    1.0)
                   (t
                    (error
                     "fatal error in cosine-data-taper!")))))
        (incf sum-of-squares (expt factor 2))
        (incf C_h-top (expt factor 4))
        (setf (aref result n-i-1) (* factor (aref a-time-series n-i-1)))))
    (let ((factor (sqrt (/ (if (eq normalization :unity) 1 N)
                           sum-of-squares))))
      (values (a*x! factor result)
              (/ (* N C_h-top) (expt sum-of-squares 2))  ; C_h
              ))))

#|
(dolist (N (list 100 1000))
  (dolist (p (list 0.0 0.2 0.5 1.0))
    (multiple-value-bind (taper C_h)
                         (cosine-data-taper!
                          (make-array N  :initial-element 1.0)
                          :taper-parameter p)
      (format t "~&;;; p = ~3,1F, N = ~D,   ~8,6F,   ~8,6F"
              p N C_h (sum-of-squares taper)))))

;;; p = 0.0, N = 100,    1.000000,   1.000000
;;; p = 0.2, N = 100,    1.110360,   1.000000
;;; p = 0.5, N = 100,    1.338254,   1.000000
;;; p = 1.0, N = 100,    1.925193,   1.000000

;;; p = 0.0, N = 1000,   1.000000,   1.000000
;;; p = 0.2, N = 1000,   1.115727,   1.000000
;;; p = 0.5, N = 1000,   1.346217,   1.000000
;;; p = 1.0, N = 1000,   1.942502,   1.000000

;;; good agreement with Table 248 of SAPA

(dolist (N (list 3 33 99))
  (dolist (p (list 0.0 0.2 0.5 1.0))
    (multiple-value-bind (taper C_h)
                         (cosine-data-taper!
                          (make-array N  :initial-element 1.0)
                          :taper-parameter p)
      (format t "~&;;; p = ~3,1F, N = ~D,   ~8,6F,   ~8,6F"
              p N C_h (sum-of-squares taper)))))

;;; p = 0.0, N = 3,    1.000000,   1.000000
;;; p = 0.2, N = 3,    1.000000,   1.000000
;;; p = 0.5, N = 3,    1.000000,   1.000000
;;; p = 1.0, N = 3,    1.500000,   1.000000

;;; p = 0.0, N = 33,   1.000000,   1.000000
;;; p = 0.2, N = 33,   1.087192,   1.000000
;;; p = 0.5, N = 33,   1.307487,   1.000000
;;; p = 1.0, N = 33,   1.887255,   1.000000

;;; p = 0.0, N = 99,   1.000000,   1.000000
;;; p = 0.2, N = 99,   1.105163,   1.000000
;;; p = 0.5, N = 99,   1.333636,   1.000000
;;; p = 1.0, N = 99,   1.925000,   1.000000
|#

;-------------------------------------------------------------------------------
(defun Hanning-data-taper!
       (a-time-series
        &key
        taper-parameter
        (normalization :unity)  ;or :N
        (result a-time-series))
  "calls cosine-data-taper! with taper-parameter set to 1.0"
  (declare (ignore taper-parameter))
  (cosine-data-taper!
   a-time-series
   :taper-parameter 1.0
   :normalization normalization
   :result result))

#|
(dolist (N (list 100 1000))
  (multiple-value-bind (taper C_h)
                       (Hanning-data-taper!
                        (make-array N  :initial-element 1.0))
    (format t "~&;;; p = ~3,1F, N = ~D,   ~8,6F,   ~8,6F"
            1.0 N C_h (sum-of-squares taper))))

;;; p = 1.0, N = 100,    1.925193,   1.000000
;;; p = 1.0, N = 1000,   1.942502,   1.000000

(dolist (N (list 3 33 99))
  (multiple-value-bind (taper C_h)
                       (Hanning-data-taper!
                        (make-array N  :initial-element 1.0))
    (format t "~&;;; p = ~3,1F, N = ~D,   ~8,6F,   ~8,6F"
            1.0 N C_h (sum-of-squares taper))))

;;; p = 1.0, N = 3,    1.500000,   1.000000
;;; p = 1.0, N = 33,   1.887255,   1.000000
;;; p = 1.0, N = 99,   1.925000,   1.000000
|#

;-------------------------------------------------------------------------------
(defun dpss-data-taper!
       (a-time-series
        &key
        (taper-parameter 4.0)   ;NW
        (normalization :unity)  ;or :N
        (result a-time-series))
  "given
   [1] a-time-series (required)
       <=> a vector containing a time series;
           this vector is modified UNLESS keyword result
           is bound to a different vector
   [2] taper-parameter (keyword; 4.0)
       ==> a number > 0.0 that specifies NW,
           the product of the sample size and
           the half-width of the concentration interval
           (see SAPA, page 211)
   [3] normalization (keyword; :unity)
       ==> if :unity, taper normalized such that
           its sum of squares is equal to unity (Equation (208a));
           if :N, taper normalized such that
           its sum of squares is equal to the number of points
           in the time series
   [4] result (keyword; a-time-series)
       <=> a vector to contain time series multiplied
           by cosine data taper
returns
   [1] a vector containing the tapered time series
   [2] C_h, the variance inflation factor
       computed using Equation (251b) in the SAPA book"
  (let* ((N (length a-time-series))
         (N-i-1 (1- N))
         (sum-of-squares 0.0)
         (C_h-top 0.0)
         (W (/ taper-parameter N))
         (beta-pi (* pi W (1- N))))
    (dotimes (i (truncate N 2))
      (let ((factor
             (bessi0-nr
              (* beta-pi
                 (sqrt (- 1.0 (expt (1- (float (/ (1+ (* 2 i)) N)))
                                    2)))))))
        (incf sum-of-squares (* 2 (expt factor 2)))
        (incf C_h-top (* 2 (expt factor 4)))
        (setf (aref result i) (* factor (aref a-time-series i))
              (aref result N-i-1) (* factor (aref a-time-series N-i-1)))
        (decf N-i-1)))
    (if (oddp N)
      (let ((factor
             (bessi0-nr
              (* beta-pi
                 (sqrt (- 1.0 (expt (1- (float (/ (1+ (* 2  n-i-1)) N)))
                                    2)))))))
        (incf sum-of-squares (expt factor 2))
        (incf C_h-top (expt factor 4))
        (setf (aref result n-i-1) (* factor (aref a-time-series n-i-1)))))
    (let ((factor (sqrt (/ (if (eq normalization :unity) 1 N)
                           sum-of-squares))))
      (values (a*x! factor result)
              (/ (* N C_h-top) (expt sum-of-squares 2))  ; C_h
              ))))

#|
(dolist (N (list 100 1000))
  (dolist (NW (list 1.0 2.0 4.0 8.0))
    (multiple-value-bind (taper C_h)
                         (dpss-data-taper!
                          (make-array N :initial-element 1.0)
                          :taper-parameter NW)
      (format t "~&;;; NW = ~3,1F, N = ~D,   ~8,6F,   ~8,6F"
              NW N C_h (sum-of-squares taper)))))

;;; NW = 1.0, N = 100,    1.404251,   1.000000
;;; NW = 2.0, N = 100,    1.995994,   1.000000
;;; NW = 4.0, N = 100,    2.820333,   1.000000
;;; NW = 8.0, N = 100,    3.984642,   1.000000

;;; NW = 1.0, N = 1000,   1.410677,   1.000000
;;; NW = 2.0, N = 1000,   2.005059,   1.000000
;;; NW = 4.0, N = 1000,   2.833080,   1.000000
;;; NW = 8.0, N = 1000,   4.002674,   1.000000

;;; These are slightly off from Table 248 of SAPA
;;; (evidently computed using a less accurate approximation
;;; to 0th order dpss):
;;;  NW = 1,              1.34    [should be 1.41, a 5% error]
;;;  NW = 2,              1.96    [should be 2.01, a 3% error]
;;;  NW = 4,              2.80    [should be 2.83, a 1% error]
;;;  NW = 8,              3.94    [should be 4.00, a 1% error]
|#

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;;  The function   supplied-data-taper!
;;;  tapers a time series using a taper supplied in the form of a vector.
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
(defun supplied-data-taper!
       (a-time-series
        &key
        (taper-parameter (Hanning-data-taper!
                          (make-array (length a-time-series)
                                      :initial-element 1.0)))
        (normalization :unity)  ;or :N
        (result a-time-series))
  "given
   [1] a-time-series (required)
       <=> a vector containing a time series;
           this vector is modified UNLESS keyword result
           is bound to a different vector
   [2] taper-parameter (keyword; vector with Hanning taper)
       ==> a vector containing the supplied data taper
           (must be the same length as a-time-series);
           unmodified upon return
   [3] normalization (keyword; :unity)
       ==> if :unity, taper normalized such that
           its sum of squares is equal to unity (Equation (208a));
           if :N, taper normalized such that
           its sum of squares is equal to the number of points
           in the time series
   [4] result (keyword; a-time-series)
       <=> a vector to contain time series multiplied
           by the supplied data taper
returns
   [1] a vector containing the tapered time series
   [2] C_h, the variance inflation factor
       computed using Equation (251b) in the SAPA book"
  (let ((N (length a-time-series))
        (sum-of-squares (sum-of-squares taper-parameter)))
    (x*y! a-time-series taper-parameter :result result)
    (let ((factor (sqrt (/ (if (eq normalization :unity) 1 N)
                           sum-of-squares))))
      (values (a*x! factor result)
              (/ (* N
                    #-allegro
                    (reduce #'+ taper-parameter
                            :key #'(lambda (x) (expt x 4)))
                    #+allegro
                    (let ((SSSS 0.0)
                          (j start))
                      (dotimes (i N SSSS)
                        (incf SSSS (expt (aref taper-parameter i) 4))))
                    )
                 (expt sum-of-squares 2))  ; C_h
              ))))

#|
(supplied-data-taper! #(5 4 3 2 1)
                      :taper-parameter #(0 1 2 1 0))
;==>
#(0.0 1.632993161855452 2.449489742783178 0.816496580927726 0.0)
5/2

(supplied-data-taper! #(5 4 3 2 1)
                      :taper-parameter #(0 2 4 2 0))
;==>
#(0.0 1.632993161855452 2.449489742783178 0.816496580927726 0.0)
5/2

(supplied-data-taper! #(5 4 3 2 1)
                      :taper-parameter #(0 1 2 1 0)
                      :normalization :N)
;==>
#(0.0 3.6514837167011076 5.477225575051661 1.8257418583505538 0.0)
5/2
|#

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;;  The function   center&taper-time-series
;;;  centers and/or tapers a time series.
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
(defun center&taper-time-series
       (time-series
        &key
        (center-data t)  ;t, nil or value to be subtracted off ...
        (start 0)
        (end (length time-series))
        (data-taper nil)
        (data-taper-parameters)
        (recenter-after-tapering-p t)
        (restore-power-option-p t)
        (result (make-array (- end start))))
  "given
   [1] time-series (required)
       ==> a sequence of time series values
   [2] center-data (keyword; t)
       ==> if t, subtract sample mean from time series;
           if a number, subtracts that number from time series;
           if nil, the effect of this function is to return
           a copy of the relevant portion of time-series
   [3] start (keyword; 0)
       ==> start index of sequence to be used
   [4] end (keyword; length of time-series)
       ==> 1 + end index of sequence to be used
   [5] data-taper (keyword; nil)
       ==> nil or a tapering function
   [6] data-taper-parameters (keyword)
       ==> parameters for tapering function (not used
           if data-taper is nil)
   [7] recenter-after-tapering-p (keyword; t)
       ==> if t and data-taper is a function,
           centers tapered series by subtracting
           off its sample mean
   [8] restore-power-option-p (keyword; t)
       ==> if t and data-taper is a function,
           normalizes tapered series to have same
           sum of squares as before tapering
   [9] result (keyword; vector of size (- end start))
       <== sequence to hold centered time series
returns
   [1] result, the sequence containing the centered and/or
       tapered time series (the contents of time-series
       are unaltered unless it is also bound to result)
   [2] the number used to center the time series
       (this is nil if center-data is nil)
   [3] C_h, the variance inflation factor due to the data taper
---
Note: see also center&prewhiten&taper-time-series
      in filtering.lisp"
  ;;; transfer relevant part of time-series to lower part of result
  (if (not (and (eq time-series result) (zerop start)))
    (copy-vector time-series result :start start :end end))
  (let ((N (- end start))
        (center-factor nil)
        (C_h 1.0))
    ;;; center the time series if required ...
    (when center-data
      (setf center-factor (if (numberp center-data)
                            center-data
                            (sample-mean result :end N)))
      (dotimes (i N)
        (decf (elt result i) center-factor)))
    ;;; taper the time series if required ...
    (when data-taper
      (let ((sum-of-squares-before (if restore-power-option-p
                                     (sum-of-squares result :end N))))
        ;;; Note: a more elegant way to do this is to use the macro
        ;;;       nth-value (Steele2, p. 184), but I have elected not
        ;;;       to use it because [1] some versions of Common Lisp don't
        ;;;       seem to have everything in Steele2 and [2] I don't
        ;;;       want to get into defining any more macros than absolutely
        ;;;       necessary (multf and divf are more than enough!).
        (setf C_h (multiple-value-bind (junk non-junk)
                                       (funcall data-taper
                                                (make-array
                                                 N
                                                 :displaced-to result)
                                                :taper-parameter
                                                data-taper-parameters
                                                :normalization :N)
                    (declare (ignore junk))
                    non-junk))
        ;;; Note: Although the sample mean has already been subtracted
        ;;; off, tapering reintroduces a nonzero mean in the tapered
        ;;; series.  Here we remove it.  This amounts to estimating
        ;;; the process mean using the alternative estimator discussed
        ;;; at the end of Section 6.4 of SAPA.
        (if recenter-after-tapering-p
          (let ((recenter-factor (sample-mean result :end N)))
            (dotimes (i N)
              (decf (elt result i) recenter-factor))))
        (if restore-power-option-p
          (let ((mult-factor (sqrt (/ sum-of-squares-before
                                      (sum-of-squares result :end N)))))
            (dotimes (i N)
              (multf (elt result i) mult-factor))))))
    (values result center-factor C_h)))

#|
(center&taper-time-series #(71 63 70 88 99 90 110))
;==> #(-94/7 -150/7 -101/7 25/7 102/7 39/7 179/7)
;    591/7
;    1.0

(sum (center&taper-time-series #(71 63 70 88 99 90 110)))
;==> 0

(center&taper-time-series #(71 63 70 88 99 90 110) :start 1 :end 3)
;==> #(-7/2 7/2)
;    133/2
;    1.0

(let ((a #(71 63 70 88 99 90 110)))
  (center&taper-time-series a :start 1 :end 3 :result a)
  a)
;==> #(-7/2 7/2 70 88 99 90 110)

(sample-mean-and-variance #(71 63 70 88 99 90 110))
;==> 591/7
;    12304/49
(float 12304/49)
;==> 251.10204081632654

(center&taper-time-series
 #(71 63 70 88 99 90 110)
 :data-taper #'dpss-data-taper!
 :data-taper-parameters 1.0)
;==> #(-7.8468508813076685 -22.4213972133101 -19.89677763285509
        6.044565561735114 21.153297157294492 6.4937737920259515
       16.473389216417303)
;    591/7
;    1.3015152611329361
(sample-mean-and-variance (center&taper-time-series
                           #(71 63 70 88 99 90 110)
                           :data-taper #'dpss-data-taper!
                           :data-taper-parameters 1.0))
;==> 0.0
;    251.10204081632654

(center&taper-time-series
 #(71 63 70 88 99 90 110)
 :data-taper #'dpss-data-taper!
 :data-taper-parameters 1.0
 :recenter-after-tapering-p nil)
;==> #(-8.369372277293243 -22.935860652629373 -20.412636880641728
        5.514363897068667 20.61474219629164 5.963323769683637
       15.937421676973303)
;    591/7
;    1.3015152611329361
(sample-mean-and-variance (center&taper-time-series
                           #(71 63 70 88 99 90 110)
                           :data-taper #'dpss-data-taper!
                           :data-taper-parameters 1.0
                           :recenter-after-tapering-p nil))
;==> -0.5268597529352993
;    250.82445961706344
(+ 250.82445961706344 (expt -0.5268597529352993 2))
;==> 251.10204081632648

(center&taper-time-series
 #(71 63 70 88 99 90 110)
 :data-taper #'dpss-data-taper!
 :data-taper-parameters 1.0
 :recenter-after-tapering-p nil
 :restore-power-option-p nil)
;==> #(-7.201263387970674 -19.73471463773794 -17.563655881256334
        4.744727026618859 17.737553464239067 5.1310258058118645
       13.713044111135646)
;    591/7
;    1.3015152611329361
(sample-mean-and-variance (center&taper-time-series
                           #(71 63 70 88 99 90 110)
                           :data-taper #'dpss-data-taper!
                           :data-taper-parameters 1.0
                           :recenter-after-tapering-p nil
                           :restore-power-option-p nil))
;==> -0.453326214165644
;    185.695553371949
|#

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;;  Everything below here consists of internal symbols in the SAPA package
;;;  and should be regarded as "dirty laundry" ...
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;; The Lisp function bessi0-nr is based on the Fortran function bessi0,
;;; Numerical Recipes, Second Edition, pages 230--1 (the coefficients are
;;; in fact from Abramowitz and Stegun).
(defun bessi0-nr (x)
  ;;; translation of BESSI0, Numerical Recipes, p. 177
  (cond
   ((<= (abs x) 3.75)
    (let ((y (expt (/ x 3.75) 2)))
      (+ 1.0
         (* y
            (+ 3.5156229
               (* y
                  (+ 3.0899424
                     (* y
                        (+ 1.2067492
                           (* y
                              (+ 0.2659732
                                 (* y
                                    (+ 0.0360768
                                       (* y 0.0045813))))))))))))))
   (t
    (let* ((ax (abs x))
           (y (/ 3.75 ax)))
      (* (/ (exp ax) (sqrt ax))
         (+ 0.39894228
            (* y
               (+ 0.01328592
                  (* y
                     (+ 0.00225319
                        (* y
                           (+ -0.00157565
                              (* y
                                 (+ 0.00916281
                                    (* y
                                       (+ -0.02057706
                                          (* y
                                             (+ 0.02635537
                                                (* y
                                                   (+ -0.01647633
                                                      (* y 0.00392377)
                                                      ))))))))))))))))))))

#|
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;; This version is from Kaiser (1974), Proc. IEEE Int. Symp.
;;; on Circuits and Systems, pp. 20-23.  We use the routine from
;;; Numerical Recipes instead, but the difference between
;;; the two bessel functions seems to be quite small (-2e-7 and 9e-8).
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
(defun mod-bessel-first-kind-of-0-order (x)
  (do* ((d 0.0 (+ d 2.0))
        (ds 1.0 (/ (* ds x x) (* d d)))
        (s 1.0 (+ s ds)))
       ((not (plusp (- ds (* 0.2e-8 s)))) s)))
|#
