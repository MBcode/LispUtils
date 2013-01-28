;;;-*- Mode: LISP; Package: :SAPA; Syntax: COMMON-LISP -*-
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;
;  parametric.lisp
;
;  a collection of Lisp functions for parametric spectral estimation ...
;  Note:  before compiling and loading parametric.lisp,
;         you should compile and load (in the order listed)
;            sapa-package.lisp, utilities.lisp, basic-math.lisp,
;            matrix.lisp, basic-statistics.lisp, dft-and-fft.lisp
;            filtering.lisp, random.lisp and acvs.lisp
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
;;; (compile-file "ccl:SAPA;parametric.lisp")
;;; (load "ccl:SAPA;parametric.fasl")
;-------------------------------------------------------------------------------
(if (not (find-package :SAPA))
  (defpackage :SAPA (:USE :COMMON-LISP)))

(in-package :SAPA)

(export '(;;; functions for fitting AR models to data ...
          yule-walker-algorithm-given-data
          yule-walker-algorithm-given-acvs
          burg-algorithm
          fast-forward-backward-ls
          forward-backward-ls
          forward-ls
          two-step-Burg-algorithm

          ;;; functions for converting from one AR quantity to another ...
          ar-coeffs->acvs
          ar-coeffs->variance
          ar-coeffs->reflection-coeffs
          ar-coeffs->prewhitening-filter
          reflection-coeffs->ar-coeffs
          reflection-coeffs->variance

          ;;; functions for computing AR sdf estimates ...
          ar-coeffs->sdf
          ar-coeffs->sdf-at-single-freq
          sdf->sdf-with-accurate-peaks
          integrate-sdf

          ;;; functions for searching for peaks in an sdf estimate ...
          find-peak-of-ar-sdf-using-quadratic-approx
          find-peak-or-valley-of-sdf-using-bisection+Newton-Raphson

          ;;; functions for generating observed innovations ...
          generate-forward-innovations
          generate-backward-innovations

          ;;; function for regression analysis with AR errors ...
          regression-with-ar-errors
          ))

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;;  The functions  yule-walker-algorithm-given-data
;;;                 yule-walker-algorithm-given-acvs
;;;                 burg-algorithm
;;;                 fast-forward-backward-ls
;;;                 forward-backward-ls
;;;                 forward-ls
;;;                 two-step-Burg-algorithm
;;;  all estimate the parameters of an AR model for a given time series
;;;  and model order p.  Each function returns at least a vector of estimated
;;;  AR coefficients and an estimate of the associated innovations
;;;  variance, from which an AR sdf estimate can be computed or a
;;;  prewhitening filter be created.  There are also other intermediate
;;;  computations returned by these functions that might be of future
;;;  interest -- what these are depend on the nature of the particular
;;;  estimation scheme.  Chapter 9 of the SAPA book is devoted to
;;;  parametric spectral estimation.
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
(defun yule-walker-algorithm-given-data
       (time-series
        p
        &key
        (start 0)
        (end (length time-series))
        (center-data t)  ;t, nil or value to be subtracted off ...
        (AR-coeffs (make-array p))
        (approximate-MSEs (make-array (1+ p)))
        (reflection-coeffs (make-array p))
        (forward-prediction-errors (make-array (- end start p)))
        (backward-prediction-errors (make-array (- end start p))))
  "given
   [1] time-series (required)
       ==> a vector containing a real-valued time series x_t
   [2] p (required)
       ==> autoregressive model order;
           p should be an integer > 0 and < end - start
   [3] start (keyword; 0)
       ==> start index of vector to be used
   [4] end (keyword; length of the-seq)
       ==> 1 + end index of vector to be used
   [5] center-data (keyword; t)
       ==> if t, subtract sample mean from time series;
           if a number, subtracts that number from time series;
           if nil, the effect of this function is to return
           a copy of the relevant portion of time-series
   [6] AR-coeffs (keyword; a vector of length p)
       <== estimates of AR coefficients phi_{1,p}, ..., phi_{p,p}
   [7] approximate-MSEs (keyword; a vector of length p+1)
       <== estimates of innovations variance (mean square errors)
           for models of order 0, 1, ..., p (the innovations variance
           for the zeroth order model is the sample variance)
   [8] reflection-coeffs (keyword; a vector of length p)
       <== estimates of reflection coefficients phi_{1,1}, ..., phi_{p,p}
   [9] forward-prediction-errors (keyword; a vector of length end - start - p)
       <== computed forward prediction errors
  [10] backward-prediction-errors (keyword; a vector of length end - start - p)
       <== computed backward prediction errors
uses the Yule-Walker method to estimate the phi's in the model
   x_t = phi_{1,p} x_{t-1} + ... + phi_{p,p} x_{t-p} + e_t
and returns
   [1] AR-coeffs, a vector containing phi_{1,p}, ..., phi_{p,p}
   [2] estimate of the innovations variance, i.e.,
       the variance of the e_t's; this number is also given in
       (elt approximate-MSEs p)
   [3] approximate-MSEs
   [4] reflection-coeffs
   [5] forward-prediction-errors
   [6] backward-prediction-errors
---
Note: see p. 420 of the SAPA book"
  (let ((N-ts (- end start)))
    (assert (and (integerp p) (> p 0) (< p N-ts)))
    (let ((forward-j (make-array (+ N-ts p) :initial-element 0.0)))
      (if center-data
        (replace forward-j (center&taper-time-series 
                            time-series
                            :center-data center-data
                            :start start
                            :end end))
        (replace forward-j time-series :start2 start :end2 end))
      (let ((scratch (make-array (+ N-ts p)))
            (backward-j (circular-shift-sequence forward-j)))
        ;;; zeros at end don't matter ...
        (setf (elt approximate-MSEs 0)
              (float (/ (sum-of-squares forward-j)
                        N-ts)))
        (dotimes (j p)
          (setf (elt reflection-coeffs j) (/ (dot-product backward-j forward-j)
                                             (dot-product backward-j backward-j))
                (elt approximate-MSEs (1+ j)) (* (elt approximate-MSEs j)
                                                 (- 1.0
                                                    (expt
                                                     (elt reflection-coeffs j)
                                                     2))))
          (a*x+y! (- (elt reflection-coeffs j))
                  backward-j
                  forward-j
                  :result scratch)
          (a*x+y! (- (elt reflection-coeffs j))
                  forward-j
                  backward-j
                  :result backward-j)
          (circular-shift-sequence backward-j :result backward-j)
          (copy-vector scratch forward-j))
        ;;; At this point we have the reflection coefficients and
        ;;; approximate mse's, from which we can now generate
        ;;; the AR coefficients.
        (reflection-coeffs->ar-coeffs
         reflection-coeffs
         :p p
         :scratch scratch
         :result AR-coeffs)
        (values AR-coeffs
                (elt approximate-MSEs p)
                approximate-MSEs
                reflection-coeffs
                ;;; forward prediction errors ...
                (copy-vector forward-j forward-prediction-errors
                             :start p)
                ;;; backward prediction errors ...
                (copy-vector backward-j backward-prediction-errors
                             :start (1+ p)))))))

#|
(yule-walker-algorithm-given-data
 #(71 63 70 88 99 90 110 135 128 154 156 141 131 132 141 104 136 146 124 129)
 3)
;==> #(0.7564278899100625 -0.046223877768903764 0.03592061284674202)
;    351.6342314211137
;    #(782.64 352.21671265971577 352.088527878192 351.6342314211137)
;    #(0.7415951139732189 -0.01907713943936502 0.03592061284674202)
;    #(5.606819467197429 3.602049495972566 -13.13807178312505 13.531670850282099 22.591971410846554 -2.0709627659062573 29.661217150751934 10.772509547539435 -4.287081120362002 -3.7821509501885506 4.016928556685038 12.33707108178707 -31.065349921167176 29.302576292578536 13.063314822386415 -13.692737312779835 7.261455431834833)
;    #(-6.385268577444424 -19.24336074828875 -25.027314595591257 -16.48244854145692 1.3518647019431667 -21.36965186210829 -21.537852188257478 9.887122637410975 -16.148745552011714 8.004246630102024 20.852505588217184 13.139722849466036 4.1283725348055595 -4.53922156293019 33.568568323879624 -26.384631892925015 -3.14543916717523)     

(yule-walker-algorithm-given-data
 #(71 63 70 88 99 90 110 135 128 154 156 141 131 132 141 104 136 146 124 129)
 3
 :start 3
 :end 9)
;==> #(0.5519841575716381 -0.2118319667773691 -0.20557341091030015)
;    214.6294761698192
;    #(322.8888888888889 253.32586368772996 224.1000395950692 214.6294761698192)
;    #(0.46415462261986695 -0.3396593834915565 -0.20557341091030015)
;    #(5.629285177048483 19.944421844632704 1.5312965426964307)
;    #(-18.722444902065973 6.621387124383649 -9.561510733986982)

(yule-walker-algorithm-given-data
 #(88 99 90 110 135 128)
 3)
;==> same results as above
|#

;-------------------------------------------------------------------------------
(defun yule-walker-algorithm-given-acvs
       (acvs
        p
        &key
        (AR-coeffs (make-array p))
        (approximate-MSEs (make-array (1+ p)))
        (reflection-coeffs (make-array p)))
  "given
   [1] acvs (required)
       ==> a vector containing values of the acvs
           for a real-valued time series x_t
           from lag 0 up to (and including) p
   [2] p (required)
       ==> autoregressive model order;
           p should be an integer > 0 and < end - start
   [3] AR-coeffs (keyword; a vector of length p)
       <== estimates of AR coefficients phi_{1,p}, ..., phi_{p,p}
   [4] approximate-MSEs (keyword; a vector of length p+1)
       <== estimates of innovations variance (mean square errors)
           for models of order 0, 1, ..., p (the innovations variance
           for the zeroth order model is the sample variance)
   [5] reflection-coeffs (keyword; a vector of length p)
       <== estimates of reflection coefficients phi_{1,1}, ..., phi_{p,p}
uses the Yule-Walker method to estimate the phi's in the model
   x_t = phi_{1,p} x_{t-1} + ... + phi_{p,p} x_{t-p} + e_t
and returns
   [1] AR-coeffs, a vector containing phi_{1,p}, ..., phi_{p,p}
   [2] estimate of the innovations variance, i.e.,
       the variance of the e_t's; this number is also given in
       (elt approximate-MSEs p)
   [3] approximate-MSEs
   [4] reflection-coeffs
---
Note: see Sections 9.3 and 9.4 of the SAPA book"
  (assert (and (integerp p) (> p 0) (< p (length acvs))))
  (setf (elt approximate-MSEs 0) (elt acvs 0))
  ;;;
  ;;; Main computational loop - once for each AR order
  ;;;
  (let ((scratch (make-array p))
        top k)
    (dotimes (k-minus-1 p)
      (setf k (1+ k-minus-1))
      (setf top (elt acvs k))
      (dotimes (j k-minus-1)
        (decf top (* (elt AR-coeffs j)
                     (elt acvs (- k-minus-1 j)))))
      ;;; Calculate k-th order reflection coefficient
      (setf (elt reflection-coeffs k-minus-1)
            (setf (elt AR-coeffs k-minus-1)
                  (/ top (elt approximate-MSEs k-minus-1))))
      ;;; Update mean square error using approximate method.
      (setf (elt approximate-MSEs k)
            (* (elt approximate-MSEs k-minus-1)
               (- 1.0
                  (expt (abs (elt AR-coeffs k-minus-1)) 2))))
      ;;; Use Levinson-Durbin recursions to generate
      ;;; the remaining (k)-th order AR coefficients
      (dotimes (j k-minus-1)
        (setf (elt scratch j) (elt AR-coeffs j)))
      (dotimes (j k-minus-1)
        (setf (elt AR-coeffs j)
              (- (elt scratch j)
                 (* (elt AR-coeffs k-minus-1)
                    (elt scratch (- k-minus-1 j 1))))))))
  (values AR-coeffs
          (elt approximate-MSEs p)
          approximate-MSEs
          reflection-coeffs))

#|
(yule-walker-algorithm-given-acvs
 (acvs
  #(71 63 70 88 99 90 110 135 128 154 156 141 131 132 141 104 136 146 124 129))
 3)
;==> #(0.7564278899100633 -0.046223877768905255 0.03592061284674314)
;    351.634231421114
;    #(782.6400000000014 352.21671265971605 352.0885278781923 351.634231421114)
;    #(0.7415951139732192 -0.019077139439365634 0.03592061284674314)
;;; ...good agreement with results from yule-walker-algorithm-given-data

(yule-walker-algorithm-given-acvs
 (acvs #(88 99 90 110 135 128))
 3)
;==> #(0.5519841575716382 -0.21183196677736926 -0.2055734109103001)
;    214.62947616981913
;    #(322.88888888888886 253.32586368772988 224.1000395950691 214.62947616981913)
;    #(0.464154622619867 -0.33965938349155667 -0.2055734109103001)
;;; ...good agreement with results from yule-walker-algorithm-given-data
|#

;-------------------------------------------------------------------------------
(defun burg-algorithm
       (time-series
        p
        &key
        (start 0)
        (end (length time-series))
        (center-data t)  ;t, nil or value to be subtracted off ...
        (AR-coeffs (make-array p))
        (approximate-MSEs (make-array (1+ p)))
        (reflection-coeffs (make-array p))
        (forward-prediction-errors (make-array (- end start p)))
        (backward-prediction-errors (make-array (- end start p)))
        (exact-MSEs (make-array (1+ p))))
  "given
   [1] time-series (required)
       ==> a vector containing a real-valued or
           complex-valued time series x_t
   [2] p (required)
       ==> autoregressive model order;
           p should be an integer > 0 and < end - start
   [3] start (keyword; 0)
       ==> start index of vector to be used
   [4] end (keyword; length of the-seq)
       ==> 1 + end index of vector to be used
   [5] center-data (keyword; t)
       ==> if t, subtract sample mean from time series;
           if a number, subtracts that number from time series;
           if nil, the effect of this function is to return
           a copy of the relevant portion of time-series
   [6] AR-coeffs (keyword; a vector of length p)
       <== estimates of AR coefficients phi_{1,p}, ..., phi_{p,p}
   [7] approximate-MSEs (keyword; a vector of length p+1)
       <== estimates of innovations variance (mean square errors)
           for models of order 0, 1, ..., p (the innovations variance
           for the zeroth order model is the sample variance)
   [8] reflection-coeffs (keyword; a vector of length p)
       <== estimates of reflection coefficients phi_{1,1}, ..., phi_{p,p}
   [9] forward-prediction-errors (keyword; a vector of length end - start - p)
       <== computed forward prediction errors
  [10] backward-prediction-errors (keyword; a vector of length end - start - p)
       <== computed backward prediction errors
  [11] exact-MSEs (keyword; a vector of length p+1)
       <== another set of estimates of innovations variance
           for models of order 0, 1, ..., p; these estimates
           are based on Equation (419a) of the SAPA book
uses Burg's algorithm to estimate the phi's in the model
   x_t = phi_{1,p} x_{t-1} + ... + phi_{p,p} x_{t-p} + e_t
and returns
   [1] AR-coeffs, a vector containing phi_{1,p}, ..., phi_{p,p}
   [2] estimate of the innovations variance, i.e.,
       the variance of the e_t's; this number is also given in
       (elt approximate-MSEs p)
   [3] approximate-MSEs
   [4] reflection-coeffs
   [5] forward-prediction-errors
   [6] backward-prediction-errors
   [7] exact-MSEs
---
Note: see Section 9.5 of the SAPA book"
  (let ((N-ts (- end start)))
    (assert (and (integerp p) (> p 0) (< p N-ts)))
    (let* ((forward-scratch (center&taper-time-series
                             time-series
                             :center-data center-data
                             :start start
                             :end end))
           (backward-scratch (copy-seq forward-scratch))
           (sh2-old (float (/ (sum-of-squares forward-scratch)
                              N-ts)))
           (mseb-old sh2-old)
           (scratch (make-array p))
           top bottom k)
      ;(declare (dynamic-extent forward-scratch backward-scratch))
      ;;; Set zeroth order mean square prediction error
      (setf (aref approximate-MSEs 0)
            (setf (aref exact-MSEs 0) sh2-old))
      ;;;
      ;;; Main computational loop - once for each AR order
      ;;;
      (dotimes (k-minus-1 p)
        (setf k (1+ k-minus-1)
              top 0.0
              bottom 0.0)
        (dotimes
          (j (- N-ts k))
          (incf top 
                (* (aref forward-scratch (1+ j))
                   (conjugate (aref backward-scratch j))))
          (incf bottom
                (+ (expt (abs (aref forward-scratch (1+ j))) 2)
                   (expt (abs (aref backward-scratch j)) 2))))
        ;;; Calculate k-th order reflection coefficient
        (setf (aref reflection-coeffs k-minus-1)
              (setf (aref AR-coeffs k-minus-1)
                    (/ (* 2.0 top) bottom)))
        ;;; Update mean square error using:
        ;;; (1) approximate method
        (setf (aref approximate-MSEs k)
              (setf sh2-old
                    (* sh2-old
                       (- 1.0
                          (expt (abs (aref AR-coeffs k-minus-1)) 2)))))
        ;;; (2) exact method
        (setf (aref exact-MSEs k)
              (setf mseb-old
                    (* (/ (- (* mseb-old (1+ (- N-ts k)))
                             (/ (+ (expt (abs (aref forward-scratch 0)) 2)
                                   (expt (abs (aref backward-scratch (- N-ts k))) 2))
                                2.0))
                          (- N-ts k))
                       (- 1.0
                          (expt (abs (aref AR-coeffs k-minus-1)) 2)))))
        ;;; Update prediction errors
        (dotimes
          (j (- N-ts k))
          (setf (aref forward-scratch j)
                (- (aref forward-scratch (1+ j))
                   (* (aref AR-coeffs k-minus-1)
                      (aref backward-scratch j))))
          (decf (aref backward-scratch j)
                (* (conjugate (aref AR-coeffs k-minus-1))
                   (aref forward-scratch (1+ j)))))
        ;;; Use Levinson-Durbin recursions to generate
        ;;; the remaining (k)-th order AR coefficients
        (cond
         ((> k 1)
          (dotimes
            (j k-minus-1)
            (setf (aref scratch j) (aref AR-coeffs j)))
          (dotimes
            (j k-minus-1)
            (setf (aref AR-coeffs j)
                  (- (aref scratch j)
                     (* (aref AR-coeffs k-minus-1)
                        (conjugate (aref scratch (- k-minus-1 j 1))))))))))
      (values AR-coeffs
              (elt approximate-MSEs p)
              approximate-MSEs
              reflection-coeffs
              (copy-vector forward-scratch forward-prediction-errors
                           :end (- N-ts p))
              (copy-vector backward-scratch backward-prediction-errors
                           :end (- N-ts p))
              exact-MSEs))))

#|
(burg-algorithm
 #(71 63 70 88 99 90 110 135 128 154 156 141 131 132 141 104 136 146 124 129)
 3)
;==> #(0.6990553262846771 -0.005832588617464066 0.15582353261696208)
;    271.77271305830277
;    #(782.64 281.6806619464492 278.53583421681344 271.77271305830277)
;    #(0.8000556894184591 0.10566226444248338 0.15582353261696208)
;    #(10.648141558530694 10.352562066664447 -7.322824655671381 16.228008168577496 25.480349486540202 2.523029895325208 30.445761242415312 10.333906323267405 -4.82179229692915 -8.375709073465082 -0.7842917051141942 9.795680071680922 -31.93174995009409 25.82996688737918 11.841978873870683 -11.196460846389641 4.254729174305107)
;    #(-4.066643091642674 -18.56910263930764 -22.68552824408707 -15.544100783114937 -2.031539389627614 -23.776066471565816 -25.344689597046013 4.388697925766872 -18.43772239114538 7.634913453192931 19.906593928671484 10.500567986583063 5.619476664683157 -4.874180094467894 28.619274487654387 -27.264052349707473 -3.1620402252232624)
;    #(782.64 274.8400818422606 278.4733264361111 272.60343018395645)

(burg-algorithm
 #(71 63 70 88 99 90 110 135 128 154 156 141 131 132 141 104 136 146 124 129)
 3
 :start 3
 :end 9)
;==> #(0.7110790464963852 -0.17650146677307085 -0.2576098242725891)
;    168.87383595887832
;    #(322.8888888888889 212.4012089094338 180.87736848843608 168.87383595887832)
;    #(0.5849656667871341 -0.3852485990185058 -0.2576098242725891)
;    #(7.817702402342421 19.841316338455563 -3.7241189069459537)
;    #(-16.50310608308572 10.866880277657888 -9.745432752851167)
;    #(322.8888888888889 202.24161908203828 190.11187075352709 159.01307057103358)

(burg-algorithm
 #(88 99 90 110 135 128)
 3)
;==> same results as above
|#

;-------------------------------------------------------------------------------
;;;  adapted from Fortran subroutine MODCOVAR, , Marple, 1987 --
;;;  this works with both real and complex-valued time series
(defun fast-forward-backward-ls
       (time-series
        p
        &key
        (start 0)
        (end (length time-series))
        (center-data t)
        (AR-coeffs (make-array p))
        (pseudo-reflection-coeffs (make-array p)))
  "given
   [1] time-series (required)
       ==> a vector containing a real-valued or
           complex-valued time series x_t
   [2] p (required)
       ==> autoregressive model order;
           p should be an integer > 0 and < end - start
   [3] start (keyword; 0)
       ==> start index of vector to be used
   [4] end (keyword; length of the-seq)
       ==> 1 + end index of vector to be used
   [5] center-data (keyword; t)
       ==> if t, subtract sample mean from time series;
           if a number, subtracts that number from time series;
           if nil, the effect of this function is to return
           a copy of the relevant portion of time-series
   [6] AR-coeffs (keyword; a vector of length p)
       <== estimates of AR coefficients phi_{1,p}, ..., phi_{p,p}
   [7] pseudo-reflection-coeffs (keyword; a vector of length p)
       <== sequence of reflection coefficients computed
           stepwise by the algorithm, but these do not correspond
           to the estimated phi_{k,p} (even if the estimated
           AR model is stationary)
uses the forward/backward least squares method
to estimate the phi's in the model
   x_t = phi_{1,p} x_{t-1} + ... + phi_{p,p} x_{t-p} + e_t
and returns
   [1] AR-coeffs, a vector containing phi_{1,p}, ..., phi_{p,p}
   [2] estimate of the innovations variance, i.e.,
       the variance of the e_t's
   [3] pseudo-reflection-coeffs
---
Note: see Section 9.7 of the SAPA book;
      this function is an adaptation of
      the Fortran routine modcovar, pp. 258-60,
      ``Digital Spectral Analysis with Applications''
      by Marple, 1987"
  (let ((N-ts (- end start)))
    (assert (and (integerp p) (> p 0) (< p N-ts)))
    (let* ((centered-ts (center&taper-time-series
                         time-series
                         :center-data center-data
                         :start start
                         :end end))
           (N-ts-1 (1- N-ts))
           (r1 (let ((temp 0.0))
                 (dotimes (k (- N-ts 2) temp)
                   (incf temp
                         (* 2.0 (expt (abs (aref centered-ts (1+ k))) 2))))))
           (r2 (expt (abs (aref centered-ts 0)) 2))
           (r3 (expt (abs (aref centered-ts N-ts-1)) 2))
           (r4 (/ (+ r1 (* 2.0 (+ r2 r3)))))
           r5
           (estimated-mse (+ r1 r2 r3))
           (delta (- 1.0 (* r2 r4)))
           (gamma (- 1.0 (* r3 r4)))
           (lambda (* (conjugate (* (aref centered-ts 0) (aref centered-ts N-ts-1)))
                      r4))
           (c (make-array (1+ p)))
           (d (make-array (1+ p)))
           (r (make-array p)))
      (setf (aref c 0) (* (aref centered-ts N-ts-1) r4)
            (aref d 0) (* (conjugate (aref centered-ts 0)) r4))
      (cond
       ((zerop p) (values
                   (flip-sign-of-coefficients! AR-coeffs)
                   (/ (+ (* 0.5 r1) r2 r3) N-ts)))
       (t
        ;;;
        ;;; Main loop
        ;;;
        (do ((m 1 (+ m 1))
             (save1 0.0 0.0)
             save2 save3 save4
             m-k-1 m-k-2 theta psi xi ef eb c1 c2 c3 c4)
            (nil)
          (dotimes (k-m (- N-ts m))
            (incf save1 (* (aref centered-ts (+ k-m m))
                           (conjugate (aref centered-ts k-m)))))
          (setf save1 (* 2.0 save1)
                (aref r (1- m)) (conjugate save1)
                theta (* (aref centered-ts N-ts-1) (aref d 0))
                psi (* (aref centered-ts N-ts-1) (aref c 0))
                xi (* (conjugate (aref centered-ts 0)) (aref d 0)))
          (if (> m 1) (dotimes (k (1- m))
                        (incf theta (* (aref centered-ts (- N-ts k 2))
                                       (aref d (1+ k))))
                        (incf psi (* (aref centered-ts (- N-ts k 2))
                                     (aref c (1+ k))))
                        (incf xi (* (conjugate (aref centered-ts (1+ k)))
                                    (aref d (1+ k))))
                        (decf (aref r k) (+ (* (aref centered-ts (- N-ts m))
                                               (conjugate
                                                (aref centered-ts (+ k 1 (- N-ts m)))))
                                            (* (conjugate
                                                (aref centered-ts (1- m)))
                                               (aref centered-ts (- m k 2)))))
                        (incf save1 (* (conjugate (aref r k))
                                       (aref AR-coeffs (- m k 2))))))
          ;;; Note: Marple (1987) claims that c1 is less than unity
          ;;;       in magnitude (true) and that it is in fact
          ;;;       part of the reflection coefficient sequence
          ;;;       for the forward/backward least squares method
          ;;;       (false -- else f/b ls would be truly order
          ;;;       recursive as, for example, Burg is).
          (setf c1 (/ (- save1) estimated-mse)
                (aref AR-coeffs (1- m)) c1
                (aref pseudo-reflection-coeffs (1- m)) c1
                estimated-mse (* estimated-mse
                                 (- 1.0 (expt (abs c1) 2))))
          (if (> m 1) (dotimes (k (truncate m 2))
                        (setf m-k-2 (- m k 2)
                              save1 (aref AR-coeffs k)
                              (aref AR-coeffs k)
                              (+ save1
                                 (* c1 (conjugate
                                        (aref AR-coeffs m-k-2)))))
                        (if (/= k m-k-2)
                          (incf (aref AR-coeffs m-k-2)
                                (* c1 (conjugate save1))))))
          (if (= m p) (return (values
                               (flip-sign-of-coefficients! AR-coeffs)
                               (* 0.5 (/ estimated-mse (float (- N-ts m))))
                               pseudo-reflection-coeffs)))
          ;;;
          ;;; Time update of c, d vectors and gamma, delta, lambda scalars
          ;;;
          (setf r1 (/ (- (* delta gamma)
                         (expt (abs lambda) 2)))
                c1 (* (+ (* theta (conjugate lambda)) (* psi delta))
                      r1)
                c2  (* (+ (* psi lambda) (* theta gamma))
                       r1)
                c3 (* (+ (* xi (conjugate lambda)) (* theta delta))
                      r1)
                c4 (* (+ (* theta lambda) (* xi gamma))
                      r1))
          (dotimes (k (1+ (truncate (1- m) 2)))
            (setf m-k-1 (- m k 1)
                  save1 (conjugate (aref c k))
                  save2 (conjugate (aref d k))
                  save3 (conjugate (aref c m-k-1))
                  save4 (conjugate (aref d m-k-1)))
            (incf (aref c k) (+ (* c1 save3) (* c2 save4)))
            (incf (aref d k) (+ (* c3 save3) (* c4 save4)))
            (when (/= k m-k-1)
              (incf (aref c m-k-1) (+ (* c1 save1) (* c2 save2)))
              (incf (aref d m-k-1) (+ (* c3 save1) (* c4 save2)))))
          (setf r2 (expt (abs psi) 2)
                r3 (expt (abs theta) 2)
                r4 (expt (abs xi) 2)
                r5 (- gamma (* (+ (* r2 delta)
                                  (* r3 gamma)
                                  (* 2.0 (realpart
                                          (* psi lambda (conjugate theta)))))
                               r1))
                r2 (- delta (* (+ (* r3 delta)
                                  (* r4 gamma)
                                  (* 2.0 (realpart
                                          (* theta lambda (conjugate xi)))))
                               r1))
                gamma r5
                delta r2)
          (incf lambda (+ (* c3 (conjugate psi)) (* c4 (conjugate theta))))
          (if  (<= estimated-mse 0.0)
            (error "estimated mse <= 0.0 (~F)" estimated-mse))
          (if (not (and (plusp delta) (<= delta 1.0)
                        (plusp gamma) (<= gamma 1.0)))
            (error "delta' and gamma' are not in the range 0 to 1 (~F,~F)"
                   delta gamma))
          ;;;
          ;;; Time update of A vector; order updates of c, d vectors
          ;;; and gamma, delta, lambda scalars
          ;;;
          (setf r1 (/ estimated-mse)
                r2 (/ (- (* delta gamma)
                         (expt (abs lambda) 2)))
                ef (aref centered-ts m)
                eb (aref centered-ts (- N-ts m 1)))
          (dotimes (k m)
            (incf ef (* (aref AR-coeffs k)
                        (aref centered-ts (- m k 1))))
            (incf eb (* (conjugate (aref AR-coeffs k))
                        (aref centered-ts (+ (- N-ts m) k)))))
          (setf c1 (* eb r1)
                c2 (* (conjugate ef) r1)
                c3 (* (+ (* (conjugate eb) delta) (* ef lambda))
                      r2)
                c4 (* (+ (* ef gamma) (conjugate (* eb lambda)))
                      r2))
          (dotimes (k m)
            (setf m-k-1 (- m k 1)
                  save1 (aref AR-coeffs m-k-1)
                  (aref AR-coeffs m-k-1) (+ save1
                                                   (* c3 (aref c m-k-1))
                                                   (* c4 (aref d m-k-1)))
                  (aref c (1+ m-k-1)) (+ (aref c m-k-1) (* c1 save1))
                  (aref d (1+ m-k-1)) (+ (aref d m-k-1) (* c2 save1))))
          (setf (aref c 0) c1
                (aref d 0) c2
                r3 (expt (abs eb) 2)
                r4 (expt (abs ef) 2))
          (decf estimated-mse (* r2 (+ (* r3 delta) (* r4 gamma)
                                       (* 2.0 (realpart (* ef eb lambda))))))
          (decf delta (* r4 r1))
          (decf gamma (* r3 r1))
          (incf lambda (* (conjugate (* ef eb)) r1))
          (if  (<= estimated-mse 0.0)
            (error "estimated mse' <= 0.0 (~F)" estimated-mse))
          (if (not (and (plusp delta) (<= delta 1.0)
                        (plusp gamma) (<= gamma 1.0)))
            (error "delta' and gamma' are not in the range 0 to 1 (~F,~F)"
                   delta gamma))))))))

#|
(fast-forward-backward-ls
 #(71 63 70 88 99 90 110 135 128 154 156 141 131 132 141 104 136 146 124 129)
 3)
;==> #(0.6736988680045488 -0.017857067378710748 0.1515182334143321)
;    271.5712900083261
;    #(-0.8000556894184591 -0.10369838100465911 -0.1515182334143321)

(fast-forward-backward-ls
 #(71 63 70 88 99 90 110 135 128 154 156 141 131 132 141 104 136 146 124 129)
 3
 :start 3
 :end 9)
;==> #(0.5310692482787671 -0.33739103356904027 -0.34747481540511216)
;    137.56127670899096
;    #(-0.5849656667871341 0.406220671240301 0.34747481540511216)

(fast-forward-backward-ls
 #(88 99 90 110 135 128)
 3)
;==> same results as above
|#

;-------------------------------------------------------------------------------
;;; This implements the forward/backward least squares algorithm
;;; a straight-forward manner, but it is considerably slower than
;;; fast-forward-backward-ls
(defun forward-backward-ls
       (time-series
        p
        &key
        (start 0)
        (end (length time-series))
        (center-data t)
        (AR-coeffs (make-array p)))
  "given
   [1] time-series (required)
       ==> a vector containing a real-valued or
           complex-valued time series x_t
   [2] p (required)
       ==> autoregressive model order;
           p should be an integer > 0 and < end - start
   [3] start (keyword; 0)
       ==> start index of vector to be used
   [4] end (keyword; length of the-seq)
       ==> 1 + end index of vector to be used
   [5] center-data (keyword; t)
       ==> if t, subtract sample mean from time series;
           if a number, subtracts that number from time series;
           if nil, the effect of this function is to return
           a copy of the relevant portion of time-series
   [6] AR-coeffs (keyword; a vector of length p)
       <== estimates of AR coefficients phi_{1,p}, ..., phi_{p,p}
uses the forward/backward least squares method
to estimate the phi's in the model
   x_t = phi_{1,p} x_{t-1} + ... + phi_{p,p} x_{t-p} + e_t
and returns
   [1] AR-coeffs, a vector containing phi_{1,p}, ..., phi_{p,p}
   [2] estimate of the innovations variance, i.e.,
       the variance of the e_t's
---
Note: see Section 9.7 of the SAPA book;
      this function is an adaptation of
      the Fortran routine covmcov, pp. 262-4,
      ``Modern Spectrum Estimation: Theory and Application''
      by Kay, 1988"
  (let ((N-ts (- end start)))
    (assert (and (integerp p) (> p 0) (< p N-ts)))
    (let* ((centered-ts (center&taper-time-series
                         time-series
                         :center-data center-data
                         :start start
                         :end end))
           (N-ts-p (- N-ts p))
           (p-1 (1- p))
           (CC (make-array (/ (* p (1+ p)) 2)))
           (L 0)
           ;;; if zero is set to 0 rather than 0.0
           ;;; and if centered-ts contains integers,
           ;;; this algorithm will use rational arithmetic
           (zero 0.0))
      ;(declare (dynamic-extent CC))
      ;;; Compute autocorrelation estimates and insert them in matrix CC
      ;;; (stored in linearized form); see Kay's Equations (7.8) and (7.22)
      (dotimes (k p)
        (dotimes (j (1+ k))
          (setf (svref CC L) zero)
          (dotimes (i N-ts-p)
            (incf (svref CC L)
                  (+ (* (conjugate (aref centered-ts (+ p-1 (- i j))))
                        (aref centered-ts (+ p-1 (- i k))))
                     (* (aref centered-ts (1+ (+ i j)))
                        (conjugate (aref centered-ts (1+ (+ i k))))
                        ))))
          (incf L)))
      ;;; Compute right-hand vector of covariance equations;
      ;;; see Kay's Equation (7.8)
      (dotimes (j p)
        (setf (aref AR-coeffs j) zero)
        (dotimes (i N-ts-p)
          (decf (aref AR-coeffs j)
                (+ (* (conjugate (aref centered-ts (+ p-1 (- i j))))
                      (aref centered-ts (+ p i)))
                   (* (aref centered-ts (1+ (+ i j)))
                      (conjugate (aref centered-ts i)))))))
      ;;; Compute AR filter parameter estimate
      (cholesky! CC AR-coeffs)
      ;;; Compute estimate of innovations variance (Kay's Equation (7.9))
      (let ((sum zero)
            C)
        (dotimes (k (1+ p) (values
                            (flip-sign-of-coefficients! AR-coeffs)
                            (/ (realpart sum) (* 2 N-ts-p))))
          (setf C zero)
          (dotimes (i N-ts-p)
            (incf C (+ (* (conjugate (aref centered-ts (+ p i)))
                          (aref centered-ts (- (+ p i) k)))
                       (* (aref centered-ts i)
                          (conjugate (aref centered-ts (+ i k)))))))
          (incf sum (if (zerop k)
                      C
                      (* C (aref AR-coeffs (1- k))))))))))

#|
(forward-backward-ls
 #(71 63 70 88 99 90 110 135 128 154 156 141 131 132 141 104 136 146 124 129)
 3)
;==> #(0.6736988680045498 -0.017857067378712857 0.15151823341433324)
;    271.571290008326
;;; agrees well with fast-forward-backward-ls ...

(forward-backward-ls
 #(71 63 70 88 99 90 110 135 128 154 156 141 131 132 141 104 136 146 124 129)
 3
 :start 3
 :end 9)
;==> #(0.5310692482787671 -0.33739103356903977 -0.3474748154051125)
;    137.5612767089909
;;; agrees well with fast-forward-backward-ls ...

(forward-backward-ls
 #(88 99 90 110 135 128)
 3)
;==> same results as above
|#

;-------------------------------------------------------------------------------
(defun forward-ls
       (time-series
        p
        &key
        (start 0)
        (end (length time-series))
        (center-data t)
        (AR-coeffs (make-array p)))
  "given
   [1] time-series (required)
       ==> a vector containing a real-valued or
           complex-valued time series x_t
   [2] p (required)
       ==> autoregressive model order;
           p should be an integer > 0 and < end - start
   [3] start (keyword; 0)
       ==> start index of vector to be used
   [4] end (keyword; length of the-seq)
       ==> 1 + end index of vector to be used
   [5] center-data (keyword; t)
       ==> if t, subtract sample mean from time series;
           if a number, subtracts that number from time series;
           if nil, the effect of this function is to return
           a copy of the relevant portion of time-series
   [6] AR-coeffs (keyword; a vector of length p)
       <== estimates of AR coefficients phi_{1,p}, ..., phi_{p,p}
uses the forward least squares method
to estimate the phi's in the model
   x_t = phi_{1,p} x_{t-1} + ... + phi_{p,p} x_{t-p} + e_t
and returns
   [1] AR-coeffs, a vector containing phi_{1,p}, ..., phi_{p,p}
   [2] estimate of the innovations variance, i.e.,
       the variance of the e_t's
---
Note: see Section 9.7 of the SAPA book;
      this function is an adaptation of
      the Fortran routine covmcov, pp. 262-4,
      ``Modern Spectrum Estimation: Theory and Application''
      by Kay, 1988"
  (let ((N-ts (- end start)))
    (assert (and (integerp p) (> p 0) (< p N-ts)))
    (let* ((centered-ts (center&taper-time-series
                         time-series
                         :center-data center-data
                         :start start
                         :end end))
           (N-ts-p (- N-ts p))
           (p-1 (1- p))
           (CC (make-array (/ (* p (1+ p)) 2)))
           (L 0))
      ;(declare (dynamic-extent CC))
      ;;; Compute autocorrelation estimates and insert them in matrix CC
      ;;; (stored in linearized form); see Kay's Equation (7.8)
      (dotimes (k p)
        (dotimes (j (1+ k))
          (setf (svref CC L) 0.0)
          (dotimes (i N-ts-p)
            (incf (svref CC L) (* (conjugate (aref centered-ts (+ p-1 (- i j))))
                                  (aref centered-ts (+ p-1 (- i k))))))
          (incf L)))
      ;;; Compute right-hand vector of covariance equations;
      ;;; see Kay's Equation (7.8)
      (dotimes (j p)
        (setf (aref AR-coeffs j) 0.0)
        (dotimes (i N-ts-p)
          (decf (aref AR-coeffs j)
                (* (conjugate (aref centered-ts (+ p-1 (- i j))))
                   (aref centered-ts (+ p i))))))
      ;;; Compute AR filter parameter estimate
      (cholesky! CC AR-coeffs)
      ;;; Compute estimate of innovations variance (Kay's Equation (7.9))
      (let ((sum 0.0)
            C)
        (dotimes (k (1+ p) (values
                            (flip-sign-of-coefficients! AR-coeffs)
                            (/ (realpart sum) N-ts-p)))
          (setf C 0.0)
          (dotimes (i N-ts-p)
            (incf C (* (conjugate (aref centered-ts (+ p i)))
                       (aref centered-ts (- (+ p i) k)))))
          (incf sum (if (zerop k)
                      C
                      (* C (aref AR-coeffs (1- k))))))))))

#|
(forward-ls
 #(71 63 70 88 99 90 110 135 128 154 156 141 131 132 141 104 136 146 124 129)
 3)
;==> #(0.5055761990420168 -0.016273987434470755 0.17082594780884994)
;    235.7359874382611

(forward-ls
 #(71 63 70 88 99 90 110 135 128 154 156 141 131 132 141 104 136 146 124 129)
 3
 :start 3
 :end 9)
;==> #(0.7287366959740863 -1.3206848681166132 -0.13280888477556643)
;    -1.4210854715202004E-14  ;;; opps -- should be positive!

(forward-ls
 #(88 99 90 110 135 128)
 3)
;==> same results as above
|#

;-------------------------------------------------------------------------------
(defun two-step-Burg-algorithm
       (time-series
        p
        &key
        (start 0)
        (end (length time-series))
        (center-data t)  ;t, nil or value to be subtracted off ...
        (AR-coeffs (make-array p))
        (approximate-MSEs (make-array (1+ p)))
        (reflection-coeffs (make-array p))
        (forward-prediction-errors (make-array (- end start p)))
        (backward-prediction-errors (make-array (- end start p)))
        (exact-MSEs (make-array (1+ p))))
  "given
   [1] time-series (required)
       ==> a vector containing a real-valued or
           complex-valued time series x_t
   [2] p (required)
       ==> autoregressive model order;
           p should be an EVEN integer > 0 and < end - start
   [3] start (keyword; 0)
       ==> start index of vector to be used
   [4] end (keyword; length of the-seq)
       ==> 1 + end index of vector to be used
   [5] center-data (keyword; t)
       ==> if t, subtract sample mean from time series;
           if a number, subtracts that number from time series;
           if nil, the effect of this function is to return
           a copy of the relevant portion of time-series
   [6] AR-coeffs (keyword; a vector of length p)
       <== estimates of AR coefficients phi_{1,p}, ..., phi_{p,p}
   [7] approximate-MSEs (keyword; a vector of length p+1)
       <== estimates of innovations variance (mean square errors)
           for models of order 0, 1, ..., p (the innovations variance
           for the zeroth order model is the sample variance)
   [8] reflection-coeffs (keyword; a vector of length p)
       <== estimates of reflection coefficients phi_{1,1}, ..., phi_{p,p}
   [9] forward-prediction-errors (keyword; a vector of length end - start - p)
       <== computed forward prediction errors
  [10] backward-prediction-errors (keyword; a vector of length end - start - p)
       <== computed backward prediction errors
  [11] exact-MSEs (keyword; a vector of length p+1)
       <== another set of estimates of innovations variance
           for models of order 0, 1, ..., p; these estimates
           are based on Equation (419a) of the SAPA book
uses the two step Burg algorithm to estimate the phi's in the model
   x_t = phi_{1,p} x_{t-1} + ... + phi_{p,p} x_{t-p} + e_t
and returns
   [1] AR-coeffs, a vector containing phi_{1,p}, ..., phi_{p,p}
   [2] estimate of the innovations variance, i.e.,
       the variance of the e_t's; this number is also given in
       (elt approximate-MSEs p)
   [3] approximate-MSEs
   [4] reflection-coeffs
   [5] forward-prediction-errors
   [6] backward-prediction-errors
   [7] exact-MSEs
---
Note: see Section 9.5 of the SAPA book"
  (let ((N-ts (- end start)))
    (assert (and (evenp p) (integerp p) (> p 0) (< p N-ts)))
    (let* ((forward-scratch (center&taper-time-series
                             time-series
                             :center-data center-data
                             :start start
                             :end end))
           (backward-scratch (copy-seq forward-scratch)))
      ;(declare (dynamic-extent forward-scratch backward-scratch))
      ;;; Set zeroth order mean square prediction error
      (setf (aref approximate-MSEs 0)
            (setf (aref exact-MSEs 0)
                  (float (/ (sum-of-squares forward-scratch)
                            N-ts))))
      (dotimes (i (/ p 2))
        (multiple-value-bind (ss ss-m-1 ref-coeff-m-1)
                             (one-iteration-of-two-step-Burg
                              (* i 2) 1
                              N-ts
                              forward-scratch
                              backward-scratch
                              AR-coeffs
                              1.0)
          (let* ((mse-index-k (* (1+ i) 2))
                 (mse-index-k-minus-1 (1- mse-index-k))
                 (rc-index-k mse-index-k-minus-1))
            (setf (aref exact-MSEs mse-index-k-minus-1) ss-m-1)
            (setf (aref exact-MSEs mse-index-k) ss)
            (setf (aref approximate-MSEs mse-index-k-minus-1)
                  (* (aref approximate-MSEs (1- mse-index-k-minus-1))
                     (- 1.0 (expt ref-coeff-m-1 2))))
            (setf (aref approximate-MSEs mse-index-k)
                  (* (aref approximate-MSEs mse-index-k-minus-1)
                     (- 1.0 (expt (aref AR-coeffs rc-index-k) 2))))
            (setf (aref reflection-coeffs (1- rc-index-k)) ref-coeff-m-1)
            (setf (aref reflection-coeffs rc-index-k)
                  (aref AR-coeffs rc-index-k)))))
      (values AR-coeffs
              (elt approximate-MSEs p)
              approximate-MSEs
              reflection-coeffs
              (copy-vector forward-scratch forward-prediction-errors
                           :start p)
              (copy-vector backward-scratch backward-prediction-errors
                           :end (- N-ts p))
              exact-MSEs))))

#|
(two-step-burg-algorithm
 #(71 63 70 88 99 90 110 135 128 154 156 141 131 132 141 104 136 146 124 129)
 4)
;==> #(0.7475191644352183 -0.01583855174510637 0.46440844342490983 -0.4428652096133231)
;    245.25050973984762
;    #(782.64 317.1313317877149 313.7211062417939 305.0871233404041 245.25050973984762)
;    #(0.771228137382831 0.10369838100465757 0.16589516290564163 -0.4428652096133231)
;    #(7.541189677934279 -16.19020798032216 5.452393054435857 18.222543695391344 1.9025289159837546 20.25717110563984 -0.04210392524786144 -1.8028505644380866 -15.733061990760522 3.0902299405116476 19.03618232922566 -26.674545308896946 28.233150201353947 9.98969974364716 2.190241048379951 -7.4530347618842505)
;    #(-0.9808164276349656 -23.02263641146349 -16.2667771190106 -4.848573521669076 -1.5143975597659964 -10.303462500921928 -20.291200583839142 2.7814408815304876 -21.08490573505456 8.26942730376083 24.845207695933038 -3.2294508544868155 17.520376061131927 0.7742590731821393 23.552182767386412 -24.778733173674873)
;    #(782.64 275.4746826295362 277.97870057136765 271.85461785540076 229.6941141226679)

;;; reconstruction of forward prediction errors ...
(filter-time-series 
 (center&taper-time-series
  #(71 63 70 88 99 90 110 135 128 154 156 141 131 132 141 104 136 146 124 129))
 (ar-coeffs->prewhitening-filter
  #(0.7475191644352183 -0.01583855174510637 0.46440844342490983 -0.4428652096133231))
 )
;==> #(7.541189677934277 -16.190207980322164 5.452393054435857 18.222543695391344 1.9025289159837548 20.25717110563984 -0.04210392524786499 -1.8028505644380886 -15.73306199076052 3.09022994051165 19.03618232922566 -26.674545308896946 28.233150201353947 9.989699743647158 2.1902410483799546 -7.453034761884252)
;    16
;;; good agreement with what two-step-burg-algorithm gave ...

;;; reconstruction of backward prediction errors ...
(filter-time-series 
 (center&taper-time-series
  #(71 63 70 88 99 90 110 135 128 154 156 141 131 132 141 104 136 146 124 129))
 (reverse
  (ar-coeffs->prewhitening-filter
   #(0.7475191644352183 -0.01583855174510637 0.46440844342490983 -0.4428652096133231)))
 )
;==> #(-0.9808164276349629 -23.022636411463495 -16.266777119010605 -4.8485735216690795 -1.514397559765996 -10.303462500921928 -20.291200583839146 2.7814408815304876 -21.084905735054566 8.26942730376083 24.845207695933038 -3.2294508544868163 17.520376061131927 0.7742590731821384 23.55218276738641 -24.778733173674876)
;    16
;;; good agreement with what two-step-burg-algorithm gave ...

;;; second test ...
(two-step-burg-algorithm
 #(71 63 70 88 99 90 110 135 128 154 156 141 131 132 141 104 136 146 124 129)
 4
 :start 3
 :end 9)
;==> #(0.7203613053679895 -0.836626599020122 0.6819467895912388 -0.9407536937974277)
;    23.9213100745261
;    #(322.8888888888889 249.5010816657653 208.3296023555775 208.04307310163574 23.9213100745261)
;    #(0.47674418604651164 -0.4062206712403009 0.0370859144408096 -0.9407536937974277)
;    #(-2.6359115620116818 5.573399522283869)
;    #(-4.997928279988322 5.5839098523630515)
;    #(322.8888888888889 205.84237425635473 183.62625741893694 169.17806158288212 23.542537082241278)

(two-step-burg-algorithm
 #(88 99 90 110 135 128)
 4)
;==> same results as above

;;; reconstruction of exact MSE ...
(/ (+ (sum-of-squares #(-2.6359115620116818 5.573399522283869))
      (sum-of-squares #(-4.997928279988322 5.5839098523630515)))
   4)
;==> 23.542537082241275
;;; agrees with last element of exact-MSEs above (23.542537082241278) ...

;;; reconstruction of approximate MSE ...
(* (sample-variance
    #(88 99 90 110 135 128))
   (- 1.0 (expt 0.47674418604651164 2))
   (- 1.0 (expt -0.4062206712403009 2))
   (- 1.0 (expt 0.0370859144408096 2))
   (- 1.0 (expt -0.9407536937974277 2)))
;==> 23.9213100745261
;;; agrees prefectly with above computation (23.9213100745261) ...
|#

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;;  The functions  ar-coeffs->acvs
;;;                 ar-coeffs->variance
;;;                 ar-coeffs->reflection-coeffs
;;;                 ar-coeffs->prewhitening-filter
;;;                 reflection-coeffs->ar-coeffs
;;;                 reflection-coeffs->variance
;;;  take the AR coefficients or the reflection coefficients and convert 
;;;  them to some other quantity of interest.
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
(defun ar-coeffs->acvs
       (AR-coeffs
        variance
        max-lag
        &key
        (process-variance-p t)
        (list-of-lower-order-phi nil)
        (list-of-lower-order-pev nil)
        (result (make-array (1+ max-lag))))
  "given
   [1] AR-coeffs (required)
       ==> vector of length p with real or
           complex-valued AR coefficients phi_{j,p}
           for an AR(p) model of order p:
           x_t = phi_{1,p} * x_{t-1} + phi_{2,p} * x_{t-2}
                 + ... + phi_{p,p} * x_{t-p} + e_t
   [2] variance (required)
       ==> process variance (if process-variance-p is true)
           or innovations variance (if nil)
   [3] max-lag (required)
       ==> acvs to be computed out to this lag
   [4] process-variance-p (keyword; t)
       ==> if true, required variance parameter
            is taken to be process variance;
           otherwise, it is taken to be the innovations
           variance (both variance and process-variance-p
           are ignored if the next two keyword parameters
           are supplied)
   [5] list-of-lower-order-phi (keyword; nil)
       ==> if supplied, bypasses call to step-down-Levinson-Durbin-recursions,
           in which case list-of-lower-order-pev must be supplied also
   [6] list-of-lower-order-pev (keyword; nil)
       ==> if supplied, bypasses call to step-down-Levinson-Durbin-recursions
   [7] result (keyword; vector of size max-lag + 1)
       <== vector to hold acvs for lags 0, 1, ..., max-lag
returns
   [1] result, i.e., the acvs
---
Note: see Section 9.4 of the SAPA book"
  (if (not list-of-lower-order-pev)
    (multiple-value-setq
      (list-of-lower-order-phi list-of-lower-order-pev)
      (step-down-Levinson-Durbin-recursions
       AR-coeffs variance
       :process-variance-p process-variance-p)))
  (let ((p (length AR-coeffs))
        (current-lag 0)
        current-phi-vector)
    (setf (aref result 0) (nth 0 list-of-lower-order-pev))
    (when (plusp max-lag)
      (dotimes (k (min p max-lag))
        (incf current-lag)   ; this is k+1
        (setf (aref result current-lag)
              (*  (aref (nth k list-of-lower-order-phi) k)   ;reflection coeff
                  (nth k list-of-lower-order-pev)))
        (when (plusp k)
          (setf current-phi-vector
                (nth (1- k) list-of-lower-order-phi))
          (dotimes (j (length current-phi-vector))
            (incf (aref result current-lag)
                  (* (aref current-phi-vector j)
                     (aref result (- current-lag j 1)))))))
      (when (> max-lag p)
        (setf current-phi-vector (nth (1- p) list-of-lower-order-phi))
        (dotimes (k (- max-lag p))
          (incf current-lag)
          (setf (aref result current-lag) 0.0)
          (dotimes (j p)
            (incf (aref result current-lag)
                  (* (aref current-phi-vector j)
                     (aref result (- current-lag j 1))))))))
    (values result)))

#|
;;; coefficients are for the AR(4) process
;;; described by Equation (46a) of the SAPA book ...
(ar-coeffs->acvs
 #(2.7607 -3.8106  2.6535 -0.9238)
 1.0 6 :process-variance-p nil)
;==> #(761.7172900314962 545.7530764979967 27.14232320481949 -487.6647246019976 -705.2431860566808 -520.8142016231673 -69.504506541503)
;;; agrees out to lag 6 with lower plot of Figure 148
|#

;-------------------------------------------------------------------------------
(defun ar-coeffs->variance (ar-coeffs innovations-variance)
  "given
   [1] AR-coeffs (required)
       ==> a sequence of AR coefficients
           phi_{1,p}, ..., phi_{p,p}
   [2] innovations-variance (required)
       ==> innovations variance of the process
returns
   [1] variance of the process"
  (svref (ar-coeffs->acvs AR-coeffs innovations-variance 0
                          :process-variance-p nil)
         0))

#|
;;; coefficients are for the AR(4) process
;;; described by Equation (46a) of the SAPA book ...
(ar-coeffs->variance
 #(2.7607 -3.8106  2.6535 -0.9238)
 1.0)
;==> 761.7172900314962
|#

;-------------------------------------------------------------------------------
(defun ar-coeffs->reflection-coeffs
       (AR-coeffs
        &key
        (list-of-lower-order-phi
         (step-down-Levinson-Durbin-recursions
          AR-coeffs 1.0))
        (result (make-array (length AR-coeffs))))
  "given
   [1] AR-coeffs (required)
       ==> vector of length p with real or
           complex-valued AR coefficients phi_{j,p}
           for an AR(p) model of order p:
           x_t = phi_{1,p} * x_{t-1} + phi_{2,p} * x_{t-2}
                 + ... + phi_{p,p} * x_{t-p} + e_t
   [2] list-of-lower-order-phi (keyword; calls step-down-Levinson-Durbin-recursions)
       ==> must contain the list that is returned by
           the function step-down-Levinson-Durbin-recursions;
           this keyword parameter is useful if the result
           of calling that function is already available
   [3] results (keyword; vector of length p)
       <== vector of size p into which
            the reflection coefficients phi_{j,j}
            of orders j = 1, 2, ..., p are placed
returns
   [1] results, i.e., the reflection coefficients
---
Note: see Section 9.4 of the SAPA book"
  (map-into result #'(lambda (a-seq)
                        (elt a-seq (1- (length a-seq))))
            list-of-lower-order-phi))

#|
;;; test case uses results of first test for two-step-burg-algorithm ...
(ar-coeffs->reflection-coeffs
 #(0.7475191644352183 -0.01583855174510637 0.46440844342490983 -0.4428652096133231)
 )
;==> #(0.7712281373828311 0.10369838100465756 0.16589516290564163 -0.4428652096133231)
;;; agrees with what two-step-burg-algorithm returned ...
|#

;-------------------------------------------------------------------------------
(defun ar-coeffs->prewhitening-filter
       (AR-coeffs
        &key
        (result (make-array (1+ (length AR-coeffs)))))
  "given
   [1] AR-coeffs (required)
       ==> a sequence of AR coefficients
           phi_{1,p}, ..., phi_{p,p}
   [2] result (keyword; vector of size (1+ (length AR-coeffs)))
       <== sequence to hold coefficients for prewhitening filter
           1, -phi_{1,p}, ..., -phi_{p,p}
returns
   [1] result, the prewhitening filter
---
Note: this filter can be used to compute
      forward prediction errors; to obtain
      the filter needed to compute
      backward prediction errors,
      reverse the elements in result; i.e.,
      evaluate (reverse result)"
  (setf (elt result 0) 1.0)
  (dotimes (i (length AR-coeffs) result)
    (setf (elt result (1+ i)) (* -1 (elt AR-coeffs i)))))

#|
;;; forward prewhitening filter ...
(ar-coeffs->prewhitening-filter #(0.75 -0.5))
;==> #(1.0 -0.75 0.5)

;;; backward prewhitening filter ...
(reverse (ar-coeffs->prewhitening-filter #(0.75 -0.5)))
;==> #(0.5 -0.75 1.0)
|#

;-------------------------------------------------------------------------------
;;; Note: inclusion of p as a keyword is useful with routines that
;;;       map from the reflection coefficients because truncation
;;;       of this sequence at a certain point yields a valid lower
;;;       order model (NOT always the case with the AR coefficients)
(defun reflection-coeffs->ar-coeffs
       (reflection-coeffs
        &key
        (p (length reflection-coeffs))
        (scratch (make-array p))
        (result (make-array p)))
  "given
   [1] reflection-coeffs (required)
       ==> vector of length p with real-valued 
           reflection coefficients
           phi_{1,1}, phi_{2,2}, ... , phi_{p,p}
   [2] p (keyword; length of reflection-coeffs)
       ==> AR model order
   [3] scratch (keyword; vector of length p)
       ==> vector of size p used for intermediate results
   [4] results (keyword; vector of length p)
       <== vector of size p into which are placed
           the AR coefficients phi_{j,p} for model
           x_t = phi_{1,p} * x_{t-1} + phi_{2,p} * x_{t-2}
                 + ... + phi_{p,p} * x_{t-p} + e_t
returns
   [1] results, i.e., the AR coefficients
---
Note: see Section 9.4 of the SAPA book"
  (when (plusp p)
    (setf (aref result 0) (aref reflection-coeffs 0))
    (if (> p 1)
      (dotimes (k-minus-1 p result)
        (setf (aref result k-minus-1)
              (aref reflection-coeffs k-minus-1))
        ;;; Use Levinson-Durbin recursions to generate
        ;;; the remaining (k)-th order AR coefficients
        (copy-vector result scratch :end k-minus-1)
        (dotimes (j k-minus-1)
          (setf (aref scratch j) (aref result j)))
        (dotimes (j k-minus-1)
          (setf (aref result j)
                (- (aref scratch j)
                   (* (aref result k-minus-1)
                      (aref scratch (- k-minus-1 j 1)))))))
      result)))

#|
;;; test case uses results of first test for two-step-burg-algorithm ...
(reflection-coeffs->ar-coeffs
 #(0.771228137382831 0.10369838100465757 0.16589516290564163 -0.4428652096133231)
 )
;==> #(0.7475191644352183 -0.01583855174510637 0.46440844342490983 -0.4428652096133231)
;;; agrees with what two-step-burg-algorithm returned ...
|#

;-------------------------------------------------------------------------------
(defun reflection-coeffs->variance
       (reflection-coeffs
        innovations-variance
        &key
        (p (length reflection-coeffs)))
  "given
   [1] reflection-coeffs (required)
       ==> vector of length p with real-valued 
           reflection coefficients
           phi_{1,1}, phi_{2,2}, ... , phi_{p,p}
   [2] innovations-variance (required)
       ==> innovations variance of the process
   [3] p (keyword; length of reflection-coeffs)
       ==> AR model order
returns
   [1] variance of the process
---
Note: this is a recursive application of
      Equation (404c) of the SAPA book"
  (dotimes (k p innovations-variance)
    (divf innovations-variance
          (- 1.0 (expt (abs (aref reflection-coeffs k)) 2)))))

#|
;;; test case uses results of first test for two-step-burg-algorithm ...
(reflection-coeffs->variance
 #(0.771228137382831 0.10369838100465757 0.16589516290564163 -0.4428652096133231)
 245.25050973984762)
;==> 782.6399999999999
(sample-variance
 #(71 63 70 88 99 90 110 135 128 154 156 141 131 132 141 104 136 146 124 129))
;==> 19566/25
(float 19566/25)
;==> 782.64
;;; good agreement ...
|#

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;;  The functions  ar-coeffs->sdf
;;;                 ar-coeffs->sdf-at-single-freq
;;;                 form-spectrum-with-accurate-peaks
;;;                 integrate-sdf
;;;  compute or manipulate an AR sdf estimate.
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
(defun ar-coeffs->sdf
       (AR-coeffs
        innovations-variance
        &key
        (N-nonzero-freqs 256)
        (sampling-time 1.0)
        (return-est-for-0-freq-p t)
        (sdf-transformation #'convert-to-dB)
        (result-sdf (make-array
                     (if return-est-for-0-freq-p
                       (1+ N-nonzero-freqs)
                       N-nonzero-freqs)))
        (return-frequencies-p nil)
        (freq-transformation nil)
        (result-freq (if return-frequencies-p
                       (make-array
                        (if return-est-for-0-freq-p
                          (1+ N-nonzero-freqs)
                          N-nonzero-freqs)))))
  "given
   [1] AR-coeffs (required)
       ==> vector with real-valued AR coefficients phi_{j,p}
           for an AR(p) model of order p:
           x_t = phi_{1,p} * x_{t-1} + phi_{2,p} * x_{t-2}
                 + ... + phi_{p,p} * x_{t-p} + e_t
   [2] innovations-variance (required)
       ==> innovations variance of the process
   [3] N-nonzero-freqs (keyword; 256)
       ==> the number of nonzero frequencies at which the AR sdf
           is to be computed (need NOT be a power of 2);
           the first nonzero frequency (and also spacing
           between frequencies) is given by
           1/(2 * N-nonzero-freqs * sampling time)
           = Nyquist frequency/N-nonzero-freqs;
           the last nonzero frequency is the Nyquist frequency
   [4] sampling-time (keyword; 1.0)
       ==> sampling time (called delta t in the SAPA book)
   [5] return-est-for-0-freq-p (keyword; t)
       ==> if t, sdf is computed at zero frequency;
           otherwise, it is not computed.
   [6] sdf-transformation (keyword; #'convert-to-dB)
       ==> a function of one argument or nil;
           if bound to a function, the function is used
           to transform all elements of result-sdf
   [7] result-sdf (keyword; vector of correct length)
       <== vector of length N-nonzero-freqs (if return-est-for-0-freq-p is nil)
           or N-nonzero-freqs +1 (if return-est-for-0-freq-p is t)
           into which the properly transformed sdf is placed
   [8] return-frequencies-p (keyword; nil)
       ==> if t, the frequencies associated with the transfer function
           are computed and returned in result-freq
   [9] freq-transformation (keyword; nil)
       ==> a function of one argument or nil;
           if bound to a function, the function is used
           to transform all elements of result-freq
           (ignored unless return-frequencies-p is true)
  [10] result-freq (keyword; nil or vector of correct length)
       <== not used if return-frequencies-p nil; otherwise,
           vector of length N-nonzero-freqs (if return-est-for-0-freq-p is nil)
           or N-nonzero-freqs +1 (if return-est-for-0-freq-p is t)
           into which the frequencies associated with the values
           in result-sdf are placed
returns
   [1] result-sdf, a vector holding
       the properly transformed sdf
   [2] nil (if return-frequencies-p is nil) or
       result-freq (if return-frequencies-p is t),
       where result-freq is a vector holding
       the properly transformed frequencies
       associated with values in  result-sdf
   [3] the length of the vector result-sdf
---
Note: see Equation (392b) of the SAPA book"
  (let* ((N-dft (* 2 N-nonzero-freqs))
         (scratch (make-array N-dft))
         (offset (if return-est-for-0-freq-p 0 1))
         (N-freqs (if return-est-for-0-freq-p
                    (1+ N-nonzero-freqs)
                    N-nonzero-freqs))
         (p (length AR-coeffs))
         (num-constant (* innovations-variance sampling-time)))
    ;(declare (dynamic-extent scratch))
    (ar-coeffs->prewhitening-filter AR-coeffs :result scratch)
    (fill scratch 0.0 :start (1+ p))
    (dft! scratch)
    (dotimes (i N-freqs)
      (setf (aref result-sdf i)
            (/ num-constant
               (expt (abs (aref scratch (+ i offset))) 2))))
    ;;; convert sdf if needed
    (if sdf-transformation
      (transform-a-sequence! sdf-transformation result-sdf))
    ;;; create vector of associated frequencies ...
    (when return-frequencies-p
      (let ((freq-constant (* 2 N-nonzero-freqs sampling-time)))
        (if freq-transformation
          (dotimes (i N-freqs)
            (setf (aref result-freq i)
                  (funcall freq-transformation
                           (/ (float (+ i offset)) freq-constant))))
          (dotimes (i N-freqs)
            (setf (aref result-freq i)
                  (/ (float (+ i offset)) freq-constant))))))
    (values result-sdf result-freq N-freqs)))

#|
(multiple-value-bind (sdf freq)
                     (ar-coeffs->sdf
                      #(0.6990553262846771
                        -0.005832588617464066
                        0.15582353261696208)
                      271.77271305830277
                      :return-frequencies-p t
                      :sampling-time 0.25)
  (let ((count 0))
    (dolist (i '(0 1 2 3 4  126 127 128 129 130 252 253 254 255 256) (values))
      (format t "~&~7,5F: ~10,5F" (svref freq i) (svref sdf i))
      (if (zerop (mod (incf count) 5))
        (format t "~&...")))))
;==>
0.00000:   34.74458
0.00781:   34.69756
0.01562:   34.55951
0.02344:   34.33891
0.03125:   34.04806
...
0.98437:   17.27619
0.99219:   17.25707
1.00000:   17.23760
1.00781:   17.21776
1.01562:   17.19750
...
1.96875:   12.93563
1.97656:   12.93224
1.98437:   12.92982
1.99219:   12.92836
2.00000:   12.92788

(multiple-value-bind (sdf freq N-freq)
                     (ar-coeffs->sdf
                      #(0.6990553262846771
                        -0.005832588617464066
                        0.15582353261696208)
                      271.77271305830277
                      :N-nonzero-freqs 5
                      :return-frequencies-p t
                      :sampling-time 0.25)
  (dotimes (i N-freq (values))
    (format t "~&~7,5F: ~10,5F" (svref freq i) (svref sdf i))
    ))
;==>
0.00000:   34.74458
0.40000:   20.98899
0.80000:   17.73602
1.20000:   16.53083
1.60000:   14.12722
2.00000:   12.92788
|#

;-------------------------------------------------------------------------------
(defun ar-coeffs->sdf-at-single-freq
       (AR-coeffs
        innovations-variance
        freq
        &key
        (sampling-time 1.0)
        (sdf-transformation #'convert-to-dB))
  "given
   [1] AR-coeffs (required)
       ==> vector with real-valued AR coefficients phi_{j,p}
           for an AR(p) model of order p:
           x_t = phi_{1,p} * x_{t-1} + phi_{2,p} * x_{t-2}
                 + ... + phi_{p,p} * x_{t-p} + e_t
   [2] innovations-variance (required)
       ==> innovations variance of the process
   [3] freq (required)
       ==> the single frequency at which the sdf is
           to be computed
   [4] sampling-time (keyword; 1.0)
       ==> sampling time (called delta t in the SAPA book)
   [6] sdf-transformation (keyword; #'convert-to-dB)
       ==> a function of one argument or nil;
           if bound to a function, the function is used
           to transform the value of the sdf at freq
returns
   [1] properly transformed AR sdf evaluated at freq
   [2] nil (if return-frequencies-p is nil) or
       result-freq (if return-frequencies-p is t),
       where result-freq is a vector holding
       the properly transformed frequencies
       associated with values in  result-sdf
   [3] the length of the vector result-sdf
---
Note: see Equation (392b) of the SAPA book"
  (let ((omega (* 2 pi freq sampling-time))
        (complex-sum 1.0))
    (dotimes (k (length AR-coeffs))
      (decf complex-sum (* (aref AR-coeffs k)
                           (exp (complex 0.0 (- (* omega (1+ k))))))))
    (let ((result (/ (* innovations-variance sampling-time)
                     (expt (abs complex-sum) 2))))
      (if sdf-transformation
        (funcall sdf-transformation result)
        result))))

#|
(ar-coeffs->sdf-at-single-freq
 #(0.6990553262846771 -0.005832588617464066 0.15582353261696208)
 271.77271305830277
 (* 4/256 2.0)        ;==> 0.03125 = freq
 :sampling-time 0.25)
;==> 34.048055842320565
|#

;-------------------------------------------------------------------------------
(defun sdf->sdf-with-accurate-peaks
       (sdf
        freqs
        AR-coeffs
        innovations-variance
        &key
        (sampling-time 1.0)
        (sdf-transformation #'convert-to-dB)
        (search-method :quadratic-approx)  ; or :bisection+Newton-Raphson
        (dB-tolerance 0.1)
        (accuracy (* 10.0 single-float-epsilon))
        (maximum-number-of-iterations 20))
  "given
   [1] sdf (required)
       ==> vector with sdf for a real-valued process
           computed over a grid of frequencies (the grid
           need not be uniform) -- note that the sdf values
           can be either untransformed or transformed via
           an order preserving transformation (such as decibels)
   [2] freqs (required)
       ==> vector with frequencies corresponding to values
           in sdf (must be same size as vector with sdf values)
   [3] AR-coeffs (required)
       ==> vector with real-valued AR coefficients phi_{j,p}
           for an AR(p) model of order p:
           x_t = phi_{1,p} * x_{t-1} + phi_{2,p} * x_{t-2}
                 + ... + phi_{p,p} * x_{t-p} + e_t
   [4] innovations-variance (required)
       ==> innovations variance of the process
   [5] sampling-time (keyword; 1.0)
       ==> sampling time (called delta t in the SAPA book)
   [6] sdf-transformation (keyword; #'convert-to-dB)
       ==> a function of one argument or nil;
           if bound to a function, the function is used
           to transform any newly computed values of the sdf
   [7] search-method (keyword; :quadratic-approx)
       ==> selects between two methods to search
           for the location of peaks;
           if set to :quadratic-approx, a quadratic approximation is used;
           if set to :bisection+Newton-Raphson, an interval search
           using a combination of bisection and Newton-Raphson is used
   [8] dB-tolerance (keyword; 0.1)
       ==> convergence criterion (used if search-method
           is :quadratic-approx)
   [9] accuracy (keyword; 10 * single-float-epsilon)
       ==> convergence criterion (used if search-method
           is :bisection+Newton-Raphson)
  [10] maximum-number-of-iterations (keyword; 20)
       ==> controls the number of iterations (used if search-method
           is :bisection+Newton-Raphson)
returns
   [1] a vector with values of sdf merged
       with an additional set of values
       chosen to represent peaks accurately
   [2] the corresponding augmented vector of frequencies
   [3] the length of the two vector returned
---
Note: see pages 524--5 of the SAPA book"
  (let ((n (length sdf))
        (additional-freqs '())
        (additional-sdf '())
        (ersatz-acs (if (eq search-method :bisection+Newton-Raphson)
                      (acvs (ar-coeffs->prewhitening-filter AR-coeffs)
                            :center-data-p nil :acs-p t))))
    ;;; search for local peaks ...
    (dotimes (i (- n 2))
      (if (and (> (aref sdf (+ i 1)) (aref sdf i))
               (> (aref sdf (+ i 1)) (aref sdf (+ i 2))))
        ;;; a local peak has been found, so we attempt to
        ;;; more accurately determine its location ...
        (let* ((left-freq (aref freqs i))
               (mid-freq (aref freqs (+ i 1)))
               (right-freq (aref freqs (+ i 2)))
               (a-new-freq
                (case search-method
                  (:quadratic-approx
                   (find-peak-of-ar-sdf-using-quadratic-approx
                    left-freq mid-freq right-freq
                    AR-coeffs
                    :sampling-time sampling-time
                    :dB-tolerance dB-tolerance))
                  (:bisection+Newton-Raphson
                   (find-peak-or-valley-of-sdf-using-bisection+Newton-Raphson
                    left-freq
                    right-freq
                    ersatz-acs
                    :sampling-time sampling-time
                    :accuracy accuracy 
                    :maximum-number-of-iterations maximum-number-of-iterations))
                  (otherwise
                   (error ":search-method (~A) must be set to :quadratic-approx or :bisection+Newton-Raphson"
                          search-method)))))
          ;;; use a-new-freq only if it is distinct from all
          (if (not (member a-new-freq `(,left-freq ,mid-freq ,right-freq)
                           :test #'=))
            (let ((a-symmetric-freq
                   (find-symmetric-frequency
                    left-freq mid-freq right-freq a-new-freq)))
              (cond
               (a-symmetric-freq
                (push (min a-new-freq a-symmetric-freq) additional-freqs)
                (push (ar-coeffs->sdf-at-single-freq
                       AR-coeffs
                       innovations-variance
                       (car additional-freqs)
                       :sampling-time sampling-time
                       :sdf-transformation sdf-transformation)
                      additional-sdf)
                (push (max a-new-freq a-symmetric-freq) additional-freqs)
                (push (ar-coeffs->sdf-at-single-freq
                       AR-coeffs
                       innovations-variance
                       (car additional-freqs)
                       :sampling-time sampling-time
                       :sdf-transformation sdf-transformation)
                      additional-sdf))
               (t
                (push a-new-freq additional-freqs)
                (push (ar-coeffs->sdf-at-single-freq
                       AR-coeffs
                       innovations-variance
                       a-new-freq
                       :sampling-time sampling-time
                       :sdf-transformation sdf-transformation)
                      additional-sdf))))))))
    ;;; all local peaks have now been found,
    ;;; so we now merge the new material with the old ...
    (cond
     ((plusp (length additional-freqs))
      (setf additional-freqs (reverse additional-freqs)
            additional-sdf (reverse additional-sdf))
      (let* ((n-additional (length additional-freqs))
             (n-total (+ n n-additional))
             (combined-freqs (make-array n-total))
             (combined-sdf (make-array n-total))
             (k 0)
             (k-add 0))
        (dotimes (i n-total (values combined-sdf combined-freqs n-total))
          (cond
           ((or (= k-add n-additional)
                (< (aref freqs k) (elt additional-freqs k-add)))
            (setf (aref combined-freqs i) (aref freqs k)
                  (aref combined-sdf i) (aref sdf k))
            (incf k))
           (t
            (setf (aref combined-freqs i) (elt additional-freqs k-add)
                  (aref combined-sdf i) (elt additional-sdf k-add))
            (incf k-add))))))
     (t
      (values sdf freqs n)))))

#|
;;; coefficients are for the AR(4) process
;;; described by Equation (46a) and shown in Figure 148
;;; of the SAPA book ...
(multiple-value-bind (sdf freqs N-freq)
                     (ar-coeffs->sdf
                      #(2.7607 -3.8106  2.6535 -0.9238)
                      1.0
                      :N-nonzero-freqs 50
                      :return-frequencies-p t)
  (dotimes (i N-freq)
    (if (<= 0.0999 (svref freqs i) 0.1401)
      (format t "~&~7,5F: ~10,5F"
              (svref freqs i)
              (svref sdf i))))
  (format t "~&finding peaks using quadratic approximation ...")
  (multiple-value-bind (extended-sdf extended-freq N-freq)
                       (sdf->sdf-with-accurate-peaks
                        sdf
                        freqs
                        #(2.7607 -3.8106  2.6535 -0.9238)
                        1.0)
    (dotimes (i N-freq (values))
      (if (<= 0.0999 (svref extended-freq i) 0.1401)
        (format t "~&~7,5F: ~10,5F"
                (svref extended-freq i)
                (svref extended-sdf i)))))
  (format t "~&finding peaks using bisection + Newton-Raphson ...")
  (multiple-value-bind (extended-sdf extended-freq N-freq)
                       (sdf->sdf-with-accurate-peaks
                        sdf
                        freqs
                        #(2.7607 -3.8106  2.6535 -0.9238)
                        1.0
                        :search-method :bisection+Newton-Raphson)
    (dotimes (i N-freq (values))
      (if (<= 0.0999 (svref extended-freq i) 0.1401)
        (format t "~&~7,5F: ~10,5F"
                (svref extended-freq i)
                (svref extended-sdf i))))))
;==>
0.10000:   31.48832
0.11000:   43.67693
0.12000:   36.12925
0.13000:   35.59567
0.14000:   42.14003
finding peaks using quadratic approximation ...
0.10000:   31.48832
0.11000:   43.67693
0.11066:   43.61783
0.11133:   43.24097
0.12000:   36.12925
0.13000:   35.59567
0.13903:   42.05075
0.13951:   42.18959
0.14000:   42.14003
finding peaks using bisection + Newton-Raphson ...
0.10000:   31.48832
0.11000:   43.67693
0.11022:   43.69726
0.11044:   43.67740
0.12000:   36.12925
0.13000:   35.59567
0.13928:   42.14315
0.13964:   42.19592
0.14000:   42.14003

;;; Let's see if bisection + Newton-Raphson did a decent job on first peak ...
(dolist (a-freq '(0.11021 0.11022 0.11023) (values))
  (print (ar-coeffs->sdf-at-single-freq
          #(2.7607 -3.8106  2.6535 -0.9238)
          1.0
          a-freq)))
;==>
43.69721694178745 
43.6972567318079 
43.697213122598804
;;; ... on second peak ...
(dolist (a-freq '(0.13963 0.13964 0.13965) (values))
  (print (ar-coeffs->sdf-at-single-freq
          #(2.7607 -3.8106  2.6535 -0.9238)
          1.0
          a-freq)))
;==>
42.195895846811524 
42.19592320450518 
42.19586698000828 
;;; ... results look quite good
|#

;-------------------------------------------------------------------------------
;;; Because display of AR sdf's is problematic due to the possibility
;;; of missing very sharp peaks, it is useful to compare the variance
;;; of a displayed sdf with the sample variance for a time series.
;;; If the sdf is evaluated over an irregular grid of frequencies from
;;; f = 0 to f = Nyquist, the following function may be used to compute
;;; its corresponding variance.  It uses a trapazoid approximation.
(defun integrate-sdf
       (sdf
        freqs
        &key
        (fiddle-factor 2.0)
        (return-integrated-sdf-p nil)
        (result (when return-integrated-sdf-p
                  (make-array (length sdf)))))
  "given
   [1] sdf (required)
       ==> vector with sdf for a real or complex-valued process
           computed over a grid of frequencies (the grid
           need not be uniform) -- note that the sdf values
           must be untransformed (e.g., not in decibels)
   [2] freqs (required)
       ==> vector with frequencies over which
           sdf has been computed
           (must be same size as vector with sdf values)
   [3] fiddle-factor (keyword; 2.0)
       ==> values returned are all multiplied by this factor;
           the default of 2 is useful for real-valued processes
           since these have symmetric two-sided sdf's.
   [4] return-integrated-sdf-p (keyword; nil)
       ==> if t, integrated sdf as a function of frequency
           is computed and returned in result
   [5] result (keyword; vector of length sdf)
       <== vector with integrated sdf (if return-integrated-sdf-p is t)
           or nil (if return-integrated-sdf-p is nil)
returns
   [1] the integral of the sdf * fiddle-factor
   [2] result, i.e., the integrated sdf
       if return-integrated-sdf-p is t" 
  (let ((sum 0.0)
        delta-f
        the-increment)
    (if return-integrated-sdf-p (setf (svref result 0) 0.0))
    (dotimes (i (1- (length sdf)) (values (* fiddle-factor sum) result))
      (setf delta-f (- (aref freqs (1+ i)) (aref freqs i))
            ;;; area of "supporting" rectangle + area of triangle on top of it
            the-increment (+ (* delta-f (min (aref sdf (1+ i))
                                             (aref sdf i)))
                             (* 0.5 delta-f (abs (- (aref sdf (1+ i))
                                                    (aref sdf i))))))
      (incf sum the-increment)
      (if return-integrated-sdf-p
        (setf (svref result (1+ i))
              (+ (* fiddle-factor the-increment) (svref result i)))))))

#|
;;; coefficients are for the AR(4) process
;;; described by Equation (46a) and shown in Figure 148
;;; of the SAPA book ...
(ar-coeffs->variance
 #(2.7607 -3.8106  2.6535 -0.9238)
 1.0)
;==> 761.7172900314962
(dolist (n-freq '(50 128 512) (values))
  (format t "~&number of nonzero frequencies = ~D:" n-freq)
  (multiple-value-bind (sdf freqs)
                       (ar-coeffs->sdf
                        #(2.7607 -3.8106  2.6535 -0.9238)
                        1.0
                        :N-nonzero-freqs n-freq
                        :return-frequencies-p t
                        :sdf-transformation nil)
    (print (integrate-sdf sdf freqs))
    (multiple-value-bind (extended-sdf extended-freq)
                         (sdf->sdf-with-accurate-peaks
                          sdf
                          freqs
                          #(2.7607 -3.8106  2.6535 -0.9238)
                          1.0
                          :sdf-transformation nil)
      (print (integrate-sdf extended-sdf extended-freq)))
    (multiple-value-bind (extended-sdf extended-freq)
                         (sdf->sdf-with-accurate-peaks
                          sdf
                          freqs
                          #(2.7607 -3.8106  2.6535 -0.9238)
                          1.0
                          :sdf-transformation nil
                          :search-method :bisection+Newton-Raphson)
      (print (integrate-sdf extended-sdf extended-freq)))))
;==>
number of nonzero frequencies = 50:
1005.8010282299119 
1019.4741312708759 
1023.8046237341628 
number of nonzero frequencies = 128:
765.713758711881 
796.38238484366 
796.5233656088724 
number of nonzero frequencies = 512:
761.7172884723057 
762.1844965261544 
762.155417339912 
;;; Note that the equally spaced grid here did better than
;;; either of the unequally spaced grids (created to get a better
;;; representation of the two spectral peaks) -- this doesn't
;;; always happen!
|#

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;;  The functions  find-peak-of-ar-sdf-using-quadratic-approx
;;;                 find-peak-or-valley-of-sdf-using-bisection+Newton-Raphson
;;;  can be used to determine the frequencies at which an sdf has a peak
;;;  or a valley.  For details, see pages 479--80 and 524--5 of the SAPA book.
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
(defun find-peak-of-ar-sdf-using-quadratic-approx
       (f_1
        f_2
        f_3
        AR-coeffs
        &key
        (sampling-time 1.0)
        (dB-tolerance 0.1))
  "given
   [1] f_1 (required)
       ==> frequency f_1 such that f_1 < f_2 and S(f_1) < S(f_2),
           where S(.) is the sdf associated with AR-coeffs
   [2] f_2 (required)
       ==> middle frequency
   [3] f_3 (required)
       ==> frequency f_3 such that f_3 > f_2 and S(f_3) < S(f_2)
   [4] AR-coeffs (required)
       ==> vector with real-valued AR coefficients phi_{j,p}
           for an AR(p) model of order p:
           x_t = phi_{1,p} * x_{t-1} + phi_{2,p} * x_{t-2}
                 + ... + phi_{p,p} * x_{t-p} + e_t
   [5] sampling-time (keyword; 1.0)
       ==> sampling time (called delta t in the SAPA book)
   [6] dB-tolerance (keyword; 0.1)
       ==> convergence criterion
returns
   [1] estimate of frequency of peak/valley in sdf
---
Note: for details on this method, see pages 20-21 of Adby and Dempster,
      ``Introduction to Optimization Methods''; note in particular that
      we search the inverse of the AR portion of the spectrum instead
      of the spectrum itself in the belief that the quadratic approximation
      is better with this reformulation"
   (let* ((S_1 (/ (ar-coeffs->sdf-at-single-freq
                   AR-coeffs 1.0 f_1
                   :sampling-time sampling-time
                   :sdf-transformation nil)))
          (S_2 (/ (ar-coeffs->sdf-at-single-freq
                   AR-coeffs 1.0 f_2
                   :sampling-time sampling-time
                   :sdf-transformation nil)))
          (S_3 (/ (ar-coeffs->sdf-at-single-freq
                   AR-coeffs 1.0 f_3
                   :sampling-time sampling-time
                   :sdf-transformation nil)))
          (a_23 (- f_2 f_3))
          (a_31 (- f_3 f_1))
          (a_12 (- f_1 f_2))
          (b_23 (- (* f_2 f_2) (* f_3 f_3)))
          (b_31 (- (* f_3 f_3) (* f_1 f_1)))
          (b_12 (- (* f_1 f_1) (* f_2 f_2)))
          (f_4 (/ (+ (* b_23 S_1) (* b_31 S_2) (* b_12 S_3))
                  (* 2.0 (+ (* a_23 S_1) (* a_31 S_2) (* a_12 S_3)))))
          (S_4 (/ (ar-coeffs->sdf-at-single-freq
                   AR-coeffs 1.0 f_4
                   :sampling-time sampling-time
                   :sdf-transformation nil))))
    (cond
     ((< (abs (convert-to-dB (/ S_2 S_4))) dB-tolerance)
      (values f_4))
     ((and (< f_1 f_4 f_2) (< S_4 S_1) (< S_4 S_2))
      (find-peak-of-ar-sdf-using-quadratic-approx
       f_1 f_4 f_2
       AR-coeffs
       :sampling-time sampling-time
       :dB-tolerance dB-tolerance))
     ((and (< f_2 f_4 f_3) (< S_4 S_2) (< S_4 S_3))
      (find-peak-of-ar-sdf-using-quadratic-approx
       f_2 f_4 f_3
       AR-coeffs
       :sampling-time sampling-time
       :dB-tolerance dB-tolerance))
     ((and (< f_2 f_4 f_3) (< S_2 S_4))
      (find-peak-of-ar-sdf-using-quadratic-approx
       f_1 f_2 f_4
       AR-coeffs
       :sampling-time sampling-time
       :dB-tolerance dB-tolerance))
     ((and (< f_1 f_4 f_2) (< S_2 S_4))
      (find-peak-of-ar-sdf-using-quadratic-approx
       f_4 f_2 f_3
       AR-coeffs
       :sampling-time sampling-time
       :dB-tolerance dB-tolerance))
     (t
      (format t "~&find-peak-of-ar-sdf-using-quadratic-approx: non-convergence at f = ~F, ~F, ~F"
              f_1 f_2 f_3)
      (values f_2)))))

#|
;;; coefficients are for the AR(4) process
;;; described by Equation (46a) and shown in Figure 148
;;; of the SAPA book ...
(find-peak-of-ar-sdf-using-quadratic-approx
 0.11000
 0.11025
 0.11050
 #(2.7607 -3.8106  2.6535 -0.9238)
 )
;==> 0.11022137202732714
|#

;-------------------------------------------------------------------------------
(defun find-peak-or-valley-of-sdf-using-bisection+Newton-Raphson
       (f-left
        f-right
        ersatz-acvs
        &key
        (sampling-time 1.0)
        (N (length ersatz-acvs))
        (accuracy (* 10.0 single-float-epsilon))
        (maximum-number-of-iterations 20))
  "given
   [1] f-left (required)
       ==> left bracket  for frequency of peak/valley
   [2] f-right (required)
       ==> right bracket for frequency of peak/valley
   [3] ersatz-acvs (required)
       ==> vector of length N with acvs (or acs) s_0 to s_{N-1}
           corresponding to sdf (only elements 1 to N-1 are used)
   [4] sampling-time (keyword; 1.0)
       ==> positive number
   [5] N (keyword; length of ersatz-acvs)
       ==> positive integer
   [6] accuracy (keyword; 10.0 * single-float-epsilon)
       ==> passed to bisection-with-Newton-Raphson
   [7] maximum-number-of-iterations (keyword; 20)
       ==> passed to bisection-with-Newton-Raphson
returns
   [1] estimate of frequency of peak/valley in sdf
   [2] number of iterations required
---
Note: searches for peak using a combination of
bisection and Newton-Raphson;
see pages 479--80 and 524--5 of the SAPA book"
  (let ((N-1 (1- N)))
    (multiple-value-bind (omega-answer iteration-count)
                         (bisection-with-Newton-Raphson
                          #'(lambda (omega)
                              (let ((first-deriv 0.0)
                                    (tau 0))
                                (dotimes (tau-1 N-1 (* -2.0 first-deriv))
                                  (incf tau)
                                  (incf first-deriv
                                        (* tau
                                           (elt ersatz-acvs tau)
                                           (sin (* tau omega)))))))
                          #'(lambda (omega)
                              (let ((second-deriv 0.0)
                                    (tau 0))
                                (dotimes (tau-1 N-1 (* -2.0 second-deriv))
                                  (incf tau)
                                  (incf second-deriv
                                        (* tau tau
                                           (elt ersatz-acvs tau)
                                           (cos (* tau omega)))))))
                          (* 2 pi f-left sampling-time)
                          (* 2 pi f-right sampling-time)
                          :accuracy accuracy
                          :maximum-number-of-iterations
                          maximum-number-of-iterations)
      (values (/ omega-answer (* 2 pi sampling-time)) iteration-count))))

#|
;;; coefficients are for the AR(4) process
;;; described by Equation (46a) and shown in Figure 148
;;; of the SAPA book ...
(find-peak-or-valley-of-sdf-using-bisection+Newton-Raphson
 0.1100
 0.1105
 (acvs (ar-coeffs->prewhitening-filter #(2.7607 -3.8106  2.6535 -0.9238))
                            :center-data-p nil :acs-p t)
 )
;==> 0.1102197683614135
;    6
|#

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;;  The functions  generate-forward-innovations
;;;                 generate-backward-innovations
;;;  compute the observed forward and backward prediction errors for
;;;  a given time series and AR prewhitening filter.
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
(defun generate-forward-innovations
       (time-series
        AR-coeffs
        &key
        (start 0)
        (end (length time-series))
        (center-data t)
        (result (make-array (- (- end start) (length AR-coeffs)))))
  "given
   [1] time-series (required)
       ==> a vector containing a real-valued 
           time series x_t
   [2] AR-coeffs (required)
       ==> vector with real-valued AR coefficients phi_{j,p}
           for an AR(p) model of order p:
           x_t = phi_{1,p} * x_{t-1} + phi_{2,p} * x_{t-2}
                 + ... + phi_{p,p} * x_{t-p} + e_t
   [3] start (keyword; 0)
       ==> start index of vector to be used
   [4] end (keyword; length of the-seq)
       ==> 1 + end index of vector to be used
   [5] center-data (keyword; t)
       ==> if t, subtract sample mean from time series;
           if a number, subtracts that number from time series
           (the vector time-series is not altered)
   [6] result (keyword; vector of appropriate length)
       <== vector to contain forward innovations
                            p
           f_t = x_{t+p} - SUM phi_{j,p} x_{t-j},  t = 1, ..., N - p
                           j=1
returns
   [1] result, i.e., the fprward innovations"
  (filter-time-series
   (center&taper-time-series time-series
                       :center-data center-data
                       :start start
                       :end end)
   (ar-coeffs->prewhitening-filter AR-coeffs)
   :result result))

#|
(generate-forward-innovations
 #(71 63 70 88 99 90 110 135 128 154 156 141 131 132 141 104 136 146 124 129)
 #(0.7475191644352183 -0.01583855174510637 0.46440844342490983 -0.4428652096133231)
 )
;==> #(7.541189677934277 -16.190207980322164 5.452393054435857 18.222543695391344 1.9025289159837548 20.25717110563984 -0.04210392524786499 -1.8028505644380886 -15.73306199076052 3.09022994051165 19.03618232922566 -26.674545308896946 28.233150201353947 9.989699743647158 2.1902410483799546 -7.453034761884252)
;    16
;;; good agreement with what two-step-burg-algorithm gave ...
|#

;-------------------------------------------------------------------------------
(defun generate-backward-innovations
       (time-series
        AR-coeffs
        &key
        (start 0)
        (end (length time-series))
        (center-data t)
        (result (make-array (- (- end start) (length AR-coeffs)))))
  "given
   [1] time-series (required)
       ==> a vector containing a real-valued 
           time series x_t
   [2] AR-coeffs (required)
       ==> vector with real-valued AR coefficients phi_{j,p}
           for an AR(p) model of order p:
           x_t = phi_{1,p} * x_{t-1} + phi_{2,p} * x_{t-2}
                 + ... + phi_{p,p} * x_{t-p} + e_t
   [3] start (keyword; 0)
       ==> start index of vector to be used
   [4] end (keyword; length of the-seq)
       ==> 1 + end index of vector to be used
   [5] center-data (keyword; t)
       ==> if t, subtract sample mean from time series;
           if a number, subtracts that number from time series
           (the vector time-series is not altered)
   [6] result (keyword; vector of appropriate length)
       <== vector to contain backward innovations
                        p
           b_t = x_t - SUM phi_{j,p} x_{t+j},  t = 1, ..., N-p
                       j=1
returns
   [1] result, i.e., the backward innovations"
  (filter-time-series
   (center&taper-time-series time-series
                       :center-data center-data
                       :start start
                       :end end)
   (reverse (ar-coeffs->prewhitening-filter AR-coeffs))
   :result result))

#|
(generate-backward-innovations
 #(71 63 70 88 99 90 110 135 128 154 156 141 131 132 141 104 136 146 124 129)
 #(0.7475191644352183 -0.01583855174510637 0.46440844342490983 -0.4428652096133231)
 )
;==> #(-0.9808164276349629 -23.022636411463495 -16.266777119010605 -4.8485735216690795 -1.514397559765996 -10.303462500921928 -20.291200583839146 2.7814408815304876 -21.084905735054566 8.26942730376083 24.845207695933038 -3.2294508544868163 17.520376061131927 0.7742590731821384 23.55218276738641 -24.778733173674876)
;    16
;;; good agreement with what two-step-burg-algorithm gave ...
|#

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;;  The function   regression-with-ar-errors
;;;  does linear regression with AR errors.
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
(defun regression-with-ar-errors
       (time-series
        ind-vars
        p
        &key
        (AR-coeff-estimator #'burg-algorithm)
        (maximum-number-of-iterations 5)
        (eps 1.0e-7)
        (trace-AR-parameter-estimates-p nil)
        (trace-LS-parameter-estimates-p nil))
  "given
   [1] time-series (required)
       ==> a vector with dependent variables
   [2] ind-vars (required)
       ==> a list of vector(s) with independent variable(s)
   [3] p (required)
       ==> autoregressive order
   [4] AR-coeff-estimator (keyword; #'burg-algorithm)
       ==> AR estimation procedure
   [5] maximum-number-of-iterations (keyword; 5)
       ==> ... like I said ...
   [6] eps (keyword; 1.0e-7)
       ==> a small number -- used to test for convergence
           in estimates of both regression and AR coefficients
   [7] trace-AR-parameter-estimates-p (keyword; nil)
       ==> if t, results of each iteration
           on AR parameters are printed
   [8] trace-LS-parameter-estimates-p (keyword; nil)
       ==> if t, results of each iteration
           on least squares parameters are printed
returns
   [1] estimates of least squares coefficients
   [2] estimates of AR coefficients
   [3] variance of residuals
   [4] either number of iterations required for convergence
       or nil if maximum number of iterations completed
       but convergence not yet achieved
---
Note: this function is based on Newton, TIMESLAB, page 241;
      it has NOT been thoroughly tested (particularly with
      regard to the convergence criterion),
      so a heavy dose of the usual ``caveat emptor''
      is appropriate here!!!"
  ;;; First, we obtain the ordinary least squares estimates;
  ;;; the least squares residuals are pointed to by transformed-ts
  ;;; and should resemble a realization of an AR(p) process.
  (flet ((trace-parameter-estimates
             (estimates iter-tag label)
           (format t "~&~A, iteration ~D:" label (1+ iter-tag))
           (dotimes (j (length estimates))
             (format t "~&   ~D:  ~F" j (elt estimates j)))))
    (multiple-value-bind (ols-estimates transformed-ts)
                         (ordinary-least-squares-Cholesky
                          time-series
                          ind-vars
                          :compute-residuals-p t)
      (when trace-LS-parameter-estimates-p
        (trace-parameter-estimates
         ols-estimates -1 "LS estimates"))
      (let (previous-ols-estimates
            (transformed-ind-vars '())
            (n (length time-series))
            (n-ind (length ind-vars))
            (current-whitening-filter (make-array (1+ p)))
            current-filter-length
            current-pev-factor
            an-ind-var-to-adjust an-ind-var
            innovations-variance
            AR-coeffs
            previous-AR-coeffs)
        ;;; Create space for transformed independent variables ...
        (dotimes (i n-ind)
          (push (make-array n) transformed-ind-vars))
        ;;; Loop until there is little change in LS and AR parameter estimates ...
        (dotimes (iter maximum-number-of-iterations
                       (values ols-estimates
                               AR-coeffs
                               (ar-coeffs->variance
                                AR-coeffs
                                innovations-variance)
                               nil))
          (setf previous-ols-estimates ols-estimates
                previous-AR-coeffs AR-coeffs)
          ;;; Fit the AR model to the current set of LS residuals
          (multiple-value-setq (AR-coeffs
                                innovations-variance)
            (funcall AR-coeff-estimator transformed-ts p :center-data nil))
          (when trace-AR-parameter-estimates-p
            (trace-parameter-estimates
             AR-coeffs iter "AR estimates"))
          (multiple-value-bind (list-of-pe-coeff list-of-pev)
                               (step-down-Levinson-Durbin-recursions
                                AR-coeffs
                                innovations-variance
                                :process-variance-p nil)
            ;;; Transform the original time series and vectors of independent
            ;;; variables in accordance with fitted AR(p) model ...
            (dotimes (i n)
              ;;; Normalized prediction error filter is constant
              ;;; for i greater than or equal to p ...
              (when (<= i p)
                (setf current-pev-factor (/ (sqrt (elt list-of-pev i)))
                      current-filter-length (1+ i)
                      (aref current-whitening-filter 0) current-pev-factor)
                (dotimes (j i)
                  (setf (aref current-whitening-filter (1+ j))
                        (* (- (aref (elt list-of-pe-coeff (1- i)) j))
                           current-pev-factor))))
              (setf (aref transformed-ts i) 0.0)
              (dotimes (j current-filter-length)
                (incf (aref transformed-ts i)
                      (* (aref time-series (- i j))
                         (aref current-whitening-filter j))))
              (dotimes (k n-ind)
                (setf an-ind-var-to-adjust (elt transformed-ind-vars k)
                      an-ind-var (elt ind-vars k))
                (setf (aref an-ind-var-to-adjust i) 0.0)
                (dotimes (j current-filter-length)
                  (incf (aref an-ind-var-to-adjust i)
                        (* (aref an-ind-var (- i j))
                           (aref current-whitening-filter j))))))
            ;;; Apply ordinary least squares to obtain new least squares
            ;;; estimates for the regression parameters ...
            (setf ols-estimates (ordinary-least-squares-Cholesky
                                 transformed-ts
                                 transformed-ind-vars))
            (when trace-LS-parameter-estimates-p
              (trace-parameter-estimates
               ols-estimates iter "LS estimates"))
            ;;; convergence test (needs to be tuned up ...)
            (if (and previous-AR-coeffs
                     (< (compare-seqs
                         previous-ols-estimates
                         ols-estimates)
                        eps)
                     (< (compare-seqs
                         previous-AR-coeffs
                         AR-coeffs)
                        eps))
              (return (values ols-estimates
                              AR-coeffs
                              (ar-coeffs->variance
                                AR-coeffs
                                innovations-variance)
                              (1+ iter))))
            ;;; Compute the residuals with respect to the original
            ;;; dependent and independent variables ...
            (copy-vector time-series transformed-ts)
            (dotimes (i n)
              (dotimes (j n-ind)
                (decf (aref transformed-ts i)
                      (* (aref ols-estimates j)
                         (aref (elt ind-vars j) i)))))))))))

#|
;;; coefficients are for the AR(2) process
;;; described by Equation (45) of the SAPA book ...
(regression-with-ar-errors
 (x+y! (sample-from-a-function #'(lambda (x) (+ pi (* 7 x)))
                               :N 500)
       (generate-ar-time-series
        #(0.75 -0.5)
        1.0
        500
        :process-variance-p t))
 (list (make-array 500 :initial-element 1.0)
       (iota 0 499 :type 'vector))
 2
 :trace-AR-parameter-estimates-p t
 :trace-LS-parameter-estimates-p t)
;==>
LS estimates, iteration 0:
   0:  3.1724362189599566
   1:  6.999820737703846
AR estimates, iteration 1:
   0:  0.6959987596867
   1:  -0.5249062346802079
LS estimates, iteration 1:
   0:  3.174976778340132
   1:  6.999811113561525
AR estimates, iteration 2:
   0:  0.6959967546900704
   1:  -0.52491167609844
LS estimates, iteration 2:
   0:  3.1749768748282645
   1:  6.999811113149029
AR estimates, iteration 3:
   0:  0.6959967546455309
   1:  -0.5249116762847136
LS estimates, iteration 3:
   0:  3.174976874824981
   1:  6.99981111314904
#(3.174976874824981 6.99981111314904)      ;truth is pi   and  7
#(0.6959967546455309 -0.5249116762847136)  ;truth is 0.75 and -0.5
0.9537407718258482                         ;truth is 1.0
3
;;; Note: don't expect agreement with the above numbers
;;;       (due to implementation dependence on random number
;;;       generation)
|#

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;;  Everything below here consists of internal symbols in the SAPA package
;;;  and should be regarded as "dirty laundry" ...
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;; handles the different conventions for AR parameters
(defun flip-sign-of-coefficients! (a-sequence)
  (map-into a-sequence #'(lambda (x) (* -1 x)) a-sequence))

;-------------------------------------------------------------------------------
(defun one-iteration-of-two-step-Burg (k step N Fk Bk phi limit)
  ;;; Input:
  ;;;  k	Order of the AR model fit so far (integer)
  ;;;  step	Factor relating AR coefficients to time lag; i.e. j (integer)
  ;;;  N	Number of data points in the original sequence (integer)
  ;;;  Fk	kth order forward differences (real*8 vector of size N; 1:N)
  ;;;  Bk	kth order backward differences (real*8 vector of size N; 1:N)
  ;;;  phi	The Autoregresive model coefficients for original order
  ;;;           (real vector of size k; 1:p)
  ;;;  limit	limit on the magnitude of the piviot elements (real)
  ;;;
  ;;;  Output:
  ;;;  Fk	forward differences that correspond to the new order (updated)
  ;;;  Bk	Backward differences that correspond to the new order (updated)
  ;;;  phi	The Autoregresive model coefficients for new order (updated)
  ;;;
  ;;; Model: X(j)= phi(1)*X(j-1*step) + ... + X(j-k*step) + epsilon(j)
  (let ((f (make-array 3 :initial-element 0.0))
        (g (make-array 3 :initial-element 0.0))
        (f-prime (make-array 2))
        (g-prime (make-array 2))
        (f*f (make-array 5))
        (f*g (make-array 5))
        (g*g (make-array 5))
        (f-prime*f*f (make-array 6))
        (g-prime*f*g (make-array 6))
        (f-prime*g*g (make-array 6))       
        (A (make-array 6))   ;A is f'*f**2 - 2*g'*f*g + f'*g**2
        (A-flip (make-array 6))
        (roots (make-array 6))
        (new-order (+ k 2))
        (degree-of-A 0)
        j F0 F1 B0 B1 a-small-number)
    (dotimes (j-times (1+ (- N (* step (1+ new-order)))))
      (setf j (+ j-times (1- (* step (1+ new-order)))))
      (setf F0 (aref Fk j))  ; FO = F_k-2
      (setf F1 (aref Fk (- j step)))  ;F1= S_1*step F_k-2
      (setf B0 (aref Bk (- j (* new-order step))))  ;B0= S_k*step B_k-2
      (setf B1 (aref Bk (- j (* (1- new-order) step))))  ;B1= S_(k-1)*step B_k-2
      
      (incf (aref f 0) (+ (* F0 F0) (* B0 B0)))
      (decf (aref f 1) (+ (* 2 F0 B1) (* 2 B0 F1)))
      (incf (aref f 2) (+ (* F1 F1) (* B1 B1)))

      (incf (aref g 0) (* 2 F0 B0))
      (decf (aref g 1) (+ (* 2 F0 F1) (* 2 B0 B1)))
      (incf (aref g 2) (* 2 F1 B1)))  ;end of 100

    (cond
     ((= new-order 2)
      ;;; In this case we must solve f-prime = 0.
      (setf (aref A 0) (aref f 1))
      (setf (aref A 1) (* 2 (aref f 2)))
      (setf degree-of-A 1))
     (t
      ;;; In this case we must solve Equation 7:
      ;;; f'*f**2 - 2*g'*f*g + f'*g**2 = 0
      (setf (aref f-prime 0) (aref f 1))
      (setf (aref f-prime 1) (* 2 (aref f 2)))
      (setf (aref g-prime 0) (aref g 1))
      (setf (aref g-prime 1) (* 2 (aref g 2)))
      (multiply-2-polynomials f f :product-polynomial f*f)
      (multiply-2-polynomials f-prime f*f :product-polynomial f-prime*f*f)
      (multiply-2-polynomials f g :product-polynomial f*g)
      (multiply-2-polynomials g-prime f*g :product-polynomial g-prime*f*g)
      (multiply-2-polynomials g g :product-polynomial g*g)
      (multiply-2-polynomials f-prime g*g :product-polynomial f-prime*g*g)
      ;;; A= f'*f**2 - 2*g'*f*g + f'*g**2
      (setf degree-of-A 0)
      (dotimes (j 6)
        (setf (aref A j)
              (- (+ (aref f-prime*f*f j) (aref f-prime*g*g j))
                 (* 2 (aref g-prime*f*g j)))))
      (setf a-small-number (* (absolute-sum-of-vector-elements A) 1.0e-5))
      (dotimes (j 6)
        (if (> (abs (aref A j)) a-small-number)
          (setf degree-of-A j)))))

    ;;; Convention conflict: must flip A for use with zeros-of-polynomial
    (dotimes (j (1+ degree-of-A))
      (setf (aref A-flip (- degree-of-A j)) (aref A j)))
    ;;; HACK: need to examine condition code and exit if roots
    ;;;       were not properly found
    (zeros-of-polynomial A-flip
                         :degree-of-polynomial degree-of-A
                         :the-roots roots)

    ;;; Check the roots for minimum of h= f - g**2/f
    ;;; Start with hmin as value on boundary; i.e. +/- one
    (let* ((pmin limit)
           (hmin (h-func f g pmin))
           (ptry (- limit))
           (htry (h-func f g ptry))
           (sum-of-squares-new-order-minus-1 0.0)
           (sum-of-squares 0.0)
           reflection-coefficient-new-order-minus-1)
      (cond
       ((< htry hmin)
        (setf pmin ptry)
        (setf hmin htry)))

      (dotimes (j degree-of-A)
        (setf ptry (realpart (aref roots j)))
        (cond
         ((<= (abs ptry) limit)
          (setf htry (h-func f g ptry))
          (cond
           ((< htry hmin)
            (setf pmin ptry)
            (setf hmin htry))))))   ;end of 220
           
      ;;; Set new pivot elements
      (setf reflection-coefficient-new-order-minus-1
            (setf (aref phi (- new-order 2)) pmin))
      (setf (aref phi (1- new-order))
            (/ (eval-poly-2 g pmin) (eval-poly-2 f pmin)))

     ;;; Set the new differences
      (dotimes (j-times (1+ (- N (* step new-order))))
        (setf j (+ j-times (1- (* step new-order))))
        (setf F0 (aref Fk j))
        (setf B0 (aref Bk (- j (* (1- new-order) step))))
	(setf (aref Fk j) (- F0 (* (aref phi (- new-order 2)) B0)))
        (setf (aref Bk (- j (* (1- new-order) step)))
              (- B0 (* (aref phi (- new-order 2)) F0)))
        (incf sum-of-squares-new-order-minus-1
              (+ (expt (aref Fk j) 2)
                 (expt (aref Bk (- j (* (1- new-order) step))) 2))))

      (dotimes (j-times (1+ (- N (* step (1+ new-order)))))
        (setf j (+ j-times (1- (* step (1+ new-order)))))
        (setf F0 (aref Fk j))
        (setf B0 (aref Bk (- j (* new-order step))))
	(setf (aref Fk j) (- F0 (* (aref phi (1- new-order)) B0)))
        (setf (aref Bk (- j (* new-order step)))
              (- B0 (* (aref phi (1- new-order)) F0)))
        (incf sum-of-squares
              (+ (expt (aref Fk j) 2)
                 (expt (aref Bk (- j (* new-order step))) 2))))

      ;;; Set New autoregressive coefficients
      (let (phij phikj)
        (dotimes (j (truncate (1- new-order) 2))
          (setf	phij (aref phi j))
          (setf phikj (aref phi (- new-order j 3)))
          (setf (aref phi j) (- phij (* (aref phi (- new-order 2)) phikj)))
          (setf (aref phi (- new-order j 3))
                (- phikj (* (aref phi (- new-order 2)) phij))))

        (dotimes (j (truncate new-order 2))
          (setf	phij (aref phi j))
          (setf phikj (aref phi (- new-order j 2)))
          (setf (aref phi j) (- phij (* (aref phi (1- new-order)) phikj)))
          (setf (aref phi (- new-order j 2))
                (- phikj (* (aref phi (1- new-order)) phij))))

        (values
         ;;; rms of forward and backward prediction errors
         (/ sum-of-squares (* 2.0 (- (1+ N) (* (1+ new-order) step))))
         (/ sum-of-squares-new-order-minus-1
            (* 2.0 (- (1+ N) (* new-order step))))
         reflection-coefficient-new-order-minus-1
         new-order
         )))))

;-------------------------------------------------------------------------------
(defun h-func (f g x)
  ;;; h(p)= fpoly(p) - gpoly(p)**2/fpoly(p)
  (let ((temp (eval-poly-2 f x)))
    (- temp
       (/ (expt (eval-poly-2 g x) 2)
          temp))))

;-------------------------------------------------------------------------------
(defun absolute-sum-of-vector-elements (vector)
  #-allegro (reduce #'+ vector :key #'(lambda (x) (abs x)))
  #+allegro (let ((the-sum 0.0))
              (dotimes (i (length vector) the-sum)
                (incf the-sum (abs (aref vector i)))))
  )

;-------------------------------------------------------------------------------
(defun eval-poly-2 (polynomial x)
  (+ (aref polynomial 0)
     (* (aref polynomial 1) x)
     (* (aref polynomial 2) x x)))

;-------------------------------------------------------------------------------
;;; Given equally spaced frequencies f_1 < f_2 < f_3 and a distinct third
;;; frequency f_N satisfying f_1 < f_N < f_3, this routine finds another
;;; frequency f_A satisfying f_1 < f_A < f_3 such that, in the set of four
;;; frequencies f_1, f_2, f_3 and f_A, two of them can be written as
;;; f_N - delta and f_N + delta for some delta > 0; if in fact f_A is
;;; equal to f_1, f_2 or f_3, nil is returned  (see the examples below).
(defun find-symmetric-frequency (f_1 f_2 f_3 f_N)
  ;;; assumes that f_1, f_2, and f_3 are equally spaced
  (let ((delta-f (- f_2 f_1))
        (f_S (- (* 2.0 f_N) f_1))
        (f_trans (/ (+ f_1 f_2) 2.0)))
    (if (> f_N f_trans)
      (decf f_S (* (ceiling (/ (- f_N f_trans) delta-f)) delta-f)))
    (if (or (= f_S f_1) (= f_S f_2) (= f_S f_3) (= f_S f_N))
      nil f_S)))

#|
(map 'list #'(lambda (x)
               (find-symmetric-frequency 1.0 2.0 3.0 x))
     '(1.1 1.46 1.5 1.56 2.1 2.46 2.5 2.56))
;==> (1.2000000000000002 1.92 nil 1.12 2.2 2.92 nil 2.12)
|#
