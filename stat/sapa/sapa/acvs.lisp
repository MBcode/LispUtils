;;;-*- Mode: LISP; Package: :SAPA; Syntax: COMMON-LISP -*-
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;
;  acvs.lisp
;
;  a collection of Lisp functions to compute the sample acvs and variogram ...
;  Note:  before compiling and loading acvs.lisp,
;         you should compile and load (in the order listed)
;            sapa-package.lisp, utilities.lisp,  basic-statistics.lisp and
;            dft-and-fft.lisp
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
;;; (compile-file "ccl:SAPA;acvs.lisp")
;;; (load "ccl:SAPA;acvs.fasl")
;-------------------------------------------------------------------------------
(if (not (find-package :SAPA))
  (defpackage :SAPA (:USE :COMMON-LISP)))

(in-package :SAPA)

(export '(acvs
          biased-acvs->unbiased-acvs
          sample-variogram
          ))

;-------------------------------------------------------------------------------
(defun acvs
       (time-series
        &key
        (start 0)
        (end (length time-series))
        (center-data-p t)
        (acs-p nil)
        (result (make-array (- end start))))
  "given
   [1] time-series (required)
       ==> a vector containing the time series
   [2] start (keyword; 0)
       ==> start index of time-series to be used
   [3] end (keyword; length of time-series)
       ==> 1 + end index of time-series to be used
   [4] center-data-p (keyword; t)
       ==> if t, subtract sample mean of time series
           prior to computing the acvs;
           if nil, do not subtract the sample mean
   [5] acs-p (keyword; nil)
       ==> return autocorrelation sequence
           rather than autocovariance sequence
   [6] result (keyword; vector of length n)
       <== vector to hold acvs (or acs)
calculates autocovariance (or autocorrelation) sequence using fft's and
returns
   [1] result, a vector with the sample acvs (or acs)
   [2] sample variance of time series (with mean treated
       as specified by center-data-p keyword)
---
Note: see Equations (191a) and (190b) of the SAPA book"
  (let* ((N (- end start))
         (ntot (next-power-of-2 (* 2 N)))
         (N*ntot (float (* N ntot)))
         (scratch (make-array ntot :initial-element 0.0))
         sample-variance scale-factor)
    #+mcl(declare (dynamic-extent scratch))
    (copy-vector time-series scratch :start start :end end)
    (if center-data-p
      (let ((the-mean (sample-mean time-series :start start :end end)))
        (dotimes (i N)
          (decf (svref scratch i) the-mean))))
    (fft! scratch)
    (dotimes (i ntot)
      (setf (svref scratch i) (expt (abs (svref scratch i)) 2)))
    (fft! scratch)
    (setf sample-variance (/ (realpart (svref scratch 0))
                             N*ntot))
    (setf scale-factor (if acs-p (* N*ntot sample-variance)
                           N*ntot))
    (dotimes (i N)
      (setf (aref result i)
            (/ (realpart (aref scratch i)) scale-factor)))
    (values result sample-variance)))

;;; A test case for acvs is given along with the next function ...

;-------------------------------------------------------------------------------
(defun biased-acvs->unbiased-acvs
       (biased-acvs)
  "given a biased estimate of the acvs,
returns the corresponding unbiased estimate"
  (let ((i -1)
        (n (length biased-acvs)))
    (map (type-of biased-acvs)
         #'(lambda (x) (* x (/ n (- n (incf i)))))
         biased-acvs)))
        
#|
(let ((test (make-array 19 :initial-element 4.0)))
  (multiple-value-bind (biased-acvs sample-var)
                       (acvs test :center-data-p nil)
    (print sample-var)
    (let ((unbiased-acvs (biased-acvs->unbiased-acvs biased-acvs)))
      (dotimes (i 19 (values))
        (format t "~&~2D: ~10,7F  ~10,7F"
                i
                (aref biased-acvs i)
                (aref unbiased-acvs i))))))
;==>
16.00000000000003 
 0: 16.0000000  16.0000000
 1: 15.1578947  16.0000000
 2: 14.3157895  16.0000000
 3: 13.4736842  16.0000000
 4: 12.6315789  16.0000000
 5: 11.7894737  16.0000000
 6: 10.9473684  16.0000000
 7: 10.1052632  16.0000000
 8:  9.2631579  16.0000000
 9:  8.4210526  16.0000000
10:  7.5789474  16.0000000
11:  6.7368421  16.0000000
12:  5.8947368  16.0000000
13:  5.0526316  16.0000000
14:  4.2105263  16.0000000
15:  3.3684211  16.0000000
16:  2.5263158  16.0000000
17:  1.6842105  16.0000000
18:  0.8421053  16.0000000
|#

;-------------------------------------------------------------------------------
(defun sample-variogram
       (time-series
        times)
  "given:
   [1] time-series (required)
       ==> a vector containing the time series
   [2] times (required)
       ==> a vector containing the associated times
returns
   [1] the sample variogram
   [2] associated lags
---
Note: see Diggle's book"
  (let* ((n (length time-series))
         (m (/ (* n (1- n)) 2))
         (lags (make-array m))
         (variogram (make-array m))
         (k 0)
         (pairs '()))
    (dotimes (i (1- n))
      (dotimes (j (- n (1+ i)))
        (setf (aref lags k)
              (- (aref times (+ i j 1)) (aref times i)))
        (setf (aref variogram k)
              (* 0.5 (expt (- (aref time-series (+ i j 1)) (aref time-series i))
                           2)))
        (incf k)))
    ;;; sort the variogram via size of lag ...
    (dotimes (i m)
      (push (list (aref lags i)
                  (aref variogram i)) pairs))
    (setf pairs (sort pairs #'< :key #'car))
    (dotimes (i m (values variogram lags))
      (setf (aref lags i) (car (elt pairs i))
            (aref variogram i) (cadr (elt pairs i))))))

#|
(sample-variogram #(1 2 3 4 5) #(1 2 3 4 5))
;==> #(0.5 0.5 0.5 0.5 2.0 2.0 2.0 4.5 4.5 8.0)
;    #(1 1 1 1 2 2 2 3 3 4)
|#
