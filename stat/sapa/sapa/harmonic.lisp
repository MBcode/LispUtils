;;;-*- Mode: LISP; Package: :SAPA; Syntax: COMMON-LISP -*-
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;
;  harmonic.lisp
;
;  a collection of Lisp functions for harmonic analysis ...
;  Note:  before compiling and loading harmonic.lisp,
;         you should compile and load (in the order listed)
;            sapa-package.lisp, utilities.lisp, basic-math.lisp,
;            basic-statistics.lisp, dft-and-fft.lisp, tapers.lisp
;            and nonparametric.lisp
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
;;; (compile-file "ccl:SAPA;harmonic.lisp")
;;; (load "ccl:SAPA;harmonic.fasl")
;-------------------------------------------------------------------------------
(if (not (find-package :SAPA))
  (defpackage :SAPA (:USE :COMMON-LISP)))

(in-package :SAPA)

(export '(;;; function to compute cosine and sine components of periodogram ...
          periodogram-at-one-frequency

          ;;; function to compute Fisher's g statistic
          Fisher-g-statistic
          ))

;-------------------------------------------------------------------------------
(defun periodogram-at-one-frequency
       (time-series
        freq
        &key
        (center-data t)
        (start 0)
        (end (length time-series))
        (sampling-time 1.0))
  "given
   [1] time-series (required)
       ==> a vector of time series values
   [2] center-data (keyword; t)
       ==> if t, subtract sample mean from time series;
           if a number, subtracts that number from time series;
           if nil, time-series is not centered
   [3] start (keyword; 0)
       ==> start index of vector to be used
   [4] end (keyword; length of time-series)
       ==> 1 + end index of vector to be used
   [5] sampling-time (keyword; 1.0)
       ==> sampling time (called delta t in the SAPA book)
returns
   [1] value of periodogram at freq
   [2] approximate conditional least squares estimate for A,
       the amplitude of cosine term in Equation (461a)
       of the SAPA book
   [3] approximate conditional least squares estimate for B
       the amplitude of sine term
---
Note: see Section 10.2 of the SAPA book"
  (let ((N (- end start))
        (j start)
        (center-factor (cond
                        ((numberp center-data) center-data)
                        (center-data (sample-mean time-series
                                                  :start start
                                                  :end end))
                        (t 0.0)))
        (cos-sum 0.0)
        (sin-sum 0.0)
        (2-pi-f-deltt (* 2.0 pi freq sampling-time))
        (t-index 1))
    (dotimes (i N (values (* (/ sampling-time N)
                             (+ (expt cos-sum 2)
                                (expt sin-sum 2)))
                          (* (/ 2.0 N) cos-sum)
                          (* (/ 2.0 N) sin-sum)))
      (incf cos-sum (* (- (aref time-series j) center-factor)
                       (cos (* 2-pi-f-deltt t-index))))
      (incf sin-sum (* (- (aref time-series j) center-factor)
                       (sin (* 2-pi-f-deltt t-index))))
      (incf j)
      (incf t-index))))

#|
;;; For this example we use the 20 point time series of Section 6.16
;;; of the SAPA book:
(let ((20-pt-ts #(71.0  63.0  70.0  88.0  99.0  90.0 110.0 135.0 128.0 154.0
                  156.0 141.0 131.0 132.0 141.0 104.0 136.0 146.0 124.0 129.0)))
  (dolist (a-freq '(0.2 0.4 0.6 1.6 1.8 1.9 2.0) (values))
    (multiple-value-bind (pgrm A B)
                         (periodogram-at-one-frequency
                          20-pt-ts
                          a-freq
                          :sampling-time 0.25)
      (format t "~&~6,4F: ~8,4F ~8,4F  ~8,4F"
              a-freq
              (convert-to-dB pgrm)
              A
              B))))
;==>
0.2000:  30.8586 -19.3691  -24.4889
0.4000:  24.9217  10.7329  -11.5441
0.6000:  21.0531   0.0949  -10.0967
1.6000:  19.9132   8.8552    0.0457
1.8000:  11.6759   1.6742   -2.9941
1.9000:   9.1024   2.4743    0.6196
2.0000:   5.0515   1.6000    0.00000
|#

;-------------------------------------------------------------------------------
(defun Fisher-g-statistic
       (time-series
        &key
        (center-data t)
        (start 0)
        (end (length time-series))
        (alpha 0.05))
    "given
   [1] time-series (required)
       ==> a vector of time series values
   [2] center-data (keyword; t)
       ==> if t, subtract sample mean from time series;
           if a number, subtracts that number from time series;
           if nil, time-series is not centered
   [3] start (keyword; 0)
       ==> start index of vector to be used
   [4] end (keyword; length of time-series)
       ==> 1 + end index of vector to be used
   [5] alpha (keyword; 0.05)
       ==> critical level at which Fisher's g test
           is performed
returns
   [1] Fisher's g statistic
   [2] approximation to critical level of Fisher's g test
       from Equation (491c)
   [3] either :reject or :fail-to-reject, depending on whether
       or not we reject or fail to reject the null hypothesis
       of white noise
---
Note: see Section 10.9 of the SAPA book"
  (let* ((N (- end start))
         (m (if (oddp N)
              (/ (1- N) 2)
              (/ (- N 2) 2))))
    (cond
     ((<= m 1) (values 1.0 1.0 :fail-to-reject))
     (t
      (let* ((g_F (- 1.0 (expt (/ alpha m) (/ (1- m)))))
             (the-periodogram (periodogram time-series
                                           :center-data center-data
                                           :start start
                                           :end end
                                           :N-nonzero-freqs :Fourier
                                           :return-est-for-0-freq-p nil
                                           :sdf-transformation nil
                                           :return-frequencies-p nil))
             (max-Shp (max-of-seq the-periodogram :end m))
             (sum-Shp (sum the-periodogram :end m))
             (g-statistic (/ max-Shp sum-Shp)))
        (values g-statistic g_F (if (> g-statistic g_F)
                                  :reject
                                  :fail-to-reject)))))))

#|
;;; example discussed in Section 10.10 of the SAPA book ...
(let ((ocean-noise-data (vector 0.36201143 2.4737856 -1.018699 1.236099
                                -0.38258758 3.590585 -0.9524991 -1.6762866
                                1.3824866 -0.34308758 -0.1589 0.1412
                                2.6574097 2.2194982 -2.7033095 2.0669982
                                -1.3256867 -2.020322 -0.0753 0.4515114
                                2.4969096 -0.26581144 -1.026599 0.31911144
                                0.5307876 1.2934105 -2.6631095 -3.0335972
                                -2.5025096 0.74761146 -0.26221144 -2.178198
                                -2.940797 -1.5878867 -4.16842 1.5515105
                                -1.136499 -1.879798 0.46968758 1.6587105
                                2.198098 -2.3408096 0.133 -1.4207866
                                -0.8249991 5.1213956 -0.64661145 -1.9440981 
                                -0.5948876 1.041699 1.6077867 -2.4599857
                                -2.989097 0.2540876 5.5098066 2.9239972 
                                0.7211114 -2.033898 -2.2643094 3.2546084
                                2.7131858 -0.896799 -0.8838991 2.9770973 
                                -0.67578757 -0.43901145 1.5423105 0.836399
                                0.0143 0.4059876 1.055099 -0.5244876 
                                -3.155997 0.83619905 0.2799876 0.0655
                                -2.3649857 -1.0694991 -1.3471105 1.3075105 
                                0.6634876 -2.0554981 2.4471095 -2.187498
                                0.6463876 1.2897105 2.4921856 -4.5647836 
                                -1.5162104 1.3197105 2.2606857 -2.0319982
                                -1.4144866 2.0942981 -1.5411105 1.4940104 
                                -1.2181991 -0.1618 0.828699 -0.79309905
                                2.6937857 -0.3346876 -1.5918106 -0.17650001 
                                1.068099 0.1367 -4.7230077 -2.5424857
                                -1.2936105 -4.000296 0.47421142 0.5830876 
                                -2.6602857 -0.46411142 -0.3632876 4.088496
                                -0.6841115 -2.8142972 -0.88159907 2.801497 
                                2.081398 -3.6499846 1.860398 -3.6201086
                                -0.6762114 2.5205097 -1.3579867 -1.4434105)))
  (Fisher-g-statistic ocean-noise-data))
;==> 0.1336096017468551      ;;; Fisher's g statistic
;    0.10876132671244287     ;;; g_F from Equation (491c)
;    :reject                 ;;; reject because g > g_F
|#
