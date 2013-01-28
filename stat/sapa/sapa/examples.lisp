;;;-*- Mode: LISP; Package: :CL-USER; Syntax: COMMON-LISP -*-
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;
;  examples.lisp
;
;  examples showing how to repeat certain results in the SAPA book
;  using the functions in the SAPA package; to run these examples,
;  you should first evaluate the "in-package" and "use-package" forms
;  and then all the "defvar" forms, after which you can evaluate the
;  Lisp forms related to the various figures in the SAPA book
;  in any order you choose.
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
;;; (compile-file "ccl:SAPA;filtering.lisp")
;;; (load "ccl:SAPA;filtering.fasl")
;-------------------------------------------------------------------------------
(in-package :CL-USER)

(use-package :SAPA)

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;; We need to define the following special variables in order to run
;;; the test cases in this file.
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
(defvar *rotation-of-earth-data*
  (vector  71.0  63.0  70.0  88.0  99.0  90.0 110.0 135.0 128.0 154.0 156.0
          141.0 131.0 132.0 141.0 104.0 136.0 146.0 124.0 129.0 110.0 119.0
           97.0 115.0 119.0 113.0 115.0 127.0 122.0 147.0 123.0 130.0 149.0
          183.0 186.0 185.0 189.0 203.0 191.0 217.0 220.0 245.0 213.0 227.0
          247.0 266.0 221.0 253.0 231.0 236.0 243.0 246.0 238.0 261.0 248.0
          264.0 270.0 282.0 255.0 286.0 277.0 260.0 261.0 260.0 289.0 297.0
          319.0 311.0 314.0 309.0 309.0 310.0 315.0 310.0 296.0 264.0 284.0
          260.0 286.0 271.0 271.0 259.0 279.0 268.0 296.0 280.0 310.0 265.0
          277.0 257.0 296.0 307.0 306.0 266.0 285.0 279.0 271.0 257.0 270.0
          232.0))

(defvar *centered-rotation-of-earth-data*
  (x+b *rotation-of-earth-data* (- (sample-mean *rotation-of-earth-data*))))

(defvar *the-acvs*)
(defvar *C_h*)

(defvar *eigenvalues-NW-4*)
(defvar *eigenspectra-NW-4*)
(defvar *freqs*)

(defvar *ar-5-coeffs*)
(defvar *forward-pred-errors*)

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;;  Some of the examples below recreate analysis on time series as described
;;;  in the SAPA book.  These time series can be obtained from StatLib
;;;  by sending the message
;;;         send sapa from datasets
;;;  (for details, see page xxvii of the SAPA book).  Once these time
;;;  series have been obtained from StatLib, it is necessary to strip
;;;  out the pertinent numbers for each times series, place them into an
;;;  appropriate ASCII file, and then read each file to load the appropriate
;;;  time series values into Lisp.  The actual details of reading these files
;;;  depend on which implementation of Common Lisp is used.  Here we show
;;;  how it is done in Macintosh Common Lisp 2.0 (MCL 2.0) and in Symbolics
;;;  Genera 8.1.1 running on a Symbolics MacIvory model 3.  Evaluation of each
;;;  of the defvar forms below loads in a time series.  For example, in MCL 2.0,
;;;  "ccl:SAPA;ocean-wave" refers to a file named ocean-wave in a folder
;;;  called SAPA that lives in the folder containing the MCL 2.0 application.
;;;  The file "ccl:SAPA;ocean-wave" consists of the 1024 lines from StatLib
;;;  containing the ocean wave time series (one number each line).
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
(defvar *ocean-wave-data*
  (let ((the-array (make-array 1024)))
    (with-open-file (in-stream
                     #+mcl "ccl:SAPA;ocean-wave"
                     #+genera "L:>dbp>ocean-wave..newest"
                     #+allegro "ocean-wave"
                     ;#+sbcl "ocean-wave.dat" ;or just turn int to reals
                     ;#+sbcl "ocean-wave"
                     #+sbcl "ocean-wave.txt"
                     :direction :input)
      (dotimes (index (length the-array) the-array)
        (setf (svref the-array index)
              (float (read in-stream)))))))

;-------------------------------------------------------------------------------
(defvar *rough-ice-profile-data*
  (let ((the-array (make-array 1121)))
    (with-open-file (in-stream
                     #+mcl "ccl:SAPA;rough-ice-profile"
                     #+genera "L:>dbp>rough-ice-profile..newest"
                     #+allegro "rough-ice-profile"
                     ;#+sbcl "rough-ice-profile.dat"
                     ;#+sbcl "rough-ice-profile"
                     #+sbcl "rough-ice-profile.txt"
                     :direction :input)
      (dotimes (index (length the-array) the-array)
        (setf (svref the-array index)
              (float (read in-stream)))))))

;-------------------------------------------------------------------------------
(defvar *smooth-ice-profile-data*
  (let ((the-array (make-array 288)))
    (with-open-file (in-stream
                     #+mcl "ccl:SAPA;smooth-ice-profile"
                     #+genera "L:>dbp>smooth-ice-profile..newest"
                     #+allegro "smooth-ice-profile"
                     ;#+sbcl "smooth-ice-profile.dat"
                     ;#+sbcl "smooth-ice-profile"
                     #+sbcl "smooth-ice-profile.txt"
                     :direction :input)
      (dotimes (index (length the-array) the-array)
        (setf (svref the-array index)
              (float (read in-stream)))))))

;-------------------------------------------------------------------------------
(defvar *Willamette-River-data*
  (let ((the-array (make-array 395)))
    (with-open-file (in-stream
                     #+mcl "ccl:SAPA;Willamette-River"
                     #+genera "L:>dbp>Willamette-River..newest"
                     #+allegro "Willamette-River"
                     ;#+sbcl "Willamette-River.dat"
                     ;#+sbcl "Willamette-River"
                     #+sbcl "Willamette-River.txt"
                     :direction :input)
      (dotimes (index (length the-array) the-array)
        (setf (svref the-array index)
              (float (read in-stream)))))))

;-------------------------------------------------------------------------------
(defvar *AR-2*
  (let ((the-array (make-array 1024)))
    (with-open-file (in-stream
                     #+mcl "ccl:SAPA;AR-2"
                     #+genera "L:>dbp>AR-2..newest"
                     #+allegro "AR-2"
                     ;#+sbcl "AR-2.dat"
                     ;#+sbcl "AR-2"
                     #+sbcl "AR-2.txt"
                     :direction :input)
      (dotimes (index (length the-array) the-array)
        (setf (svref the-array index)
              (float (read in-stream)))))))

#|
(svref *ocean-wave-data* 0)            ;==>  477.0
(svref *ocean-wave-data* 1023)         ;==> -113.0

(svref *rough-ice-profile-data* 0)     ;==>  -24.6
(svref *rough-ice-profile-data* 1120)  ;==>  -22.5

(svref *smooth-ice-profile-data* 0)    ;==> -9.1
(svref *smooth-ice-profile-data* 287)  ;==> -8.9

(svref *Willamette-River-data* 0)      ;==> 8.95361
(svref *Willamette-River-data* 394)    ;==> 9.06933

(svref *AR-2* 0)                       ;==> 1.619873110842541
(svref *AR-2* 1023)                    ;==> 1.081772237497269
|#

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;;  Figure 172: filtering of rotation of earth data using simple
;;;              3 point filter with coefficients 1/4, 1/2 and 1/4.
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
(let* ((filtered-series (filter-time-series
                         *centered-rotation-of-earth-data*
                         #(1/4 1/2 1/4)))
       (residuals (x-y (make-array
                        (length filtered-series)
                        :displaced-to *centered-rotation-of-earth-data*
                        :displaced-index-offset 1)
                       filtered-series)))
  (format t "~&Figure 172 ...")
  (dotimes (i 5)
    (format t "~&~8,2F ~8,2F ~8,2F"
            (svref *centered-rotation-of-earth-data* (1+ i))
            (svref filtered-series i)
            (svref residuals i)))
  (format t "~&...")
  (let ((N-fs (length filtered-series)))
    (dotimes (i 5)
      (format t "~&~8,2F ~8,2F ~8,2F"
              (svref *centered-rotation-of-earth-data* (1+ (+ i (- N-fs 5))))
              (svref filtered-series (+ i (- N-fs 5)))
              (svref residuals (+ i (- N-fs 5))))))
  (format t "~&... Figure 172")
  (values))
                        
#|
;;; Here is what is printed when the above form is evaluated:
Figure 172 ...
 -152.73  -148.98    -3.75
 -145.73  -142.98    -2.75
 -127.73  -129.48     1.75
 -116.73  -121.73     5.00
 -125.73  -118.48    -7.25
...
   69.27    63.02     6.25
   63.27    62.77     0.50
   55.27    53.77     1.50
   41.27    48.02    -6.75
   54.27    41.52    12.75
... Figure 172
|#

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;;  Figure 177: squared modulus of transfer function for
;;               the Kth order least squares approximation
;;;              to an ideal low-pass filter
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
(let ((65-point-ls-filter
       (create-least-squares-low-pass-filter 65 0.1)))
  (format t "~&Figure 177 ...")
  (dolist (i '(0 1))
    (format t "~&~3D: ~F" (- i 32) (svref 65-point-ls-filter i)))
  (format t "~&...")
  (dolist (i '(31 32 33))
    (format t "~&~3D: ~F" (- i 32) (svref 65-point-ls-filter i)))
  (format t "~&...")
  (dolist (i '(63 64))
    (format t "~&~3D: ~F" (- i 32) (svref 65-point-ls-filter i)))
  (format t "~& sum =  ~F" (sum 65-point-ls-filter))
  (multiple-value-bind (mod-sq-trans-func freqs)
                       (transfer-function-for-filter
                        65-point-ls-filter
                        :return-frequencies-p t)
    (dolist (i '(0 1 2 3 4 5))
      (format t "~&~6,4F: ~F"
              (svref freqs i)
              (svref mod-sq-trans-func i)))
    (format t "~&...")
    (dolist (i '(51 52 53))
      (format t "~&~6,4F: ~F"
              (svref freqs i)
              (svref mod-sq-trans-func i)))
    (format t "~&...")
    (dolist (i '(254 255 256))
      (format t "~&~6,4F: ~F"
              (svref freqs i)
              (svref mod-sq-trans-func i))))
  (format t "~&... Figure 177")
  (values))

#|
;;; Here is what is printed when the above form is evaluated:
Figure 177 ...
-32: 0.009474353340135194
-31: 0.00604435859161769
...
 -1: 0.18737511634014858
  0: 0.2002963792180472
  1: 0.18737511634014858
...
 31: 0.00604435859161769
 32: 0.009474353340135194
 sum =  1.0000000000000002
0.0000: 1.9286549331065735E-15
0.0020: -9.97589282841272E-4
0.0039: -0.003518706679958886
0.0059: -0.006268132747772469
0.0078: -0.0074626548387638734
0.0098: -0.005314313156846105
...
0.0996: -5.4415656590533095
0.1016: -7.797097385966063
0.1035: -10.914299436697938
...
0.4961: -42.85386826084243
0.4980: -40.44459647657306
0.5000: -39.73441540905796
... Figure 177
|#

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;;  Figure 178: squared modulus of transfer function for
;;;              the Kth order least squares
;;;              + triangular convergence factors approximation
;;;              to an ideal low-pass filter
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
(let ((65-point-ls-filter
       (create-least-squares-low-pass-filter
        65 0.1
        :convergence-factors
        #'(lambda (k) (triangular-convergence-factors k 65)))))
  (format t "~&Figure 178 ...")
  (dolist (i '(0 1))
    (format t "~&~3D: ~F" (- i 32) (svref 65-point-ls-filter i)))
  (format t "~&...")
  (dolist (i '(31 32 33))
    (format t "~&~3D: ~F" (- i 32) (svref 65-point-ls-filter i)))
  (format t "~&...")
  (dolist (i '(63 64))
    (format t "~&~3D: ~F" (- i 32) (svref 65-point-ls-filter i)))
  (format t "~& sum =  ~F" (sum 65-point-ls-filter))
  (multiple-value-bind (mod-sq-trans-func freqs)
                       (transfer-function-for-filter
                        65-point-ls-filter
                        :return-frequencies-p t)
    (dolist (i '(0 1 2 3 4 5))
      (format t "~&~6,4F: ~F"
              (svref freqs i)
              (svref mod-sq-trans-func i)))
    (format t "~&...")
    (dolist (i '(51 52 53))
      (format t "~&~6,4F: ~F"
              (svref freqs i)
              (svref mod-sq-trans-func i)))
    (format t "~&...")
    (dolist (i '(254 255 256))
      (format t "~&~6,4F: ~F"
              (svref freqs i)
              (svref mod-sq-trans-func i)))
    )
  (format t "~&... Figure 178")
  (values))

#|
;;; Here is what is printed when the above form is evaluated:
Figure 178 ...
-32: 2.958988584176635E-4
-31: 3.7754952616136017E-4
...
 -1: 0.18726456497603497
  0: 0.20643377319025294
  1: 0.18726456497603497
...
 31: 3.7754952616136017E-4
 32: 2.958988584176635E-4
 sum =  0.9999999999999998
0.0000: -1.9286549331065743E-15
0.0020: 9.19973377882888E-4
0.0039: 0.0035297560721751367
0.0059: 0.007399570616709966
0.0078: 0.011879123556194419
0.0098: 0.016187305184551266
...
0.0996: -5.636216008278292
0.1016: -6.812966030344137
0.1035: -8.143286403474598
...
0.4961: -49.51447433638478
0.4980: -49.421175373585505
0.5000: -49.38851214695436
... Figure 178
|#

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;;  Figure 181: squared modulus of transfer function for
;;;              dpss approximations to an ideal low-pass filter
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
(let ((17-point-dpss
       (dpss-data-taper!
        (make-array 17 :initial-element 1.0)
        :taper-parameter (* 0.1 17)))
      (65-point-dpss
       (dpss-data-taper!
        (make-array 65 :initial-element 1.0)
        :taper-parameter (* 0.1 65))))
  (a*x! (/ (sum 17-point-dpss)) 17-point-dpss)
  (a*x! (/ (sum 65-point-dpss)) 65-point-dpss)
  (format t "~&Figure 181 ...")
  (multiple-value-bind (mod-sq-trans-func-17 freqs)
                       (transfer-function-for-filter
                        17-point-dpss
                        :return-frequencies-p t
                        :N-fft 512)
    (let ((mod-sq-trans-func-65
           (transfer-function-for-filter
            65-point-dpss
            :return-frequencies-p t
            :N-fft 512)))
      (dolist (i '(0 1 2 3 4 5 6 7))
        (format t "~&~6,4F: ~7,3F, ~7,3F"
                (svref freqs i)
                (svref mod-sq-trans-func-17 i)
                (svref mod-sq-trans-func-65 i)))
      (format t "~&...")
      (dolist (i '(50 51 52 53 54))
        (format t "~&~6,4F: ~7,3F, ~7,3F"
                (svref freqs i)
                (svref mod-sq-trans-func-17 i)
                (svref mod-sq-trans-func-65 i)))
      (format t "~&...")
      (dolist (i '(254 255 256))
        (format t "~&~6,4F: ~7,3F, ~7,3F"
                (svref freqs i)
                (svref mod-sq-trans-func-17 i)
                (svref mod-sq-trans-func-65 i)))
      ))
  (format t "~&... Figure 181")
  (values))

#|
;;; Here is what is printed when the above form is evaluated:
Figure 181 ...
0.0000:   0.000,  -0.0000
0.0020:  -0.008,  -0.033
0.0039:  -0.030,  -0.131
0.0059:  -0.068,  -0.294
0.0078:  -0.121,  -0.523
0.0098:  -0.189,  -0.818
0.0117:  -0.272,  -1.179
0.0137:  -0.371,  -1.607
...
0.0977: -26.612, -134.511
0.0996: -28.537, -176.340
0.1016: -30.737, -157.182
0.1035: -33.328, -172.494
0.1055: -36.530, -160.819
...
0.4961: -49.526, -184.556
0.4980: -49.369, -182.150
0.5000: -49.318, -181.440
... Figure 181
;;; Note: the above listing is from Macintosh Common Lisp.  When this form
;;;       is evaluated under Genera 8.1 or Allegro Common Lisp, the last
;;;       column differs somewhat at low dB levels:
0.0977: -26.612, -135.267
0.0996: -28.537, -157.133
0.1016: -30.737, -150.720
0.1035: -33.328, -157.869
0.1055: -36.530, -164.541
...
0.4961: -49.526, -163.929
0.4980: -49.369, -165.401
0.5000: -49.318, -165.964
;;;       This difference is evidently due to different default precisions
;;;       for numerical computations.
|#

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;;  Figure 182: squared modulus of transfer function for
;;;              approximations to an ideal low-pass filter
;;;              using dpss as convergence factors to least squares
;;;              approximations
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
(let ((delta-point-04
       (create-dpss-low-pass-filter
        65 0.04 0.1))
      (delta-point-01
       (create-dpss-low-pass-filter
        65 0.01 0.1)))
  (a*x! (/ (sum delta-point-04)) delta-point-04)
  (a*x! (/ (sum delta-point-01)) delta-point-01)
  (format t "~&Figure 182 ...")
  (print (sum delta-point-04))
  (print (sum delta-point-01))
  (multiple-value-bind (mod-sq-trans-func-04 freqs)
                       (transfer-function-for-filter
                        delta-point-04
                        :return-frequencies-p t
                        :N-fft 512)
    (let ((mod-sq-trans-func-01
           (transfer-function-for-filter
            delta-point-01
            :return-frequencies-p t
            :N-fft 512)))
      (dolist (i '(0 1 2 3 4 5 6))
        (format t "~&~6,4F: ~7,3F, ~7,3F"
                (svref freqs i)
                (svref mod-sq-trans-func-04 i)
                (svref mod-sq-trans-func-01 i)))
      (format t "~&...")
      (dolist (i '(50 51 52 53 54))
        (format t "~&~6,4F: ~7,3F, ~7,3F"
                (svref freqs i)
                (svref mod-sq-trans-func-04 i)
                (svref mod-sq-trans-func-01 i)))
      (format t "~&...")
      (dolist (i '(254 255 256))
        (format t "~&~6,4F: ~7,3F, ~7,3F"
                (svref freqs i)
                (svref mod-sq-trans-func-04 i)
                (svref mod-sq-trans-func-01 i)))))
  (format t "~&... Figure 182")
  (values))

#|
;;; Here is what is printed when the above form is evaluated:
Figure 182 ...
0.9999999999999999 
1.0 
0.0000:   0.000,   0.000
0.0020:   0.0001,   0.0005
0.0039:   0.0002,   0.002
0.0059:   0.0004,   0.005
0.0078:   0.001,   0.009
0.0098:   0.001,   0.014
0.0117:   0.001,   0.021
...
0.0977:  -4.944,  -4.095
0.0996:  -5.830,  -5.601
0.1016:  -6.821,  -7.452
0.1035:  -7.923,  -9.733
0.1055:  -9.144, -12.575
...
0.4961: -96.614, -50.080
0.4980: -94.150, -47.668
0.5000: -93.426, -46.957
... Figure 182
|#

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;;  Figure 200: Fejer's kernel for N = 4, 16 and 64
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
(multiple-value-bind (the-spec-wind freqs N-f)
                     (spectral-window-for-direct-spectral-estimate
                      4)
  (format t "~&Figure 200, top plot ...")
  (dotimes (i N-f)
    (format t "~&~6,4F: ~8,4F"
            (svref freqs i)
            (svref the-spec-wind  i)))
  (format t "~&... Figure 200, top plot")
  (values))

#|
;;; Here is what is printed when the above form is evaluated:
Figure 200, top plot ...
0.0000:   6.0206
0.0625:   5.1644
0.1250:   2.3226
0.1875:  -3.9257
0.2500: -100.0000
0.3125:  -7.4278
0.3750:  -5.3329
0.4375:  -8.8624
0.5000: -100.0000
... Figure 200, top plot
|#

;;; Here we repeat the above calculation on a much finer grid of frequencies ...
(multiple-value-bind (the-spec-wind freqs N-f)
                     (spectral-window-for-direct-spectral-estimate
                      4
                      :n-nonzero-freqs 512)
  (format t "~&Figure 200, top plot ...")
  (dotimes (i 7)
    (format t "~&~6,4F: ~8,4F"
            (svref freqs i)
            (svref the-spec-wind  i)))
  (format t "~&...")
  (dotimes (i 7)
    (format t "~&~6,4F: ~8,4F"
            (svref freqs (+ i (- N-f 7)))
            (svref the-spec-wind  (+ i (- N-f 7)))))
  (format t "~&... Figure 200, top plot")
  (values))

#|
;;; Here is what is printed when the above form is evaluated:
Figure 200, top plot ...
0.0000:   6.0206
0.0010:   6.0204
0.0020:   6.0198
0.0029:   6.0188
0.0039:   6.0173
0.0049:   6.0155
0.0059:   6.0132
...
0.4941: -28.6858
0.4951: -30.2674
0.4961: -32.2040
0.4971: -34.7016
0.4980: -38.2225
0.4990: -44.2426
0.5000: -100.0000
... Figure 200, top plot
|#

(multiple-value-bind (the-spec-wind freqs N-f)
                     (spectral-window-for-direct-spectral-estimate
                      16)
  (format t "~&Figure 200, middle plot ...")
  (dotimes (i 7)
    (format t "~&~6,4F: ~8,4F"
            (svref freqs i)
            (svref the-spec-wind  i)))
  (format t "~&...")
  (dotimes (i 7)
    (format t "~&~6,4F: ~8,4F"
            (svref freqs (+ i (- N-f 7)))
            (svref the-spec-wind  (+ i (- N-f 7)))))
  (format t "~&... Figure 200, middle plot")
  (values))

#|
;;; Here is what is printed when the above form is evaluated:
Figure 200, middle plot ...
0.0000:  12.0412
0.0156:  11.1326
0.0312:   8.1328
0.0469:   1.6181
0.0625: -100.0000
0.0781:  -2.7629
0.0937:  -1.2977
...
0.4062: -11.6589
0.4219: -14.7872
0.4375: -100.0000
0.4531: -14.9570
0.4687: -11.9993
0.4844: -15.0410
0.5000: -100.0000
... Figure 200, middle plot
|#

(multiple-value-bind (the-spec-wind freqs N-f)
                     (spectral-window-for-direct-spectral-estimate
                      64)
  (format t "~&Figure 200, bottom plot ...")
  (dotimes (i 7)
    (format t "~&~6,4F: ~8,4F"
            (svref freqs i)
            (svref the-spec-wind  i)))
  (format t "~&...")
  (dotimes (i 7)
    (format t "~&~6,4F: ~8,4F"
            (svref freqs (+ i (- N-f 7)))
            (svref the-spec-wind  (+ i (- N-f 7)))))
  (format t "~&... Figure 200, bottom plot")
  (values))

#|
;;; Here is what is printed when the above form is evaluated:
Figure 200, bottom plot ...
0.0000:  18.0618
0.0039:  17.1499
0.0078:  14.1403
0.0117:   7.6092
0.0156: -100.0000
0.0195:   3.1758
0.0234:   4.6048
...
0.4766: -18.0382
0.4805: -21.0557
0.4844: -100.0000
0.4883: -21.0662
0.4922: -18.0592
0.4961: -21.0714
0.5000: -100.0000
... Figure 200, bottom plot
|#

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;;  Figure 211: dpss data tapers and spectral windows for N = 64,
;;;              NW = 1 and 2
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
(let ((the-taper (dpss-data-taper!
                  (make-array 64 :initial-element 1.0)
                  :taper-parameter 1.0)))
  (format t "~&sum of squares of taper elements =  ~8,4F"
          (sum-of-squares the-taper))
  (format t "~&Figure 211, top left-hand plot ...")
  (dotimes (i 7)
    (format t "~&~2D: ~8,4F"
            (1+ i)
            (svref the-taper i)))
  (format t "~&...")
  (dotimes (i 7)
    (format t "~&~2D: ~8,4F"
            (+ (1+ i) (- 64 7))
            (svref the-taper (+ i (- 64 7)))))
  (format t "~&... Figure 211, top left-hand plot")
  (values))

#|
;;; Here is what is printed when the above form is evaluated:
sum of squares of taper elements =    1.0000
Figure 211, top left-hand plot ...
 1:   0.0353
 2:   0.0404
 3:   0.0457
 4:   0.0512
 5:   0.0568
 6:   0.0626
 7:   0.0684
...
58:   0.0684
59:   0.0626
60:   0.0568
61:   0.0512
62:   0.0457
63:   0.0404
64:   0.0353
... Figure 211, top left-hand plot
|#

(let ((the-taper (dpss-data-taper!
                  (make-array 64 :initial-element 1.0)
                  :taper-parameter 2.0)))
  (format t "~&sum of squares of taper elements =  ~8,4F"
          (sum-of-squares the-taper))
  (format t "~&Figure 211, top right-hand plot ...")
  (dotimes (i 7)
    (format t "~&~2D: ~8,4F"
            (1+ i)
            (svref the-taper i)))
  (format t "~&...")
  (dotimes (i 7)
    (format t "~&~2D: ~8,4F"
            (+ (1+ i) (- 64 7))
            (svref the-taper (+ i (- 64 7)))))
  (format t "~&... Figure 211, top right-hand plot")
  (values))

#|
;;; Here is what is printed when the above form is evaluated:
sum of squares of taper elements =    1.0000
Figure 211, top right-hand plot ...
 1:   0.0034
 2:   0.0055
 3:   0.0079
 4:   0.0110
 5:   0.0146
 6:   0.0188
 7:   0.0236
...
58:   0.0236
59:   0.0188
60:   0.0146
61:   0.0110
62:   0.0079
63:   0.0055
64:   0.0034
... Figure 211, top right-hand plot
|#

(multiple-value-bind (the-spec-wind freqs N-f)
                     (spectral-window-for-direct-spectral-estimate
                      64
                      :data-taper #'dpss-data-taper!
                      :data-taper-parameters 1.0)
  (format t "~&Figure 211, bottom left-hand plot ...")
  (dotimes (i 7)
    (format t "~&~6,4F: ~8,4F"
            (svref freqs i)
            (svref the-spec-wind  i)))
  (format t "~&...")
  (dotimes (i 7)
    (format t "~&~6,4F: ~8,4F"
            (svref freqs (+ i (- N-f 7)))
            (svref the-spec-wind  (+ i (- N-f 7)))))
  (format t "~&... Figure 211, bottom left-hand plot")
  (values))

#|
;;; Here is what is printed when the above form is evaluated:
Figure 211, bottom left-hand plot ...
0.0000:  17.4690
0.0039:  16.8738
0.0078:  15.0172
0.0117:  11.6357
0.0156:   6.0027
0.0195:  -4.8372
0.0234: -12.4591
...
0.4766: -29.6551
0.4805: -32.6327
0.4844: -78.3605
0.4883: -32.7070
0.4922: -29.6760
0.4961: -32.6802
0.5000: -100.0000
... Figure 211, bottom left-hand plot
|#

(multiple-value-bind (the-spec-wind freqs N-f)
                     (spectral-window-for-direct-spectral-estimate
                      64
                      :data-taper #'dpss-data-taper!
                      :data-taper-parameters 2.0)
  (format t "~&Figure 211, bottom right-hand plot ...")
  (dotimes (i 7)
    (format t "~&~6,4F: ~8,4F"
            (svref freqs i)
            (svref the-spec-wind  i)))
  (format t "~&...")
  (dotimes (i 7)
    (format t "~&~6,4F: ~8,4F"
            (svref freqs (+ i (- N-f 7)))
            (svref the-spec-wind  (+ i (- N-f 7)))))
  (format t "~&... Figure 211, bottom right-hand plot")
  (values))

#|
;;; Here is what is printed when the above form is evaluated:
Figure 211, bottom right-hand plot ...
0.0000:  16.3413
0.0039:  15.9770
0.0078:  14.8693
0.0117:  12.9709
0.0156:  10.1904
0.0195:   6.3636
0.0234:   1.1838
...
0.4766: -51.8112
0.4805: -54.6679
0.4844: -88.3687
0.4883: -54.9352
0.4922: -51.8305
0.4961: -54.8103
0.5000: -100.0000
... Figure 211, bottom right-hand plot
|#

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;;  Figure 234: normalized cumulative periodogram for first 32 points
;;;              of AR(2) series
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;; For this example we use the first 64 points of the AR(2) time series
;;; shown in Figure 45 of the SAPA book.
(let* ((32-pt-ts (make-array 32 :displaced-to *AR-2*))
       (cum-per-results (multiple-value-list
                         (cumulative-periodogram 32-pt-ts)))
       (cum-per (elt cum-per-results 0))
       (cum-per-freqs (elt cum-per-results 1))
       (KS-results (subseq cum-per-results 2))
       (the-periodogram (periodogram 32-pt-ts
                                     :N-nonzero-freqs :Fourier
                                     :sdf-transformation nil)))
  (format t "~&Figure 234, left-hand plot, right-hand plot ...")
  (dotimes (i 5)
    (format t "~&~6,4F:      ~8,4F         ~8,4F"
            (svref cum-per-freqs i)
            (svref the-periodogram i)
            (svref cum-per i)))
  (format t "~&...")
  (let ((N-cum-per (length cum-per)))
    (dotimes (i 5)
      (format t "~&~6,4F:      ~8,4F         ~8,4F"
              (svref cum-per-freqs (+ i (- N-cum-per 5)))
              (svref the-periodogram (+ i (- N-cum-per 5)))
              (svref cum-per (+ i (- N-cum-per 5))))))
  (apply #'format t "~&... Figure 234
Kolmogorov test statistic             = ~A
index of maximum deviation            = ~A
frequency of maximum deviation        = ~A
quantile of Kolmogorov test statistic = ~A
reject/fail to reject null hypothesis = ~A
slope of upper and lower lines        = ~A
intercept of upper line               = ~A
intercept of lower line               = ~A"
         KS-results)
  (values))

#|
;;; Here is what is printed when the above form is evaluated:
Figure 234, left-hand plot, right-hand plot ...
0.0312:        0.2003           0.0055
0.0625:        2.9395           0.0866
0.0937:        7.8185           0.3022
0.1250:        7.9642           0.5219
0.1562:        5.6365           0.6773
...
0.3437:        2.0928           0.9507
0.3750:        0.6617           0.9689
0.4062:        0.1675           0.9735
0.4375:        0.7901           0.9953
0.4687:        0.1696           1.0000
... Figure 234
Kolmogorov test statistic             = 0.3916144875655616
index of maximum deviation            = 4
frequency of maximum deviation        = 0.15625
quantile of Kolmogorov test statistic = 0.349
reject/fail to reject null hypothesis = reject
slope of upper and lower lines        = 2.2857142857142856
intercept of upper line               = 0.2775714285714286
intercept of lower line               = -0.349
|#

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;;  Figure 265: Parzen lag window, smoothing window and spectral windows
;;;              using default and dpss data tapers
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
(progn
  (format t "~&Figure 265, top left-hand plot ...")
  (dotimes (i 7)
    (format t "~&~2D: ~7,5F"
            i
            (parzen-lag-window i 37)))
  (format t "~&...")
  (dotimes (i 7)
    (format t "~&~2D: ~7,5F"
            (+ i 33)
            (parzen-lag-window (+ i 33) 37)))
  (format t "~&... Figure 265, top left-hand plot")
  (values))

#|
;;; Here is what is printed when the above form is evaluated:
 0: 1.00000
 1: 0.99574
 2: 0.98342
 3: 0.96375
 4: 0.93746
 5: 0.90524
 6: 0.86781
...
33: 0.00253
34: 0.00107
35: 0.00032
36: 0.00004
37: 0.00000
38: 0.00000
39: 0.00000
... Figure 265, top left-hand plot
|#

(multiple-value-bind (the-smooth-wind freqs N-f)
                     (smoothing-window-for-lag-window-spectral-estimate
                      64
                      #'(lambda (tau)
                          (parzen-lag-window tau 37)))
  (format t "~&Figure 265, top right-hand plot ...")
  (dotimes (i 7)
    (format t "~&~6,4F: ~8,4F"
            (svref freqs i)
            (svref the-smooth-wind  i)))
  (format t "~&...")
  (dotimes (i 7)
    (format t "~&~6,4F: ~8,4F"
            (svref freqs (+ i (- N-f 7)))
            (svref the-smooth-wind  (+ i (- N-f 7)))))
  (format t "~&... Figure 265, top right-hand plot")
  (values))

#|
;;; Here is what is printed when the above form is evaluated:
Figure 265, top right-hand plot ...
0.0000:  14.4326
0.0039:  14.2831
0.0078:  13.8316
0.0117:  13.0682
0.0156:  11.9756
0.0195:  10.5271
0.0234:   8.6825
...
0.4766: -46.8807
0.4805: -45.6696
0.4844: -44.6896
0.4883: -44.5244
0.4922: -45.2033
0.4961: -46.3784
0.5000: -47.0461
... Figure 265, top right-hand plot
|#

(multiple-value-bind (the-spec-wind freqs N-f)
                     (spectral-window-for-lag-window-spectral-estimate
                      64
                      #'(lambda (tau)
                          (parzen-lag-window tau 37)))
  (format t "~&Figure 265, bottom left-hand plot ...")
  (dotimes (i 7)
    (format t "~&~6,4F: ~8,4F"
            (svref freqs i)
            (svref the-spec-wind  i)))
  (format t "~&...")
  (dotimes (i 7)
    (format t "~&~6,4F: ~8,4F"
            (svref freqs (+ i (- N-f 7)))
            (svref the-spec-wind  (+ i (- N-f 7)))))
  (format t "~&... Figure 265, bottom left-hand plot")
  (values))

#|
;;; Here is what is printed when the above form is evaluated:
Figure 265, bottom left-hand plot ...
0.0000:  13.8038
0.0039:  13.6754
0.0078:  13.2890
0.0117:  12.6417
0.0156:  11.7287
0.0195:  10.5453
0.0234:   9.0893
...
0.4766: -21.0246
0.4805: -21.0307
0.4844: -21.0353
0.4883: -21.0396
0.4922: -21.0440
0.4961: -21.0475
0.5000: -21.0489
... Figure 265, bottom left-hand plot
|#

(multiple-value-bind (the-spec-wind freqs N-f)
                     (spectral-window-for-lag-window-spectral-estimate
                      64
                      #'(lambda (tau)
                          (parzen-lag-window tau 37))
                      :data-taper #'dpss-data-taper!
                      :data-taper-parameters 4.0)
  (format t "~&Figure 265, bottom right-hand plot ...")
  (dotimes (i 7)
    (format t "~&~6,4F: ~8,4F"
            (svref freqs i)
            (svref the-spec-wind  i)))
  (format t "~&...")
  (dotimes (i 7)
    (format t "~&~6,4F: ~8,4F"
            (svref freqs (+ i (- N-f 7)))
            (svref the-spec-wind  (+ i (- N-f 7)))))
  (format t "~&... Figure 265, bottom right-hand plot")
  (values))

#|
;;; Here is what is printed when the above form is evaluated:
Figure 265, bottom right-hand plot ...
0.0000:  13.2324
0.0039:  13.1422
0.0078:  12.8715
0.0117:  12.4186
0.0156:  11.7814
0.0195:  10.9566
0.0234:   9.9399
...
0.4766: -45.0465
0.4805: -45.2264
0.4844: -45.3652
0.4883: -45.4628
0.4922: -45.5266
0.4961: -45.5628
0.5000: -45.5747
... Figure 265, bottom right-hand plot
|#

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;;  Figure 296: periodogram and other direct spectral estimates
;;;              for ocean wave data
;;;  Note: in what follows we assume that the 1024 values for
;;;        ocean wave data have been placed into the array pointed
;;;        to by *ocean-wave-data* -- at the beginning of this file
;;;        are examples of how to load numbers from an ASCII file
;;;        into a Lisp vector.
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
(sample-mean-and-variance *ocean-wave-data*)
;==> 209.103515625
;    143954.35647201538

(multiple-value-bind (the-periodogram freqs N-f)
                     (periodogram
                      *ocean-wave-data*
                      :sampling-time 1/4)
  (format t "~&Figure 296, top plot ...")
  (dotimes (i 7)
    (format t "~&~6,4F: ~8,4F"
            (svref freqs i)
            (svref the-periodogram i)))
  (format t "~&...")
  (dotimes (i 7)
    (format t "~&~6,4F: ~8,4F"
            (svref freqs (+ i (- N-f 7)))
            (svref the-periodogram (+ i (- N-f 7)))))
  (format t "~&... Figure 296, top plot")
  (values))

#|
;;; Here is what is printed when the above form is evaluated:
Figure 296, top plot ...
0.0039:  47.8344
0.0078:  39.8411
0.0117:  43.0482
0.0156:  43.9995
0.0195:  50.3106
0.0234:  54.7701
0.0273:  54.0996
...
1.9766:  13.2352
1.9805:  11.5889
1.9844:  11.8137
1.9883:  12.3749
1.9922:  14.7094
1.9961:  12.9344
2.0000:  12.2424
... Figure 296, top plot
|#

(multiple-value-bind (the-direct-spectral-estimate freqs N-f)
                     (direct-spectral-estimate
                      *ocean-wave-data*
                      :sampling-time 1/4
                      :data-taper #'dpss-data-taper!
                      :data-taper-parameters 1.0)
  (format t "~&Figure 296, second plot ...")
  (dotimes (i 7)
    (format t "~&~6,4F: ~8,4F"
            (svref freqs i)
            (svref the-direct-spectral-estimate i)))
  (format t "~&...")
  (dotimes (i 7)
    (format t "~&~6,4F: ~8,4F"
            (svref freqs (+ i (- N-f 7)))
            (svref the-direct-spectral-estimate (+ i (- N-f 7)))))
  (format t "~&... Figure 296, second plot")
  (values))

#|
;;; Here is what is printed when the above form is evaluated:
Figure 296, second plot ...
0.0039:  46.2041
0.0078:  23.2523
0.0117:  42.4188
0.0156:  47.8951
0.0195:  51.3775
0.0234:  54.3370
0.0273:  52.6524
...
1.9766:   2.3258
1.9805:  -3.7210
1.9844:  -0.6480
1.9883:  -1.8448
1.9922:   6.4357
1.9961:  -0.5324
2.0000:  -2.6338
... Figure 296, second plot
|#

(multiple-value-bind (the-direct-spectral-estimate freqs N-f)
                     (direct-spectral-estimate
                      *ocean-wave-data*
                      :sampling-time 1/4
                      :data-taper #'dpss-data-taper!
                      :data-taper-parameters 2.0)
  (format t "~&Figure 296, third plot ...")
  (dotimes (i 7)
    (format t "~&~6,4F: ~8,4F"
            (svref freqs i)
            (svref the-direct-spectral-estimate i)))
  (format t "~&...")
  (dotimes (i 7)
    (format t "~&~6,4F: ~8,4F"
            (svref freqs (+ i (- N-f 7)))
            (svref the-direct-spectral-estimate (+ i (- N-f 7)))))
  (format t "~&... Figure 296, third plot")
  (values))

#|
;;; Here is what is printed when the above form is evaluated:
Figure 296, third plot ...
0.0039:  43.8021
0.0078:  36.5615
0.0117:  42.9830
0.0156:  49.5223
0.0195:  51.8054
0.0234:  53.3146
0.0273:  50.7869
...
1.9766: -11.0994
1.9805:  -8.6132
1.9844:  -5.4962
1.9883:  -6.0073
1.9922:  -0.1094
1.9961: -14.6655
2.0000: -16.8471
... Figure 296, third plot
|#

(multiple-value-bind (the-direct-spectral-estimate freqs N-f)
                     (direct-spectral-estimate
                      *ocean-wave-data*
                      :sampling-time 1/4
                      :data-taper #'dpss-data-taper!
                      :data-taper-parameters 4.0)
  (format t "~&Figure 296, bottom plot ...")
  (dotimes (i 7)
    (format t "~&~6,4F: ~8,4F"
            (svref freqs i)
            (svref the-direct-spectral-estimate i)))
  (format t "~&...")
  (dotimes (i 7)
    (format t "~&~6,4F: ~8,4F"
            (svref freqs (+ i (- N-f 7)))
            (svref the-direct-spectral-estimate (+ i (- N-f 7)))))
  (format t "~&... Figure 296, bottom plot")
  (values))

#|
;;; Here is what is printed when the above form is evaluated:
Figure 296, bottom plot ...
0.0039:  40.9897
0.0078:  42.3745
0.0117:  46.2331
0.0156:  50.5033
0.0195:  51.8640
0.0234:  52.3855
0.0273:  50.8260
...
1.9766: -19.6861
1.9805:  -8.4228
1.9844:  -5.0683
1.9883:  -3.3882
1.9922:  -2.3835
1.9961:  -8.3321
2.0000: -22.7452
... Figure 296, bottom plot
|#

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;;  Figure 298: periodogram for ocean wave data 
;;;              at a finer grid of frequencies ...
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
(multiple-value-bind (the-periodogram freqs N-f)
                     (periodogram
                      *ocean-wave-data*
                      :N-nonzero-freqs :next-power-of-2
                      :sampling-time 1/4)
  (format t "~&Figure 298 ...")
  (dotimes (i 7)
    (format t "~&~6,4F: ~8,4F"
            (svref freqs i)
            (svref the-periodogram i)))
  (format t "~&...")
  (dotimes (i 7)
    (format t "~&~6,4F: ~8,4F"
            (svref freqs (+ i (- N-f 7)))
            (svref the-periodogram (+ i (- N-f 7)))))
  (format t "~&... Figure 298")
  (values))

#|
;;; Here is what is printed when the above form is evaluated:
Figure 298 ...
0.0020:  50.5514
0.0039:  47.8344
0.0059:  37.1331
0.0078:  39.8411
0.0098:  39.0109
0.0117:  43.0482
0.0137:  39.6363
...
1.9883:  12.3749
1.9902:   0.1867
1.9922:  14.7094
1.9941:  -5.7852
1.9961:  12.9344
1.9980: -10.8475
2.0000:  12.2424
... Figure 298
|#

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;;  Figure 301: lag window spectral estimates for ocean wave data
;;;              based upon direct spectral estimate with NW=2 dpss data taper
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
(multiple-value-bind (the-direct-spectral-estimate freqs N-f C_h the-acvs)
                     (direct-spectral-estimate
                      *ocean-wave-data*
                      :sampling-time 1/4
                      :return-acvs-p t
                      :data-taper #'dpss-data-taper!
                      :data-taper-parameters 2.0)
  (declare (ignore the-direct-spectral-estimate freqs N-f))
  (format t "~&C_h = ~5,3F" C_h)
  (format t "~& lag      acvs")
  (dotimes (i 7)
    (format t "~&~4D: ~12,4F"
            i
            (svref the-acvs i)))
  (format t "~&...")
  (dotimes (i 7)
    (format t "~&~4D: ~12,4F"
            (+ i (- 1024 7))
            (svref the-acvs (+ i (- 1024 7)))))
  (setf *the-acvs* the-acvs
        *C_h* C_h)
  (values))

#|
;;; Here is what is printed when the above form is evaluated:
C_h = 2.005
 lag      acvs
   0:  143954.3565
   1:  138834.3641
   2:  124334.1234
   3:  102687.0485
   4:   76732.5318
   5:   49174.6362
   6:   22178.5321
...
1017:      -0.3232
1018:      -0.2984
1019:      -0.2523
1020:      -0.1911
1021:      -0.1258
1022:      -0.0679
1023:      -0.0250
|#

(multiple-value-bind (Parzen-m-150 freqs N-f nu B_W)
                     (lag-window-spectral-estimate
                      *the-acvs*
                      #'(lambda (lag)
                          (parzen-lag-window lag 150))
                      :sampling-time 1/4
                      :C_h *C_h*)
  (format t "~&Figure 301, top plot ...")
  (dotimes (i 7)
    (format t "~&~6,4F: ~8,4F"
            (svref freqs i)
            (svref Parzen-m-150 i)))
  (format t "~&...")
  (dotimes (i 7)
    (format t "~&~6,4F: ~8,4F"
            (svref freqs (+ i (- N-f 7)))
            (svref Parzen-m-150 (+ i (- N-f 7)))))
  (format t "~&... Figure 301, top plot")
  (format t "~&equivalent degrees of freedom = ~5,1F" nu)
  (format t "~&smoothing window bandwidth    = ~6,4F" B_W)
  (values))

#|
;;; Here is what is printed when the above form is evaluated:
Figure 301, top plot ...
0.0039:  47.5847
0.0078:  48.2507
0.0117:  49.1616
0.0156:  50.1528
0.0195:  51.1108
0.0234:  51.9681
0.0273:  52.6864
...
1.9766:  -5.8887
1.9805:  -5.7822
1.9844:  -5.5922
1.9883:  -5.3665
1.9922:  -5.1577
1.9961:  -5.0117
2.0000:  -4.9595
... Figure 301, top plot
equivalent degrees of freedom =  12.6
smoothing window bandwidth    = 0.0494
|#

(multiple-value-bind (Parzen-m-55 freqs N-f nu B_W)
                     (lag-window-spectral-estimate
                      *the-acvs*
                      #'(lambda (lag)
                          (parzen-lag-window lag 55))
                      :sampling-time 1/4
                      :C_h *C_h*)
  (format t "~&Figure 301, middle plot ...")
  (dotimes (i 7)
    (format t "~&~6,4F: ~8,4F"
            (svref freqs i)
            (svref Parzen-m-55 i)))
  (format t "~&...")
  (dotimes (i 7)
    (format t "~&~6,4F: ~8,4F"
            (svref freqs (+ i (- N-f 7)))
            (svref Parzen-m-55 (+ i (- N-f 7)))))
  (format t "~&... Figure 301, middle plot")
  (format t "~&equivalent degrees of freedom = ~5,1F" nu)
  (format t "~&smoothing window bandwidth    = ~6,4F" B_W)
  (values))

#|
;;; Here is what is printed when the above form is evaluated:
Figure 301, middle plot ...
0.0039:  51.7292
0.0078:  51.7425
0.0117:  51.7636
0.0156:  51.7913
0.0195:  51.8242
0.0234:  51.8604
0.0273:  51.8985
...
1.9766:  -1.8860
1.9805:  -1.8645
1.9844:  -1.8520
1.9883:  -1.8457
1.9922:  -1.8433
1.9961:  -1.8428
2.0000:  -1.8428
... Figure 301, middle plot
equivalent degrees of freedom =  34.4
smoothing window bandwidth    = 0.1349
|#

(multiple-value-bind (Daniell-m-30 freqs N-f nu B_W)
                     (lag-window-spectral-estimate
                      *the-acvs*
                      #'(lambda (lag)
                          (daniell-lag-window lag 30))
                      :sampling-time 1/4
                      :C_h *C_h*)
  (format t "~&Figure 301, bottom plot ...")
  (dotimes (i 7)
    (format t "~&~6,4F: ~8,4F"
            (svref freqs i)
            (svref Daniell-m-30 i)))
  (format t "~&...")
  (dotimes (i 7)
    (format t "~&~6,4F: ~8,4F"
            (svref freqs (+ i (- N-f 7)))
            (svref Daniell-m-30 (+ i (- N-f 7)))))
  (format t "~&... Figure 301, bottom plot")
  (format t "~&equivalent degrees of freedom = ~5,1F" nu)
  (format t "~&smoothing window bandwidth    = ~6,4F" B_W)
  (values))

#|
;;; Here is what is printed when the above form is evaluated:
Figure 301, bottom plot ...
0.0039:  52.1040
0.0078:  52.2497
0.0117:  52.4965
0.0156:  52.5851
0.0195:  52.4657
0.0234:  52.0096
0.0273:  51.6623
...
1.9766:  -6.0432
1.9805:  -5.9070
1.9844:  -5.8789
1.9883:  -5.7424
1.9922:  -5.4674
1.9961:  -5.2132
2.0000:  -5.1368
... Figure 301, bottom plot
equivalent degrees of freedom =  34.1
smoothing window bandwidth    = 0.1337
;;; Note: page 302 of the SAPA book gives the degrees of freedom
;;;       for these three cases as 13, 35 and 35 -- the above
;;;       calculations differs slightly because we have obtained
;;;       C_h using Equation (251b) rather from Table 248.
|#

;;; Here we show how to compute and relocate the spectral window
;;; shown in the lower left-hand corner of the top plot ...
(multiple-value-bind (the-smooth-wind freqs N-f)
                     (smoothing-window-for-lag-window-spectral-estimate
                      1024
                      #'(lambda (tau)
                          (parzen-lag-window tau 150))
                      :sampling-time 1/4
                      :N-nonzero-freqs :half-next-power-of-2)
  (let ((dB-offset (+ (- (svref the-smooth-wind 0)) -10.0)))
    (setf the-smooth-wind (one-sided-sdf->two-sided-sdf the-smooth-wind)
          freqs (one-sided-freq->two-sided-freq freqs)
          N-f (1- (* 2 N-f)))
    (x+b! the-smooth-wind dB-offset)
    (x+b! freqs 0.25)
  (dotimes (i N-f (values))
    (if (<= 0.2 (svref freqs i) 0.3)
      (format t "~&~6,4F: ~8,4F"
              (svref freqs i)
              (svref the-smooth-wind i))))))

#|
;;; Here is what is printed when the above form is evaluated:
0.2031: -44.8539
0.2070: -35.7970
0.2109: -29.5888
0.2148: -24.9154
0.2187: -21.2410
0.2227: -18.2942
0.2266: -15.9187
0.2305: -14.0171
0.2344: -12.5259
0.2383: -11.4022
0.2422: -10.6175
0.2461: -10.1536
0.2500: -10.0000
0.2539: -10.1536
0.2578: -10.6175
0.2617: -11.4022
0.2656: -12.5259
0.2695: -14.0171
0.2734: -15.9187
0.2773: -18.2942
0.2812: -21.2410
0.2852: -24.9154
0.2891: -29.5888
0.2930: -35.7970
0.2969: -44.8539
|#

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;;  Figure 305: periodograms for rough and smooth ice profile data
;;;  Note: in what follows we assume that the 1121 values for
;;;        rough ice profile data have been placed into the array pointed
;;;        to by *rough-ice-profile-data* and that the 288 values for
;;;        smooth ice profile data have been placed into the array pointed
;;;        to by *smooth-ice-profile-data* -- at the beginning of this file
;;;        are examples of how to load numbers from an ASCII file
;;;        into a Lisp vector.
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
(sample-mean *rough-ice-profile-data*)   ;==> -19.916859946476382

(multiple-value-bind (the-periodogram freqs N-f)
                     (periodogram
                      *rough-ice-profile-data*
                      :sampling-time 1.7712)
  (format t "~&Figure 305, top plot ...")
  (dotimes (i 7)
    (format t "~&~6,4F: ~8,4F"
            (svref freqs i)
            (svref the-periodogram i)))
  (format t "~&...")
  (dotimes (i 7)
    (format t "~&~6,4F: ~8,4F"
            (svref freqs (+ i (- N-f 7)))
            (svref the-periodogram (+ i (- N-f 7)))))
  (format t "~&... Figure 305, top plot")
  (values))

#|
;;; Here is what is printed when the above form is evaluated:
Figure 305, top plot ...
0.0003:  19.3206
0.0006:  29.8243
0.0008:  33.4726
0.0011:  33.9632
0.0014:  34.7072
0.0017:  35.0793
0.0019:  36.2898
...
0.2806:  -0.3305
0.2809:   0.2546
0.2812:  -1.4455
0.2815:   2.7054
0.2817:   4.2203
0.2820:  -5.3819
0.2823:  -0.1654
... Figure 305, top plot
|#

(sample-mean *smooth-ice-profile-data*)   ;==> -8.934027777777777

(multiple-value-bind (the-periodogram freqs N-f)
                     (periodogram
                      *smooth-ice-profile-data*
                      :sampling-time 1.7712)
  (format t "~&Figure 305, bottom plot ...")
  (dotimes (i 7)
    (format t "~&~6,4F: ~8,4F"
            (svref freqs i)
            (svref the-periodogram i)))
  (format t "~&...")
  (dotimes (i 7)
    (format t "~&~6,4F: ~8,4F"
            (svref freqs (+ i (- N-f 7)))
            (svref the-periodogram (+ i (- N-f 7)))))
  (format t "~&... Figure 305, bottom plot")
  (values))

#|
;;; Here is what is printed when the above form is evaluated:
Figure 305, bottom plot ...
0.0011:   5.7414
0.0022:  -0.3703
0.0033:   0.9999
0.0044:   4.3387
0.0055:   7.5413
0.0066:   4.8875
0.0077:   1.8933
...
0.2757: -11.9119
0.2768: -17.4537
0.2779: -22.4416
0.2790: -15.1718
0.2801:  -9.8917
0.2812: -10.6674
0.2823: -16.0906
... Figure 305, bottom plot
|#

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;;  Figure 307: lag window spectral estimates for rough and smooth
;;;              ice profile data
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
(let ((rough-acvs (acvs *rough-ice-profile-data*)))
  (multiple-value-bind (Parzen-m-88 freqs N-f nu B_W)
                       (lag-window-spectral-estimate
                        rough-acvs
                        #'(lambda (lag)
                            (parzen-lag-window lag 88))
                        :sampling-time 1.7712)
    (format t "~&Figure 307, top plot ...")
    (dotimes (i 7)
      (format t "~&~6,4F: ~8,4F"
              (svref freqs i)
              (svref Parzen-m-88 i)))
    (format t "~&...")
    (dotimes (i 7)
      (format t "~&~6,4F: ~8,4F"
              (svref freqs (+ i (- N-f 7)))
              (svref Parzen-m-88 (+ i (- N-f 7)))))
    (format t "~&... Figure 307, top plot")
    (format t "~&equivalent degrees of freedom = ~5,1F" nu)
    (format t "~&smoothing window bandwidth    = ~6,4F" B_W)
    (format t "~&time series bandwidth         = ~6,4F"
            (time-series-bandwidth rough-acvs :sampling-time 1.7712))))
#|
;;; Here is what is printed when the above form is evaluated:
Figure 307, top plot ...
0.0003:  32.2615
0.0006:  32.2518
0.0008:  32.2357
0.0011:  32.2130
0.0014:  32.1838
0.0017:  32.1480
0.0019:  32.1056
...
0.2806:   0.6920
0.2809:   0.6401
0.2812:   0.5964
0.2815:   0.5615
0.2817:   0.5360
0.2820:   0.5206
0.2823:   0.5154
... Figure 307, top plot
equivalent degrees of freedom =  47.2
smoothing window bandwidth    = 0.0119
time series bandwidth         = 0.0239
|#

(let ((smooth-acvs (acvs *smooth-ice-profile-data*)))
  (multiple-value-bind (Parzen-m-20 freqs N-f nu B_W)
                       (lag-window-spectral-estimate
                        smooth-acvs
                        #'(lambda (lag)
                            (parzen-lag-window lag 20))
                        :sampling-time 1.7712)
    (format t "~&Figure 307, middle plot ...")
    (dotimes (i 7)
      (format t "~&~6,4F: ~8,4F"
              (svref freqs i)
              (svref Parzen-m-20 i)))
    (format t "~&...")
    (dotimes (i 7)
      (format t "~&~6,4F: ~8,4F"
              (svref freqs (+ i (- N-f 7)))
              (svref Parzen-m-20 (+ i (- N-f 7)))))
    (format t "~&... Figure 307, middle plot")
    (format t "~&equivalent degrees of freedom = ~5,1F" nu)
    (format t "~&smoothing window bandwidth    = ~6,4F" B_W)
    (format t "~&time series bandwidth         = ~6,4F"
            (time-series-bandwidth smooth-acvs :sampling-time 1.7712)))
  (values))

#|
;;; Here is what is printed when the above form is evaluated:
Figure 307, middle plot ...
0.0011:   3.2732
0.0022:   3.2655
0.0033:   3.2527
0.0044:   3.2349
0.0055:   3.2120
0.0066:   3.1841
0.0077:   3.1513
...
0.2757: -13.2314
0.2768: -13.2507
0.2779: -13.2666
0.2790: -13.2789
0.2801: -13.2877
0.2812: -13.2930
0.2823: -13.2948
... Figure 307, middle plot
equivalent degrees of freedom =  53.4
smoothing window bandwidth    = 0.0523
time series bandwidth         = 0.1069
|#

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;;  Figure 312: WOSA spectral estimates of AR(2) series
;;;  Note: in what follows we assume that the 1024 values for AR(20 series
;;;        have been placed into a vector pointed to by *AR-2* -- at the
;;;        beginning of this file are examples of how to load numbers
;;;        from an ASCII file into a Lisp vector.
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;; block size = 8 points
(multiple-value-bind (wosa freqs dof)
                     (wosa-spectral-estimate
                      *AR-2*
                      8
                      :oversampling-factor 8)
  (format t "~&Figure 307, top plot ...")
  (dotimes (i 7)
    (format t "~&~6,4F: ~8,4F"
            (svref freqs i)
            (svref wosa i)))
  (format t "~&...")
  (let ((N-f (length freqs)))
    (dotimes (i 7)
      (format t "~&~6,4F: ~8,4F"
              (svref freqs (+ i (- N-f 7)))
              (svref wosa (+ i (- N-f 7))))))
  (format t "~&... Figure 307, top plot")
  (format t "~&equivalent degrees of freedom = ~5,1F" dof)
  (values))

#|
;;; Here is what is printed when the above form is evaluated:
Figure 307, top plot ...
0.0000:   3.1104
0.0156:   3.1860
0.0312:   3.3992
0.0469:   3.7143
0.0625:   4.0842
0.0781:   4.4621
0.0937:   4.8075
...
0.4062:  -4.2055
0.4219:  -4.5725
0.4375:  -4.8838
0.4531:  -5.1387
0.4687:  -5.3309
0.4844:  -5.4517
0.5000:  -5.4930
... Figure 307, top plot
equivalent degrees of freedom = 453.2
|#

;;; block size = 16 points
(multiple-value-bind (wosa freqs dof)
                     (wosa-spectral-estimate
                      *AR-2*
                      16
                      :oversampling-factor 8)
  (format t "~&Figure 307, middle plot ...")
  (dotimes (i 7)
    (format t "~&~6,4F: ~8,4F"
            (svref freqs i)
            (svref wosa i)))
  (format t "~&...")
  (let ((N-f (length freqs)))
    (dotimes (i 7)
      (format t "~&~6,4F: ~8,4F"
              (svref freqs (+ i (- N-f 7)))
              (svref wosa (+ i (- N-f 7))))))
  (format t "~&... Figure 307, middle plot")
  (format t "~&equivalent degrees of freedom = ~5,1F" dof)
  (values))

#|
;;; Here is what is printed when the above form is evaluated:
Figure 307, middle plot ...
0.0000:   2.7104
0.0078:   2.7175
0.0156:   2.7407
0.0234:   2.7860
0.0312:   2.8624
0.0391:   2.9807
0.0469:   3.1503
...
0.4531:  -5.7505
0.4609:  -5.9087
0.4687:  -6.0551
0.4766:  -6.1807
0.4844:  -6.2774
0.4922:  -6.3384
0.5000:  -6.3593
... Figure 307, middle plot
equivalent degrees of freedom = 233.8
|#

;;; block size = 64 points
(multiple-value-bind (wosa freqs dof)
                     (wosa-spectral-estimate
                      *AR-2*
                      64
                      :oversampling-factor 8)
  (format t "~&Figure 307, bottom plot ...")
  (dotimes (i 7)
    (format t "~&~6,4F: ~8,4F"
            (svref freqs i)
            (svref wosa i)))
  (format t "~&...")
  (let ((N-f (length freqs)))
    (dotimes (i 7)
      (format t "~&~6,4F: ~8,4F"
              (svref freqs (+ i (- N-f 7)))
              (svref wosa (+ i (- N-f 7))))))
  (format t "~&... Figure 307, bottom plot")
  (format t "~&equivalent degrees of freedom = ~5,1F" dof)
  (values))

#|
;;; Here is what is printed when the above form is evaluated:
Figure 307, bottom plot ...
0.0000:   2.6662
0.0020:   2.6924
0.0039:   2.7645
0.0059:   2.8654
0.0078:   2.9718
0.0098:   3.0595
0.0117:   3.1077
...
0.4883:  -8.0203
0.4902:  -8.1908
0.4922:  -8.3616
0.4941:  -8.5197
0.4961:  -8.6494
0.4980:  -8.7351
0.5000:  -8.7650
... Figure 307, bottom plot
equivalent degrees of freedom =  58.5
;;; Note: the above values for the equivalent degrees of freedom
;;;       differ from what is stated on page 311 of the SAPA book
;;;       (the book has 483.3, 240.7 and 58.8 rather than 453.2, 233.8 and 58.5).
;;;       This discrepancy is due to the fact that wosa-spectral-estimate
;;;       uses Equation (292b), whereas the values quoted in the book
;;;       are based upon Equation (294) (an approximation to Equation (292b)).
;;;       Also, the spectra plotted in Figure 312 were actually computed
;;;       using a slightly different definition for the Hanning data taper
;;;       (essentially the one in Bloomfield's book).  The spectra computed
;;;       above thus are slightly different from what are plotted in the SAPA
;;;       book (the biggest changes are a 0.5 dB difference at f = 0 and
;;;       f = 0.5 for the 8 point block size, i.e., the top plot of Figure 312).
|#

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;;  Figure 373: multitaper spectral estimates for ocean wave data
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
(multiple-value-bind (dpss-NW-4 eigenvalues-NW-4)
                     (dpss-tapers-tri-diag
                      (length *ocean-wave-data*)
                      7
                      :taper-parameter 4.0
                      :compute-true-eigenvalues-p t)
  (setf *eigenvalues-NW-4* eigenvalues-NW-4)
  (format t "~&order   eigenvalue")
  (dotimes (k 7)
    (format t "~&k = ~1D:  ~F" k (svref eigenvalues-NW-4 k)))
  ;;; We compute 7 eigenspectra but use only 6 of them to form
  ;;; the simple multitaper spectral estimate in plot (a).
  ;;; Note: for Figure 373, the eigenspectra are computed by setting
  ;;;       the keyword options recenter-after-tapering-p and
  ;;;       restore-power-option-p to nil.  This produces a spectral
  ;;;       estimate in agreement with the equations in Chapter 7,
  ;;;       but arguably it might be better to just leave these keywords
  ;;;       set to their default values of t.
  (multiple-value-bind
    (simple-multitaper-NW-4-0-to-6 freqs N-f eigenspectra-NW-4)
    (multitaper-spectral-estimate
     *ocean-wave-data*
     dpss-NW-4
     :sampling-time 1/4
     :recenter-after-tapering-p nil
     :restore-power-option-p nil)
    (declare (ignore simple-multitaper-NW-4-0-to-6))
    (setf *eigenspectra-NW-4* eigenspectra-NW-4
          *freqs* freqs)
    (let ((simple-multitaper-NW-4-0-to-5
           (eigenspectra->multitaper-spectral-estimate
            eigenspectra-NW-4 :N-eigenspectra 6)))
      (format t "~&Figure 373, plot (a) ...")
      (dotimes (i 7)
        (format t "~&~6,4F: ~8,4F"
                (svref freqs i)
                (svref simple-multitaper-NW-4-0-to-5 i)))
      (format t "~&...")
      (dotimes (i 7)
        (format t "~&~6,4F: ~8,4F"
                (svref freqs (+ i (- N-f 7)))
                (svref simple-multitaper-NW-4-0-to-5 (+ i (- N-f 7)))))
      (format t "~&... Figure 373, plot (a)")
      (values))))

#|
;;; Here is what is printed when the above form is evaluated:
order   eigenvalue
k = 0:  0.9999999997056523
k = 1:  0.9999999723287881
k = 2:  0.9999987902598706
k = 3:  0.9999675626065103
k = 4:  0.9994101803916527
k = 5:  0.9925053051988856
k = 6:  0.9366554082073846
Figure 373, plot (a) ...
0.0039:  45.0265
0.0078:  46.7926
0.0117:  48.5376
0.0156:  50.5297
0.0195:  51.1797
0.0234:  53.3410
0.0273:  53.9492
...
1.9766:  -5.0610
1.9805:  -4.9186
1.9844:  -4.7007
1.9883:  -3.5230
1.9922:  -3.0773
1.9961:  -3.3919
2.0000:  -3.3314
... Figure 373, plot (a)
|#

(multiple-value-bind (adaptive-multitaper-NW-4 dof)
                     (eigenspectra->adaptive-multitaper-spectral-estimate
                      *eigenspectra-NW-4*
                      *eigenvalues-NW-4*
                      (sample-variance *ocean-wave-data*)
                      :sampling-time 1/4)
  (multiple-value-bind (upper lower)
                       (create-ci-for-amt-sdf-estimate
                        adaptive-multitaper-NW-4
                        dof)
    (format t "~&Figure 373, plot (c) ...               plot (d) ...")
    (dotimes (i 7)
      (format t "~&~6,4F: ~8,4F ~8,4F ~8,4F    ~8,4F"
              (svref *freqs* i)
              (svref upper i)
              (svref adaptive-multitaper-NW-4 i)
              (svref lower i)
              (svref dof i)))
    (format t "~&...")
    (let ((N-f (length *freqs*)))
      (dotimes (i 7)
        (format t "~&~6,4F: ~8,4F ~8,4F ~8,4F    ~8,4F"
                (svref *freqs* (+ i (- N-f 7)))
                (svref upper (+ i (- N-f 7)))
                (svref adaptive-multitaper-NW-4 (+ i (- N-f 7)))
                (svref lower (+ i (- N-f 7)))
                (svref dof (+ i (- N-f 7))))))
    (format t "~&... Figure 373, plots (c) and (d)")
    (values)))

#|
;;; Here is what is printed when the above form is evaluated:
Figure 373, plot (c) ...               plot (d) ...
0.0039:  48.5041  44.5444  41.8350     13.9860
0.0078:  50.9711  47.0137  44.3053     13.9985
0.0117:  53.7760  49.8188  47.1104     13.9996
0.0156:  54.1360  50.1787  47.4704     13.9994
0.0195:  55.6164  51.6589  48.9505     13.9982
0.0234:  56.6806  52.7230  50.0145     13.9973
0.0273:  57.6636  53.7058  50.9973     13.9966
...
1.9766:   1.0163  -5.8131  -9.6216      6.0346
1.9805:  -0.5008  -7.4550 -11.3023      5.8786
1.9844:   2.9011  -3.7747  -7.5345      6.2375
1.9883:   2.5575  -4.1425  -7.9100      6.2047
1.9922:   3.3604  -3.2706  -7.0160      6.2991
1.9961:   2.2524  -4.4749  -8.2511      6.1681
2.0000:   4.0624  -2.5117  -6.2389      6.3789
... Figure 373, plots (c) and (d)
|#

(multiple-value-bind (simple-multitaper-NW-6-0-to-9 freqs N-f)
                     (multitaper-spectral-estimate
                      *ocean-wave-data*
                      (dpss-tapers-tri-diag
                       (length *ocean-wave-data*)
                       10
                       :taper-parameter 6.0)
                      :sampling-time 1/4
                      :recenter-after-tapering-p nil
                      :restore-power-option-p nil)
  (format t "~&Figure 373, plot (b) ...")
  (dotimes (i 7)
    (format t "~&~6,4F: ~8,4F"
            (svref freqs i)
            (svref simple-multitaper-NW-6-0-to-9 i)))
  (format t "~&...")
  (dotimes (i 7)
    (format t "~&~6,4F: ~8,4F"
            (svref freqs (+ i (- N-f 7)))
            (svref simple-multitaper-NW-6-0-to-9 (+ i (- N-f 7)))))
  (format t "~&... Figure 373, plot (b)")
  (values))

#|
Figure 373, plot (b) ...
0.0039:  48.0430
0.0078:  49.0666
0.0117:  49.7126
0.0156:  51.5444
0.0195:  51.9033
0.0234:  53.5943
0.0273:  53.7669
...
1.9766:  -4.2660
1.9805:  -4.8461
1.9844:  -4.4768
1.9883:  -4.8493
1.9922:  -3.9439
1.9961:  -2.6031
2.0000:  -1.7041
... Figure 373, plot (b)
|#

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;;  Figures 440-1: designing a prewhitening filter for ocean wave data
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;; estimation of AR(5) model using the Yule-Walker method,
;;; from which we compute a parametric sdf estimate ...
(multiple-value-bind (ar-5-coeffs innovations-variance)
                     (yule-walker-algorithm-given-data
                      *ocean-wave-data*
                      5)
  (multiple-value-bind (Y-W-5-sdf freqs N-f)
                       (ar-coeffs->sdf ar-5-coeffs
                                       innovations-variance
                                       :sampling-time 1/4
                                       :return-frequencies-p t)
    (format t "~&Figure 440, thin curve, top plot ...")
    (dotimes (i 7)
      (format t "~&~6,4F: ~8,4F"
              (svref freqs i)
              (svref Y-W-5-sdf i)))
    (format t "~&...")
    (dotimes (i 7)
      (format t "~&~6,4F: ~8,4F"
              (svref freqs (+ i (- N-f 7)))
              (svref Y-W-5-sdf (+ i (- N-f 7)))))
    (format t "~&... Figure 440, thin curve, top plot")
    (values)))

#|
;;; Here is what is printed when the above form is evaluated:
Figure 440, thin curve, top plot ...
0.0000:  51.2480
0.0078:  51.2659
0.0156:  51.3197
0.0234:  51.4097
0.0312:  51.5365
0.0391:  51.7008
0.0469:  51.9034
...
1.9531:  10.5844
1.9609:  10.5978
1.9687:  10.6089
1.9766:  10.6175
1.9844:  10.6237
1.9922:  10.6275
2.0000:  10.6287
... Figure 440, thin curve, top plot
|#

;;; estimation of AR(27) model using Burg's algorithm,
;;; from which we compute a parametric sdf estimate ...
(multiple-value-bind (ar-27-coeffs innovations-variance)
                     (Burg-algorithm
                      *ocean-wave-data*
                      27)
  (multiple-value-bind (Burg-27-sdf freqs N-f)
                       (ar-coeffs->sdf ar-27-coeffs
                                       innovations-variance
                                       :sampling-time 1/4
                                       :return-frequencies-p t)
    (format t "~&Figure 440, thin curve, bottom plot ...")
    (dotimes (i 7)
      (format t "~&~6,4F: ~8,4F"
              (svref freqs i)
              (svref Burg-27-sdf i)))
    (format t "~&...")
    (dotimes (i 7)
      (format t "~&~6,4F: ~8,4F"
              (svref freqs (+ i (- N-f 7)))
              (svref Burg-27-sdf (+ i (- N-f 7)))))
    (format t "~&... Figure 440, thin curve, bottom plot")
    (values)))

#|
;;; Here is what is printed when the above form is evaluated:
Figure 440, thin curve, bottom plot ...
0.0000:  51.8188
0.0078:  51.8771
0.0156:  52.0414
0.0234:  52.2796
0.0312:  52.5356
0.0391:  52.7342
0.0469:  52.7986
...
1.9531:  -6.2274
1.9609:  -5.8871
1.9687:  -5.5495
1.9766:  -5.2402
1.9844:  -4.9885
1.9922:  -4.8231
2.0000:  -4.7653
... Figure 440, thin curve, bottom plot
|#

;;; estimation of AR(5) model using Burg's algorithm,
;;; from which we [a] compute a parametric sdf estimate
;;;               [b] compute the periodogram for the forward prediction errors
;;;               [c] smooth the periodogram using a Parzen lag window and
;;;                   then postcolor this lag window estimate using the
;;;                   AR prewhitening filter
(multiple-value-bind (ar-5-coeffs innovations-variance
                                  junk-1 junk-2
                                  forward-pred-errors)
                     (Burg-algorithm *ocean-wave-data* 5)
  (declare (ignore junk-1 junk-2))
  (setf *ar-5-coeffs* ar-5-coeffs
        *forward-pred-errors* forward-pred-errors)
  (multiple-value-bind (Burg-5-sdf freqs N-f)
                       (ar-coeffs->sdf ar-5-coeffs
                                       innovations-variance
                                       :sampling-time 1/4
                                       :return-frequencies-p t)
    (format t "~&Figure 440, thick curve, both plots ...")
    (dotimes (i 7)
      (format t "~&~6,4F: ~8,4F"
              (svref freqs i)
              (svref Burg-5-sdf i)))
    (format t "~&...")
    (dotimes (i 7)
      (format t "~&~6,4F: ~8,4F"
              (svref freqs (+ i (- N-f 7)))
              (svref Burg-5-sdf (+ i (- N-f 7)))))
    (format t "~&... Figure 440, thick curve, both plots")
    (values)))

#|
;;; Here is what is printed when the above form is evaluated:
Figure 440, thick curve, both plots ...
0.0000:  51.2758
0.0078:  51.2919
0.0156:  51.3403
0.0234:  51.4215
0.0312:  51.5361
0.0391:  51.6849
0.0469:  51.8690
...
1.9531:  -7.9316
1.9609:  -7.9411
1.9687:  -7.9489
1.9766:  -7.9549
1.9844:  -7.9592
1.9922:  -7.9618
2.0000:  -7.9627
... Figure 440, thick curve, both plots
|#

(progn
  (format t "~&Figure 441, plot (a) ...")
  (dotimes (i 7)
    (format t "~&~4D: ~8,4F"
            (+ i 6)
            (svref *forward-pred-errors* i)))
  (format t "~&...")
  (let ((N-fpe (length *forward-pred-errors*)))
    (dotimes (i 7)
      (format t "~&~4D: ~8,4F"
              (+  i 6 (- N-fpe 7))
              (svref *forward-pred-errors* (+ i (- N-fpe 7))))))
  (format t "~&... Figure 441, plot (a)")
  (values))

#|
;;; Here is what is printed when the above form is evaluated:
Figure 441, plot (a) ...
   6:   6.6713
   7:  14.7491
   8:  -5.1810
   9:   2.9906
  10:  -9.3390
  11:  21.8462
  12:  -0.5174
...
1018: -12.8457
1019:   0.9300
1020:  -6.7758
1021:   3.0929
1022:   3.7940
1023:   3.0392
1024:   0.2890
... Figure 441, plot (a)
|#

(multiple-value-bind (the-periodogram freqs N-f)
                     (periodogram
                      *forward-pred-errors*
                      :sampling-time 1/4)
  (format t "~&Figure 441, plot (b) ...")
  (dotimes (i 7)
    (format t "~&~6,4F: ~8,4F"
            (svref freqs i)
            (svref the-periodogram i)))
  (format t "~&...")
  (dotimes (i 7)
    (format t "~&~6,4F: ~8,4F"
            (svref freqs (+ i (- N-f 7)))
            (svref the-periodogram (+ i (- N-f 7)))))
  (format t "~&... Figure 441, plot (b)")
  (values))

#|
;;; Here is what is printed when the above form is evaluated:
Figure 441, plot (b) ...
0.0039:  12.3693
0.0078:   4.4997
0.0117:   7.5193
0.0156:   8.1898
0.0195:  14.8114
0.0234:  19.1417
0.0273:  18.2599
...
1.9766:  17.3194
1.9805:  17.9742
1.9844:  18.6913
1.9883:  10.7566
1.9922:  25.4055
1.9961:  10.0394
2.0000:   6.1584
... Figure 441, plot (b)
|#

(multiple-value-bind (Parzen-m-55 freqs N-f nu B_W)
                     (lag-window-spectral-estimate
                      (acvs *forward-pred-errors*)
                      #'(lambda (lag)
                          (parzen-lag-window lag 55))
                      :sampling-time 1/4
                      :sdf-transformation nil)
  (let ((postcolored-Parzen-m-55 (postcolor-spectral-estimate
                                  Parzen-m-55
                                  (ar-coeffs->prewhitening-filter *ar-5-coeffs*)
                                  (length *forward-pred-errors*))))
    (format t "~&Figure 441, thick curve, plot (c) ...")
    (dotimes (i 7)
      (format t "~&~6,4F: ~8,4F"
              (svref freqs i)
              (svref postcolored-Parzen-m-55 i)))
    (format t "~&...")
    (dotimes (i 7)
      (format t "~&~6,4F: ~8,4F"
              (svref freqs (+ i (- N-f 7)))
              (svref postcolored-Parzen-m-55 (+ i (- N-f 7)))))
    (format t "~&... Figure 441, thick curve, plot (c)")
    (format t "~&equivalent degrees of freedom = ~5,1F" nu)
    (format t "~&smoothing window bandwidth    = ~6,4F" B_W))
  (values))

#|
;;; Here is what is printed when the above form is evaluated:
Figure 441, thick curve, plot (c) ...
0.0039:  51.6943
0.0078:  51.7057
0.0117:  51.7240
0.0156:  51.7487
0.0195:  51.7789
0.0234:  51.8134
0.0273:  51.8512
...
1.9766:  -5.6470
1.9805:  -5.5772
1.9844:  -5.5188
1.9883:  -5.4726
1.9922:  -5.4392
1.9961:  -5.4190
2.0000:  -5.4122
... Figure 441, thick curve, plot (c)
equivalent degrees of freedom =  68.7
smoothing window bandwidth    = 0.1349
|#
