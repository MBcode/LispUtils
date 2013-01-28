;;;-*- Mode: LISP; Package: :SAPA; Syntax: COMMON-LISP -*-
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;
;  random.lisp
;
;  a collection of Lisp functions to simulate stationary random processes ...
;  Note:  before compiling and loading random.lisp,
;         you should compile and load (in the order listed)
;            sapa-package.lisp, utilities.lisp and dft-and-fft.lisp
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
;;; (compile-file "ccl:SAPA;random.lisp")
;;; (load "ccl:SAPA;random.fasl")
;-------------------------------------------------------------------------------
(if (not (find-package :SAPA))
  (defpackage :SAPA (:USE :COMMON-LISP)))

(in-package :SAPA)

(export '(;;; functions to generate normally distributed random deviates ...
          ranorm
          ranorms

          ;;; white noise with different distributions ...
          generate-white-noise
          
          ;;; normally distributed moving average and autoregressive processes
          generate-ma-time-series
          generate-ar-time-series
          step-down-Levinson-Durbin-recursions

          ;;; simulate a stationary process using frequency domain techniques
          simulate-time-series-from-sdf
          acvs-for-time-series-simulated-from-sdf
          ))

;-------------------------------------------------------------------------------
(defconstant +sapa-sqrt-8-over-e+ (sqrt (/ 8.0d0 (exp 1.0d0))))
(defconstant +sapa-4-time-exp-of-1-over-4+ (* 4.0d0 (exp 0.25d0)))
(defconstant +sapa-4-time-exp-of-minus-1-point-35+ (* 4.0d0 (exp (- 1.35d0))))

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;;  The functions  ranorm
;;;                 ranorms
;;;  generate one or more uncorrelated normally distributed deviates.
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
(defun ranorm
       ()
  "returns a random deviate from a normal distribution
with zero mean and unit variance"
  (let ((u (random 1.0)))
    (cond
     ((= u 0.0) (ranorm))   ;bad choice!
     (t
      (let* ((x (/ (* (- (random 1.0) 0.5)
                      +sapa-sqrt-8-over-e+) u))
             (xs (* x x)))
        (cond
         ((<= xs (- 5.0 (* u +sapa-4-time-exp-of-1-over-4+)))
          x)   ;done
         ((>= xs (+ 1.4 (/ +sapa-4-time-exp-of-minus-1-point-35+ u)))
          (ranorm))   ;do it again
         ((<= xs (- (* 4.0 (log u))))
          x)
         (t
          (ranorm))))))))

#|
;;; each evaluation of ranorm produces a different random deviate,
;;; so don't expect to get the same result!
(ranorm)  ;==> -0.2719851788703296
|#

;-------------------------------------------------------------------------------
(defun ranorms
       (n
        &key
        (result (make-array n)))
  "given:
   [1] n (required)
       ==> number of normal random deviates
           to be generated
   [2] result (keyword; vector of length n)
       <== vector to hold random deviates
returns:
   [1] result, a vector with n normal random deviates
       (i.e., a realization of length n of a white
       noise process from a normal distribution)"
  (dotimes (i n result)
    (setf (svref result i) (ranorm))))

#|
(ranorms 3)
;==> #(-1.1128679745343053 1.4875994154684462 -0.06612547414267966)
|#

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;;  The function  generate-white-noise
;;;  generate a sequence of white noise with a specified distribution.
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
(defun generate-white-noise
       (n
        &key
        (distribution :normal)
        (result (make-array n)))
  "given
   [1] n (required)
       ==> a sample size
   [2] distribution (keyword; :normal)
       ==> either a keyword or a function with no arguments
           that returns a random deviate from a particular
           distribution. Choices are
            :binary :cauchy :chi-square-2
            :double-exponential :exponential
            :extreme-value :Gaussian (same as :normal)
            :logistic :lognormal :normal :uniform
   [3] result (keyword; vector of length n)
       <== a sequence in which results are to be stored
return
   [1] result, containing n samples from a white noise process
       with a distribution specified by the keyword distribution"
  (let ((generating-function
         (if (keywordp distribution)
           (case distribution
             (:binary #'(lambda ()
                          (if (<= (random 1.0) 0.5)
                            0 1)))
             (:cauchy #'(lambda ()
                          (tan (* pi (- (random 1.0) 0.5)))))
             (:chi-square-2 #'(lambda ()
                                (* 2 (- (log (- 1.0 (random 1.0)))))))
             (:double-exponential #'(lambda ()
                                      (let ((U (random 1.0)))
                                        (if (<= U 0.5)
                                          (log (* 2 U))
                                          (- (log (* 2 (- 1.0 U))))))))
             (:exponential #'(lambda ()
                               (- (log (- 1.0 (random 1.0))))))
             (:extreme-value #'(lambda ()
                                 (log (- (log (- 1.0 (random 1.0)))))))
             (:Gaussian #'ranorm)
             (:logistic #'(lambda ()
                            (let ((U (random 1.0)))
                              (log (/ U (- 1.0 U))))))
             (:lognormal #'(lambda ()
                             (exp (ranorm))))
             (:normal #'ranorm)
             (:uniform #'(lambda () (random 1.0)))
             (otherwise
              (error "~&generate-white-noise: unknown distribution keyword (~A)"
                     distribution)))
           distribution)))
    (dotimes (i n result)
      (setf (elt result i) (funcall generating-function)))))

#|
(generate-white-noise 10 :distribution :binary)
;==> #(1 0 1 0 1 0 0 0 0 0)
(generate-white-noise 3 :distribution :cauchy)
;==> #(-27.51099440214296 -0.6075812207412569 0.36550059169755994)
(generate-white-noise 3 :distribution :lognormal)
;==> #(0.5822708648379962 3.833558848750849 0.3592735989502123)
|#

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;;  The functions  generate-ma-time-series
;;;                 generate-ar-time-series
;;;                 step-down-Levinson-Durbin-recursions
;;;  can be used to generate realizations of specified lengths from
;;;  normally distributed moving average (MA) or autoregressive processes (AR)
;;;  (these processes are discussed in Chapter 2 and 9 of the SAPA book).
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
(defun generate-ma-time-series
       (coeffs
        variance
        sample-size
        &key
        (process-variance-p t)
        (result (make-array sample-size)))
  "given
   [1] coeffs (required)
       ==> sequence of length q with MA coefficients:
           x_t = e_t - coeffs_0*e_{t-1}
                 - coeffs_1*e_{t-2}
                 - ... - coeffs_{q-1}*e_{t-q}
       (see Equation (43a) in the SAPA book)
   [2] variance (required)
       ==> process variance or innovations variance
           (see keyword process-variance-p)
   [3] sample-size (required)
       ==> length of generated time series
   [4] process-variance-p (keyword; t)
       ==> if t, variance is taken to be process variance
           if nil, variance is taken to be innovations variance
   [5] result (keyword; vector of length sample-size)
       <== vector with simulated series
generates realization of zero mean normally distributed MA(q) process and
returns
   [1] vector of length sample-size with realization"
  (let ((q+1 (1+ (length coeffs)))
        (sd (sqrt (if process-variance-p
                    (/ variance (1+ (sum-of-squares coeffs)))
                    variance)))
        (scratch-1 (make-array (1+ (length coeffs))))
        (scratch-2 (make-array (1+ (length coeffs)))))
    #+mcl(declare (dynamic-extent scratch-1 scratch-2))
    (generate-white-noise q+1 :result scratch-1)
    (a*x! sd scratch-1)
    (dotimes (i (1- q+1))
      (setf (aref scratch-2 (1+ i)) (- (elt coeffs i))))
    (setf (aref scratch-2 0) 1.0)
    (dotimes (i sample-size result)
      (setf (aref result i)
            (dot-product scratch-2 scratch-1))
      (circular-shift-sequence scratch-1 :result scratch-1)
      (setf (aref scratch-1 0) (* sd (ranorm))))))

#|
;;; See top plot of Figure 44 of the SAPA book.
(generate-ma-time-series #(1.0) 1.0 4 :process-variance-p nil)
;==> #(-0.555869655695869 -0.6755180985339049 0.3536345814801179 0.7421943411372343)
;;; See bottom plot of Figure 44 of the SAPA book.
(generate-ma-time-series #(-1.0) 1.0 4 :process-variance-p nil)
#(0.2105828208289749 1.1611109339530934 1.7492356283253514 0.07032022771512858)
|#

;-------------------------------------------------------------------------------
(defun generate-ar-time-series
       (coeffs
        variance
        sample-size
        &key
        (process-variance-p t)
        (list-of-lower-order-phi nil)
        (list-of-lower-order-pev nil)
        (result (make-array
                 sample-size
                 :initial-element 0.0)
                result-supplied-p))
  "given
   [1] coeffs (required)
       ==> sequence of length p with AR coefficients:
           x_t = coeffs_0*x_{t-1} + coeffs_1*x_{t-2}
                 + ... + coeffs_{p-1}*x_{t-p} + e_t
           (see Equation (392a) in the SAPA book;
           the coefficients can be real or complex-valued)
   [2] variance (required)
       ==> process variance or innovations variance
           (see keyword process-variance-p)
   [3] sample-size (required)
       ==> length of generated time series
   [4] process-variance-p (keyword; t)
       ==> if t, variance is taken to be process variance
           if nil, variance is taken to be innovations variance
   [5] list-of-lower-order-phi (keyword; nil)
       ==> to bypass call to step-down-Levinson-Durbin-recursions
           (useful for multiple realizations)
   [6] list-of-lower-order-pev (keyword; nil)
       ==> to bypass call to step-down-Levinson-Durbin-recursions
   [7] result (keyword; vector of length sample-size)
       <== vector with simulated series
generates realization of zero mean normally distributed AR(p) process and
returns
   [1] result, vector of length sample-size with realization
   [2] list-of-lower-order-phi (can be used on subsequent calls
       to this function to bypass call to
       step-down-Levinson-Durbin-recursions)
   [3] list-of-lower-order-pev (can be also used on subsequent calls)
---
Note: this function generates the proper stationary initial conditions"
  (if result-supplied-p (fill result 0.0 :end sample-size))
  (if (not list-of-lower-order-pev)
    (multiple-value-setq (list-of-lower-order-phi list-of-lower-order-pev)
      (step-down-Levinson-Durbin-recursions
       coeffs variance
       :process-variance-p process-variance-p)))
  (let ((p (length coeffs))
        (sd (sqrt (nth 0 list-of-lower-order-pev))))
    (dotimes (i sample-size (values result
                                    list-of-lower-order-phi
                                    list-of-lower-order-pev))
      (cond
       ((< i p)
        (dotimes (j i)
          (incf (aref result i)
                (* (aref result (- i j 1))
                   (aref (nth (1- i) list-of-lower-order-phi) j))))
        (incf (aref result i) (* sd (ranorm)))
        (setf sd (sqrt (nth (1+ i) list-of-lower-order-pev))))
       (t
        (dotimes (j p)
          (incf (aref result i) (* (aref result (- i j 1))
                                   (elt coeffs j))))
        (incf (aref result i) (* sd (ranorm))))))))

#|
;;; AR(2) process described by Equation (45) of the SAPA book:
(generate-ar-time-series #(0.75 -0.5) 1.0 4 :process-variance-p nil)
;==> #(-0.5263377203675295 -1.5334622586392335 -1.628199801947855 1.0935751830627396)
;    (#(0.5) #(0.75 -0.5))
;    (1.7777777777777777 1.3333333333333333 1.0)

;;; AR(4) process described by Equation (46a) of the SAPA book:
(generate-ar-time-series #(2.7607 -3.8106  2.6535 -0.9238)
                         1.0 4 :process-variance-p nil)
;==> #(-6.887266139675448 -31.09831681568327 -34.70711181816937 -17.248058702547237)
     (#(0.7164772070165697)
      #(1.4197722065109775 -0.9816013581547822)
      #(2.1105749802378733 -1.9807672315209488 0.7037508332562511)
      #(2.7607 -3.8106 2.6535 -0.9238))
;    (761.7172900314962 370.6976500615111 13.515181723106767 6.821582066770185 1.0)
|#

;-------------------------------------------------------------------------------
(defun step-down-Levinson-Durbin-recursions
       (coeffs
        variance
        &key
        (process-variance-p t))
  "given
   [1] coeffs (required)
       ==> sequence of length p with AR coefficients:
           x_t = coeffs_0*x_{t-1} + coeffs_1*x_{t-2}
                 + ... + coeffs_{p-1}*x_{t-p} + e_t
           (see Equation (392a) in the SAPA book;
           the coefficients can be real or complex-valued)
   [2] variance (required)
       ==> process variance or innovations variance
           (see keyword process-variance-p)
   [3] process-variance-p (keyword; t)
       ==> if t, variance is taken to be process variance
           if nil, variance is taken to be innovations variance
computes best linear prediction coefficients of orders 1, 2, ..., p-1
and prediction error variances of orders 0 (process variance), 1, ..., p and
returns
   [1] list of vectors with best linear prediction coefficients
       going from order 1 to order p;
   [2] list of prediction error variances going from order 0 to order p
---
Note: see item [4] of the Comments and Extensions
      to Section 9.4 of the SAPA book.  The values returned by
      this function can be used to set the keyword parameters
      list-of-lower-order-phi and list-of-lower-order-pev in
      the function generate-ar-time-series"
  (let* ((p (length coeffs))
         (p-1 (1- p))
         (list-of-lower-order-phi `(,coeffs))
         (list-of-lower-order-pev `(,variance))
         k-1 phi-k phi-k-1 den)
    ;;; generate lower order coefficients via step-down Levinson-Durbin
    (dotimes (i p-1)
      (setf k-1 (- p-1 i)
            phi-k (car list-of-lower-order-phi)
            phi-k-1 (car (push (make-array k-1) list-of-lower-order-phi))
            den (- 1.0 (realpart
                        (* (aref phi-k k-1) (conjugate (aref phi-k k-1))))))
      (dotimes (j k-1)
        (setf (aref phi-k-1 j) (/ (+ (aref phi-k j)
                                     (* (aref phi-k k-1)
                                        (conjugate (aref phi-k (- k-1 j 1)))))
                                  den))))
    (values (if (plusp p) list-of-lower-order-phi)
            (cond
             (process-variance-p
              (dotimes (i p (reverse list-of-lower-order-pev))
                (push (* (car list-of-lower-order-pev)
                         (- 1.0
                            (realpart
                             (* (aref (nth i list-of-lower-order-phi) i)
                                (conjugate
                                 (aref (nth i list-of-lower-order-phi) i))))))
                      list-of-lower-order-pev)))
             (t
              (dotimes (i p list-of-lower-order-pev)
                (push (/ (car list-of-lower-order-pev)
                         (- 1.0
                            (realpart
                             (* (aref (nth (- p-1 i) list-of-lower-order-phi)
                                      (- p-1 i))
                                (conjugate
                                 (aref (nth (- p-1 i) list-of-lower-order-phi)
                                       (- p-1 i)))))))
                      list-of-lower-order-pev)))))))

#|
;;; AR(2) process described by Equation (45) of the SAPA book:
(step-down-Levinson-Durbin-recursions #(0.75 -0.5)
                                      1.0 :process-variance-p nil)
;==> (#(0.5) #(0.75 -0.5))
;    (1.7777777777777777 1.3333333333333333 1.0)

;;; AR(4) process described by Equation (46a) of the SAPA book:
(step-down-Levinson-Durbin-recursions #(2.7607 -3.8106  2.6535 -0.9238)
                                      1.0 :process-variance-p nil)
;==> (#(0.7164772070165697)
      #(1.4197722065109775 -0.9816013581547822)
      #(2.1105749802378733 -1.9807672315209488 0.7037508332562511)
      #(2.7607 -3.8106 2.6535 -0.9238))
;    (761.7172900314962 370.6976500615111 13.515181723106767 6.821582066770185 1.0)
|#

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;;  The functions  simulate-time-series-from-sdf
;;;                 acvs-for-time-series-simulated-from-sdf
;;;  can be used to generate realizations of specified lengths
;;;  from normally distributed stationary processes with a specified
;;;  spectral density function or autocovariance sequence.  These functions
;;;  use frequency domain techniques.  The techniques are discussed
;;;  in the paper ``Simulating Gaussian Random Processes
;;;  with Specified Spectra'' by Percival in "Computing Science and Statistics",
;;;  vol 24, 534--8 (contains Proc. of 24th Symp. on the Interface of Computer
;;;  Science and Statistics).
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
(defun simulate-time-series-from-sdf
       (n-series
        sdf
        &key
        (sampling-time 1.0)
        (n-total
         (* 4 (next-power-of-2 n-series)))
        (result (make-array n-series)))
  "given
   [1] n-series (required)
       ==> length of time series to be simulated
   [2] sdf (required)
       ==> spectral density function (sdf) defined for
           0 <= f <= 1/(2.0 sampling-time) -- the
           sdf is assumed to be two-sided and symmetric
           about f = 0
   [3] sampling-time (keyword; 1.0)
       ==> the assumed sampling time (delta t)
   [4] n-total (keyword; 4 * next power of 2 for n-series)
       ==> a power of 2 controlling degree of accuracy
           of the approximation (the larger, the better --
           see Percival, 1992, for details)
   [5] result (keyword; vector of length n-series)
       <== a vector to contain simulated time series
returns
   [1] a vector of length n-series generated from sdf
---
Note: the method used here is an approximate frequency domain
      technique; in particular, it will NOT simulate the DC component
      correctly, so beware of using this function in studies
      where the process mean is of important"
  ;;; This assertion fails if N is not a power of two 
  (assert n-total
          ()
          "n-total = ~D not a power of 2"
          n-total)
  (let* ((n-sampling-time (float (* n-total sampling-time)))
         (vector-for-fft (make-array n-total))
         (j 0)
         (n-j n-total)
         (n-over-2 (/ n-total 2))
         (1-over-sqrt-n-sampling-time (/ (sqrt (* n-total sampling-time)))))
    (setf (svref vector-for-fft 0)
          (* (sqrt (funcall sdf 0.0))
             (ranorm))
          (svref vector-for-fft n-over-2)
          (* (sqrt (funcall sdf (/ 0.5 sampling-time)))
             (ranorm)))
    (dotimes (i (1- n-over-2))
      (incf j) (decf n-j)
      (setf (svref vector-for-fft n-j)
            (conjugate
             (setf (svref vector-for-fft j)
                   (* (sqrt (* 0.5 (funcall sdf (/ j n-sampling-time))))
                      (complex (ranorm) (ranorm)))))))
    (fft! vector-for-fft)
    (dotimes (i n-series result)
      (setf (svref result i)
            (* (realpart (svref vector-for-fft i))
               1-over-sqrt-n-sampling-time)))))

#|
(simulate-time-series-from-sdf
 4
 #'(lambda (f) (1+ f)))
;==> #(-0.775118374075842 -0.13513773684982933 -1.4345372479995293 0.31204493073149375)
|#

;-------------------------------------------------------------------------------
(defun acvs-for-time-series-simulated-from-sdf
       (n-series
        sdf
        &key
        (sampling-time 1.0)
        (n-total (* 4 (next-power-of-2 n-series)))
        (result (make-array n-series)))
  "given
   [1] n-series (required)
       ==> length of time series simulated using
           simulate-time-series-from-sdf
           (must be a power of 2)
   [2] sdf (required)
       ==> spectral density function (sdf) defined for
           0 <= f <= 1/(2.0 sampling-time) -- the
           sdf is assumed to be two-sided and symmetric
           about f = 0
   [3] sampling-time (keyword; 1.0)
       ==> the assumed sampling time (delta t)
   [4] n-total (keyword; 4 * next power of 2 for n-series)
       ==> a power of 2 controlling degree of accuracy
           of the approximation (the larger, the better --
           see Percival, 1992, for details)
   [5] result (keyword; vector of length n-series)
       <== a vector to contain simulated time series
returns
   [1] a vector with the theoretical acvs from lag 0
       to n-series - 1 for a time series generated via a call
       to simulate-time-series-from-sdf with n-series,
       sdf, sampling-time and n-total set to the same values"
  (assert (power-of-2 n-total))
  (let* ((sigma^2 (make-array n-total))
         (n-total-sampling-time (* n-total sampling-time))
         (j 1)
         (N-j (1- n-total)))
    (setf (aref sigma^2 0)
          (/ (funcall sdf 0.0) n-total-sampling-time)
          (aref sigma^2 (/ n-total 2))
          (/ (funcall sdf (/ 0.5 sampling-time)) n-total-sampling-time))
    (dotimes (i (- (/ n-total 2) 1))
      (setf (aref sigma^2 j)
            (/ (funcall sdf (/ j n-total-sampling-time))
               n-total-sampling-time)
            (aref sigma^2 N-j) (aref sigma^2 j))
      (incf j)
      (decf N-j))
    (fft! sigma^2)
    (dotimes (i n-series result)
      (setf (aref result i) (realpart (aref sigma^2 i))))))

#|
(acvs-for-time-series-simulated-from-sdf
 4
 #'(lambda (f) (1+ f)))
;==> #(1.25 -0.10263336862925071 0.0 -0.012655581284545123)
|#
