;;;-*- Mode: LISP; Package: :SAPA; Syntax: COMMON-LISP -*-
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;
;  basic-math.lisp
;
;  a collection of Lisp functions for certain basic mathematical operations ...
;  Note:  before compiling and loading basic-math.lisp,
;         you should compile and load
;            sapa-package.lisp
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
;;; (compile-file "ccl:SAPA;basic-math.lisp")
;;; (load "ccl:SAPA;basic-math.fasl")
;-------------------------------------------------------------------------------
(if (not (find-package :SAPA))
  (defpackage :SAPA (:USE :COMMON-LISP)))

(in-package :SAPA)

(export '(;;; functions related to the gamma function and its derivatives ...
          log-of-gamma
          factorial
          digamma
          trigamma

          ;;; functions for dealing with polynomials ...
          evaluate-polynomial
          multiply-2-polynomials
          multiply-polynomials
          zeros-of-polynomial

          ;;; functions for finding the root of a function ...
          Newton-Raphson
          bisection-with-Newton-Raphson
          secant-method

          ;;; functions for numerical integation ...
          simple-numerical-integration
          Gauss-Legendre-quadrature
          ))

;-------------------------------------------------------------------------------
;;; used by the next set of functions ...
(defparameter +coefficents-for-log-of-gamma+
  (list 76.18009172947146d0  -86.50532032941677d0      24.01409824083091d0
        -1.231739572450155d0   0.1208650973866179d-2   -0.5395239384953d-5))

;-------------------------------------------------------------------------------
;;; used by the next set of functions ...
(defconstant +log-pi+ (log pi))
(defconstant +euler-constant+ 0.577215664901532860606512d0)

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;;  The functions  log-of-gamma
;;;                 factorial
;;;                 digamma
;;;                 trigamma
;;;  compute various quantities that are related to the gamma function
;;;  (log-of-gamma, factorial), its first derivative (digamma) and
;;;  its second derivative (trigamma).
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
(defun log-of-gamma (xx)
  "given xx (a real or complex valued number
whose real part is greater than 0),
returns the log (base e) of gamma(xx)
---
Note: based upon the discussion in Section 6.1,
Numerical Recipes, Second Edition"
  (assert (plusp (realpart xx)))
  (if (< (realpart xx) 1)
    ;;; use reflection formula (6.1.4)
    (let ((1-xx (- 1.0 xx)))
      (- (+ +log-pi+ (log 1-xx))
         (+ (log-of-gamma (1+ 1-xx)) (log (sin (* pi 1-xx))))))
    ;;; since Re(xx) > 1, use approximation due to Lanczos
    (let* ((x (1- xx))
           (tmp-1 (+ x 5.5))
           (tmp-2 (- (* (+ x 0.5) (log tmp-1)) tmp-1))
           (ser 1.000000000190015d0))
      (dolist (a-coefficient +coefficents-for-log-of-gamma+)
        (incf x)
        (incf ser (/ a-coefficient x)))
      (+ tmp-2 (log (* 2.5066282746310005 ser))))))

;;; (log-of-gamma 6.0)  ;==> 4.787491742782046
;;; above should agree with
;;; (log (* 2 3 4 5))   ;==> 4.787491742782046
;;; and it does!
;;; (log-of-gamma 0.5)  ;==> 0.5723649429246563
;;; above should agree with
;;; (* 0.5 (log pi))    ;==> 0.5723649429247001
;;; and it does!
;;; (exp (log-of-gamma 1.755))  ;==> 0.9202092223790562
;;; above should agree with entry in row 2, column 2,
;;; page 270 of Abramowitz and Stegun, which has
;;;                                  0.9202092224
;;; and it does!

;-------------------------------------------------------------------------------
(defun factorial (k)
  "given an integer k, returns k!"
  (assert (and (integerp k) (not (minusp k))))
  (cond
   ((> k 15)  ; arbitrary choice ...
    (exp (log-of-gamma (1+ k))))
   ((or (= k 1) (= k 0))
    1)
   (t
    (* k (factorial (1- k))))))

#|
(factorial 0)   ;==> 1
(factorial 1)   ;==> 1
(factorial 6)   ;==> 720
(factorial 25)  ;==> 1.5511210043610457E+25
|#

;-------------------------------------------------------------------------------
(defun digamma
       (x
        &key
        (x-recursion 8.5)
        (x-small 1.0e-5))
  "given
   [1] x (required)
       ==> a positive number
   [2] x-recursion (keyword; 8.5)
       ==> for noninteger x,
           if x-small < x < x-recursion,
           recursive formula is used
   [3] x-small (keyword; 1.0e-5)
       ==> if x <= x-small,
           small x approximation used (default )
returns
   [1] value of digamma (psi, derivative of log(gamma)) function at x
---
Note: see Abramowitz and Stegun's equation 6.3.2;
expansion 6.3.18 plus recurrence 6.3.5;
the small x formula is Equation (5) from
Algorithm AS 103 -- Psi (Digamma) Function -- by J. M. Bernardo
in Applied Statistics, Vol. 25, No. 3, 1976, pp. 315--317;
note that better accuracy can be obtained by increasing
x-recursion --- 10.0 to 100.0 are probably useful values"
  (assert (plusp x))
  (cond
   ((integerp x)
    (let ((sum (- +euler-constant+)))
      (dotimes (i (1- x) sum)
        (incf sum (/ (1+ i))))))
   ((<= x x-small)
    (- (+ +euler-constant+ (/ x))))
   ((< x x-recursion)
    (- (digamma (1+ x) :x-recursion x-recursion) (/ x)))
   (t
    (let* ((x2 (* x x))
           (x4 (* x2 x2)))
      (- (log x)
         (/ (* 2.0 x))
         (/ (* 12.0 x2))
         (/ (* -120.0 x4))
         (/ (* 252.0 x2 x4)))))))

#|
(let ((x*1000 1500)
      x)
  (dotimes (i 15)
    (setf x (float (/ x*1000 1000)))
    (format t "~&~5,3F   ~12,10F" x (digamma x))
    (incf x*1000 5)))
;==>
1.500   0.0364899738
1.505   0.0411536541
1.510   0.0457967894
1.515   0.0504195526
1.520   0.0550221144
1.525   0.0596046437
1.530   0.0641673072
1.535   0.0687102696
1.540   0.0732336935
1.545   0.0777377398
1.550   0.0822225674
1.555   0.0866883332
1.560   0.0911351924
1.565   0.0955632983
1.570   0.0999728023
;;; good agreement with 4th column of top of page 269, Abramowitz and Stegun
|#

;-------------------------------------------------------------------------------
(defun trigamma
       (x
        &key
        (x-recursion 2.0))
  "given
   [1] x (required)
       ==> a positive number
   [2] x-recursion (keyword; 2.0)
       ==> if  x < x-recursion,
           recursive formula is used
returns
   [1] value of trigamma function at x
---
Note: expansion 6.4.12 plus recurrence 6.4.6
of Abramowitz and Stegun;
better accuracy can be obtained by increasing
x-recursion ---10.0 to 30.0 is probably a useful upper limit"
  (assert (plusp x))
  (if (< x x-recursion)
    (+ (trigamma (1+ x) :x-recursion x-recursion) (/ (* x x)))
    (let* ((x2 (* x x))
           (x3 (* x x2))
           (x5 (* x2 x3))
           (x7 (* x2 x5)))
      (+ (/ x)
         (/ (* 2.0 x2))
         (/ (* 6.0 x3))
         (/ (* -30.0 x5))
         (/ (* 42.0 x7))
         (/ (* -30.0 x2 x7))))))
#|
(let ((x*1000 1500)
      x)
  (dotimes (i 15)
    (setf x (float (/ x*1000 1000)))
    (format t "~&~5,3F   ~12,10F" x (trigamma x))
    (incf x*1000 5)))
;==>
1.500   0.9348000492
1.505   0.9306736517
1.510   0.9265820503
1.515   0.9225248209
1.520   0.9185015462
1.525   0.9145118155
1.530   0.9105552242
1.535   0.9066313746
1.540   0.9027398746
1.545   0.8988803384
1.550   0.8950523864
1.555   0.8912556443
1.560   0.8874897441
1.565   0.8837543230
1.570   0.8800490239
;;; 5 place agreement with 5th column of top of page 269, Abramowitz and Stegun
|#

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;;  The functions  evaluate-polynomial
;;;                 multiply-2-polynomials
;;;                 multiply-polynomials
;;;                 zeros-of-polynomial
;;;  evaluate, multiply and find the roots of polynomials whose coefficients
;;;  are represented by sequences of numbers.
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
(defun evaluate-polynomial
       (polynomial
        z
        &key
        (degree-of-polynomial (1- (length polynomial)))
        (number-of-derivatives 0)
        (complex-values (make-array (1+ number-of-derivatives)))
        (bounds-to-be-computed nil)
        (bounds (if bounds-to-be-computed
                  (make-array (1+ number-of-derivatives)))))
  "given
   [1] polynomial (required)
       ==> sequence of length n+1 with real or
           complex-valued numbers giving the n+1
           coefficients of a polynomial of nth order;
           (elt polynomial i) = coefficient of z^(n-i)
   [2] z (required)
       ==> complex point at which the polynomial
           to be evaluated
   [3] degree-of-polynomial (keyword; (1- (length polynomial)))
       ==> degree of polynomial
   [4] number-of-derivatives (keyword; 0)
       ==> number of derivatives to be evaluated
   [5] complex-values (keyword; array of length (1+ number-of-derivatives))
       <== sequence with values of polynomial
       and derivatives
   [6] bounds-to-be-computed (keyword; nil)
       ==> if t, error bounds are computed
       for values in complex-values
   [7] bounds (keyword; array of length (1+ number-of-derivatives))
       <== sequence with error bounds
returns
   [1] the sequence complex-values containing the value
       of the polynomial and its derivatives
   [2] bounds, a sequence containing optional error bounds
---       
Note: this is a Lisp version of cmlib routine cpevl;
d1 was specified in cpevl as (expt 2 (- 1 (i1mach 11))),
where (i1mach 11) ==> THE NUMBER OF BASE-2 DIGITS (SINGLE PRECISION).
I have taken this to be equivalent to machine epsilon.
If this is in fact NOT the case, then the optional error bounds
might not be computed correctly."
  (let ((d1 single-float-epsilon)
        ci cim1)
    (dotimes (j (1+ degree-of-polynomial))
      (dotimes (i (min (1+ number-of-derivatives)
                       (1+ (- degree-of-polynomial j))))
        (setf ci (if (plusp j) (elt complex-values i) 0.0))
        (setf cim1 (if (plusp i)
                     (elt complex-values (1- i)) (elt polynomial j)))
        (setf (elt complex-values i) (+ cim1 (* z ci)))
        (if bounds-to-be-computed
          (let* ((bi (if (plusp j) (elt bounds i) 0.0))
                 (bim1 (if (plusp i) (elt bounds (1- i)) 0.0))
                 (tf (+ bi (* (complex-of-absolutes ci)
                             (+ (* 3.0 d1) (* 4.0 d1 d1)))))
                 (r (realpart (* (complex-of-absolutes z)
                                 (complex (realpart tf) (- (imagpart tf))))))
                 (s (imagpart (* (complex-of-absolutes z) tf))))
            (setf (elt bounds i)
                  (if (plusp j)
                    (* (1+ (* 8 d1))
                       (+ bim1
                          (* d1 (complex-of-absolutes cim1))
                          (complex r s)))
                    0.0)))))))
  (values complex-values bounds))

#|
(evaluate-polynomial #(1 2 3) 1)
;==> #(6.0)
;    nil
(evaluate-polynomial #(1 2 3) 2)
;==> #(11.0)
;    nil
|#
        
;-------------------------------------------------------------------------------
(defun multiply-2-polynomials
       (polynomial-1
        polynomial-2
        &key               
        (degree-of-1
         (1- (length polynomial-1)))
        (degree-of-2
         (1- (length polynomial-2)))
        (product-polynomial
         (make-array (+ degree-of-1 degree-of-2 1))))
  "given
   [1] polynomial-1 (required)
       ==> sequence of length n+1 with real or
           complex-valued numbers giving the n+1
           coefficients of a polynomial of nth order;
           (elt polynomial-1 i) = coefficient of z^(n-i)
   [2] polynomial-2 (required)
       ==> another sequence representing another polynomial
   [3] degree-of-1 (keyword; (1- (length polynomial-1)))
       ==> degree of first polynomial
   [4] degree-of-2 (keyword; (1- (length polynomial-2)))
       ==> degree of second polynomial
   [5] product-polynomial (keyword; array of length (+ degree-of-1 degree-of-2 1))
       <== a sequence with product of two polynomials
returns
   [1] product-polynomial, an sequence with coefficients
       of polynomial given by the product of
       polynomial-1 and polynomial-2
---       
Note: this routine works for a polynomial p represented either by
        p(0) + p(1)*z + ... + p(degp)*z^degp
                  or by
        p(0)*z**degp + p(1)*z**(degp-1) + ... + p(degp)"
  (let (k)
    (dotimes (i (+ degree-of-1 degree-of-2 1) product-polynomial)
      (setf (elt product-polynomial i) 0.0)
      (dotimes (j (1+ degree-of-1))
        (setf k (- i j))
        (if (and (>= k 0) (<= k degree-of-2))
          (incf (elt product-polynomial i)
                (* (elt polynomial-1 j) (elt polynomial-2 k))))))))

#|
(multiply-2-polynomials #(1 2 3) #(4 3 2 1))
;==>#(4.0 11.0 20.0 14.0 8.0 3.0)
(multiply-2-polynomials #(4 3 2 1) #(1 2 3))
;==>#(4.0 11.0 20.0 14.0 8.0 3.0)
(multiply-2-polynomials #(4 3 2 1) #(2))
;==>#(8.0 6.0 4.0 2.0)
|#

;-------------------------------------------------------------------------------
(defun multiply-polynomials (&rest polys)
  "given
   [1] sequences representing any number of polynomials (required)
returns
   [1] a sequence representing their product
---
Note: this function was written by Andrew G. Bruce"
  (let ((n (length polys)))
    (cond ((= n 0) polys)
          ((= n 1) (first polys))
          ((> n 1) (reduce #'multiply-2-polynomials polys)))))

#|
(multiply-2-polynomials #(1 2) #(4 3))
;==> #(4.0 11.0 6.0)
(multiply-2-polynomials #(4.0 11.0 6.0) #(1 2 3))
;==> #(4.0 19.0 40.0 45.0 18.0)
(multiply-polynomials #(1 2) #(4 3) #(1 2 3))
;==> #(4.0 19.0 40.0 45.0 18.0)
(multiply-polynomials #(1) #(4) #(2))
;==> #(8.0)
|#

;-------------------------------------------------------------------------------
(defun zeros-of-polynomial
       (polynomial
        &key
        (degree-of-polynomial (1- (length polynomial)))
        (the-roots (make-array degree-of-polynomial))
        (maximum-number-of-iterations 25))
  "given
   [1] polynomial (required)
       ==> sequence with coefficients of a polynomial;
           (elt polynomial 0) must be nonzero
   [2] degree-of-polynomial (keyword; (1- (length polynomial)))
       ==> degree of polynomial
   [3] the-roots (keyword; array of length degree-of-polynomial)
       <== number of derivatives to be evaluated
   [4] maximum-number-of-iterations (keyword; 25)
       ==> maximum number of iterations
returns
   [1] t or nil, where t indicates that all went well,
       whereas nil indicates that convergence did not occur
       at end of specificed number of iterations
   [2] the-roots, a vector with the required roots
       of the polynomial"
  (cond
   ;;; if this is a first degree, we're done
   ((= degree-of-polynomial 1)
    (setf (elt the-roots 0) (- (/ (elt polynomial 1) (elt polynomial 0))))
    (values t the-roots))  ;t = all went well
   ;;; If constant term is zero, we know one root and can get
   ;;; the others by dealing with a polynomial of one less degree.
   ((zerop (abs (elt polynomial degree-of-polynomial)))
    (setf (elt the-roots (1- degree-of-polynomial)) 0.0)
    (zeros-of-polynomial polynomial
                         :degree-of-polynomial
                         (1- degree-of-polynomial)
                         :the-roots
                         the-roots
                         :maximum-number-of-iterations
                         maximum-number-of-iterations))
   ;;; Here we have to do some honest work: we first generate
   ;;; some initial estimates for the roots and then the final estimates.
   (t
    (let ((initial-estimates (make-array degree-of-polynomial))
          (scratch (make-array (* 2 (1+ degree-of-polynomial)))))
      (initial-estimates-for-zeros-of-polynomial
       polynomial
       :degree-of-polynomial degree-of-polynomial
       :initial-estimates initial-estimates
       :scratch scratch)
      ;;; If this function return nil, convergence was NOT found
      (if (zeros-of-polynomial-with-initial-estimates
           polynomial initial-estimates
           :degree-of-polynomial degree-of-polynomial
           :scratch scratch
           :final-results the-roots
           :maximum-number-of-iterations maximum-number-of-iterations)
        (values t the-roots)    ;convergence occurred
        (values nil the-roots)  ;did not converge
        )))))

#|
(zeros-of-polynomial #(1 -2))
;==> t
;    #(2)
(zeros-of-polynomial #(1 -2 0))
;==> t
;    #(2 0.0)
(multiply-polynomials #(1 -2) #(1 -3) #(1 5))
;==> #(1.0 0.0 -19.0 30.0)
(zeros-of-polynomial #(1.0 0.0 -19.0 30.0))
;==>t
;   #(#c(2.9999999999999996 -2.2190775908547207E-22)
      #c(-5.0                1.9700224365534028E-19)
      #c(1.9999999999999998  2.219061434983382E-22))
|#

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;;  The functions  Newton-Raphson
;;;                 bisection-with-Newton-Raphson
;;;                 secant-method
;;;  can be used to find the zero (root) of a function in a specfied interval.
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
(defun Newton-Raphson
       (f
        f-prime
        x-left
        x-right
        &key
        (accuracy (* 10.0 single-float-epsilon))
        (maximum-number-of-iterations 20)
        (prevent-bracket-jumping-p t))
  "given
   [1] f (required)
       ==> a function with a single argument
   [2] f-prime (required)
       ==> another function with a single argument,
           this one being the first derivative of f
   [3] x-left (required)
       ==> left-hand bracket for the desired root;
           i.e., left-hand bracket <= desired root
   [4] x-right (required)
       ==> right-hand bracket for the desired root;
           i.e., desired root <= right-hand bracket
   [5] accuracy (keyword; (* 10.0 single-float-epsilon))
       ==> desired relative accuracy for computed rood
   [6] maximum-number-of-iterations (keyword; 20)
       ==> maximum number of iterations
   [7] prevent-bracket-jumping-p (keyword; t)
       ==> if t, allows Newton-Raphson to continue
           if it jumps out of the interval
           [x-left, x-right];
           if nil, jumping out of the interval
           causes an error to be signaled
returns
   [1] a root of f in [x-left, x-right];
       i.e., a value x in that interval
       such that f(x) = 0
   [2] the number of iterations required
---
Note: this function is based loosely on rtnewt,
Section 9.4, Numerical Recipes, Second Edition"
  (assert (< x-left x-right))
  (let ((x (* 0.5 (+ x-left x-right)))
        delta-x denom-for-accuracy-test)
    (dotimes (j maximum-number-of-iterations
                (if (not (cerror "returns solution so far"
                                 "exceeding maximum number of iterations"))
                  (values x maximum-number-of-iterations)))
      (setf delta-x (/ (funcall f x)  (funcall f-prime x)))
      (setf denom-for-accuracy-test (+ (abs x)
                                       (abs (decf x delta-x))))
      (cond
       (prevent-bracket-jumping-p
        (if (< x x-left) (setf x x-left))
        (if (> x x-right) (setf x x-right))
        (if (< (/ (abs delta-x) denom-for-accuracy-test) accuracy)
          (return (values x (1+ j)))))
       ((<= x-left x x-right)
        (if (< (/ (abs delta-x) denom-for-accuracy-test) accuracy)
          (return (values x (1+ j)))))
       (t
        (error "jumped out of brackets")
        )))))
  
#|
(defun exp-2 (x) (- (exp x) 2.0))

(Newton-Raphson
 #'exp-2
 #'exp        ;first derivative of exp-2
 0.0
 20.0)

;==> 0.6931471805599453   ;the root
;    15                   ;number of iterations needed
(exp-2 0.6931471805599453)
;==> 0.0
|#

;-------------------------------------------------------------------------------
(defun bisection-with-Newton-Raphson
       (f
        f-prime
        x-left
        x-right
        &key
        (accuracy (* 10.0 single-float-epsilon))
        (maximum-number-of-iterations 100))
  "given
   [1] f (required)
       ==> a function with a single argument
   [2] f-prime (required)
       ==> another function with a single argument,
           this one being the first derivative of f
   [3] x-left (required)
       ==> left-hand bracket for the desired root;
           i.e., left-hand bracket <= desired root
   [4] x-right (required)
       ==> right-hand bracket for the desired root;
           i.e., desired root <= right-hand bracket
   [5] accuracy (keyword; (* 10.0 single-float-epsilon))
       ==> desired relative accuracy for computed rood
   [6] maximum-number-of-iterations (keyword; 100)
       ==> maximum number of iterations
returns
   [1] a root of f in [x-left, x-right];
       i.e., a value x in that interval
       such that f(x) = 0
   [2] the number of iterations required
---
Note: this function is based loosely on rtsafe,
Section 9.4, Numerical Recipes, Second Edition"
  (let ((f-low (funcall f x-left))
        (f-high (funcall f x-right))
        df f-new
        x-low x-high
        rtsafe dxold dx temp)
    (when (>= (* f-low f-high) 0.0)
      (cond ((zerop f-low)
             (values x-left 0))
            ((zerop f-high)
             (values x-right 0))
            (t
             (error "root not bracketed"))))
    (cond ((< f-low 0.0)
           (setf x-low x-left
                 x-high x-right))
          (t
           (setf x-high x-left
                 x-low x-right)))
    (setf rtsafe (* 0.5 (+ x-left x-right))
          dxold (abs (- x-right x-left))
          dx dxold
          f-new (funcall f rtsafe)
          df (funcall f-prime rtsafe))
    (dotimes (j maximum-number-of-iterations
                (if (not (cerror "returns solution so far"
                                 "exceeding maximum number of iterations"))
                  (values rtsafe maximum-number-of-iterations)))
      (cond ((or (>= (* (- (* (- rtsafe x-high) df) f-new)
                        (- (* (- rtsafe x-low) df) f-new))
                     0.0)
                 (> (abs (* 2.0 f-new)) (abs (* dxold df))))
             (setf dxold dx
                   dx (* 0.5 (- x-high x-low))
                   rtsafe (+ x-low dx))
             (when (= x-low rtsafe) (return (values rtsafe (1+ j)))))
            (t
             (setf dxold dx
                   dx (/ f-new df)
                   temp rtsafe
                   rtsafe (- rtsafe dx))
             (when (= temp rtsafe) (return (values rtsafe (1+ j))))))
      (when (< (abs dx) accuracy) (return (values rtsafe (1+ j))))
      (setf f-new (funcall f rtsafe)
            df (funcall f-prime rtsafe))
      (if (< f-new 0.0)
        (setf x-low rtsafe)
        (setf x-high rtsafe)))))

#|
(defun exp-2 (x)
  (- (exp x) 2.0))

(bisection-with-Newton-Raphson
 #'exp-2 #'exp 0.0 20.0
 :maximum-number-of-iterations 100)
;==> 0.6931471805599447
;    60
;;; good agreement with Newton-Raphson, but takes a lot longer!
|#

;-------------------------------------------------------------------------------
(defun secant-method
       (f
        x-left
        x-right
        &key
        (accuracy (* 10.0 single-float-epsilon))
        (maximum-number-of-iterations 50))
  "given
   [1] f (required)
       ==> a function with a single argument
   [2] x-left (required)
       ==> left-hand bracket for the desired root;
           i.e., left-hand bracket <= desired root
   [3] x-right (required)
       ==> right-hand bracket for the desired root;
           i.e., desired root <= right-hand bracket
   [4] accuracy (keyword; (* 10.0 single-float-epsilon))
       ==> desired relative accuracy for computed rood
   [5] maximum-number-of-iterations (keyword; 50)
       ==> maximum number of iterations
returns
   [1] a root of f in [x-left, x-right];
       i.e., a value x in that interval
       such that f(x) = 0
   [2] the number of iterations required
---
Note: this function is based loosely on rtsec,
Section 9.2, Numerical Recipes, Second Edition"
  (let ((f-left (funcall f x-left))
        (f-right (funcall f x-right))
        x-mid f-mid approx-f-mid approx-f-prime-mid delta-x x-new f-new
        denom-for-accuracy-test)
    (dotimes (j maximum-number-of-iterations
                (if (not (cerror "returns solution so far"
                                 "exceeding maximum number of iterations"))
                  (values x-new maximum-number-of-iterations)))
      (setf x-mid (* 0.5 (+ x-left x-right))
            f-mid (funcall f x-mid)
            approx-f-mid (* 0.5 (+ f-left f-right))
            approx-f-prime-mid (/ (- f-right f-left)
                                  (- x-right x-left))
            delta-x (/ approx-f-mid  approx-f-prime-mid)
            x-new (- x-mid delta-x)
            f-new (funcall f x-new))
      (setf denom-for-accuracy-test (+ (abs x-mid) (abs x-new)))
      (if (or (zerop f-new)
              (< (/ (abs delta-x) denom-for-accuracy-test) accuracy))
        (return (values x-new (1+ j))))
      (if (>= (* f-mid f-left) 0)
        (setf x-left x-mid
              f-left f-mid))
      (if (>= (* f-mid f-right) 0)
        (setf x-right x-mid
              f-right f-mid))
      (if (>= (* f-new f-left) 0)
        (setf x-left x-new
              f-left f-new)
        (setf x-right x-new
              f-right f-new))
      )))
  
#|
(defun exp-2 (x)
  (- (exp x) 2.0))

(secant-method #'exp-2 0.0 20.0)
;==> 0.6931471805599453
;    13
|#

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;;  The functions  simple-numerical-integration
;;;                 Gauss-Legendre-quadrature
;;;  are used for numerical integration.
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
(defun simple-numerical-integration
       (f
        a
        b
        &key
        (accuracy 1.0e-6)
        (maximum-number-of-iterations 20))
  "given
   [1] f (required)
       ==> a function with a single argument
   [2] a (required)
       ==> left-hand limit for numerical integration
   [3] b (required)
       ==> right-hand limit for numerical integration
           i.e., a < b
   [4] accuracy (keyword; 1.0e-6)
       ==> desired relative accuracy for computed rood
   [5] maximum-number-of-iterations (keyword; 20)
       ==> maximum number of iterations
returns
   [1] the integral of f over the interval [a, b]
   [2] the number of iterations required
---
Note: this function is based on qtrap,
Section 4.2, Numerical Recipes, Second Edition"
  (let ((s-old nil)
        s-new)
    (dotimes (i maximum-number-of-iterations
                (if (not (cerror "returns solution so far"
                                 "exceeding maximum number of iterations"))
                  (values s-old maximum-number-of-iterations)))
      (setf s-new (trapezoidal-rule f a b s-old i))
      (if (and s-old (< (abs (- s-new s-old))
                        (* accuracy (abs s-old))))
        (return (values s-new (1+ i))))
      (setf s-old s-new))))

#|
;;; first test of trapezoidal-rule
(- (exp 1) 1) 
;==> 1.718281828459045

(let (s)
  (dotimes (i 20)
    (setf s (print (trapezoidal-rule #'exp 0.0 1.0 s i)))))
;==> 1.718281828459572  (great agreement, but takes a long time!)

;;; second test
(- (exp pi) (exp 1.0))
;==> 20.42241080432022

(let (s)
  (dotimes (i 10)
    (setf s (print (trapezoidal-rule #'exp 1.0 pi s i)))))
;==> 20.42244057984665 

;;; now test simple-numerical-integration on these two cases
(simple-numerical-integration #'exp 0.0 1.0)
;==> 1.7182823746860927  (good agreement)
;    10

(simple-numerical-integration #'exp 1.0 pi)
;==> 20.422412665291343
;    12
|#

;-------------------------------------------------------------------------------
(defun Gauss-Legendre-quadrature
       (lower-limit
        upper-limit
        N)
  "given
   [1] lower-limit (required)
       ==> lower limit of integration
   [2] upper-limit (required)
       ==> upper limit of integration
   [3] N (required)
       ==> number of points to be computed
           in Gauss-Legendre quadrature
returns
   [1] an N-dimensional vector of abscissas points
   [2] an N-dimensional vector of weights
---
Note: this function is based on gauleg,
Section 4.5, Numerical Recipes, Second Edition"
  (let ((abscissas (make-array N))
        (weights (make-array N))
        (eps 3.0d-14)
        (m (truncate (+ N 1) 2))
        (N+half (+ N 0.5))
        (xm (* 0.5 (+ upper-limit lower-limit)))
        (xl (* 0.5 (- upper-limit lower-limit)))
        z pp)
    ;;; Loop over desired roots.
    (dotimes (i m (values abscissas weights))
      (setf z (cos (* pi (/ (+ i 0.75) N+half))))
      :label-1
      (do ((p1 1.0 1.0)
           (p2 0.0 0.0)
           (z1 nil)
            p3)
          ;;; since z1 is initially nil,
          ;;; we cannot exit before the first iteration
          ((and z1 (<= (abs (- z z1)) eps)))
        (dotimes (jm1 n)
          (setf p3 p2
                p2 p1
                p1 (/ (- (* (1+ (* 2.0 jm1)) z p2) (* jm1 p3))
                      (1+ jm1))))
        (setf pp (* n (/ (- (* z p1) p2) (- (* z z) 1.0)))
              z1 z
              z (- z1 (/ p1 pp))))
    (setf (aref abscissas i) (- xm (* xl z))
          (aref abscissas (- n (1+ i))) (+ xm (* xl z))
          (aref weights i) (* 2.0 (/ xl (* (- 1.0 (* z z)) pp pp)))
          (aref weights (- n (1+ i))) (aref weights i)))))

#|
(multiple-value-bind (abscissas weights)
                     (gauss-legendre-quadrature -1.0 1.0 32)
  (dotimes (i (length abscissas))
    (format t "~& ~2D  ~13,10F   ~13,10F"
            (1+ i)
            (aref abscissas i)
            (aref weights i))))

  1  -0.9972638618    0.0070186100
  2  -0.9856115115    0.0162743947
  3  -0.9647622556    0.0253920653
  4  -0.9349060759    0.0342738629
  5  -0.8963211558    0.0428358980
  6  -0.8493676137    0.0509980593
  7  -0.7944837960    0.0586840935
  8  -0.7321821187    0.0658222228
  9  -0.6630442669    0.0723457941
 10  -0.5877157572    0.0781938958
 11  -0.5068999089    0.0833119242
 12  -0.4213512761    0.0876520930
 13  -0.3318686023    0.0911738787
 14  -0.2392873623    0.0938443991
 15  -0.1444719616    0.0956387201
 16  -0.0483076657    0.0965400885
 17   0.0483076657    0.0965400885
 18   0.1444719616    0.0956387201
 19   0.2392873623    0.0938443991
 20   0.3318686023    0.0911738787
 21   0.4213512761    0.0876520930
 22   0.5068999089    0.0833119242
 23   0.5877157572    0.0781938958
 24   0.6630442669    0.0723457941
 25   0.7321821187    0.0658222228
 26   0.7944837960    0.0586840935
 27   0.8493676137    0.0509980593
 28   0.8963211558    0.0428358980
 29   0.9349060759    0.0342738629
 30   0.9647622556    0.0253920653
 31   0.9856115115    0.0162743947
 32   0.9972638618    0.0070186100

;;; excellent agreement with top of page 917 of Abramowitz and Stegun
|#

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;;  Everything below here consists of internal symbols in the SAPA package
;;;  and should be regarded as "dirty laundry" ...
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
(defun zeros-of-polynomial-with-initial-estimates
       (polynomial initial-estimates
        &key
        (degree-of-polynomial
         (1- (length polynomial)))
        (scratch (make-array (1+ degree-of-polynomial)))
        (final-results initial-estimates)
        (maximum-number-of-iterations 25))
  (if (< (length initial-estimates) degree-of-polynomial)
    (error (format nil "too few initial estimates (~D<~D)"
                   (length initial-estimates) degree-of-polynomial)))
  (cond
   ((not (eq final-results initial-estimates))
    (if (< (length final-results) degree-of-polynomial)
      (error (format nil "not enough space for results (~D<~D)"
                     (length final-results) degree-of-polynomial)))
    (dotimes (i degree-of-polynomial)
      (setf (elt final-results i) (elt initial-estimates i)))))
  (let ((value-of-polynomial (make-array 1))
        (error-bound-for-value (make-array 1))
        (number-of-roots-found 0)
        temp)
    (dotimes (number-of-iterations (* maximum-number-of-iterations
                                      degree-of-polynomial))
      (dotimes (i degree-of-polynomial)
        (cond
         ((or (zerop number-of-iterations) (plusp (abs (elt scratch i))))
          ;;; evaluate polynomial at point (elt final-results i)
          ;;; and stuff result into (elt value-of-polynomial 0)
          (evaluate-polynomial polynomial (elt final-results i)
                               :degree-of-polynomial degree-of-polynomial
                               :complex-values value-of-polynomial
                               :bounds-to-be-computed t
                               :bounds error-bound-for-value)
          (cond
           ((> (+ (abs (realpart (elt value-of-polynomial 0)))
                  (abs (imagpart (elt value-of-polynomial 0))))
               (+ (abs (realpart (elt error-bound-for-value 0)))
                  (abs (imagpart (elt error-bound-for-value 0)))))
            (setf temp (elt polynomial 0))
            (dotimes (j degree-of-polynomial)
              (if (not (= j i))
                (setf temp (* temp (- (elt final-results i)
                                      (elt final-results j))))))
            (setf (elt scratch i) (/ (elt value-of-polynomial 0) temp)))
           (t
            (setf (elt scratch i) 0.0)
            (incf number-of-roots-found))))))
      (dotimes (i degree-of-polynomial)
        (decf (elt final-results i) (elt scratch i)))
      ;;; if exit is done here, the routine will return t;
      ;;; otherwise, the routine will quit when the maximum number of
      ;;; interations have been made and return nil.
      (if (= number-of-roots-found degree-of-polynomial) (return t)))))

;-------------------------------------------------------------------------------
(defun initial-estimates-for-zeros-of-polynomial
       (polynomial
        &key
        (degree-of-polynomial (1- (length polynomial)))
        (initial-estimates (make-array degree-of-polynomial))
        (scratch (make-array (* 2 (1+ degree-of-polynomial)))))
  (let ((imax (1+ degree-of-polynomial))
        (pn (make-array 1))
        (scratch-offset
         (make-array (+ degree-of-polynomial 2)
                     :displaced-to scratch
                     :displaced-index-offset degree-of-polynomial))
        (temp (- (/ (elt polynomial 1)
                    (* (elt polynomial 0) degree-of-polynomial))))
        x u v)
    (evaluate-polynomial polynomial temp
                         :degree-of-polynomial degree-of-polynomial
                         :number-of-derivatives degree-of-polynomial
                         :complex-values scratch)
    (setf (elt scratch degree-of-polynomial)
          (abs (elt scratch degree-of-polynomial)))
    (dotimes (i degree-of-polynomial)
      (setf (elt scratch (+ degree-of-polynomial i 1))
            (- (abs (elt scratch (- degree-of-polynomial i 1)))))
      (if (< (realpart (elt scratch (+ degree-of-polynomial i 1)))
             (realpart (elt scratch imax)))
        (setf imax (+ degree-of-polynomial i 1))))
    (setf x (expt (- (/ (realpart (elt scratch imax))
                        (realpart (elt scratch degree-of-polynomial))))
                  (/ (float (- imax degree-of-polynomial)))))
    (loop
      (setf x (* 2.0 x))
      (evaluate-polynomial scratch-offset x
                           :degree-of-polynomial degree-of-polynomial
                           :complex-values pn)
      (if (>= (realpart (elt pn 0)) 0.0) (return)))
    (setf u (* 0.5 x))
    (setf v x)
    (loop
      (setf x (* 0.5 (+ u v)))
      (evaluate-polynomial scratch-offset x
                           :degree-of-polynomial degree-of-polynomial
                           :complex-values pn)
      (if (plusp (realpart (elt pn 0)))
        (setf v x)
        (setf u x))
      (if (<= (- v u) (* 0.001 (1+ v))) (return)))
    (dotimes (i degree-of-polynomial)
      (setf u (* (/ pi degree-of-polynomial) (+ 0.5 (* 2.0 i))))
      (setf (elt initial-estimates i)
            (+ temp (* (max x (* 0.001 (abs temp)))
                       (complex (cos u) (sin u))))))))

;-------------------------------------------------------------------------------
(defun complex-of-absolutes (z)
  (complex (abs (realpart z)) (abs (imagpart z))))

;-------------------------------------------------------------------------------
;;; trapezoidal-rule is based upon trapzd, Section 4.2, Numerical Recipes,
;;; Second Edition. It integrates f from a to b;
;;; on the first pass, should set n = 0, in which case s is ignored;
;;; for n = 1, 2, ..., calculates refined estimate of integral using s
;;; returned at previous stage; i.e.,
;;; at each stage n=m, function returns updated value of s,
;;; which should be used to call the function at stage n=m+1
(defun trapezoidal-rule (f a b s n)
  (cond
   ((zerop n)
    (* 0.5 (- b a) (+ (funcall f a)
                      (funcall f b))))
   (t
    (let* ((it (expt 2 (- n 1)))
           (del (/ (- b a) it))
           (x (+ a (* 0.5 del)))
           (sum 0.0))
      (dotimes (j it (* 0.5 (+ s (/ (* sum (- b a)) it))))
        (incf sum (funcall f x))
        (incf x del))))))

