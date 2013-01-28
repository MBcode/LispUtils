;;;-*- Mode: LISP; Package: :SAPA; Syntax: COMMON-LISP -*-
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;
;  multitaper.lisp
;
;  a collection of Lisp functions for multitaper spectral estimation ...
;  Note:  before compiling and loading multitaper.lisp,
;         you should compile and load (in the order listed)
;            sapa-package.lisp, utilities.lisp, basic-math.lisp,
;            basic-statistics.lisp, dft-and-fft.lisp, tapers.lisp,
;            random.lisp, acvs.lisp and nonparametric.lisp
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
;;; (compile-file "ccl:SAPA;multitaper.lisp")
;;; (load "ccl:SAPA;multitaper.fasl")
;-------------------------------------------------------------------------------
(if (not (find-package :SAPA))
  (defpackage :SAPA (:USE :COMMON-LISP)))

(in-package :SAPA)

(export '(;;; utility function to check orthonormality of tapers ...
          check-orthonormality
          
          ;;; functions to compute a set of dpss data tapers ...
          dpss-tapers-tri-diag
          dpss-tapers-Thomson-approx
          dpss-tapers-inverse-iteration
          
          ;;; function to compute an approximation to dpss data tapers ...
          trig-prolate-tapers
          
          ;;; functions to compute multitaper spectral estimates ...
          multitaper-spectral-estimate
          eigenspectra->multitaper-spectral-estimate
          eigenspectra->adaptive-multitaper-spectral-estimate

          ;;; function to create confidence intervals for adaptive estimate ...
          create-ci-for-amt-sdf-estimate
          ))

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;;  The function  check-orthonormality
;;;  checks the orthonormality of a list of vectors.
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
(defun check-orthonormality (list-of-vectors)
  "given a list of vectors, computes and prints
their sum of squares and pairwise dot products, and 
returns the maximum absolute deviation from 0
of the pairwise dot products"
  (let ((the-rest-of-them (cdr list-of-vectors))
        (max-dev 0.0)
        temp
        kth-order-vector)
    (dotimes (k (length list-of-vectors))
      (format t "~&k = ~D: sum of squares = ~F"
              k (sum-of-squares (nth k list-of-vectors))))
    (dotimes (k (1- (length list-of-vectors)) max-dev)
      (setf kth-order-vector (nth k list-of-vectors))
      (format t "~&k = ~D:" k)
      (dolist (another-vector the-rest-of-them)
        (format t "~&      dot product = ~F" 
                (setf temp (dot-product kth-order-vector another-vector))))
      (if (> (abs temp) max-dev)
        (setf max-dev (abs temp)))
      (setf the-rest-of-them (cdr the-rest-of-them)))))

#|
single-float-epsilon  ;==> 1.1107651257113995E-16
(check-orthonormality `(#(1 0 0) #(0 1 0) #(,single-float-epsilon 0 1)))
;==>
k = 0: sum of squares = 1.0
k = 1: sum of squares = 1.0
k = 2: sum of squares = 1.0
k = 0:
      dot product = 0.0
      dot product = 1.1107651257113995E-16
k = 1:
      dot product = 0.0
1.1107651257113995E-16
|#

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;;  The functions  dpss-tapers-tri-diag
;;;                 dpss-tapers-Thomson-approx
;;;                 dpss-tapers-inverse-iteration
;;;  each compute a set of orthonormal dpss data tapers for a specified
;;;  sample size N and taper parameter NW (the product of the duration N
;;;  and half bandwidth W -- note that W is taken to be in standardized units
;;;  so that 0 < W < 1/2).  In general, dpss-tapers-tri-diag should be used
;;;  because it is the fastest method and is generally quite accurate.
;;;  The function dpss-tapers-Thomson-approx implements the scheme
;;;  in Thomson's 1982 paper (with some modifications), but,
;;;  although it is about as fast as dpss-tapers-tri-diag, it is less
;;;  accurate.  The function dpss-tapers-inverse-iteration is potentially
;;;  the most accurate of the three methods, but it is much slower.
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
(defun dpss-tapers-tri-diag
       (N
        number-of-tapers
        &key
        (taper-parameter 4.0)  ;NW
        (print-progress-p nil)
        (compute-true-eigenvalues-p nil))
  "given
   [1] N (required)
       the sample size
   [2] number-of-tapers (required)
       number of orthonormal dpss data tapers
       to be computed
   [3] taper-parameter (keyword; 4.0)
       NW, the duration-half-bandwidth product
       (must be such that 0 < NW/N < 1/2)
   [4] print-progress-p (keyword; nil)
       if t, prints a dot after each eigenvalue
       and eigenvector has been computed
   [5] compute-true-eigenvalues-p (keyword; nil)
       if t, returns eigenvalues for eigenproblem
       of Equation (378) of the SAPA book;
       if nil, returns eigenvalues for tridiagonal
       formulation
returns
   [1] a list of length number-of-tapers of N dimensional vectors
       with orthonormal dpss's of orders 0, 1, ..., number-of-tapers - 1;
   [2] a vector of length number-of-tapers with
       eigenvalues as specified by compute-true-eigenvalues-p
---
Note: computes the dpss tapers using the tridiagonal
      formulation (see Section 8.3 of the SAPA book)"
  (assert (< 0 (/ taper-parameter N) 0.5))
  (let* ((Nm1 (1- N))
         (Nm1o2 (float (/ Nm1 2.0)))
         (ctpiW (cos (* 2 pi (/ taper-parameter N))))
         (diag (make-array N))
         (off-diag (make-array Nm1))
         (results '()))
    ;;; generate diagonal elements of symmetric tridiagonal system ...
    (dotimes (i N)
      (setf (aref diag i) (* ctpiW (expt (- Nm1o2 i) 2))))
    ;;; generate off-diagonal elements ...
    (dotimes (i Nm1)
      (setf (aref off-diag i) (* 0.5 (1+ i) (- N (1+ i)))))
    (if print-progress-p
      (format t "~&finding eigenvalues  "))
    ;;; get eigenvalues ...
    (let ((eigenvalues (largest-eigenvalues-of-tridiagonal-matrix
                        diag off-diag number-of-tapers
                        :print-progress-p print-progress-p)))
      (if print-progress-p
        (format t "~&finding eigenvectors "))
      ;;; get eigenvectors (the dpss's) ...
      (dotimes (k number-of-tapers (values (reverse results) eigenvalues))
        (push (fast-tridiag-eigenvalue->dpss!
               (aref eigenvalues k) k diag off-diag)
              results)
        (if compute-true-eigenvalues-p
          (setf (aref eigenvalues k)
                (dpss->eigenvalue (car results) taper-parameter)))
        (if print-progress-p (format t "."))))))

#|
(multiple-value-bind (list-of-tapers eigenvalues)
                     (dpss-tapers-tri-diag
                      16 4
                      :taper-parameter 4
                      :compute-true-eigenvalues-p t)
  (dotimes (i 4)
    (format t "~&~2D: ~16,13F" i (svref eigenvalues i)))
  (format t "~&---")
  (dotimes (i 16)
    (format t "~&~2D: ~11,8F ~11,8F ~11,8F ~11,8F"
            i
            (svref (elt list-of-tapers 0) i)
            (svref (elt list-of-tapers 1) i)
            (svref (elt list-of-tapers 2) i)
            (svref (elt list-of-tapers 3) i)))
  (check-orthonormality list-of-tapers))
;==>
 0:  0.9999999999819
 1:  0.9999999970607
 2:  0.9999997876512
 3:  0.9999910301796
---
 0:  0.00083944  0.00442231  0.01607017  0.04634116
 1:  0.00655153  0.02829405  0.08186426  0.18053656
 2:  0.02694253  0.09460907  0.21480029  0.35196200
 3:  0.07617175  0.21249814  0.36208333  0.39776040
 4:  0.16388770  0.34799492  0.40188579  0.19828108
 5:  0.28236294  0.42176848  0.24234799 -0.13646373
 6:  0.40070313  0.35562544 -0.05975464 -0.31466700
 7:  0.47568617  0.14005351 -0.30328392 -0.16191173
 8:  0.47568617 -0.14005351 -0.30328392  0.16191173
 9:  0.40070313 -0.35562544 -0.05975464  0.31466700
10:  0.28236294 -0.42176848  0.24234799  0.13646373
11:  0.16388770 -0.34799492  0.40188579 -0.19828108
12:  0.07617175 -0.21249814  0.36208333 -0.39776040
13:  0.02694253 -0.09460907  0.21480029 -0.35196200
14:  0.00655153 -0.02829405  0.08186426 -0.18053656
15:  0.00083944 -0.00442231  0.01607017 -0.04634116
k = 0: sum of squares = 1.0
k = 1: sum of squares = 1.0
k = 2: sum of squares = 1.0
k = 3: sum of squares = 1.0
k = 0:
      dot product = 9.17082571992231E-18
      dot product = -2.802662615875029E-16
      dot product = 5.319366908757006E-18
k = 1:
      dot product = -2.847385955490056E-17
      dot product = 1.2495430037895439E-17
k = 2:
      dot product = -3.859759734048396E-17
3.859759734048396E-17
|#

;-------------------------------------------------------------------------------
(defun dpss-tapers-Thomson-approx
       (N
        number-of-tapers
        &key
        (taper-parameter 4.0)  ;NW
        (print-progress-p nil)
        (compute-true-eigenvalues-p nil)
        (abscissas (let ()
                     (declare (special *abscissas-32-point*))
                     *abscissas-32-point*))
        (weights (let ()
                   (declare (special *weights-32-point*))
                   *weights-32-point*)))
  "given
   [1] N (required)
       the sample size
   [2] number-of-tapers (required)
       number of orthonormal dpss data tapers
       to be computed
   [3] taper-parameter (keyword; 4.0)
       NW, the duration-half-bandwidth product
       (must be such that 0 < NW/N < 1/2)
   [4] print-progress-p (keyword; nil)
       if t, prints a dot after each eigenvalue
       and eigenvector has been computed
   [5] compute-true-eigenvalues-p (keyword; nil)
       if t, returns eigenvalues for eigenproblem
       of Equation (378) of the SAPA book;
       if nil, returns nil
   [6] abscissas (keyword; *abscissas-32-point*)
       a vector of abscissas points used
       in Gauss-Legendre quadrature
   [7] weights (keyword; *abscissas-32-point*)
       a vector of weights used
       in Gauss-Legendre quadrature
returns
   [1] a list of length number-of-tapers of N dimensional vectors
       with orthonormal dpss's of orders 0, 1, ..., number-of-tapers - 1;
   [2] a vector of length number-of-tapers with
       eigenvalues if compute-true-eigenvalues-p is t;
       nil if if compute-true-eigenvalues-p is nil
---
Note: computes the dpss tapers using Thomson's numerical
      integration scheme (see Section 8.2 of the SAPA book)"
  (declare (special *abscissas-32-point* *weights-32-point*))
  (assert (< 0 (/ taper-parameter N) 0.5))
  (let* ((c (* pi taper-parameter))
         (Cap-J (length abscissas))
         (sqrt-weights (transform-a-sequence #'sqrt weights))
         (Cap-Psi (make-array `(,Cap-J ,Cap-J))))
    ;;; Compute elements of Cap-Psi matrix
    (dotimes (i Cap-J)
      (dotimes (j (1+ i))
        (setf (aref Cap-Psi j i)
              (setf (aref Cap-Psi i j)
                    (if (= i j)
                      (/ (* c (aref weights i)) pi)
                      (/ (* (aref sqrt-weights i)
                            (aref sqrt-weights j)
                            (sin (* c (- (aref abscissas i)
                                         (aref abscissas j)))))
                         (* pi (- (aref abscissas i)
                                  (aref abscissas j)))))))))
    (multiple-value-bind (diagonal-elements off-diagonal-elements)
                         (Householder-reduction-of-real-sym-matrix! Cap-Psi)
      (multiple-value-bind (eigenvalues array-with-eigenvectors)
                           (eigenvalues-and-vectors-of-sym-tridiag-matrix
                            diagonal-elements off-diagonal-elements
                            :z Cap-Psi)
        (let ((sorted-eigenvalues (sort (copy-seq eigenvalues) #'>=))
              (true-eigenvalues (if compute-true-eigenvalues-p
                                  (make-array number-of-tapers)))
              (list-of-dpss '())
              (N-minus-1 (1- N))
              (N-over-2 (/ N 2))
              index-unsorted a-dpss x i-down factor)
          (if print-progress-p
            (format t "~&finding eigenvectors "))
          (dotimes (k number-of-tapers (values (reverse list-of-dpss)
                                               true-eigenvalues))
            (setf index-unsorted (position (aref sorted-eigenvalues k)
                                           eigenvalues))
            (setf a-dpss (make-array N :initial-element 0.0))
            (push a-dpss list-of-dpss)
            (dotimes (i N)
              (setf x (float (1- (/ (1+ (* 2 i)) N))))
              (dotimes (j Cap-J)
                (incf (aref a-dpss i)
                      (* (aref sqrt-weights j)
                         (if (= x (aref abscissas j))
                           (/ c pi)
                           (/ (sin (* c (- x (aref abscissas j))))
                              (* pi (- x (aref abscissas j)))))
                         (aref array-with-eigenvectors j index-unsorted)))))
            ;;; force symmetry ...
            (setf i-down N-minus-1)
            (if (evenp k)
              (dotimes (i-up N-over-2)
                (setf (aref a-dpss i-up)
                      (setf (aref a-dpss i-down)
                            (* 0.5 (+ (aref a-dpss i-up)
                                      (aref a-dpss i-down)))))
                (decf i-down))
              (dotimes (i-up N-over-2)
                (setf (aref a-dpss i-up)
                      (- (setf (aref a-dpss i-down)
                               (* 0.5 (- (aref a-dpss i-up)
                                         (aref a-dpss i-down))))))
                (decf i-down)))
            ;;; normalize properly ...
            (setf factor (/ (sqrt (sum-of-squares a-dpss))))
            (if (or (and (evenp k) (minusp (sum a-dpss)))
                    (and (oddp k) (minusp (let ((the-sum 0.0))
                                            (dotimes (j N the-sum)
                                              (incf the-sum
                                                    (* (- N-minus-1 (* 2 j))
                                                       (aref a-dpss j))))))))
              (setf factor (- factor)))
            (a*x! factor a-dpss)
            (if compute-true-eigenvalues-p
              (setf (svref true-eigenvalues k)
                    (dpss->eigenvalue a-dpss taper-parameter)))
            (if print-progress-p (format t "."))))))))

#|
(multiple-value-bind (list-of-tapers eigenvalues)
                     (dpss-tapers-Thomson-approx
                      16 4
                      :taper-parameter 4
                      :compute-true-eigenvalues-p t)
  (dotimes (i 4)
    (format t "~&~2D: ~16,13F" i (svref eigenvalues i)))
  (format t "~&---")
  (dotimes (i 16)
    (format t "~&~2D: ~11,8F ~11,8F ~11,8F ~11,8F"
            i
            (svref (elt list-of-tapers 0) i)
            (svref (elt list-of-tapers 1) i)
            (svref (elt list-of-tapers 2) i)
            (svref (elt list-of-tapers 3) i)))
  (check-orthonormality list-of-tapers))
;==>
 0:  0.9999999996987
 1:  0.9999999691381
 2:  0.9999988186339
 3:  0.9999757483772
---
 0:  0.00029156  0.00199509  0.00917919  0.03251189
 1:  0.00391148  0.01960816  0.06457599  0.15858593
 2:  0.02019310  0.07831855  0.19367378  0.34072886
 3:  0.06496358  0.19446610  0.35286192  0.41188210
 4:  0.15154808  0.33902060  0.41377799  0.22832948
 5:  0.27490901  0.42755451  0.26579779 -0.11427479
 6:  0.40237312  0.36919277 -0.04345084 -0.31005778
 7:  0.48467585  0.14703590 -0.29995992 -0.16346730
 8:  0.48467585 -0.14703590 -0.29995992  0.16346730
 9:  0.40237312 -0.36919277 -0.04345084  0.31005778
10:  0.27490901 -0.42755451  0.26579779  0.11427479
11:  0.15154808 -0.33902060  0.41377799 -0.22832948
12:  0.06496358 -0.19446610  0.35286192 -0.41188210
13:  0.02019310 -0.07831855  0.19367378 -0.34072886
14:  0.00391148 -0.01960816  0.06457599 -0.15858593
15:  0.00029156 -0.00199509  0.00917919 -0.03251189
k = 0: sum of squares = 0.9999999999999999
k = 1: sum of squares = 1.0
k = 2: sum of squares = 1.0000000000000002
k = 3: sum of squares = 0.9999999999999997
k = 0:
      dot product = 1.889836384442751E-18
      dot product = -3.781650095408522E-9
      dot product = -4.313091767418897E-18
k = 1:
      dot product = -3.18890963982299E-17
      dot product = -3.870056405406281E-8
k = 2:
      dot product = -2.9761349634727097E-17
3.870056405406281E-8
|#

;-------------------------------------------------------------------------------
(defun dpss-tapers-inverse-iteration
       (N 
        number-of-tapers
        &key
        (taper-parameter 4.0)
        (print-progress-p nil)
        (eps 0.5e-4))
  "given
   [1] N (required)
       the sample size
   [2] number-of-tapers (required)
       number of orthonormal dpss data tapers
       to be computed
   [3] taper-parameter (keyword; 4.0)
       NW, the duration-half-bandwidth product
       (must be such that 0 < NW/N < 1/2)
   [4] print-progress-p (keyword; nil)
       if t, prints a dot after each eigenvalue
       and eigenvector has been computed
   [5] eps (keyword;  0.5e-4)
       controls accuracy
returns
   [1] a list of length number-of-tapers of N dimensional vectors
       with orthonormal dpss's of orders 0, 1, ..., number-of-tapers - 1;
   [2] a vector of length number-of-tapers with the
       corresponding eigenvalues
---
Note: computes the dpss tapers using inverse
      iteration (see Section 8.1 of the SAPA book)"
  (assert (< 0 (/ taper-parameter N) 0.5))
  (let* ((list-of-vectors nil)
         (eigenvalues (make-array number-of-tapers))
         (sines (make-array (1- (* 2 N))))
         (W (/ taper-parameter N))
         (two-pi-W (* 2.0 pi W))
         (root-N (sqrt N))
         (displacement N)
         (v-old (make-array N))
         1-over-norm sum diff)
    #+mcl(declare (dynamic-extent v-old))
    ;;; set up the Toeplitz vector of sines
    (dotimes (m (1- N))
      (decf displacement)
      (setf (aref sines m)
            (setf (aref sines (- (* 2 n) m 2))
                  (/ (sin (* two-pi-W displacement))
                     (* pi displacement)))))
    ;;; loop once for each sequence
    (if print-progress-p
      (format t "~&finding eigenvalues and eigenvectors "))
    (dotimes (k number-of-tapers)
      (setf (aref sines (1- N))
            (if (zerop k)
              (1- (* 2 W))
              (- (* 2 W) (1+ (aref eigenvalues (1- k))))))
      (let ((v (generate-initial-guess-at-dpss N k)))
        ;;; The `dotimes' form returns nil if the convergence
        ;;; criterion was satisfied --- a value of t indicates
        ;;; non-convergence.
        (if (dotimes (it (ceiling (* root-N (+ k 3))) t)
              (replace v-old v)
              (toeplitz sines v-old v)
              (dolist (v-lower-order list-of-vectors)
                (a*x+y! (- (dot-product v-lower-order v)) v-lower-order v))
              (setf 1-over-norm (/ (euclidean-norm v)))
              (a*x! 1-over-norm v)
              (a*x+y! -1.0 v v-old)
              (setf diff (euclidean-norm v-old))
              (a*x+y! 2.0 v v-old)
              (setf sum (euclidean-norm v-old))
              (setf (aref eigenvalues k)
                    (+ (if (plusp k) (aref eigenvalues (1- k)) 0.0)
                       (if (< sum diff)
                         (- 1-over-norm) 1-over-norm)))
              (if (<= (min diff sum) eps) (return nil)))
          (format t "~&dpss: order ~D did not converge, err = ~F"
                  k (min diff sum)))
        ;;; normalize dpss properly ...
        (if (minusp (sum v :end (max 2 (truncate N (1+ k)))))
          (a*x! -1 v))
        (push v list-of-vectors)
        (if print-progress-p (format t "."))))
    ;;; convert from sigma to actual eigenvalue of interest
    (x+b! eigenvalues 1)
    (values (reverse list-of-vectors) eigenvalues)))

#|
(multiple-value-bind (list-of-tapers eigenvalues)
                     (dpss-tapers-inverse-iteration
                      16 4
                      :taper-parameter 4)
  (dotimes (i 4)
    (format t "~&~2D: ~16,13F" i (svref eigenvalues i)))
  (format t "~&---")
  (dotimes (i 16)
    (format t "~&~2D: ~11,8F ~11,8F ~11,8F ~11,8F"
            i
            (svref (elt list-of-tapers 0) i)
            (svref (elt list-of-tapers 1) i)
            (svref (elt list-of-tapers 2) i)
            (svref (elt list-of-tapers 3) i)))
  (check-orthonormality list-of-tapers))
;==>
 0:  0.9999999999819
 1:  0.9999999970607
 2:  0.9999997876512
 3:  0.9999910301796
---
 0:  0.00083944  0.00442231  0.01607017  0.04634116
 1:  0.00655153  0.02829405  0.08186426  0.18053656
 2:  0.02694253  0.09460907  0.21480029  0.35196201
 3:  0.07617175  0.21249814  0.36208333  0.39776040
 4:  0.16388770  0.34799492  0.40188579  0.19828107
 5:  0.28236294  0.42176848  0.24234798 -0.13646374
 6:  0.40070313  0.35562544 -0.05975464 -0.31466699
 7:  0.47568617  0.14005351 -0.30328392 -0.16191172
 8:  0.47568617 -0.14005351 -0.30328392  0.16191173
 9:  0.40070313 -0.35562544 -0.05975464  0.31466699
10:  0.28236294 -0.42176848  0.24234798  0.13646373
11:  0.16388770 -0.34799492  0.40188579 -0.19828108
12:  0.07617175 -0.21249814  0.36208333 -0.39776040
13:  0.02694253 -0.09460907  0.21480029 -0.35196200
14:  0.00655153 -0.02829405  0.08186426 -0.18053656
15:  0.00083944 -0.00442231  0.01607017 -0.04634116
k = 0: sum of squares = 0.9999999999999999
k = 1: sum of squares = 1.0000000000000002
k = 2: sum of squares = 1.0000000000000002
k = 3: sum of squares = 0.9999999999999999
k = 0:
      dot product = 3.260695682102792E-17
      dot product = 2.659683454378503E-19
      dot product = 1.9203930980149497E-17
k = 1:
      dot product = 2.2386064356394453E-16
      dot product = -6.830473686658678E-17
k = 2:
      dot product = -1.283261691353843E-15
1.283261691353843E-15
|#

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;;  The function  trig-prolate-tapers
;;;  computes the trig-prolate data tapers, an orthonormal set of tapers
;;;  that are a useful approximation to the dpss data tapers.  Their chief
;;;  advantage is the speed with which they can be computed.
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
(defun trig-prolate-tapers
       (N
        number-of-tapers
        &key
        (taper-parameter 4)  ;NW
        (print-progress-p nil)
        (compute-true-eigenvalues-p nil))
  "given
   [1] N (required)
       the sample size
   [2] number-of-tapers (required)
       number of orthonormal trig prolate tapers
       to be computed; currently restricted to
       one of the following maximum values:
       2 if taper-parameter is 2;
       4 if taper-parameter is 3;
       5 if taper-parameter is 4;
       7 if taper-parameter is 5
   [3] taper-parameter (keyword; 4.0)
       NW, the duration-half-bandwidth product;
       currently restricted to one of the
       following integers: 2, 3, 4, 5
       (must also be such that 0 < NW/N < 1/2)
   [4] print-progress-p (keyword; nil)
       if t, prints a dot after each taper
       has been computed
   [5] compute-true-eigenvalues-p (keyword; nil)
       if t, returns eigenvalues for eigenproblem
       of Equation (378) of the SAPA book;
       if nil, returns nil
returns
   [1] a list of length number-of-tapers of N dimensional vectors
       with orthonormal trig prolate data tapers of orders
       0, 1, ..., number-of-tapers - 1;
   [2] a vector of length number-of-tapers with
       eigenvalues if compute-true-eigenvalues-p is t;
       nil if if compute-true-eigenvalues-p is nil
---
Note: computes the trig prolate approximation to
      the dpss tapers (see Section 8.4 of the SAPA book)"
  (assert (and (integerp taper-parameter) (<= 2 taper-parameter 5)))
  (case taper-parameter
    (2 (assert (<= number-of-tapers 2)))
    (3 (assert (<= number-of-tapers 4)))
    (4 (assert (<= number-of-tapers 5)))
    (5 (assert (<= number-of-tapers 7))))
  (if print-progress-p
    (format t "~&computing trig prolates "))
  (let ((results '())
        (true-eigenvalues (if compute-true-eigenvalues-p
                            (make-array number-of-tapers))))
    (dotimes (k number-of-tapers (values (reverse results)
                                         true-eigenvalues))
      (push (generate-tri-prolate-taper
             N
             (produce-upsilon-func
              (trig-prolate-sign-cosine-coefficients
               taper-parameter k)))
            results)
      (if compute-true-eigenvalues-p
        (setf (svref true-eigenvalues k)
              (dpss->eigenvalue (car results) taper-parameter)))
      (if print-progress-p (format t ".")))))

#|
(multiple-value-bind (list-of-tapers eigenvalues)
                     (trig-prolate-tapers
                      16 4
                      :taper-parameter 4
                      :compute-true-eigenvalues-p t)
  (dotimes (i 4)
    (format t "~&~2D: ~16,13F" i (svref eigenvalues i)))
  (format t "~&---")
  (dotimes (i 16)
    (format t "~&~2D: ~11,8F ~11,8F ~11,8F ~11,8F"
            i
            (svref (elt list-of-tapers 0) i)
            (svref (elt list-of-tapers 1) i)
            (svref (elt list-of-tapers 2) i)
            (svref (elt list-of-tapers 3) i)))
  (check-orthonormality list-of-tapers))
;==>
 0:  0.9999999577008
 1:  1.0000000927627
 2:  0.9999979984130
 3:  0.9999612860661
---
 0:  0.00025830  0.00192627  0.00826665  0.03228915
 1:  0.00369660  0.01882494  0.06316808  0.15240000
 2:  0.01950040  0.07672601  0.19103732  0.33537529
 3:  0.06367965  0.19247292  0.35125873  0.41454498
 4:  0.15001960  0.33788630  0.41520495  0.23422290
 5:  0.27391468  0.42811787  0.26873512 -0.11173293
 6:  0.40252728  0.37068759 -0.04152554 -0.31135663
 7:  0.48578549  0.14781044 -0.29954212 -0.16468134
 8:  0.48578549 -0.14781044 -0.29954212  0.16468134
 9:  0.40252728 -0.37068759 -0.04152554  0.31135663
10:  0.27391468 -0.42811787  0.26873512  0.11173293
11:  0.15001960 -0.33788630  0.41520495 -0.23422290
12:  0.06367965 -0.19247292  0.35125873 -0.41454498
13:  0.01950040 -0.07672601  0.19103732 -0.33537529
14:  0.00369660 -0.01882494  0.06316808 -0.15240000
15:  0.00025830 -0.00192627  0.00826665 -0.03228915
k = 0: sum of squares = 0.99999995816752
k = 1: sum of squares = 1.00000013603332
k = 2: sum of squares = 1.0000001262701201
k = 3: sum of squares = 0.99999995576222
k = 0:
      dot product = -3.1223116743451206E-17
      dot product = -3.757875999225208E-8
      dot product = -2.6359665318553827E-18
k = 1:
      dot product = -1.1868625656927256E-17
      dot product = -2.1731819964144203E-8
k = 2:
      dot product = -3.6483403104137224E-17
2.1731819964144203E-8
|#

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;;  The functions  multitaper-spectral-estimate
;;;                 eigenspectra->multitaper-spectral-estimate
;;;                 eigenspectra->adaptive-multitaper-spectral-estimate
;;;  implement multitaper spectral estimation.  The simpliest formulation
;;;  is implemented by multitaper-spectral-estimate, which computes
;;;  the simple average of direct spectral estimates given by Equation (333)
;;;  of the SAPA book.  This function also optionally returns a list
;;;  of the individual eigenspectra, all or part of which can then be presented
;;;  to either eigenspectra->multitaper-spectral-estimate
;;;  or        eigenspectra->adaptive-multitaper-spectral-estimate
;;;  to compute, respectively, a simple average with a few number of terms
;;;  or the adaptive weighted multitaper spectral estimate given by
;;;  Equation (370a) of the SAPA book.
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
(defun multitaper-spectral-estimate
       (time-series
        list-of-data-tapers
        &key
        (center-data t)
        (start 0)
        (end (length time-series))
        (N-tapers (length list-of-data-tapers))
        (N-nonzero-freqs :half-next-power-of-2)
        (return-est-for-0-freq-p nil)
        (sampling-time 1.0)
        (scratch-dft (make-array (get-N-dft N-nonzero-freqs (- end start))))
        (recenter-after-tapering-p t)
        (restore-power-option-p t)
        (sdf-transformation #'convert-to-dB)
        (result-sdf (make-array (get-N-freqs N-nonzero-freqs (- end start)
                                             return-est-for-0-freq-p)))
        (return-eigenpspectra-p t)
        (return-frequencies-p t)
        (freq-transformation nil)
        (result-freq (if return-frequencies-p
                       (make-array (get-N-freqs N-nonzero-freqs (- end start)
                                                return-est-for-0-freq-p)))))
  "given
   [1] time-series (required)
       ==> a sequence of time series values
   [2] list-of-data-tapers (required)
       ==> a list of orthonormal data tapers,
           each of length (- end start)
   [3] center-data (keyword; t)
       ==> if t, subtract sample mean from time series;
           if a number, subtracts that number from time series;
           if nil, time-series is not centered
   [4] start (keyword; 0)
       ==> start index of sequence to be used
   [5] end (keyword; length of time-series)
       ==> 1 + end index of sequence to be used
   [6] N-tapers (keyword; length of list-of-data-tapers)
       ==> number of data tapers to be used in list-of-data-tapers 
   [7] N-nonzero-freqs (keyword; :half-next-power-of-2)
       ==> specifies at how many nonzero frequencies
           multitaper spectral estimate is to be computed -- choices are:
           :half-next-power-of-2
            ==> 1/2 * power of two >= sample size;
           :next-power-of-2
            ==> power of two >= sample size;
           :twice-next-power-of-2
            ==> 2 * power of two >= sample size;
           :Fourier
            ==> just at Fourier frequencies
            -- or --
           any power of 2 >= 1/2 * [power of two >= sample size]
   [8] return-est-for-0-freq-p (keyword; nil)
       ==> if t, sdf is computed at zero frequency;
           otherwise, it is not computed.
   [9] sampling-time (keyword; 1.0)
       ==> sampling time (called delta t in the SAPA book)
  [10] scratch-dft (keyword; vector of correct length)
       ==> vector in which the in-place dft is done
  [11] recenter-after-tapering-p (keyword; t)
       ==> if t and data-taper is a function,
           centers tapered series by subtracting
           off its sample mean
  [12] restore-power-option-p (keyword; t)
       ==> if t and data-taper is a function,
           normalizes tapered series to have same
           sum of squares as before tapering
  [13] sdf-transformation (keyword; #'convert-to-dB)
       ==> a function of one argument or nil;
           if bound to a function, the function is used
           to transform all elements of result-sdf
  [14] result-sdf (keyword; vector of correct length)
       <== vector into which multitaper spectral estimate is placed;
           it must be exactly of the length dictated
           by N-nonzero-freqs and return-est-for-0-freq-p
  [15] return-eigenpspectra-p (keyword; t)
       ==> if t, individual eigenspectra are returned in a list
           (each eigenspectrum in the list is associated with
           the corresponding taper in list-of-data-tapers)
  [16] return-frequencies-p (keyword; t)
       ==> if t, the frequencies associated with the spectral estimate
           are computed and returned in result-freq
  [17] freq-transformation (keyword; nil)
       ==> a function of one argument or nil;
           if bound to a function, the function is used
           to transform all elements of result-freq
           (ignored unless return-frequencies-p is true)
  [18] result-freq (keyword; nil or vector of correct length)
       <== not used if return-frequencies-p nil; otherwise,
           vector of length N-nonzero-freqs (if return-est-for-0-freq-p is nil)
           or N-nonzero-freqs +1 (if return-est-for-0-freq-p is t)
           into which the frequencies associated with the values
           in result-sdf are placed
returns
   [1] result-sdf, a vector holding
       the properly transformed multitaper spectral estimate
   [2] result-freq (if return-frequencies-p is t),
       where result-freq is a vector holding
       the properly transformed frequencies
       associated with values in  result-sdf,
       or
       nil (if return-frequencies-p is nil)
   [3] the length of the vector result-sdf
   [4] a list of untransformed eigenspectra (return-eigenpspectra-p is t),
       or
       nil (if return-eigenpspectra-p is nil)
---
Note: see Section 7.1 of the SAPA book"
  (let* ((sample-size (- end start))
         (N-freqs (get-N-freqs N-nonzero-freqs sample-size
                               return-est-for-0-freq-p))
         (N-dft (get-N-dft N-nonzero-freqs sample-size))
         (offset (if return-est-for-0-freq-p 0 1))
         (fiddle-factor-sdf (/ sampling-time sample-size))
         (fiddle-factor-freq (/ (* N-dft sampling-time)))
         (list-of-eigenspectra '()))
    (fill result-sdf 0.0)
    ;;; loop once for each data taper ...
    (dotimes (k N-tapers)
      (center&taper-time-series time-series
                                :center-data center-data
                                :start start
                                :end end
                                :data-taper #'supplied-data-taper!
                                :data-taper-parameters
                                (elt list-of-data-tapers k)
                                :recenter-after-tapering-p
                                recenter-after-tapering-p
                                :restore-power-option-p
                                restore-power-option-p
                                :result scratch-dft)
        ;;; zero the rest of scratch-dft ...
        (fill scratch-dft 0.0 :start sample-size :end N-dft)
        (dft! scratch-dft :N N-dft)
        (if return-eigenpspectra-p
          (let ((an-eigenpspectrum (make-array N-freqs)))
            (dotimes (i N-freqs)
              (setf (aref an-eigenpspectrum i)
                    (* fiddle-factor-sdf
                       (expt (abs (aref scratch-dft (+ i offset))) 2))))
            (push an-eigenpspectrum list-of-eigenspectra)
            (x+y! result-sdf an-eigenpspectrum))
          (dotimes (i N-freqs)
            (incf (aref result-sdf i)
                  (* fiddle-factor-sdf
                     (expt (abs (aref scratch-dft (+ i offset))) 2))))))
    (a*x! (/ (float N-tapers)) result-sdf)
    (if sdf-transformation
      (transform-a-sequence! sdf-transformation result-sdf))
    (when return-frequencies-p
      (dotimes (i N-freqs)
        (setf (aref result-freq i)
              (* fiddle-factor-freq (float (+ i offset)))))
      (if freq-transformation
        (transform-a-sequence! freq-transformation result-freq)))
    (values result-sdf result-freq N-freqs (reverse list-of-eigenspectra))))

#|
;;; Here we test the multitaper spectral estimate using the Parseval result
;;; analogous to the one we used to check the periodogram ... 
(dolist (N '(22 32 63 64 65) (values))
  (let* ((time-series (x+b! (ranorms N) 100))
         (sigma^2 (sample-variance time-series))
         (sampling-time 0.25)
         (the-sdf-est (multitaper-spectral-estimate
                       time-series
                       (dpss-tapers-tri-diag N 4)
                       :N-nonzero-freqs :Fourier
                       :sdf-transformation nil
                       :sampling-time sampling-time))
         (N-f (length the-sdf-est))
         (Parseval-sum (if (evenp N)
                         (/ (+ (* 2 (sum the-sdf-est  :end (1- N-f)))
                               (svref the-sdf-est  (1- N-f)))
                            (* N sampling-time))
                         (/ (* 2 (sum the-sdf-est ))
                            (* N sampling-time)))))
    (format t "~& N = ~2D, N-f = ~2D: ~11,8F ~11,8F ~F"
            N N-f sigma^2 Parseval-sum (/ Parseval-sum sigma^2))))
;==>
 N = 22, N-f = 11:  0.55906682  0.55906682 1.0000000000000064
 N = 32, N-f = 16:  0.63422723  0.63422723 0.9999999999999991
 N = 63, N-f = 31:  1.20061865  1.20061865 1.0000000000000042
 N = 64, N-f = 32:  0.77272584  0.77272584 1.0000000000000024
 N = 65, N-f = 32:  1.01794147  1.01794147 1.000000000000031
;;; Note: the important thing is that the last column of numbers
;;;       be close to unity -- since we are using random numbers,
;;;       these three columns of numbers will change each time
;;;       the above Lisp form is evaluated.
|#

;-------------------------------------------------------------------------------
(defun eigenspectra->multitaper-spectral-estimate
       (list-of-eigenspectra
        &key
        (N-eigenspectra (length list-of-eigenspectra))
        (sdf-transformation #'convert-to-dB)
        (result-sdf (make-array (length (car list-of-eigenspectra)))))
  "given
   [1] list-of-eigenspectra (required)
       ==> list of eigenspectra (such as optionally returned
           by multitaper-spectral-estimate); each eigenspectrum
           is assumed to be untransformed (e.g., not expressed
           in dB) and represented by a vector
   [2] N-eigenspectra (keyword; length of list-of-eigenspectra)
       ==> number of eigenspectra to be used (must be less than
           or equal to the length of list-of-eigenspectra)
   [3] sdf-transformation (keyword; #'convert-to-dB)
       ==> a function of one argument or nil;
           if bound to a function, the function is used
           to transform all elements of result-sdf
   [4] result-sdf (keyword; vector of correct length)
       <== vector into which multitaper spectral estimate is placed;
           it must be exactly the same length as each of the vectors
           in list-of-eigenspectra
returns
   [1] result-sdf, a vector holding
       the properly transformed multitaper spectral estimate
---
Note: see Section 7.1 of the SAPA book"
  (replace result-sdf (car list-of-eigenspectra))
  (let ((the-rest (cdr list-of-eigenspectra)))
    (dotimes (k (1- N-eigenspectra))
      (x+y! result-sdf (elt the-rest k))))
  (a*x! (/ (float N-eigenspectra)) result-sdf)
  (if sdf-transformation
    (transform-a-sequence! sdf-transformation result-sdf))
  (values result-sdf))

#|
;;; Here we create two multitaper spectral estimates, one using 4 tapers
;;; and the other using 3 tapers.  We check the Parseval result in
;;; both cases.
(let* ((time-series (x+b! (ranorms 32) 100))
       (sigma^2 (sample-variance time-series))
       (sampling-time 0.25))
  (multiple-value-bind (mt-4 junk N-f list-of-eigenspectra)
                       (multitaper-spectral-estimate
                        time-series
                        (dpss-tapers-tri-diag 32 4)
                        :N-nonzero-freqs :Fourier
                        :sdf-transformation nil
                        :sampling-time sampling-time)
    (declare (ignore junk))
    (print (/ (+ (* 2 (sum mt-4 :end (1- N-f)))
                 (svref mt-4 (1- N-f)))
              (* 32 sampling-time sigma^2)))
    (let ((mt-3 (eigenspectra->multitaper-spectral-estimate
                 list-of-eigenspectra
                 :N-eigenspectra 3
                 :sdf-transformation nil)))
      (print (/ (+ (* 2 (sum mt-3 :end (1- N-f)))
                   (svref mt-3 (1- N-f)))
                (* 32 sampling-time sigma^2)))
      (values))))
;==>
0.9999999999999991 
0.9999999999999993
|#

;-------------------------------------------------------------------------------
(defun eigenspectra->adaptive-multitaper-spectral-estimate
       (list-of-eigenspectra
        eigenvalues
        variance-of-time-series
        &key
        (sampling-time 1.0)
        (N-eigenspectra (length list-of-eigenspectra))
        (maximum-number-of-iterations 100)
        (result-dof (make-array (length (car list-of-eigenspectra))))
        (sdf-transformation #'convert-to-dB)
        (result-sdf (make-array (length (car list-of-eigenspectra)))))
  "given
   [1] list-of-eigenspectra (required)
       ==> list of eigenspectra (such as optionally returned
           by multitaper-spectral-estimate); each eigenspectrum
           is assumed to be untransformed (e.g., not expressed
           in dB)
   [2] eigenvalues (required)
       ==> vector of eigenvalues corresponding to the dpss's
           used to create the eigenspectra (the length of eigenvalues 
           should be at least as large as the length specified
           by N-eigenspectra)
   [3] variance-of-time-series (required)
       ==> variance of time series
           from which eigenspectra were computed
   [4] sampling-time (keyword; 1.0)
       ==> sampling time (called delta t in the SAPA book)
   [5] N-eigenspectra (keyword; length of list-of-eigenspectra)
       ==> number of eigenspectra to be used (must be less than
           or equal to the length of list-of-eigenspectra)
   [6] maximum-number-of-iterations (keyword; 100)
       ==> maximum number of iterations
   [7] result-dof (keyword; vector of correct length)
       <== vector into which degrees of freedom for each value
           in the adaptive multitaper spectral estimate is placed;
           it must be exactly the same length as each of the vectors
           in list-of-eigenspectra
   [8] sdf-transformation (keyword; #'convert-to-dB)
       ==> a function of one argument or nil;
           if bound to a function, the function is used
           to transform all elements of result-sdf
   [9] result-sdf (keyword; vector of correct length)
       <== vector into which adaptive multitaper spectral estimate is placed;
           it must be exactly the same length as each of the vectors
           in list-of-eigenspectra
returns
   [1] result-sdf, a vector holding
       the properly transformed adaptive multitaper spectral estimate
   [2] result-dof, a vector holding corresponding degrees of freedom
   [3] the maximum number of iterations required to reach convergence
       for all of the values in result-sdf
---
Note: see Section 7.4 of the SAPA book"
  (assert (> (length list-of-eigenspectra) 1))
  (let* ((N-f (length (car list-of-eigenspectra)))
         (sig2*Delta-t (* variance-of-time-series sampling-time))
         (lambda-0 (elt eigenvalues 0))
         (lambda-1 (elt eigenvalues 1))
         (lambda-0+lambda-1 (+ lambda-0 lambda-1))
         (weights (make-array N-eigenspectra))
         previous-est new-est-top new-est-bot
         (max-iterations 0)
         local-max-iterations
         (first-eigenspectrum  (elt list-of-eigenspectra 0))
         (second-eigenspectrum (elt list-of-eigenspectra 1)))
    ;;; determine multitaper estimator, freq by freq ...
    (dotimes (i N-f)
      (setf previous-est (/ (+ (* lambda-0 (aref first-eigenspectrum i))
                               (* lambda-1 (aref second-eigenspectrum i)))
                            lambda-0+lambda-1))
      (setf local-max-iterations 1)
      (dotimes (j maximum-number-of-iterations
                  (format t "~&nonconvergence at index ~D" i))
        ;;; compute current set of weights and sums needed to new est ...
        (setf new-est-top 0.0
              new-est-bot 0.0)
        (dotimes (k N-eigenspectra)
          (setf (aref weights k)
                (/ previous-est
                   (+ (* (elt eigenvalues k) previous-est)
                      (* (- 1.0 (elt eigenvalues k)) sig2*Delta-t))))
          (incf new-est-top (* (aref weights k)
                               (aref weights k)
                               (elt eigenvalues k)
                               (aref (elt list-of-eigenspectra k) i)))
          (incf new-est-bot (* (aref weights k)
                               (aref weights k)
                               (elt eigenvalues k))))
        (setf (aref result-sdf i) (/ new-est-top new-est-bot))
        (if (< (/ (abs (- (aref result-sdf i) previous-est))
                  previous-est) 0.05) (return))
        (setf previous-est (aref result-sdf i))
        (incf local-max-iterations))
      (if (> local-max-iterations max-iterations)
        (setf max-iterations local-max-iterations))
      (setf (aref result-dof i) (/ (* 2.0 new-est-bot new-est-bot)
                            (let ((the-sum 0.0))
                              (dotimes (k n-eigenspectra the-sum)
                                (incf the-sum
                                      (* (expt (aref weights k) 4)
                                         (elt eigenvalues k)
                                         (elt eigenvalues k))))))))
    (if sdf-transformation
      (transform-a-sequence! sdf-transformation result-sdf))
    (values result-sdf result-dof max-iterations)))

#|
;;; For this example we use the 20 point time series of Section 6.16
;;; of the SAPA book:
(let* ((20-pt-ts #(71.0  63.0  70.0  88.0  99.0  90.0 110.0 135.0 128.0 154.0
                   156.0 141.0 131.0 132.0 141.0 104.0 136.0 146.0 124.0 129.0))
       (N-ts (length 20-pt-ts))
       (sigma^2 (sample-variance 20-pt-ts))
       (sampling-time 0.25))
  (multiple-value-bind (dpss-NW-4-tapers eigenvalues)
                       (dpss-tapers-tri-diag
                        N-ts 7
                        :compute-true-eigenvalues-p t)
    (multiple-value-bind (mt-4 freqs N-f list-of-eigenspectra)
                         (multitaper-spectral-estimate
                          20-pt-ts
                          dpss-NW-4-tapers
                          :N-nonzero-freqs :Fourier
                          :sdf-transformation nil
                          :sampling-time sampling-time)
      (print (/ (+ (* 2 (sum mt-4 :end (1- N-f)))
                   (svref mt-4 (1- N-f)))
                (* N-ts sampling-time sigma^2)))
      (multiple-value-bind (amt-4 dof)
                           (eigenspectra->adaptive-multitaper-spectral-estimate 
                            list-of-eigenspectra eigenvalues sigma^2
                            :sdf-transformation nil)
        (print (/ (+ (* 2 (sum amt-4 :end (1- N-f)))
                     (svref amt-4 (1- N-f)))
                  (* N-ts sampling-time sigma^2)))
        (transform-a-sequence! #'convert-to-dB amt-4)
        (dotimes (i N-f)
          (format t "~&~6,4F: ~8,4F  ~5,1F"
                  (svref freqs i)
                  (svref amt-4 i)
                  (svref dof i)))
        (values)))))
;==>
1.0000000000000062 
0.9989962635770837 
0.2000:  27.3962   14.0
0.4000:  27.0953   14.0
0.6000:  25.3427   13.9
0.8000:  23.6051   13.9
1.0000:  19.4616   13.4
1.2000:  17.1247   13.0
1.4000:  18.2214   13.2
1.6000:  16.8903   13.0
1.8000:  16.9013   13.0
2.0000:  15.4482   12.7
|#

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;;  The function  create-ci-for-amt-sdf-estimate
;;;  takes the spectral estimate and vector of degrees of freedom returned by
;;;  eigenspectra->adaptive-multitaper-spectral-estimate and creates
;;;  a confidence interval for the true sdf at each frequency.
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
(defun create-ci-for-amt-sdf-estimate
       (sdf-dB
        dofs
        &key
        (confidence-level 0.95))
    "given
   [1] sdf-dB (required)
       ==> vector containing an adaptive multitaper spectral
           estimate expressed in decibels
   [2] dofs (required)
       ==> vector containing the degrees of freedom associated
           with the values in sdf-dB
   [3] confidence-level (keyword; 0.95)
       ==> the level of the confidence intervals to be created
returns
   [1] a vector containing the upper confidence interval
   [2] a vector containing the lower confidence interval
---
Note: see Section 7.4 of the SAPA book"
  (let* ((p (/ (- 1.0 confidence-level) 2.0))
         (one-minus-p (- 1.0 p))
         (n-f (length sdf-dB))
         (upper-ci (copy-seq sdf-dB))
         (lower-ci (copy-seq sdf-dB)))
    (dotimes (i n-f (values upper-ci lower-ci))
      (incf (aref upper-ci i)
            (convert-to-dB (/ (aref dofs i)
                              (quantile-of-chi-square-distribution
                               (aref dofs i)
                               p
                               :accuracy-level :accurate))))
      (incf (aref lower-ci i)
            (convert-to-dB (/ (aref dofs i)
                              (quantile-of-chi-square-distribution
                               (aref dofs i)
                               one-minus-p
                               :accuracy-level :accurate)))))))

#|
;;; For this example we use the 20 point time series of Section 6.16
;;; of the SAPA book:
(let* ((20-pt-ts #(71.0  63.0  70.0  88.0  99.0  90.0 110.0 135.0 128.0 154.0
                   156.0 141.0 131.0 132.0 141.0 104.0 136.0 146.0 124.0 129.0))
       (N-ts (length 20-pt-ts))
       (sigma^2 (sample-variance 20-pt-ts))
       (sampling-time 0.25))
  (multiple-value-bind (dpss-NW-4-tapers eigenvalues)
                       (dpss-tapers-tri-diag
                        N-ts 7
                        :compute-true-eigenvalues-p t)
    (multiple-value-bind (mt-4-dB freqs N-f list-of-eigenspectra)
                         (multitaper-spectral-estimate
                          20-pt-ts
                          dpss-NW-4-tapers
                          :N-nonzero-freqs :Fourier
                          :sampling-time sampling-time)
      (multiple-value-bind (amt-4-dB dof)
                           (eigenspectra->adaptive-multitaper-spectral-estimate 
                            list-of-eigenspectra eigenvalues sigma^2)
        (multiple-value-bind (upper-dB lower-dB)
                             (create-ci-for-amt-sdf-estimate
                              amt-4-dB dof)
          (dotimes (i N-f)
            (format t "~&~6,4F: ~8,4F    ~8,4F ~8,4F ~8,4F"
                    (svref freqs i)
                    (svref mt-4-dB i)
                    (svref lower-dB i)
                    (svref amt-4-dB i)
                    (svref upper-dB i)))
        (values))))))
;==>
0.2000:  27.3532     24.6867  27.3962  31.3562
0.4000:  27.1112     24.3855  27.0953  31.0559
0.6000:  25.3879     22.6297  25.3427  29.3099
0.8000:  23.6973     20.8867  23.6051  27.5840
1.0000:  19.8763     16.7062  19.4616  23.5201
1.2000:  16.7150     14.3342  17.1247  21.2598
1.4000:  17.8006     15.4454  18.2214  22.3247
1.6000:  16.5783     14.0954  16.8903  21.0347
1.8000:  16.7305     14.1055  16.9013  21.0478
2.0000:  16.3822     12.6271  15.4482  19.6505
|#

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;;  Everything below here consists of internal symbols in the SAPA package
;;;  and should be regarded as "dirty laundry" ...
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
(defun dpss->eigenvalue (dpss NW)
  "given dpss (a vector of length N) and NW,
computes the corresponding eigenvalue using
the method of Exercise [8.1], page 390,
of the SAPA book"
  (let* ((eigenvalue 0.0)
         (dpss-acvs
          (acvs dpss :center-data-p nil))
         (N (length dpss))
         (W (/ NW N))
         (vector-of-ratios (let ((ratios (make-array N))
                                 (j 0))
                             (setf (aref ratios 0) (* 2 W))
                             (dotimes (i (1- N) ratios)
                               (incf j)
                               (setf (aref ratios j)
                                     (/ (sin (* 2 pi W j))
                                        (* pi j))))))
         
         (j (1- N)))
    #+mcl(declare (dynamic-extent dpss-acvs vector-of-ratios))
    (dotimes (i N)
      (setf (aref dpss-acvs i)
            (* N (aref dpss-acvs i))))
    ;;; Note: both vector of ratios and dpss-acvs
    ;;;       roughy decrease in amplitude with increasing index,
    ;;;       so we sum things up in reverse order.
    (dotimes (i (1- N) (+ (* 2 eigenvalue)
                          (* 2 W (aref dpss-acvs 0))))
      (incf eigenvalue
            (* (aref dpss-acvs j)
               (aref vector-of-ratios j)))
      (decf j))))

#|
(multiple-value-bind (list-of-dpss eigenvalues)
                     (dpss-tapers-inverse-iteration
                      63 6 :taper-parameter 3.0)
  (dotimes (k 6)
    (let ((temp (dpss->eigenvalue (elt list-of-dpss k) 3.0)))
      (format t "~&k = ~D: ~14,11F ~14,11F ~14,11F"
              k
              (svref eigenvalues k)
              temp
              (- (svref eigenvalues k) temp)))))
;==>
k = 0:  0.99999987329  0.99999987329 -0.000000000000
k = 1:  0.99999116408  0.99999116408 -0.000000000000
k = 2:  0.99972363504  0.99972363504 -0.000000000000
k = 3:  0.99500772991  0.99500779999 -0.00000007008
k = 4:  0.94627164450  0.94659849352 -0.00032684902
k = 5:  0.70835607249  0.70835638986 -0.00000031737
|#

;-------------------------------------------------------------------------------
(defun largest-eigenvalues-of-tridiagonal-matrix
       (diag
        off-diag
        number-of-tapers
        &key
        (squared-off-diag (map 'vector #'(lambda (x) (* x x))  off-diag))
        (macheps single-float-epsilon)
        (print-progress-p nil))
  (let* ((n (length diag))
         (n-1 (1- n))
         (n-2 (1- n-1)))
    ;;; Set to zero all elements of squared-off-diag that correspond to
    ;;; small elements of off-diag (see do-loop 40 of tridib) ...
    (let ((previous-abs (aref diag 0)))
      (dotimes (i n-1)
        (if (<= (aref off-diag i) (* macheps (+ previous-abs
                                                (setf previous-abs
                                                      (aref diag (1+ i))))))
          (setf (aref squared-off-diag i) 0.0))))
    ;;; Use Equation (6) of Barth, Martin and Wilkinson to find
    ;;; upper and lower bounds for all eigenvalues ...
    (let* ((abs-off-diag-behind (abs (aref off-diag 0)))
           (abs-off-diag-ahead  (abs (aref off-diag n-2)))
           sum-of-abs
           (lower-bound-all-eigenvalues
            (min (- (aref diag 0) abs-off-diag-behind)
                 (- (aref diag n-1) abs-off-diag-ahead)))
           (upper-bound-all-eigenvalues
            (max (+ (aref diag 0) abs-off-diag-behind)
                 (+ (aref diag n-1) abs-off-diag-ahead)))
           (i 0))
      (dotimes (j n-2)
        (setf abs-off-diag-ahead (abs (aref off-diag (incf i)))
              sum-of-abs (+ abs-off-diag-behind abs-off-diag-ahead))
        (if (> lower-bound-all-eigenvalues (- (aref diag i) sum-of-abs))
          (setf lower-bound-all-eigenvalues (- (aref diag i) sum-of-abs)))
        (if (< upper-bound-all-eigenvalues (+ (aref diag i) sum-of-abs))
          (setf upper-bound-all-eigenvalues (+ (aref diag i) sum-of-abs)))
        (setf abs-off-diag-behind abs-off-diag-ahead))
      ;;; Expand upper and lower bounds a little (evidently to avoid
      ;;; numerical problems -- see code following do-loop 40 of tridib) ...
      (let ((eigenvalues (make-array number-of-tapers))
            (upper-bounds (make-array number-of-tapers
                                      :initial-element
                                      upper-bound-all-eigenvalues))
            (lower-bounds (make-array number-of-tapers
                                      :initial-element
                                      lower-bound-all-eigenvalues)))
        (dotimes (k number-of-tapers (values eigenvalues))
          (setf (aref eigenvalues k)
                ;;; use bisection to isolate eigenvalues, keeping track
                ;;; of bisections at each k to speed up subsequent k's
                (do* ((update-other-bounds (< k (1- number-of-tapers)))
                      (upper-target-count (- n k))
                      (lower-target-count (1- upper-target-count))
                      current-count
                      (U (svref upper-bounds k))
                      (L (svref lower-bounds k))
                      (mid-point (/ (+ U L) 2.0) (/ (+ U L) 2.0)))
                     ((<= (abs (- U L)) (* macheps (+ (abs U) (abs L))))
                      mid-point)
                  (setf current-count (sturm-sequence-count
                                       mid-point
                                       diag
                                       off-diag
                                       squared-off-diag
                                       macheps))
                  (when update-other-bounds
                    (let* ((N-current-count (- n current-count))
                           (j-to-inc N-current-count))
                      (dotimes (j (min N-current-count number-of-tapers))
                        (if (> mid-point (svref lower-bounds j))
                          (setf (svref lower-bounds j) mid-point)))
                      (dotimes (j (- number-of-tapers N-current-count))
                        (if (< mid-point (svref upper-bounds j-to-inc))
                          (setf (svref upper-bounds j-to-inc) mid-point))
                        (incf j-to-inc)))
                    (if (>= current-count lower-target-count)
                      (setf update-other-bounds nil)))
                  (if (<= current-count lower-target-count)
                    (setf L mid-point)
                    (setf U mid-point))))
          (if print-progress-p (format t ".")))))))

;-------------------------------------------------------------------------------
(defun sturm-sequence-count
       (test-lambda
        diag
        off-diag
        squared-off-diag
        machep
        &key
        (start 0)
        (end (length diag)))
  (let ((count start)
        (bottom 1.0))
    (dotimes (i (- end start) count)
      (let ((ratio (cond
                    ((zerop start)
                     0.0)
                    ((zerop bottom)
                     (if (zerop (aref squared-off-diag (1- start)))
                       0.0
                       (/ (abs (aref off-diag (1- start))) machep)))
                    (t
                     (/ (aref squared-off-diag (1- start))
                        bottom)))))
        (setf bottom
              (- (aref diag start) test-lambda ratio))
        (incf start)
        (if (minusp bottom) (incf count))))))

;-------------------------------------------------------------------------------
(defun symmetric-tridiagonal-solve! (diag off-diag b)
  "given
   [1] diag (required)
       <=> diagonal part of symmetric tridiagonal matrix A;
           trash on output
   [1] off-diag (required)
       <=> off-diagonal part of symmetric tridiagonal matrix;
           trash on output
   [2] b (required)
       <=> on input, the right-hand side vector;
           on output, the solution X to A X = b
returns
   [4] X, the solution to A X = b, where X is the vector
       that contained b on input
---
Note: this is an implementation of Algorithm 4.3.6,
p. 156, Golub and Van Loan, 1989, with modifications
to avoid divides by zero"
  (let* (temp
         (n (length diag))
         (n-1 (1- n))
         (zero-replacement (* single-float-epsilon
                              (symmetric-tridiagonal-infinity-norm
                               diag off-diag))))
    (if (< (abs (aref diag 0)) single-float-epsilon)
      (setf (aref diag 0) zero-replacement))
    (dotimes (k n-1)
      (setf temp (aref off-diag k)
            (aref off-diag k) (/ temp (aref diag k)))
      (decf (aref diag (1+ k)) (* temp (aref off-diag k)))
      (if (< (abs (aref diag (1+ k))) single-float-epsilon)
        (setf (aref diag (1+ k)) zero-replacement)))
    (dotimes (k n-1)
      (decf (aref b (1+ k)) (* (aref b k) (aref off-diag k))))
    (setf (aref b n-1) (/ (aref b n-1) (aref diag n-1)))
    (let ((k n-1))
      (dotimes (j n-1 (values b))
        (decf k)
        (setf (aref b k) (- (/ (aref b k) (aref diag k))
                            (* (aref off-diag k) (aref b (1+ k)))))))))

;-------------------------------------------------------------------------------
(defun symmetric-tridiagonal-infinity-norm (diag off-diag)
  (let* ((n (length diag))
         (n-2 (- n 2))
         (infinity-norm
          (max (+ (abs (aref diag 0)) (abs (aref off-diag 0)))
               (+ (abs (aref diag (1+ n-2))) (abs (aref off-diag n-2)))))
         (j 0))
    (dotimes (i n-2 infinity-norm)
      (if (> (+ (abs (aref off-diag j))
                (abs (aref off-diag (incf j)))
                (abs (aref diag j)))
             infinity-norm)
        (setf infinity-norm
              (+ (abs (aref off-diag (1- j)))
                 (abs (aref off-diag j))
                 (abs (aref diag j))))))))
           
;-------------------------------------------------------------------------------
(defun fast-tridiag-eigenvalue->dpss!
       (eigenvalue
        order-of-taper
        diag
        off-diag
        &key
        (eps (* 10 single-float-epsilon))
        (maximum-number-of-iterations 25)
        (b (generate-initial-guess-at-dpss (length diag) order-of-taper
                                           :half-size-p t)))
  (let* ((N (length diag))
         (M (/ (if (evenp N) N (1- N)) 2))
         (shorter-diag (make-array M))
         (shorter-off-diag (make-array M))
         (scratch-diag (make-array M))
         (scratch-off-diag (make-array M)))
    #+mcl(declare (dynamic-extent shorter-diag shorter-off-diag
                                  scratch-diag scratch-off-diag))
    (replace shorter-diag diag)
    (replace shorter-off-diag off-diag)
    (if (evenp N)
      ;;; even N
      (if (evenp order-of-taper)
        (incf (svref shorter-diag (1- M))
              (aref off-diag (1- M)))
        (decf (svref shorter-diag (1- M))
              (aref off-diag (1- M))))
      ;;; odd N
      (if (evenp order-of-taper)
        (incf (svref shorter-diag (1- M))
              (/ (* 2.0 (expt (aref off-diag (1- M)) 2)) eigenvalue))))
    (x+b! shorter-diag (- eigenvalue))
    (let ((b-old (copy-seq b))
          (end-sum (max 2 (truncate N (* 2 (1+ order-of-taper)))))
          iter-total)
      (setf iter-total
            (dotimes (i maximum-number-of-iterations)
              (symmetric-tridiagonal-solve!
               (replace scratch-diag shorter-diag)
               (replace scratch-off-diag shorter-off-diag)
               b)
              (a*x! (* (/ (sqrt (sum-of-squares b)))
                       (if (plusp (sum b :end end-sum))
                         1.0
                         -1.0))
                    b)
              (if (< (compare-seqs b b-old) eps) (return (values (1+ i)))
                  (replace b-old b))))
      (let ((result-dpss (make-array N)))
        (replace result-dpss b)
        (cond
         ((evenp N)
          (let ((i-rev N))
            (if (evenp order-of-taper)
              (dotimes (i M)
                (setf (svref result-dpss (decf i-rev))
                      (svref result-dpss i)))
              (dotimes (i M)
                (setf (svref result-dpss (decf i-rev))
                      (* -1 (svref result-dpss i)))))))
         ;;; N is odd - branch according to order of taper
         ((evenp order-of-taper)
          (let ((i-rev N))
            (dotimes (i M)
              (setf (svref result-dpss (decf i-rev))
                    (svref result-dpss i)))
            (setf (svref result-dpss M)
                  (/ (* 2
                        (svref result-dpss (1- M))
                        (aref off-diag (1- M)))
                     eigenvalue))))
         ;;; N is odd, and order of taper is odd
         (t 
          (let ((i-rev N))
            (dotimes (i M)
              (setf (svref result-dpss (decf i-rev))
                    (* -1 (svref result-dpss i))))
            (setf (svref result-dpss M) 0.0))))
        (values (a*x! (/ (sqrt (sum-of-squares result-dpss))) result-dpss)
                iter-total)))))

;-------------------------------------------------------------------------------
(defun generate-initial-guess-at-dpss
       (N
        order-of-taper
        &key
        (half-size-p nil)
        (result (make-array (if half-size-p
                              (/ (if (evenp N) N (1- N)) 2)
                              N))))
  (iota 1 (length result) :result result)
  (a*x! (float (/ (1+ N))) result)
  (map-into result #'(lambda (x)
                       (* x x (expt (1- x) 2)
                          (let ((prod 1.0)
                                (order+1 (1+ order-of-taper)))
                            (dotimes (j order-of-taper prod)
                              (multf prod (- (/ (1+ j) order+1) x))))))
            result)
  (let ((factor (/ (sqrt (sum-of-squares result)))))
    (a*x! factor result)))

;-------------------------------------------------------------------------------
;;; not currently used, but useful for pulling out a single eigenvalue ...
(defun kth-eigenvalue-of-tridiagonal-matrix
       (diag
        off-diag
        k
        &key
        (squared-off-diag (map 'vector #'(lambda (x) (* x x))  off-diag))
        (macheps single-float-epsilon))
  (let* ((n (length diag))
         (n-1 (1- n))
         (n-2 (1- n-1)))
    ;;; Set to zero all elements of squared-off-diag that correspond to
    ;;; small elements of off-diag (see do-loop 40 of tridib) ...
    (let ((previous-abs (aref diag 0)))
      (dotimes (i n-1)
        (if (<= (aref off-diag i) (* macheps (+ previous-abs
                                                (setf previous-abs
                                                      (aref diag (1+ i))))))
          (setf (aref squared-off-diag i) 0.0))))
   ;;; Use Equation (6) of Barth, Martin and Wilkinson to find
   ;;; upper and lower bounds for all eigenvalues ...
    (let* ((abs-off-diag-behind (abs (aref off-diag 0)))
           (abs-off-diag-ahead  (abs (aref off-diag n-2)))
           sum-of-abs
           (lower-bound-all-eigenvalues
            (min (- (aref diag 0) abs-off-diag-behind)
                 (- (aref diag n-1) abs-off-diag-ahead)))
           (upper-bound-all-eigenvalues
            (max (+ (aref diag 0) abs-off-diag-behind)
                 (+ (aref diag n-1) abs-off-diag-ahead)))
           (i 0))
      (dotimes (j n-2)
        (setf abs-off-diag-ahead (abs (aref off-diag (incf i)))
              sum-of-abs (+ abs-off-diag-behind abs-off-diag-ahead))
        (if (> lower-bound-all-eigenvalues (- (aref diag i) sum-of-abs))
          (setf lower-bound-all-eigenvalues (- (aref diag i) sum-of-abs)))
        (if (< upper-bound-all-eigenvalues (+ (aref diag i) sum-of-abs))
          (setf upper-bound-all-eigenvalues (+ (aref diag i) sum-of-abs)))
        (setf abs-off-diag-behind abs-off-diag-ahead))
      ;;; Expand upper and lower bounds a little (evidently to avoid
      ;;; numerical problems -- see code following do-loop 40 of tridib) ...
      (let ((delta (* N macheps (max (abs lower-bound-all-eigenvalues)
                                     (abs upper-bound-all-eigenvalues)))))
        (decf lower-bound-all-eigenvalues delta)
        (incf upper-bound-all-eigenvalues delta))
      ;;; use bisection to isolate kth eigenvalue ...
      (do* ((upper-target-count (- n k))
            (lower-target-count (1- upper-target-count))
            current-count
            (U upper-bound-all-eigenvalues)
            (L lower-bound-all-eigenvalues)
            (mid-point (/ (+ U L) 2.0) (/ (+ U L) 2.0)))
           ((<= (abs (- U L)) (* macheps (+ (abs U) (abs L))))
            mid-point)
        (setf current-count (sturm-sequence-count
                             mid-point
                             diag
                             off-diag
                             squared-off-diag
                             macheps))
        (if (<= current-count lower-target-count)
          (setf L mid-point)
          (setf U mid-point))))))

;-------------------------------------------------------------------------------
;;; not currently used -- superceded by fast-tridiag-eigenvalue->dpss! ...
(defun tridiag-eigenvalue->dpss!
       (eigenvalue
        order-of-taper
        diag
        off-diag
        &key
        (eps (* 10 single-float-epsilon))
        (maximum-number-of-iterations 25)
        (b (generate-initial-guess-at-dpss (length diag) order-of-taper)))
  (x+b! diag (- eigenvalue))
  (let* ((N (length diag))
         (b-old (copy-seq b))
         (end-sum (max 2 (truncate N (1+ order-of-taper)))))
    (dotimes (i maximum-number-of-iterations (values b nil))
      (symmetric-tridiagonal-solve! (copy-seq diag)
                                       (copy-seq off-diag)
                                       b)
       (a*x! (* (/ (sqrt (sum-of-squares b)))
               (if (plusp (sum b :end end-sum))
                 1.0
                 -1.0))
            b)
      (if (< (compare-seqs b b-old) eps) (return (values b (1+ i))))
      (replace b-old b))))

;-------------------------------------------------------------------------------
(defun toeplitz
       (r
        y
        &optional
        (x (make-array (length y))))
  "given
   [1] r (required)
       ==> vector of length 2*N-1 representing
           the Toeplitz matrix R(i,j) with 0 <= i,j <= N-1,
           via the 2*N-1 values
           r(0,N-1), r(0,N-2),..., r(0,1),
           r(0,0), r(1,0),..., r(N-1,0)
   [2] y (required)
       ==> right-hand side vector of length N
   [3] x (optional; vector of same size as y)
       <== solution vector of length N
solves the Toeplitz system sum_{j=0}^{N-1} r_{N-1+i-j} x_j = y_j
for i = 0, 1, ..., N-1 and
returns
   [1] x (a vector of length N)
       or
       nil (in case of a failure of the Levinson method
       due to a singular principal minor
---
Note: this function is essentially a Lisp version
      of the Fortran routine toeplz from Numerical Recipes"
  (let* ((n (array-total-size y))
         (index-of-0 (1- n))
         (g (make-array n))
         (h (make-array n))
         m1 sxn sd sgn shn sgd k pp qq pt1 pt2 qt1 qt2)
    (block main
      (if (zerop (aref r index-of-0)) (return-from main nil))
      (setf (aref x 0) (/ (aref y 0)
                          (aref r index-of-0)))
      (if (= n 1) (return-from main x))
      (setf (aref g 0) (/ (aref r (1- index-of-0))
                          (aref r index-of-0))
            (aref h 0) (/ (aref r (1+ index-of-0))
                          (aref r index-of-0)))
      (dotimes (m n)
        (setf m1 (1+ m)
              sxn (- (aref y m1))
              sd (- (aref r index-of-0)))
        (dotimes (j (1+ m))
          (incf sxn (* (aref r (+ index-of-0 (- m1 j)))
                       (aref x j)))
          (incf sd (* (aref r (+ index-of-0 (- m1 j)))
                      (aref g (- m j)))))
        (if (zerop sd) (return-from main nil))
        (setf (aref x m1) (/ sxn sd))
        (dotimes (j (1+ m))
          (decf (aref x j) (* (aref x m1) (aref g (- m j)))))
        (if (= (1+ m1) n) (return-from main x))   ;usual exit point
        (setf sgn (- (aref r (- index-of-0 (1+ m1))))
              shn (- (aref r (+ index-of-0 (1+ m1))))
              sgd (- (aref r index-of-0)))
        (dotimes (j (1+ m))
          (incf sgn (* (aref r (+ index-of-0 (- j m1)))
                       (aref g j)))
          (incf shn (* (aref r (+ index-of-0 (- m1 j)))
                       (aref h j)))
          (incf sgd (* (aref r (+ index-of-0 (- j m1)))
                       (aref h (- m j)))))
        (if (or (zerop sd) (zerop sgd)) (return nil))
        (setf (aref g m1) (/ sgn sgd)
              (aref h m1) (/ shn sd)
              k m
              pp (aref g m1)
              qq (aref h m1))
        (dotimes (j (truncate (+ m 2) 2))
          (setf pt1 (aref g j)
                pt2 (aref g k)
                qt1 (aref h j)
                qt2 (aref h k)
                (aref g j) (- pt1 (* pp qt2))
                (aref g k) (- pt2 (* pp qt1))
                (aref h j) (- qt1 (* qq pt2))
                (aref h k) (- qt2 (* qq pt1)))
          (decf k))))
    ;;; NOTE: this function will exit from this point (with a value of nil
    ;;;       since this is the value of dotimes) only if there was some sort of
    ;;;       gross screw-up
    ))

#|
;;; [ 3 2 0 ] [ x_0 ]   [  7 ]
;;; [ 2 3 2 ] [ x_1 ] = [ 14 ]
;;; [ 1 2 3 ] [ x_2 ]   [ 14 ]
(let* ((r-test #(0 2 3 2 1))
       (y-test-1 #(7 14 14))
       (y-test-2 #(3 2 1))
       (y-test-3 #(2 3 2))
       (y-test-4 #(0 2 3)))
  (print (toeplitz r-test y-test-1))
  (print (toeplitz r-test y-test-2))
  (print (toeplitz r-test y-test-3))
  (print (toeplitz r-test y-test-4)))
;==> #(1 2 3) 
;    #(1 0 0) 
;    #(0 1 0) 
;    #(0 0 1) 
;    #(0 0 1)
|#

;-------------------------------------------------------------------------------
(defun Householder-reduction-of-real-sym-matrix!
       (real-sym-matrix &key (prepare-for-eigenvectors-p t))
  "given
   [1] real-sym-matrix (required)
       ==> real, symmetric matrix (n by n)
   [2] prepare-for-eigenvectors-p (keyword; t)
       ==> if true, crunch real-sym-matrix for use with
           eigenvalues-and-vectors-of-sym-tridiag-matrix
returns
   [1] n-dimensional vector with diagonal elements of
       associated tridiagonal matrix
   [2] (n-1)-dimensional vector with off-diagonal elements of
       associated tridiagonal matrix
---
See tred2 in Numerical Recipes"
  (let* ((n (car (array-dimensions real-sym-matrix)))
         (diag (make-array n))
         (off-diag (make-array n))
         i-local l-local f g h hh scale)
    (if (> n 1)
      (dotimes (i-index (1- n))
        (setf i-local (- n i-index 1)
              l-local (- i-local 1)
              h 0.0
              scale 0.0)
        (cond ((plusp l-local)
               ;;; BEGIN LOOP 11
               (dotimes (k (1+ l-local))
                 (incf scale (abs (aref real-sym-matrix i-local k))))
               ;;; END LOOP 11
               (cond ((zerop scale)
                      (setf (aref off-diag i-local)
                            (aref real-sym-matrix i-local l-local)))
                     (t
                      ;;; BEGIN LOOP 12
                      (dotimes (k (1+ l-local))
                        (setf (aref real-sym-matrix i-local k)
                              (/ (aref real-sym-matrix i-local k) scale))
                        (incf h (expt (aref real-sym-matrix i-local k) 2)))
                      ;;; END LOOP 12
                      (setf f (aref real-sym-matrix i-local l-local)
                            g (- (sign (sqrt h) f))
                            (aref off-diag i-local) (* scale g)
                            h (- h (* f g))
                            (aref real-sym-matrix i-local l-local) (- f g)
                            f 0.0)
                      ;;; BEGIN LOOP 15
                      (dotimes (j (1+ l-local))
                        (if prepare-for-eigenvectors-p
                          (setf (aref real-sym-matrix j i-local)
                                (/ (aref real-sym-matrix i-local j) h)))
                        (setf g 0.0)
                        ;;; BEGIN LOOP 13
                        (dotimes (k (1+ j))
                          (incf g (* (aref real-sym-matrix j k)
                                     (aref real-sym-matrix i-local k))))
                        ;;; END LOOP 13
                        (if (> l-local j)
                          ;;; BEGIN LOOP 14
                          (dotimes (k (- l-local j))
                            (incf g (* (aref real-sym-matrix (+ k j 1) j)
                                       (aref real-sym-matrix i-local (+ k j 1)))))
                          ;;; END LOOP 14
                          )
                        (setf (aref off-diag j) (/ g h)
                              f (+ f (* (aref off-diag j)
                                        (aref real-sym-matrix i-local j)))))
                      ;;; END LOOP 15
                      (setf hh (/ f (+ h h)))
                      ;;; BEGIN LOOP 17
                      (dotimes (j (1+ l-local))
                        (setf f (aref real-sym-matrix i-local j)
                              g (- (aref off-diag j) (* hh f))
                              (aref off-diag j) g)
                        ;;; BEGIN LOOP 16
                        (dotimes (k (1+ j))
                          (decf (aref real-sym-matrix j k)
                                (+ (* f (aref off-diag k))
                                   (* g (aref real-sym-matrix i-local k)))))
                        ;;; END LOOP 16
                        )
                      ;;; END LOOP 17
                      )))
              (t
               (setf (aref off-diag i-local)
                     (aref real-sym-matrix i-local l-local))))
        (setf (aref diag i-local) h)))
    ;;; END OF ``IF'' CLAUSE ...
    (if prepare-for-eigenvectors-p
      (setf (aref diag 0) 0.0))
    (setf (aref off-diag 0) 0.0)
    ;;; BEGIN LOOP 23
    (dotimes (i-local n)
      (when prepare-for-eigenvectors-p
        (setf l-local (1- i-local))
        (when (/= (aref diag i-local) 0.0)
          (dotimes (j (1+ l-local))
            (setf g 0.0)
            (dotimes (k (1+ l-local))
              (incf g (* (aref real-sym-matrix i-local k)
                         (aref real-sym-matrix k j))))
            (dotimes (k (1+ l-local))
              (decf (aref real-sym-matrix k j)
                    (* g (aref real-sym-matrix k i-local)))))))
      (setf (aref diag i-local) (aref real-sym-matrix i-local i-local))
      (when prepare-for-eigenvectors-p
        (setf (aref real-sym-matrix i-local i-local) 1.0)
        (when (>= l-local 0)
          (dotimes (j (1+ l-local))
            (setf (aref real-sym-matrix i-local j) 0.0
                  (aref real-sym-matrix j i-local) 0.0)))))
    ;;; END LOOP 23
    (values diag (make-array (1- n)
                             :displaced-to off-diag
                             :displaced-index-offset 1))))

;-------------------------------------------------------------------------------
(defun eigenvalues-and-vectors-of-sym-tridiag-matrix
       (diag
        off-diag
        &key
        (maximum-number-of-iterations 30)
        (return-eigenvectors-p t)
        (z (if return-eigenvectors-p
             (let* ((n (length diag))
                    (temp (make-array `(,n ,n)
                                      :initial-element 0.0)))
               (dotimes (i n temp)
                 (setf (aref temp i i) 1.0))))))
  "given
   [1] diag (required)
       ==> sequence of diagonal elements
           (length of, say, n)
   [2] off-diag (required)
       ==> sequence of off-diagonal elements
           (length of n-1)
   [3] maximum-number-of-iterations (keyword; 30)
       ==> maximum number of iterations
   [4] return-eigenvectors-p (keyword; t)
       ==> if true, returns eigenvalues and eigenvectors;
           if nil, just do the eigenvalues (this is faster)
   [5] z (keyword; n by n diagonal matrix)
       ==> usually bound to output from
           Householder-reduction-of-real-sym-matrix
returns
   [1] vector with n eigenvalues
       (NOT necessarily ordered by size)
   [2] if return-eigenvectors-p is true, an n by n array
       whose jth column is the eigenvector corresponding
       to the jth eigenvalue; if return-eigenvectors-p is nil,
       that's what you get!
---
This is based upon tqli in Numerical Recipes"
  (assert (= (length diag) (1+ (length off-diag))))
  (let* ((n (length diag))
         (d (copy-seq diag))
         (e (make-array n :initial-element 0.0))
         dd m m-local g s c p i-local f b r)
    (cond
     ((<= n 1) d)
     (t
      (dotimes (i (1- n))
        (setf (elt e i) (elt off-diag i)))
      ;;; LOOP 15
      ;;; routine returns d --- vector of eigenvalues
      (dotimes (l n (values d z))
        (do ((iteration-count 0 (+ iteration-count 1)))
            (nil)    ;do forever
          (setf m (dotimes (mm (- n l 1) (1- n))
                    (setf m-local (+ mm l)
                          dd (+ (abs (elt d m-local))
                                (abs (elt d (1+ m-local)))))
                    (if (= (+ (abs (elt e  m-local)) dd)
                           dd)
                      (return m-local))))
          (cond
           ((= m l) (return))
           (t
            (assert (< iteration-count maximum-number-of-iterations))
            (setf g (/ (- (elt d (1+ l)) (elt d l))
                       (* 2.0 (elt e l)))
                  r (sqrt (+ 1.0 (* g g)))
                  g (+ (/ (elt e l) (+ g (sign r g)))
                       (elt d m)
                       (- (elt d l)))
                  s 1.0
                  c 1.0
                  p 0.0)
            ;;; LOOP 14
            (dotimes (i (- m l))
              (setf i-local (- m 1 i)
                    f (* s (elt e i-local))
                    b (* c (elt e i-local)))
              (if (>= (abs f) (abs g))
                (setf c (/ g f)
                      r (sqrt (+ 1.0 (* c c)))
                      (elt e (1+ i-local)) (* f r)
                      s (/ r)
                      c (* c s))
                (setf s (/ f g)
                      r (sqrt (+ 1.0 (* s s)))
                      (elt e (1+ i-local)) (* g r)
                      c (/ r)
                      s (* s c)))
              (setf g (- (elt d (1+ i-local)) p)
                    r (+ (* s (- (elt d i-local) g))
                         (* 2.0 c b))
                    p (* s r)
                    (elt d (1+ i-local)) (+ g p)
                    g (- (* c r) b))
              (when return-eigenvectors-p
                (dotimes (k n)
                  (setf f (aref z k (1+ i-local))
                        (aref z k (1+ i-local)) (+ (* s (aref z k i-local))
                                                   (* c f))
                        (aref z k i-local) (- (* c (aref z k i-local))
                                              (* s f)))))
              )
            (setf (elt d l) (-  (elt d l) p)
                  (elt e l) g
                  (elt e m) 0.0)))))))))

#|
;;; The following set of abscissa points and weights
;;; are used as defaults in dpss-tapers-Thomson-approx and
;;; were computed using the following Lisp form:
(multiple-value-setq (*abscissas-32-point* *weights-32-point*)
  (gauss-legendre-quadrature -1.0 1.0 32))
|#
(defvar *abscissas-32-point* (vector -0.9972638618494816D0
                                     -0.9856115115452684D0
                                     -0.9647622555875064D0
                                     -0.9349060759377397D0
                                     -0.8963211557660521D0
                                     -0.84936761373257D0
                                     -0.7944837959679424D0
                                     -0.7321821187402897D0
                                     -0.6630442669302152D0
                                     -0.5877157572407623D0
                                     -0.5068999089322294D0
                                     -0.4213512761306353D0
                                     -0.3318686022821277D0
                                     -0.2392873622521371D0
                                     -0.1444719615827965D0
                                     -4.830766568773831D-2 
                                     4.830766568773831D-2 
                                     0.1444719615827965D0
                                     0.2392873622521371D0
                                     0.3318686022821277D0
                                     0.4213512761306353D0
                                     0.5068999089322294D0
                                     0.5877157572407623D0
                                     0.6630442669302152D0
                                     0.7321821187402897D0
                                     0.7944837959679424D0
                                     0.84936761373257D0
                                     0.8963211557660521D0
                                     0.9349060759377397D0
                                     0.9647622555875064D0
                                     0.9856115115452684D0
                                     0.9972638618494816D0))

(defvar *weights-32-point* (vector 7.018610009469521D-3 
                                   1.627439473090571D-2 
                                   2.539206530926214D-2 
                                   3.427386291302141D-2 
                                   4.283589802221816D-2 
                                   0.0509980592623745D0
                                   5.868409347853513D-2 
                                   6.582222277636181D-2 
                                   7.234579410884862D-2 
                                   7.819389578707044D-2 
                                   8.331192422694672D-2 
                                   8.765209300440374D-2 
                                   0.0911738786957639D0
                                   9.384439908080441D-2 
                                   9.563872007927485D-2 
                                   9.654008851472785D-2 
                                   9.654008851472785D-2 
                                   9.563872007927485D-2 
                                   9.384439908080441D-2 
                                   0.0911738786957639D0
                                   8.765209300440374D-2 
                                   8.331192422694672D-2 
                                   7.819389578707044D-2 
                                   7.234579410884862D-2 
                                   6.582222277636181D-2 
                                   5.868409347853513D-2 
                                   0.0509980592623745D0
                                   4.283589802221816D-2 
                                   3.427386291302141D-2 
                                   2.539206530926214D-2 
                                   1.627439473090571D-2 
                                   7.018610009469521D-3))

;-------------------------------------------------------------------------------
;;; This and the following two functions are used to generate Greenhall's
;;; trig prolate data tapers.
(defun trig-prolate-sign-cosine-coefficients (M k)
  (case M
    (2 (case k
         (0 '(0.8202108 0.4041691 0.0165649))
         (1 '(0.0 -0.7007932 -0.0942808))
         (otherwise
          (error "can't handle k = ~A for M = 2" k ))))
    (3 (case k
         (0 '(0.7499700 0.4596063 0.0867984 0.0007513))
         (1 '(0.0 -0.6507499 -0.2765560 -0.0064282))
         (2 '(0.4969513 -0.3050683 -0.5312499 -0.0350227))
         (3 '(0.0 -0.2731233 0.6397174 0.1271430))
         (otherwise
          (error "can't handle k = ~A for M = 3" k ))))
    (4 (case k
         (0 '(0.6996910 0.4830013 0.1473918 0.0141997 0.0000368))
         (1 '(0.0 -0.5927723 -0.3805986 -0.0613650 -0.0003329))
         (2 '(0.4783016 -0.1666510 -0.5724443 -0.1736202 -0.0022015))
         (3 '(0.0 -0.3540569 0.4929565 0.3626279 0.0117722))
         (4 '(0.3862293 -0.3223025 0.0856254 0.5584413 0.0484379))
         (otherwise
          (error "can't handle k = ~A for M = 4" k ))))
    (5 (case k
         (0 '(0.6632850 0.4915713 0.1927963 0.0347859 0.0019243 0.0000018))
         (1 '(0.0 -0.5401300 -0.4383060 -0.1266343 -0.0105462 -0.0000191))
         (2 '(0.4560698 -0.0704481 -0.5519198 -0.2915206 -0.0379143 -0.0001319))
         (3 '(0.0 -0.3866087 0.3363930 0.4760267 0.1037856 0.0007467))
         (4 '(0.3821638 -0.2527019 -0.1138304 0.5457777 0.2286313 0.0037712))
         (5 '(0.0 -0.2216043 0.3885522 -0.3657298 -0.4072901 -0.0165910))
         (6 '(0.3246026 -0.2957322 0.1964585 0.0266965 -0.5631039 -0.0588589))
         (otherwise
          (error "can't handle k = ~A for M = 5" k ))))
    (otherwise
     (error "can't handle M = ~A" M))))

;-------------------------------------------------------------------------------
(defun produce-upsilon-func (coefficients)
  (if (zerop (car coefficients))
    #'(lambda (t-index)
        (let ((sum 0.0))
          (dotimes (j (1- (length coefficients)) sum)
            (incf sum (* 2.0 (elt coefficients (1+ j))
                         (sin (* 2.0 pi (1+ j) t-index)))))))
    #'(lambda (t-index)
        (let ((sum (car coefficients)))
          (dotimes (j (1- (length coefficients)) sum)
            (incf sum (* 2.0 (elt coefficients (1+ j))
                         (cos (* 2.0 pi (1+ j) t-index)))))))))

;-------------------------------------------------------------------------------
(defun generate-tri-prolate-taper (N generating-function)
  (let ((the-taper (make-array N))
        (factor (float (/ (sqrt N))))
        (factor-top (float (1- N)))
        (factor-bot (float (* 2 N))))
    (dotimes (i N the-taper)
      (setf (aref the-taper i)
            (* factor
               (funcall generating-function
                        (/ (- (* 2 i) factor-top)
                           factor-bot)))))))
