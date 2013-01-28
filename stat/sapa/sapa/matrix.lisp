;;;-*- Mode: LISP; Package: :SAPA; Syntax: COMMON-LISP -*-
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;
;  matrix.lisp
;
;  a collection of Lisp functions for some basic matrix operations ...
;  Note:  before compiling and loading matrix.lisp,
;         you should compile and load
;         sapa-package.lisp
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
;;; (compile-file "ccl:SAPA;matrix.lisp")
;;; (load "ccl:SAPA;matrix.fasl")
;-------------------------------------------------------------------------------
(if (not (find-package :SAPA))
  (defpackage :SAPA (:USE :COMMON-LISP)))

(in-package :SAPA)

(export '(;;; functions for printing arrays ...
          print-1-or-2-d-array
          print-rows-of-nxm-matrix

          ;;; functions for converting to/from linearized storage ...
          linearized-upper-trangular->2d-matrix
          2d-matrix->linearized-upper-trangular

          ;;; functions to do elementary matrix operations ...
          transpose
          Hermitian-transpose
          zero-strict-lower-diagonal!
          multiply-two-matrices
          multiply-matrix-and-vector
          multiply-matrix-and-scalar
          subtract-two-matrices
          trace-matrix
          2d-matrix-move!
          
          ;;; functions to do Cholesky and Gram-Schmidt (Q-R) decompositions ...
          cholesky!
          spofa!
          sposl!
          modified-Gram-Schmidt!
          Q-R!

          ;;; functions to solve triangular system of equations ...
          upper-triangular-solve!
          lower-triangular-solve!
          ))

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;;  The functions  print-1-or-2-d-array
;;;                 print-rows-of-nxm-matrix
;;;  can be used to print out the elements of one and two dimensional array.
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
(defun print-1-or-2-d-array
       (an-array
        &key
        (tag "array")
        (format-control-for-array-element "~F"))
  "given
   [1] an-array (required)
       ==> a one-dimensional or two-dimensional array
   [2] tag (keyword; `array')
       ==> an optional tag to be printed
           along with array elements
   [3] format-control-for-array-element (keyword; `~F')
       ==> format control for a single array element
prints the elements of an-array and
returns
   [1] an-array"
  (let ((dims (array-dimensions an-array))
        (for-format (concatenate 'string
                                 "~&~A, element ~A: "
                                 format-control-for-array-element)))
    (cond
     ((= (length dims) 1)
      ;;; a vector ...
      (dotimes (i (car dims) (values an-array))
        (format t for-format tag i (aref an-array i))))
     ((= (length dims) 2)
      (dotimes (i (nth 0 dims) (values an-array))
        (dotimes (j (nth 1 dims))
          (format t for-format tag (list i j) (aref an-array i j)))))
     (t
      (error "can't print ~A" an-array)))))

#|
(print-1-or-2-d-array #(1 2 3 4))
;==>
array, element 0: 1.0
array, element 1: 2.0
array, element 2: 3.0
array, element 3: 4.0
#(1 2 3 4)

(print-1-or-2-d-array (make-array '(3 2)
                                  :initial-contents
                                  '((1 2) (3 4) (5 6))))
;==>
array, element (0 0): 1.0
array, element (0 1): 2.0
array, element (1 0): 3.0
array, element (1 1): 4.0
array, element (2 0): 5.0
array, element (2 1): 6.0
#2a((1 2) (3 4) (5 6))
|#

;-------------------------------------------------------------------------------
(defun print-rows-of-nxm-matrix
       (an-nxm-matrix
        &key
        (format-control-for-array-element "~F "))
  "given
   [1] an-nxm-matrix (required)
       ==> a two-dimensional array
   [3] format-control-for-array-element (keyword; `~F')
       ==> format control for a single array element
prints the elements of the 2d array and
returns
   [1] an-nxm-matrix"
  (let ((dimensions (array-dimensions an-nxm-matrix)))
    (assert (= (length dimensions) 2))
    (let ((m-columns (elt dimensions 1)))
      (dotimes (i-row (elt dimensions 0))
        (format t "~&")
        (dotimes (j-col m-columns)
          (format t format-control-for-array-element
                  (aref an-nxm-matrix i-row j-col))))
      (format t "~&")
      (values an-nxm-matrix))))

#|
(print-rows-of-nxm-matrix (make-array '(3 2)
                                      :initial-contents
                                      '((1 2) (3 4) (5 6))))
;==>
1.0 2.0 
3.0 4.0 
5.0 6.0
#2a((1 2) (3 4) (5 6))
|#

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;;  The functions  linearized-upper-trangular->2d-matrix
;;;                 2d-matrix->linearized-upper-trangular
;;;  convert back and forth between a straightforward representation for 
;;;  a 2d square matrix and a scheme for storing the upper triangular
;;;  portion of a two dimensional matrix in a vector.
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
(defun linearized-upper-trangular->2d-matrix
       (A
        &key
        (result (let ((N (round (/ (1- (sqrt (1+ (* 8 (length A)))))
                                   2))))
                  (make-array `(,N ,N)))))
  "given
   [1] A (required)
       ==> a vector with an upper triangular matrix
           stored in linearized form
   [2] result (keyword; new 2d array of appropriate size)
       <== a 2d hermitian matrix filled according
           to contents of A
converts linearized form A into 2d hermitian array and
returns
   [1] result, a 2d hermitian matrix"
  (let ((L -1))
    (dotimes (i (car (array-dimensions result)) result)
      (dotimes (j (1+ i))
        (setf (aref result i j)
              (conjugate (setf (aref result j i)
                               (aref A (incf L)))))))))

#|
(linearized-upper-trangular->2d-matrix
 #(1 #C(1 1) 1))
;==> #2a((1 #c(1 1)) (#c(1 -1) 1))
|#

;-------------------------------------------------------------------------------
(defun 2d-matrix->linearized-upper-trangular
       (A
        &key
        (result (let ((N (car (array-dimensions A))))
                  (make-array (/ (* N (1+ N)) 2)))))
  "given
   [1] A (required)
       ==> a 2d hermitian matrix
   [2] result (keyword; new vector of appropriate size)
       <== a vector filled linearly with elements
           of A
converts 2d hermitian array into linearized form and
returns
   [1] result, a vector with 2d hermitian array
stored in linearized form"
  (let ((L -1))
    (dotimes (i (car (array-dimensions A)) result)
      (dotimes (j (1+ i))
        (setf (aref result (incf L))
              (aref A j i))))))

#|
(2d-matrix->linearized-upper-trangular
 (linearized-upper-trangular->2d-matrix
   #(1 #C(1 1) 1)))
;==> #(1 #c(1 1) 1)
|#

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;;  The functions  transpose
;;;                 Hermitian-transpose
;;;                 zero-strict-lower-diagonal!
;;;                 multiply-two-matrices
;;;                 multiply-matrix-and-vector
;;;                 multiply-matrix-and-scalar
;;;                 subtract-two-matrices
;;;                 trace-matrix
;;;                 2d-matrix-move!
;;;  perform fairly simple operations on matrices and vectors.
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
(defun transpose
       (a-matrix
        &key
        (result
         (make-array
          (reverse (array-dimensions a-matrix)))))
  "given
   [1] A (required)
       ==> a 2d matrix
   [2] result (keyword; new 2d array of appropriate size)
       <== a 2d matrix to contain transpose of a-matrix
returns
   [1] transpose of a-matrix (placed in result)"
  (let ((list-of-two-integers (array-dimensions a-matrix)))
    (dotimes (i (nth 0 list-of-two-integers) result)
      (dotimes (j (nth 1 list-of-two-integers))
        (setf (aref result j i)
              (aref a-matrix i j))))))

#|
(transpose #2a((1 2) (3 4) (5 6)))
;==> #2a((1 3 5) (2 4 6))
|#

;-------------------------------------------------------------------------------
(defun Hermitian-transpose
       (a-matrix
        &key
        (result
         (make-array
          (reverse (array-dimensions a-matrix)))))
  "given
   [1] A (required)
       ==> a 2d matrix
   [2] result (keyword; new 2d array of appropriate size)
       <== a 2d matrix to contain Hermitian transpose of a-matrix
returns
   [1] Hermitian transpose of a-matrix (placed in result)"
  (let ((list-of-two-integers (array-dimensions a-matrix)))
    (dotimes (i (nth 0 list-of-two-integers) result)
      (dotimes (j (nth 1 list-of-two-integers))
        (setf (aref result j i)
              (conjugate (aref a-matrix i j)))))))

#|
(Hermitian-transpose #2a((1 2) (3 4) (5 6)))
;==> #2a((1 3 5) (2 4 6))
(Hermitian-transpose #2a((1 #c(2 1)) (#c(3 -1) 1)))
;==>                 #2a((1 #c(3 1)) (#c(2 -1) 1))
|#

;-------------------------------------------------------------------------------
(defun zero-strict-lower-diagonal!
       (a-matrix)
  "given a square matrix,
zeros its lower diagonal and returns
the modified square matrix"
  (let* ((n (nth 0 (array-dimensions a-matrix)))
         (n-in-column-to-zap (1- n))
         (row-start 1))
    (dotimes (j-column (1- n) a-matrix)
      (dotimes (i n-in-column-to-zap)
        (setf (aref a-matrix (+ i row-start) j-column ) 0.0))
      (incf row-start)
      (decf n-in-column-to-zap))))

#|
(zero-strict-lower-diagonal! #2a((1 #c(2 1)) (#c(3 -1) 1)))
;==>                         #2a((1 #c(2 1)) (0.0 1))
(zero-strict-lower-diagonal! #2a((1 2 3)   (4 5 6)     (7 8 9)))
;==>                         #2a((1 2 3) (0.0 5 6) (0.0 0.0 9))
|#


;-------------------------------------------------------------------------------
(defun multiply-two-matrices
       (a-matrix
        b-matrix
        &key
        (result
         (make-array
          (list (nth 0 (array-dimensions a-matrix))
                (nth 1 (array-dimensions b-matrix))))))
  "given
   [1] a-matrix (required)
       ==> a 2d matrix
   [2] b-matrix (required)
       ==> another 2d matrix, with dimensions such that
           the product of a-matrix and b-matrix is defined
   [3] result (keyword; new 2d array of appropriate size)
       <== a 2d matrix to contain product of two matrices
returns
   [1] product of two matrices (placed in result)"
  (let ((m (nth 0 (array-dimensions a-matrix)))
        (n (nth 1 (array-dimensions b-matrix)))
        (common (nth 0 (array-dimensions b-matrix))))
    (dotimes (i m result)
      (dotimes (j n)
        (setf (aref result i j) 0.0)
        (dotimes (k common)
          (incf (aref result i j)
                (* (aref a-matrix i k) (aref b-matrix k j))))))))

#|
(multiply-two-matrices #2a((0 0 1) (0 1 0) (1 0 0))
                       #2a((10 9) (8 7) (6 5)))
;==> #2a((6.0 5.0) (8.0 7.0) (10.0 9.0))
|#

;-------------------------------------------------------------------------------
(defun multiply-matrix-and-vector
       (a-matrix
        b-vector
        &key
        (result
         (make-array
          (nth 0 (array-dimensions a-matrix)))))
  "given
   [1] a-matrix (required)
       ==> a 2d matrix
   [2] b-vector (required)
       ==> a vector, with dimensions such that
           the product of a-matrix and b-vector is defined
   [3] result (keyword; new vector of appropriate size)
       <== a vector to contain product of a-matrix and b-vector
returns
   [1] product of a-matrix and b-vector (placed in result)"
  (let ((m (nth 0 (array-dimensions a-matrix)))
        (n (length b-vector)))
    (dotimes (i m result)
      (setf (aref result i) 0.0)
      (dotimes (j n)
        (incf (aref result i)
              (* (aref a-matrix i j) (aref b-vector j)))))))

#|
(multiply-matrix-and-vector #2a((0 0 1) (0 1 0) (1 0 0))
                            #(10 9 8))
;==> #(8.0 9.0 10.0)
|#

;-------------------------------------------------------------------------------
(defun multiply-matrix-and-scalar
       (a-matrix
        scalar
        &key
        (result
         (make-array
          (array-dimensions a-matrix))))
  "given
   [1] a-matrix (required)
       ==> a 2d matrix
   [2] scalar (required)
       ==> an arbitrary number
   [3] result (keyword; new matrix of same size as a-matrix)
       <== a matrix to contain product of a-matrix and scalar
returns
   [1] product of a-matrix and scalar (placed in result)"
  (let ((m (nth 0 (array-dimensions a-matrix)))
        (n (nth 1 (array-dimensions a-matrix))))
    (dotimes (i m result)
      (dotimes (j n)
        (setf (aref result i j)
              (* scalar (aref a-matrix i j)))))))

#|
(multiply-matrix-and-scalar #2a((0 0 1) (0 1 0) (1 0 0))
                            1/3)
;==> #2a((0 0 1/3) (0 1/3 0) (1/3 0 0))
|#

;-------------------------------------------------------------------------------
(defun subtract-two-matrices
       (a-matrix
        b-matrix
        &key
        (result
         (make-array (array-dimensions a-matrix))))
  "given
   [1] a-matrix (required)
       ==> a 2d matrix
   [2] b-matrix (required)
       ==> a 2d matrix, with dimensions the same
           as a-matrix
   [3] result (keyword; new vector of appropriate size)
       <== a matrix to contain result of subtracting
           b-matrix from a-matrix
returns
   [1] a-matrix minus b-matrix (placed in result)"
  (let ((m (nth 0 (array-dimensions a-matrix)))
        (n (nth 1 (array-dimensions a-matrix))))
    (dotimes (i m result)
      (dotimes (j n)
        (setf (aref result i j)
              (- (aref a-matrix i j) (aref b-matrix i j)))))))

#|
(subtract-two-matrices #2a((1 0 0) (0 1 0) (0 0 1))
                       #2a((0 0 1) (0 1 0) (1 0 0)))
;==> #2a((1 0 -1) (0 0 0) (-1 0 1))
|#

;-------------------------------------------------------------------------------
(defun trace-matrix (a-matrix)
  "given a square matrix,
returns its trace"
  (let ((m (nth 0 (array-dimensions a-matrix)))
        (n (nth 1 (array-dimensions a-matrix))))
    (assert (= m n))
    (let ((sum 0.0))
      (dotimes (i m sum)
        (incf sum (aref a-matrix i i))))))

;;; (trace-matrix #2a((1 2 3) (4 5 6) (7 8 9)))  ;==> 15.0

;-------------------------------------------------------------------------------
(defun 2d-matrix-move!
       (from-this
        to-this)
  "given
   [1] from-this (required)
       ==> a 2d matrix
   [2] to-this (required)
       <== another 2d matrix
transfer contents of from-this to corresponding
locations in to-this, and
returns
   [1] to-this"
  (let* ((temp (array-dimensions from-this))
         (n-rows (nth 0 temp))
         (n-columns (nth 1 temp)))
    (dotimes (i n-rows to-this)
      (dotimes (j n-columns)
        (setf (aref to-this i j) (aref from-this i j))))))

#|
(2d-matrix-move! #2a((1 2 3) (4 5 6) (7 8 9)) (make-array '(3 3)))
;==> #2a((1 2 3) (4 5 6) (7 8 9))
(2d-matrix-move! #2a((1 2 3) (4 5 6) (7 8 9)) (make-array '(4 4)))
;==> #2a((1 2 3 nil) (4 5 6 nil) (7 8 9 nil) (nil nil nil nil))
|#

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;;  The functions  cholesky!
;;;                 spofa!
;;;                 sposl!
;;;                 modified-Gram-Schmidt!
;;;                 Q-R!
;;;  carry out Cholesky and modified-Gram-Schmidt factorizations of a two
;;;  dimensional matrix.  These functions destroy whatever matrices
;;;  are given to them, and hence all have names that end with a ``!''.
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
(defun cholesky! (A b &key (eps single-float-epsilon))
  "given
   [1] A (required)
       ==> vector of length N*(N+1)/2 representing
           a two dimensional N by N positive definite matrix
           with elements stored columnwise; i.e, elements with indices
           0     1     2     3     4     5    ...
           correspond to matrix positions
           1,1   1,2   2,2   1,3   2,3   3,3  ...
   [2] b (required)
       <=> right-hand vector on input;
           solution X on output
   [3] eps (keyword; single-float-epsilon)
       number used to test for a computationally singular matrix
solves A X = b using the Cholesky decomposition method, and
returns
   [1] X, the solution vector (stored in b)"
  (let* ((N (length b))
         (N-1 (1- N))
         (XL (make-array `(,N ,N)))
         (Y (make-array N))
         (D (make-array N))
         (L 0))
    ;(declare (dynamic-extent XL Y D))
    (assert (= (length A) (/ (* N (1+ N)) 2)))
    ;;; Factor into triangular and diagonal form ...
    (setf (aref D 0) (realpart (aref A 0)))
    (dotimes (i N-1)
      (dotimes (j (1+ i))
        (incf L)
        (setf (aref XL (1+ i) j) (/ (conjugate (aref A L)) (aref D j)))
        (when (not (zerop j))
          (dotimes (k j)
            (decf (aref XL (1+ i) j)
                  (/ (* (aref XL (1+ i) k)
                        (conjugate (aref XL j k))
                        (aref D k))
                     (aref D j))))))
      (incf L)
      (setf (aref D (1+ I)) (realpart (aref A L)))
      (dotimes (k (1+ I))
        (decf (aref D (1+ I))
              (* (aref D k) (expt (abs (aref XL (1+ i) k)) 2))))
      ;;; Test for nonpositive value (i.e., matrix is too close to
      ;;; being singular)
      (if (< (aref D (1+ I)) eps)
        (error "(aref D ~D) = ~F < ~F = esp~&         matrix is computationally singular"
               (1+ I) (aref D (1+ I)) eps)))
    ;;; Solve for intermediate column vector solution ...
    (setf (aref Y 0) (aref b 0))
    (dotimes (k N-1)
      (setf (aref Y (1+ k)) (aref b (1+ k)))
      (dotimes (j (1+ k))
        (decf (aref Y (1+ k))
              (* (aref XL (1+ k) j) (aref Y j)))))
    ;;; Solve for final column vector solution ...
    (setf (aref b N-1) (/ (aref Y N-1) (aref D N-1)))
    (let ((k-index N-1))
      (dotimes (k-count N-1 (values b))
        (decf k-index)
        (setf (aref b k-index) (/ (aref Y k-index) (aref D k-index)))
        (dotimes (j (- N k-index 1))
          (decf (aref b k-index)
                (* (conjugate (aref XL (+ k-index j 1) k-index))
                   (aref b (+ k-index j 1)))))))))

#|
;;; Example inspired by Yule-Walker equations for AR(1) model
;;; with phi = 0.9
(2d-matrix->linearized-upper-trangular
 #2a((1.0 0.9 0.81) (0.9 1.0 0.9) (0.81 0.9 1.0)))
;==> #(1.0 0.9 1.0 0.81 0.9 1.0)
(cholesky! #(1.0 0.9 1.0 0.81 0.9 1.0)
           #(0.9 0.81 0.729))
;==> #(0.9 5.258951169277058E-16 -5.84327907697451E-16)
|#

;-------------------------------------------------------------------------------
(defun spofa!
       (A
        &key
        (n (car (array-dimensions a))))
  "given
   [1] A (required)
       <=> real symmetric positive definite matrix
           (this gets trashed)
   [2] n (keyword; (array-dimensions a))
       ==> size of matrix to be factored
calculates the upper triangular matrix U
of the Cholesky decomposition A = LU,
where L = U^T, and
returns
   [1] U, stored in upper triangular part of A"
  (let (info temp sum)
    (dotimes (j n (values A 0))
      (setf info j
            sum 0.0)
      (when (plusp j)
        (dotimes (k j)
          (setf temp (/ (- (aref A k j)
                           (let ((dot-product 0.0))
                             (dotimes (m k dot-product)
                               (incf dot-product
                                     (* (aref A m j) (aref A m k))))))
                        (aref A k k))
                (aref A k j) temp)
          (incf sum (* temp temp))))
      (setf sum (- (aref A j j) sum))
      (if (not (plusp sum)) (return (values A (1+ info))))
      (setf (aref A j j) (sqrt sum)))))

#|
(setf *U* (spofa! #2a((1.0 0.9 0.81) (0.9 1.0 0.9) (0.81 0.9 1.0))))
;==> #2a((1.0 0.9 0.81) (0.9 0.4358898943540673 0.39230090491866054) (0.81 0.9 0.4358898943540673))
(zero-strict-lower-diagonal! *U*)
;==> #2a((1.0 0.9 0.81) (0.0 0.4358898943540673 0.39230090491866054) (0.0 0.0 0.4358898943540673))
(multiply-two-matrices (transpose *U*) *U*)
;==> #2a((1.0 0.9 0.81) (0.9 1.0 0.9) (0.81 0.9 1.0))
|#

;-------------------------------------------------------------------------------
(defun sposl!
       (A
        b
        &key
        (N (length b)))
  "given
   [1] A (required)
       ==> real symmetric positive definite matrix
           AFTER it has been crunched by spofa!
   [2] b (required)
       <=> the right-hand side vector on input;
            on output, this gets replaced by the solution X
   [3] N (keyword; length of b)
       ==> order of matrix A (default is length of b)
returns
   [1] X, the solution to A X = b; note that X is the same as
       the vector which contained b"
  (let ((kb (1- N))
        temp)
    (dotimes (k-col N)
      (setf (aref b k-col) (/ (- (aref b k-col)
                                 (if (zerop k-col) 0.0
                                     (let ((dot-product 0.0))
                                       (dotimes (j-row k-col dot-product)
                                         (incf dot-product
                                               (* (aref A j-row k-col)
                                                  (aref b j-row)))))))
                              (aref a k-col k-col))))
    (dotimes (k N b)
      (setf (aref b kb) (/ (aref b kb) (aref a kb kb)))
      (setf temp (- (aref b kb)))
      (dotimes (j kb)
        (setf (aref b j) (+ (* temp (aref a j kb))
                            (aref b j))))
      (decf kb))))

#|
(setf *U* (spofa! #2a((1.0 0.9 0.81) (0.9 1.0 0.9) (0.81 0.9 1.0))))
;==> #2a((1.0 0.9 0.81) (0.9 0.4358898943540673 0.39230090491866054) (0.81 0.9 0.4358898943540673))
(sposl! *U* #(0.9 0.81 0.729))
;==> #(0.9 5.25895116927706E-16 -5.8432790769745105E-16)
|#

;-------------------------------------------------------------------------------
(defun modified-Gram-Schmidt! (A-matrix)
  "given
   [1] A (required)
       <=> a matrix of real or complex-valued numbers with
           size m by n with rank N
computes the factorization A = QR, where
  Q is of size m by n and has orthonormal columns and
  R is of size n by n and is upper triangular, and
returns
   [1] Q
   [2] R
---
Note: see Algorithm 5.2.5, p. 219, of Golub and Van Loan,
Second Edition, with simple changes to handle matrices
with real or complex-valued numbers"
  (let* ((dim-of-A-matrix (array-dimensions A-matrix))
         (m (car dim-of-A-matrix))
         (n (cadr dim-of-A-matrix))
         (Q (make-array dim-of-A-matrix))
         (R (make-array `(,n ,n) :initial-element 0.0))
         j)
    (dotimes (k n (values Q R))
      (setf (aref R k k)
            (let ((sum 0.0))
              (dotimes (i m (sqrt sum))
                (incf sum (realpart
                           (* (conjugate (aref A-matrix i k))
                              (aref A-matrix i k)))))))
      (dotimes (i m)
        (setf (aref Q i k) (/ (aref A-matrix i k)
                              (aref R k k))))
      (setf j k)
      (dotimes (j-shifted (1- (- n k)))
        (incf j)
        (setf (aref R k j)
              (let ((sum 0.0))
                (dotimes (i m sum)
                  (incf sum (* (conjugate (aref Q i k))
                               (aref A-matrix i j))))))
        (dotimes (i m)
          (setf (aref A-matrix i j) (- (aref A-matrix i j)
                                       (* (aref Q i k)
                                          (aref R k j)))))))))

#|
;;; example from pages 219--20 of Golub and Van Loan:
(multiple-value-bind (Q R)
                     (modified-Gram-Schmidt!
                      (make-array '(3 2)
                                  :initial-contents
                                  '((1.0   1.0)
                                    (0.001 0.0)
                                    (0.0   0.001))))
  (print-rows-of-nxm-matrix Q
                            :format-control-for-array-element "~A ")
  (terpri)
  (print-rows-of-nxm-matrix (multiply-two-matrices Q R)
                            :format-control-for-array-element "~A ")
  (terpri)
  (print-rows-of-nxm-matrix (multiply-two-matrices
                             (transpose Q) Q)
                            :format-control-for-array-element "~A ")
  (values))

;==> 0.999999500000375    7.0710625089215E-4 
     9.99999500000375E-4 -0.7071062508568814 
     0.0                  0.7071069579631323 

    1.0   1.0            ;recovers original matrix perfectly!
    0.001 0.0 
    0.0   0.001

    1.0                     3.5268662990084465E-14 ;Q^T Q is close to identity
    3.5268662990084465E-14  0.9999999999999998 

;;; results look quite reasonable and agree fairly well with Golub and Van Loan

;;; complex-valued test case ...
(multiple-value-bind (Q R)
                     (modified-Gram-Schmidt!
                      (make-array '(3 2)
                                  :initial-contents
                                  `(,(list #C( 0.40586 -1.24797)
                                           #C(-0.21314 -1.13773))
                                    ,(list #C( 0.28417 -0.82029)
                                           #C( 0.72524 -0.73988))
                                    ,(list #C(-0.27198 -1.11286)
                                           #C( 1.48759 -1.290507)))))
  (print-rows-of-nxm-matrix Q
                            :format-control-for-array-element "~A ")
  (terpri)
  (print-rows-of-nxm-matrix (multiply-two-matrices Q R)
                            :format-control-for-array-element "~A ")
  (terpri)
  (print-rows-of-nxm-matrix (multiply-two-matrices
                             (Hermitian-transpose Q) Q)
                            :format-control-for-array-element "~A ")
  (values))
;==> #c(0.2085255208117019 -0.6411905440481438) #c(-0.6474034073977328 -0.15810480125045623) 
     #c(0.14600280207229419 -0.421454194714017) #c(0.0744424854739143 -0.10374014288729083) 
     #c(-0.13973974067502756 -0.5717728061166673) #c(0.7211687432221876 -0.1395838200540448) 

     #c(0.40586 -1.24797)  #c(-0.2131400000000001 -1.13773) 
     #c(0.28417 -0.82029)  #c(0.72524 -0.73988) 
     #c(-0.27198 -1.11286) #c(1.48759 -1.290507) 

     #c(1.0 0.0) #c(-1.249000902703301E-16 -2.220446049250313E-16) 
     #c(-1.249000902703301E-16 2.220446049250313E-16)  #c(1.0 0.0)
|#


;-------------------------------------------------------------------------------
(defun Q-R! (A-matrix)
  "same as modified-Gram-Schmidt!"
  (modified-Gram-Schmidt! A-matrix))

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;;  The functions  upper-triangular-solve!
;;;                 lower-triangular-solve!
;;;  solve upper and lower triangular systems of equations.
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
(defun upper-triangular-solve!
       (A
        b
        &key
        (n (length b)))
  "given
   [1] A (required)
       ==> upper n by n triangular matrix
           (note: lower subdiagonal part of this matrix is ignored)
   [2] b (required)
       <=> on input, the right-hand side vector;
           on output, the solution X to A X = b
   [3] n (keyword; length of b)
       ==> order of matrix A
returns
   [4] X, the solution to A X = b, where X is the same as vector
which contained b (note that A is left unchanged)"
  (let ((current-row (1- n))
        (k-total 0))
    (dotimes (i n b)
      (setf (aref b current-row)
            (/ (- (aref b current-row)
                  (let ((sum 0.0)
                        (k (1+ current-row)))
                    ;(declare (dynamic-extent sum))
                    (dotimes (j k-total sum)
                      (incf sum (* (aref b k) (aref A current-row k)))
                      (incf k))))
               (aref A current-row current-row)))
      (decf current-row)
      (incf k-total))))

#|
(upper-triangular-solve!
 #2A((1 2 3) (0 4 5) (0 0 6))
 #(8 9 10))
;==> #(2.666666666666667 0.16666666666666652 1.6666666666666667)

(multiply-matrix-and-vector
 #2A((1 2 3) (0 4 5) (0 0 6))
 #(2.666666666666667 0.16666666666666652 1.6666666666666667))
;==> #(8.0 9.0 10.0)
|#

;-------------------------------------------------------------------------------
(defun lower-triangular-solve!
       (A
        b
        &key
        (n (length b)))
  "given
   [1] A (required)
       ==> lower n by n triangular matrix
           (note: upper superdiagonal part of this matrix is ignored)
   [2] b (required)
       <=> on input, the right-hand side vector;
           on output, the solution X to A X = b
   [3] n (keyword; length of b)
       ==> order of matrix A
returns
   [4] X, the solution to A X = b, where X is the same as vector
which contained b (note that A is left unchanged)"
  (let ((current-row 0)
        (k-total 0))
    (dotimes (i n b)
      (setf (aref b current-row)
            (/ (- (aref b current-row)
                  (let ((sum 0.0))
                    ;(declare (dynamic-extent sum))
                    (dotimes (k k-total sum)
                      (incf sum (* (aref b k) (aref A current-row k))))))
               (aref A current-row current-row)))
      (incf current-row)
      (incf k-total))))

#|
(lower-triangular-solve!
 (transpose #2A((1 2 3) (0 4 5) (0 0 6)))
 #(8 9 10))
;==> #(8.0 -1.75 -0.875)

(multiply-matrix-and-vector
 (transpose #2A((1 2 3) (0 4 5) (0 0 6)))
 #(8.0 -1.75 -0.875))
;==> #(8.0 9.0 10.0)
|#
