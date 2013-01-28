;;;-*- Mode: LISP; Package: :SAPA; Syntax: COMMON-LISP -*-
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;
;  basic-statistics.lisp
;
;  a collection of Lisp functions for certain basic statistical operations ...
;  Note:  before compiling and loading matrix.lisp,
;         you should compile and load (in the order listed)
;            sapa-package.lisp, utilities.lisp, basic-math.lisp and matrix.lisp
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
;;; (compile-file "ccl:SAPA;basic-statistics.lisp")
;;; (load "ccl:SAPA;basic-statistics.fasl")
;-------------------------------------------------------------------------------
(if (not (find-package :SAPA))
  (defpackage :SAPA (:USE :COMMON-LISP)))

(in-package :SAPA)

(export '(;;; functions to compute various summary statistics ...
          sample-mean
          weighted-mean
          sample-variance
          sample-mean-and-variance
          sample-skewness-and-kurtosis
          sample-median
          sample-correlation-coefficient
          histogram
          box-plot
          symmetry-plot

          ;;; quantiles ...
          quantile-of-ordered-seq
          quantile-plot
          interquartile-range
          quantile-of-normal-distribution
          quantile-of-gamma-distribution
          quantile-of-exponential-distribution
          quantile-of-chi-square-2-distribution
          quantile-of-chi-square-distribution
          q-q-plot

          ;;; the normal (Gaussian) distribution ...
          standard-normal-pdf
          tail-area-of-normal-distribution

          ;;; robust statistics ...
          median-absolute-deviation
          Thomson-weight-function
          Huber-weight-function
          m-location-estimate

          ;;; least squares calculations ...
          ordinary-least-squares-Cholesky
          ordinary-least-squares-Q-R
          weighted-linear-least-squares
          Durbin-Watson-test-statistic
          predicted-y-at-x
          var-predicted-y-at-x
          var-predicted-mean-at-x

          ;;; probability density function estimation ...
          kernel-pdf-estimation
          window-width-from-Silverman
          ))

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;;  The functions  sample-mean
;;;                 weighted-mean
;;;                 sample-variance
;;;                 sample-mean-and-variance
;;;                 sample-skewness-and-kurtosis
;;;                 sample-median
;;;                 sample-correlation-coefficient
;;;                 histogram
;;;                 box-plot
;;;                 symmetry-plot
;;;  all take one or more sequences of numbers as input
;;;  and return the sample statistic(s) indicated by the function names.
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
(defun sample-mean
       (the-seq
        &key
        (start 0)
        (end (length the-seq)))
  "given
   [1] the-seq (required)
       ==> a sequence of real or complex-valued numbers
   [2] start (keyword; 0)
       ==> start index of sequence to be used
   [3] end (keyword; length of the-seq)
       ==> 1 + end index of sequence to be used
returns
   [1] sample mean of the specified numbers
       in the-seq"
  (/ (reduce #'+ the-seq :start start :end end) (- end start)))

#|
(sample-mean #(1 2 3 4 5 6 7 8 9 10))                    ;==> 11/2
(sample-mean #(1.0 2 3 4 5 6 7 8 9 10.0))                ;==> 5.5
(sample-mean #(1.0 2 3 #C(4 10) 5 6 7 8 9 10.0))         ;==> #c(5.5 1.0)
(sample-mean #(1 2 3 4 5 6 7 8 9 10) :start 5)           ;==> 8
(sample-mean #(1 2 3 4 5 6 7 8 9 10) :end 5)             ;==> 3
(sample-mean #(1 2 3 4 5 6 7 8 9 10) :start 0 :end 10)   ;==> 11/2
(sample-mean #(1 2 3 4 5 6 7 8 9 10) :start 1 :end 6)    ;==> 4
|#

;-------------------------------------------------------------------------------
(defun weighted-mean
       (the-seq
        &key
        (weights nil)
        (start 0)
        (end (length the-seq)))
  "given
   [1] the-seq (required)
       ==> a sequence of real or complex-valued numbers
   [2] weights (keyword; nil)
       ==> a sequence with (- end start) values
           to be used as weights; if set to nil,
           equal weights for all numbers
           is assumed
   [3] start (keyword; 0)
       ==> start index of sequence to be used
   [4] end (keyword; length of the-seq)
       ==> 1 + end index of sequence to be used
returns
   [1] the weighted average of the specified numbers
       in the-seq"
  (if weights
    (/ (let ((weighted-sum 0)
             (j (1- start)))
         (dotimes (i (- end start) weighted-sum)
           (incf weighted-sum (* (elt weights i)
                                 (elt the-seq (incf j))))))
       (sum weights :end (- end start)))
    (sample-mean the-seq :start start :end end)))

#|
(weighted-mean '(1 2 3 4 5 6 7 8 9 10))                    ;==> 11/2
(weighted-mean '(1 2 3 4 5 6 7 8 9 10) :start 5)           ;==> 8
(weighted-mean '(1 2 3 4 5 6 7 8 9 10) :end 5)             ;==> 3
(weighted-mean '(1 2 3 4 5 6 7 8 9 10) :start 0 :end 10)   ;==> 11/2
(weighted-mean '(1 2 3 4 5 6 7 8 9 10) :start 1 :end 6)    ;==> 4
(weighted-mean '(1 2 3 4 5 6 7 8 9 10)
               :weights '(0 1 0 1 0 1 0 1 0 1))            ;==> 6
(weighted-mean '(1 2 3 4 5 6 7 8 9 10)
               :weights '(0 1 0 1 0 1 0 1 0 1)
               :start 5)                                   ;==> 8
(weighted-mean '(1 2 3 4 5 6 7 8 9 10)
               :weights '(0 1 0 1 0 1 0 1 0 1)
               :end 5)                                     ;==> 3
(weighted-mean '(1 2 3 4 5 6 7 8 9 10)
               :weights '(0 1 0 1 0 1 0 1 0 1)
               :start 0 :end 10)                           ;==> 6
(weighted-mean '(1 2 3 4 5 6 7 8 9 10)
               :weights '(0 1 0 1 0 1 0 1 0 1)
               :start 1 :end 6)                            ;==> 4
|#

;-------------------------------------------------------------------------------
(defun sample-variance
       (the-seq
        &key
        (start 0)
        (end (length the-seq)))
  "given
   [1] the-seq (required)
       ==> a sequence of real or complex-valued numbers
   [2] start (keyword; 0)
       ==> start index of sequence to be used
   [3] end (keyword; length of the-seq)
       ==> 1 + end index of sequence to be used
returns
   [1] sample variance of the specified numbers
       in the-seq"
  (elt (multiple-value-list 
        (sample-mean-and-variance the-seq :start start :end end))
       1))

#|
(sample-variance #(1 2 3 4 5 6 7 8 9 10))                    ;==> 33/4
(sample-variance #(1.0 2 3 4 5 6 7 8 9 10.0))                ;==> 8.25
(sample-variance #(1.0 2 3 #C(4 10) 5 6 7 8 9 10.0))         ;==> 17.25
(sample-variance #(1 2 3 4 5 6 7 8 9 10) :start 5)           ;==> 2
(sample-variance #(1 2 3 4 5 6 7 8 9 10) :end 5)             ;==> 2
(sample-variance #(1 2 3 4 5 6 7 8 9 10) :start 0 :end 10)   ;==> 33/4
(sample-variance #(1 2 3 4 5 6 7 8 9 10) :start 1 :end 6)    ;==> 2
(sample-variance #(1 2 3 4 5 6 7 8 9 10) :start 2 :end 7)    ;==> 2
|#

;-------------------------------------------------------------------------------
(defun sample-mean-and-variance
       (the-seq
        &key
        (start 0)
        (end (length the-seq)))
  "given
   [1] the-seq (required)
       ==> a sequence of real or complex-valued numbers
   [2] start (keyword; 0)
       ==> start index of sequence to be used
   [3] end (keyword; length of the-seq)
       ==> 1 + end index of sequence to be used
returns
   [1] sample mean of the specified numbers
       in the-seq
   [2] sample variance of the specified numbers
       in the-seq"
  (let ((the-mean (sample-mean the-seq :start start :end end)))
    (values the-mean (/ #-allegro
                        (reduce #'+ the-seq
                                :start start
                                :end end
                                :key #'(lambda (x)
                                         (expt (abs (- x the-mean)) 2)))
                        #+allegro
                        (let ((SS 0.0)
                              (j start))
                          (dotimes (i (- end start) SS)
                            (incf SS (expt (abs (- (elt the-seq j) the-mean)) 2))
                            (incf j)))
                        (- end start)))))

#|
(sample-mean-and-variance #(1 2 3 4 5 6 7 8 9 10))
;==> 11/2
;    33/4
(sample-mean-and-variance #(1.0 2 3 4 5 6 7 8 9 10.0))
;==> 5.5
;    8.25
(sample-mean-and-variance #(1.0 2 3 #C(4 10) 5 6 7 8 9 10.0))
;==> #c(5.5 1.0)
;    17.25
(sample-mean-and-variance #(1 2 3 4 5 6 7 8 9 10) :start 5)
;==> 8
;    2
(sample-mean-and-variance #(1 2 3 4 5 6 7 8 9 10) :end 5)
;==> 3
;    2
(sample-mean-and-variance #(1 2 3 4 5 6 7 8 9 10) :start 0 :end 10)
;==> 11/2
;    33/4
(sample-mean-and-variance #(1 2 3 4 5 6 7 8 9 10) :start 1 :end 6)
;==> 4
;    2
(sample-mean-and-variance #(1 2 3 4 5 6 7 8 9 10) :start 2 :end 7)
;==> 5
;    2
|#

;-------------------------------------------------------------------------------
(defun sample-skewness-and-kurtosis
       (the-seq
        &key
        (start 0)
        (end (length the-seq)))
  "given
   [1] the-seq (required)
       ==> a sequence of real-valued numbers
   [2] start (keyword; 0)
       ==> start index of sequence to be used
   [3] end (keyword; length of the-seq)
       ==> 1 + end index of sequence to be used
returns
   [1] sample skewness of the specified numbers
       in the-seq
   [2] sample kurtosis
   [3] sample mean
   [4] sample variance"
  (multiple-value-bind (the-mean sample-variance)
                       (sample-mean-and-variance
                        the-seq :start start :end end)
    (let ((n (- end start))
          (the-sum-of-cubes 0)
          (the-sum-of-quads 0)
          (j (1- start)))
      (dotimes (i n (values (/ (/ the-sum-of-cubes n)
                               (expt sample-variance 3/2))
                            (- (/ (/ the-sum-of-quads n)
                                  (* sample-variance sample-variance)) 3)
                            the-mean sample-variance))
        (let ((centered-value (- (elt the-seq (incf j)) the-mean)))
          (incf the-sum-of-cubes
                (* centered-value centered-value centered-value))
          (incf the-sum-of-quads
                (* centered-value centered-value
                   centered-value centered-value)))))))

#|
(sample-skewness-and-kurtosis #(1 2 3 4 5 6 7 8 9 10))
;==> 0.0
;    -202/165
;    11/2
;    33/4
(sample-skewness-and-kurtosis #(1 2 3 4 5 6 7 8 9 10) :start 1 :end 6)
;==> 0.0
;    -13/10
;    4
;    2
|#

;-------------------------------------------------------------------------------
(defun sample-median
       (the-seq
        &key
        (start 0)
        (end (length the-seq))
        (the-seq-is-ordered-p nil))
  "given
   [1] the-seq (required)
       ==> a sequence of real-valued numbers
   [2] start (keyword; 0)
       ==> start index of sequence to be used
   [3] end (keyword; length of the-seq)
       ==> 1 + end index of sequence to be used
   [4] the-seq-is-ordered-p (keyword; nil)
       ==> if t, the-seq is assumed to
           be sorted from smallest to largest
           value; if nil, the-seq will be sorted
returns
   [1] sample median
   [2] minimum value in sequence
   [3] maximum value
   [4] sorted sequence"
  ;;; Note: subseq generates a new sequence ...
  (let ((seq-to-be-sorted (subseq the-seq start end))
        (n-effective (- end start)))
    (if (not the-seq-is-ordered-p)
      (setf seq-to-be-sorted (sort seq-to-be-sorted #'<)))
    (values
     (if (oddp n-effective)
       (elt seq-to-be-sorted (/ (1- n-effective) 2))
       (/ (+ (elt seq-to-be-sorted (/ n-effective 2))
             (elt seq-to-be-sorted (/ (- n-effective 2) 2)))
          2))
     (elt seq-to-be-sorted 0)
     (elt seq-to-be-sorted (1- n-effective))
     seq-to-be-sorted)))

#|
(sample-median '(1 10 2 9 5))
;==> 5
;    1
;    10
;    (1 2 5 9 10)
(sample-median '(1 10 2 9 5 0 0 0 0 0) :end 5)
;==> 5
;    1
;    10
;    (1 2 5 9 10)
(sample-median #(1.0 2.0 3.0))
;==> 2.0
;    1.0
;    3.0
;    #(1.0 2.0 3.0)
(sample-median #(1.0 2.0 3.0 4.0))
;==> 2.5
;    1.0
;    4.0
;    #(1.0 2.0 3.0 4.0)
(sample-median #(1.0 2.0 3.0 4.0) :end 3)
;==> 2.0
;    1.0
;    3.0
;    #(1.0 2.0 3.0)
(sample-median (vector 1.0 2.0 3.0 4.0) :start 1 :end 3)
;==> 2.5
;    2.0
;    3.0
;    #(2.0 3.0)
|#

;-------------------------------------------------------------------------------
(defun sample-correlation-coefficient
       (seq-1
        seq-2
        &key
        (start 0)
        (end (length seq-1)))
  "given
   [1] seq-1 (required)
       ==> a sequence of real-valued numbers
   [2] seq-2 (required)
       ==> another sequence of real-valued numbers;
           should have the same length as seq-1
   [3] start (keyword; 0)
       ==> start index of sequence to be used
   [4] end (keyword; length of the-seq)
       ==> 1 + end index of sequence to be used
returns
   [1] sample correlation coefficient
       between two sequences"
  (multiple-value-bind (mean-1 variance-1)
                       (sample-mean-and-variance
                        seq-1 :start start :end end)
    (multiple-value-bind (mean-2 variance-2)
                         (sample-mean-and-variance
                          seq-2 :start start :end end)
      (let ((sum 0.0)
            (n (- end start))
            (j (1- start)))
        (dotimes (i n (/ (/ sum n) (sqrt (* variance-1 variance-2))))
          (incf sum (* (- (elt seq-1 (incf j)) mean-1)
                       (- (elt seq-2 j) mean-2))))))))

#|
(sample-correlation-coefficient
 #(0.0 -2.0 -1.0 0.0  1.0  2.0 10.0)
 #(7.0  2.0  1.0 0.0 -1.0 -2.0 10.0))
;==> 0.6190943270844442
(sample-correlation-coefficient
 #(0.0 -2.0 -1.0 0.0  1.0  2.0 10.0)
 #(7.0  2.0  1.0 0.0 -1.0 -2.0 10.0)
 :start 1 :end 6)
;==> -1.0
|#

;-------------------------------------------------------------------------------
;;; The function histogram computes all the goodies needed to construct
;;; a histogram for a sequence of deviates.
(defun histogram
       (sequence-of-deviates
        &key
        (number-of-bins 10)
        (left-of-first-bin (min-of-seq sequence-of-deviates))
        (right-of-last-bin (max-of-seq sequence-of-deviates))
        (scale-as-density-p t)
        (result (make-array number-of-bins)))
  "given
   [1] sequence-of-deviates (required)
       ==> a sequence of real-valued numbers
   [2] number-of-bins (keyword; 10)
       ==> number of bins in histogram
   [3] left-of-first-bin (keyword; min of sequence-of-deviates)
       ==> leftmost value of first bin
   [4] right-of-last-bin (keyword; max of sequence-of-deviates)
       ==> rightmost value of last bin
   [5] scale-as-density-p (keyword; t)
       ==> if t, histogram is scaled as a density;
           otherwise, cell counts are returned
   [6] result (keyword; vector of size number-of-bins)
       <== vector to hold the histogram
returns
   [1] the vector result, which contains the values for the histogram
       for sequence-of-deviates (one value for each bin); note that,
       if a bin has boundaries a and b, all points x in sequence-of-deviates
       for which a<=x<b are counted as being in that bin
       EXCEPT for the last bin, where the rule is a<=x<=b.
   [2] maximum value in the histogram (i.e., result)
   [3] leftmost value of first bin
   [4] bin-width of bins
   [5] the number of unbinned elements in sequence-of-deviates
       less than left-of-first-bin
   [6] the number of unbinned elements in sequence-of-deviates
       greater than right-of-last-bin
---
Note: the value in (svref result 0) is the histogram value
      for the interval [leftmost value of first bin,
                        leftmost value of first bin + bin-width of bins);
      the value in (svref result 1) is the histogram value
      for the interval [leftmost value of first bin + bin-width of bins,
                        leftmost value of first bin + 2*bin-width of bins);
      etc."
  (assert (plusp (length sequence-of-deviates)))
  (assert (plusp number-of-bins))
  (assert (< left-of-first-bin right-of-last-bin))
  (let* ((n (length sequence-of-deviates))
         (low-elements-not-binned 0)
         (high-elements-not-binned 0)
         (spacing (/ (- right-of-last-bin left-of-first-bin) number-of-bins))
         (density-factor (if scale-as-density-p
                           (float (* n spacing))
                           1))
         (the-maximum 0.0)
         j)
    (fill result 0 :end number-of-bins)
    (dotimes (i n)
      (setf j (floor (- (elt sequence-of-deviates i) left-of-first-bin)
                     spacing))
      (cond
       ((< -1 j number-of-bins)
        (incf (elt result j)))
       ((= (elt sequence-of-deviates i) right-of-last-bin)
        (incf (elt result (1- number-of-bins))))
       (t
        (if (minusp j)
          (incf low-elements-not-binned)
          (incf high-elements-not-binned)))))
    (dotimes (i number-of-bins (values result
                                       the-maximum
                                       left-of-first-bin
                                       spacing
                                       low-elements-not-binned
                                       high-elements-not-binned))
      (if (> (setf (elt result i) (/ (elt result i) density-factor))
             the-maximum)
        (setf the-maximum (elt result i))))))

#|
(histogram '(9 1 9 9 1 8 1 9 6 7 9 3 9)
           :number-of-bins 10
           :left-of-first-bin 0.5
           :right-of-last-bin 10.5
           :scale-as-density-p nil)
;==> #(3 0 1 0 0 1 1 1 6 0)     3 values in [0.5,1.5); 6 in [8.5,9.5); etc.
;    6                          maximum value in histogram vector
;    0.5                        left-hand side of first histogram bin 
;    1.0                        width of each histogram bin 
;    0                          number of values < 0.5
;    0                          number of values > 10.5

;;; Now we repeat the above, but now scale the histogram as a density:
(histogram '(9 1 9 9 1 8 1 9 6 7 9 3 9)
           :number-of-bins 10
           :left-of-first-bin 0.5
           :right-of-last-bin 10.5)
;==> #(0.23076923076923078 0.0 0.07692307692307693 0.0 0.0 0.07692307692307693 0.07692307692307693 0.07692307692307693 0.46153846153846156 0.0)
;    0.46153846153846156
;    0.5
;    1.0
;    0
;    0
;;; Since the bin width is 1.0, the elements of the vector above
;;; sum to unity; had the bin width been 0.5, the elements would have
;;; summed to 2.0:
(sum (histogram '(9 1 9 9 1 8 1 9 6 7 9 3 9)
                :number-of-bins 20
                :left-of-first-bin 0.5
                :right-of-last-bin 10.5))
;==> 2.0
|#

;-------------------------------------------------------------------------------
;;; The function box-plot computes all the goodies needed to construct
;;; a box plot for a sequence of deviates.
(defun box-plot
       (a-seq
        &key
        (seq-sorted-p nil)
        (get-bottom-line
         #'(lambda (v)
             (let* ((lower-quartile (quantile-of-ordered-seq v 0.25))
                    (iqr (- (quantile-of-ordered-seq v 0.75)
                            lower-quartile))
                    (lower-limit (- lower-quartile (* 1.5 iqr)))
                    (temp (binary-search lower-limit v))
                    (lower-index (if temp (1+ temp) 0))
                    (lower-adjacent-value (elt v lower-index)))
               (values lower-adjacent-value lower-index))))
        (get-bottom-of-box
         #'(lambda (v)
             (quantile-of-ordered-seq v 0.25)))
        (get-line-through-box
         #'(lambda (v)
             (quantile-of-ordered-seq v 0.50)))
        (get-top-of-box
         #'(lambda (v)
             (quantile-of-ordered-seq v 0.75)))
        (get-top-line
         #'(lambda (v)
             (let* ((upper-quartile (quantile-of-ordered-seq v 0.75))
                    (iqr (- upper-quartile
                            (quantile-of-ordered-seq v 0.25)))
                    (upper-limit (+ upper-quartile (* 1.5 iqr)))
                    (temp (binary-search upper-limit v))
                    (upper-index (if temp temp (1- (length v))))
                    (upper-adjacent-value (elt v upper-index)))
               (values upper-adjacent-value upper-index)))))
  "given
   [1] a-seq (required)
       ==> a sequence of real-valued numbers
   [2] seq-sorted-p (keyword; nil)
       ==> if t, a-seq is already sorted;
           if nil, a copy of a-seq is sorted
           for use by the function
   [3] get-bottom-line (keyword; computes lower adjacent value)
       ==> a function for calculating
           the line below the box (from sorted data)
   [4] get-bottom-of-box (keyword; computes lower quartile)
       ==> a function for calculating
           bottom of the box
   [5] get-line-through-box (keyword; computes median)
       ==> a function for calculating
           the line through the middle of the box
   [6] get-top-of-box (keyword; computes upper quartile)
       ==> a function for calculating
           top of the box
   [7] get-top-line (keyword; computes upper adjacent value)
       ==> a function for calculating
           the line above the box
returns
   [1] bottom line below box
   [2] bottom line of box
   [3] line through middle of box
   [4] top line of box
   [5] top line above box
   [6] a vector containing outside values
       (note: this vector can be of length 0)
---
Note: the values that this function returns can
      be used to contruct a box plot as defined in
      Section 2.5 of ``Graphical Methods for Data Analysis''
      by Chambers, Cleveland, Kleiner, Tukey"
  (if (not seq-sorted-p)
    (setf a-seq (sort (copy-seq a-seq) #'<)))
  (multiple-value-bind (top-value top-index)
                       (funcall get-top-line a-seq)
    (multiple-value-bind (bottom-value bottom-index)
                         (funcall get-bottom-line a-seq)
      (let* ((n (length a-seq))
             (n-upper (- n top-index 1))
             (n-outside (+ bottom-index n-upper))
             (lower-outside-values '())
             (upper-outside-values '())
             (outside-values (cond
                              ((plusp n-outside)
                               (dotimes (i bottom-index)
                                 (push (elt a-seq i)
                                       lower-outside-values))
                               (dotimes (i (- n top-index 1))
                                 (push (elt a-seq (- n i 1))
                                       upper-outside-values))
                               (make-array n-outside
                                           :initial-contents
                                           (append
                                            (reverse lower-outside-values)
                                            upper-outside-values)))
                              (t
                               (make-array 0)))))
        (values
         bottom-value                           ;;; bottom line below box
         (funcall get-bottom-of-box a-seq)      ;;; bottom line of box
         (funcall get-line-through-box a-seq)   ;;; line through middle of box
         (funcall get-top-of-box a-seq)         ;;; top line of box
         top-value                              ;;; top line above box
         outside-values                         ;;; outside-values
         )))))

#|
(box-plot '(9 5 4 5 6 5 4 5 6 -100 5))
;     4                    bottom line below box
;     4.25                 bottom line of box
;     5.0                  line through middle of box
;     5.75                 top line of box
;     6                    top line above box
;     #(-100 9)            outside values
|#

;-------------------------------------------------------------------------------
;;; The function symmetry-plot computes all the goodies needed to construct
;;; a symmetry plot for a sequence of deviates.
(defun symmetry-plot
       (a-seq
        &key
        (a-seq-sorted-p nil))
  "given
   [1] a-seq (required)
       ==> any sequence of real-valued numbers
   [2] a-seq-sorted-p (keyword; nil)
       ==> if t, a-seq is already sorted;
           if nil, a copy of a-seq is sorted
           for use by the function
returns
   [1] y values for a symmetry plot for a-seq
   [2] x values for a symmetry plot for a-seq"
  (if (not a-seq-sorted-p)
    (setf a-seq (sort (copy-seq a-seq) #'<)))
  (let* ((n (length a-seq))
         (n-1 (1- n))
         (m (truncate n 2))
         (m-1 (1- m))
         (x-values (make-array m))
         (y-values (make-array m))
         (the-median (quantile-of-ordered-seq a-seq 0.5)))
    (dotimes (i m (values y-values x-values))
      (setf (elt x-values (- m-1 i)) (- the-median (elt a-seq i))
            (elt y-values (- m-1 i)) (- (elt a-seq (- n-1 i)) the-median)))))

#|
(symmetry-plot '(9 5 4 5 6 5 4 5 6 -100 5))
;==>  #(0.0 0.0 1.0 1.0 4.0)
;     #(0.0 0.0 1.0 1.0 105.0)
|#

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;;  The functions  quantile-of-ordered-seq
;;;                 quantile-plot
;;;                 interquartile-range
;;;                 quantile-of-normal-distribution
;;;                 quantile-of-gamma-distribution
;;;                 quantile-of-exponential-distribution
;;;                 quantile-of-chi-square-2-distribution
;;;                 quantile-of-chi-square-distribution
;;;                 q-q-plot
;;;  are all concerned with computing either sample or theoretical quantiles
;;;  for, respectively, either a sequence of numbers or a theoretical
;;;  distribution.
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
(defun quantile-of-ordered-seq
       (the-seq
        p)
  "given
   [1] the-seq (required)
       ==> a sequence of real-valued numbers,
           ordered from smallest to largest
   [2] p (required)
       ==> a percentile; i.e., 0 <= p <= 1
returns
   [1] the sample quantile for p
---
Note: see Section 2.2 of ``Graphical Methods for Data Analysis''
      by Chambers, Cleveland, Kleiner, and Tukey"
  (assert (and (plusp (length the-seq)) (<= 0.0 p 1.0)))
  (let* ((n (length the-seq))
         (p-sub-0 (/ 0.5 n))
         (p-sub-n-1 (- 1.0 (/ 0.5 n))))
    (cond
     ((<= p p-sub-0)
      (elt the-seq 0))
     ((>= p p-sub-n-1)
      (elt the-seq (1- n)))
     (t
      (let* ((i (max 0 (truncate (- (* n p) 0.5))))
             (p-sub-i (/ (+ i 0.5) n))
             (i+1 (min (1+ i) (1- n)))
             (p-sub-i+1 (/ (+ i+1 0.5) n))
             (delta-p (- p-sub-i+1 p-sub-i))
             (f (if (zerop delta-p)
                  0.0
                  (/ (- p p-sub-i) delta-p))))
        (+ (* (- 1.0 f) (elt the-seq i))
           (* f (elt the-seq i+1))))))))

#|
(quantile-of-ordered-seq
 #(1.0 1.1 2.0 2.1 3.0 3.1 4.0 4.1 5.0 5.1)
 0.1)
;==> 1.05
(quantile-of-ordered-seq
 #(1.0 1.1 2.0 2.1 3.0 3.1 4.0 4.1 5.0 5.1)
 0.9)
;==> 5.05
|#

;-------------------------------------------------------------------------------
;;; The function quantile-plot computes all the goodies needed to construct
;;; a quantile plot for a sequence of deviates.
(defun quantile-plot
       (a-seq
        &key
        (a-seq-sorted-p nil))
  "given
   [1] a-seq (required)
       ==> any sequence of real-valued numbers
   [2] a-seq-sorted-p (keyword; nil)
       ==> if t, a-seq is already sorted;
           if nil, a copy of a-seq is sorted
           for use by the function
returns
   [1] sequence with sample quantiles for a-seq
   [2] sequence with corresponding percentiles
---
Note: see Section 2.2 of ``Graphical Methods for Data Analysis''
      by Chambers, Cleveland, Kleiner, and Tukey"
  (if (not a-seq-sorted-p)
    (setf a-seq (sort (copy-seq a-seq) #'<)))
  (let* ((n (length a-seq))
         (x-values (copy-seq a-seq)))
    (dotimes (i n (values a-seq x-values))
      (setf (elt x-values i) (/ (+ i 0.5) n)))))

#|
(quantile-plot '(9 5 4 5 6 5 4 6 -100 5))
;==> (-100 4 4 5 5 5 5 6 6 9)
;    (0.05 0.15 0.25 0.35 0.45 0.55 0.65 0.75 0.85 0.95)
|#

;-------------------------------------------------------------------------------
(defun interquartile-range
       (the-seq
        &key
        (start 0)
        (end (length the-seq))
        (the-seq-is-ordered-p nil))
  "given
   [1] the-seq (required)
       ==> a sequence of real-valued numbers
   [2] start (keyword; 0)
       ==> start index of sequence to be used
   [3] end (keyword; length of the-seq)
       ==> 1 + end index of sequence to be used
   [4] the-seq-is-ordered-p (keyword; nil)
       ==> if t, the-seq is assumed to
           be sorted from smallest to largest
           value; if nil, the-seq will be sorted
returns
   [1] interquartile range; i.e,
       0.75 quantile minus 0.25 quantile"
  (let ((seq-to-be-sorted (subseq the-seq start end)))
    (if (not the-seq-is-ordered-p)
      (setf seq-to-be-sorted (sort seq-to-be-sorted #'<)))
    (values (- (quantile-of-ordered-seq seq-to-be-sorted 0.75)
               (quantile-of-ordered-seq seq-to-be-sorted 0.25)))))

#|
(quantile-of-ordered-seq
 #(1.0 1.1 2.0 2.1 3.0 3.1 4.0 4.1 5.0 5.1)
 0.75)
;==> 4.1
(quantile-of-ordered-seq
 #(1.0 1.1 2.0 2.1 3.0 3.1 4.0 4.1 5.0 5.1)
 0.25)
;==> 2.0
(interquartile-range #(1.0 1.1 2.0 2.1 3.0 3.1 4.0 4.1 5.0 5.1))
;==> 2.0999999999999996
|#

;-------------------------------------------------------------------------------
(defun quantile-of-normal-distribution
       (p
        &key
        (accuracy-level :quick))
  "given
   [1] p (required)
       ==> a number > 0 and < 1
   [2] accuracy-level (keyword; :quick)
       ==> if :quick, gives quick approximation;
           if :better, gives slower, but better, approximation;
           if :accurate, uses slowest, but more accurate,
           approximation;
returns
   [1] quantile of standard normal distribution;
---
Note: for :quick, see Table 6.5, page 227,
      ``Graphical Methods for Data Analysis''
      by Chambers, Cleveland, Kleiner, & Tukey;
      for :better, see Sections 26.2.22 and 26.2.23
      of Abramowitz and Stegun;
      for :accurate, see AS 111 by Beasley and Springer,
      Applied Statistics, 1977, vol. 26, p.118"
  (case accuracy-level
    (:quick
     (let ((c (sqrt (* -2.0 (log (min p (- 1.0 p)))))))
       (* (signum (- p 0.5))
          (- c (/ (+ 2.30753 (* 0.27061 c))
                  (+ 1.0 (* 0.99229 c) (* 0.04481 c c)))))))
    (:better
     (let ((c (sqrt (* -2.0 (log (min p (- 1.0 p)))))))
       (* (signum (- p 0.5))
          (- c (/ (+ 2.515517 (* 0.802853 c) (* 0.010328 c c))
                  (+ 1.0 (* 1.432788 c) (* 0.189269 c c) (* 0.001308 c c c)))))))
    (:accurate
     (as-111 p))
    (otherwise
     (error "accuracy-level set to ~A; should be either quick, better or accurate"
            accuracy-level))))

#|
(quantile-of-normal-distribution 0.975) 
;==> 1.960448273742351
(quantile-of-normal-distribution 0.975 :accuracy-level :better)
;==> 1.9603949169253396
(quantile-of-normal-distribution 0.975 :accuracy-level :accurate)
;==> 1.9599639822884356
|#

;-------------------------------------------------------------------------------
(defun quantile-of-gamma-distribution
       (alpha
        p)
  "given
   [1] alpha (required)
       ==> shape parameter for gamma distribution
   [2] p (required)
       ==> a number > 0 and < 1
returns
   [1] p-th quantile of standard gamma distribution
---
Note: Table 6.4, page 225, ``Graphical Methods for Data Analysis''
      by Chambers, Cleveland, Kleiner, & Tukey"
  (* alpha (expt (- 1.0
                    (/ (* 9.0 alpha))
                    (/ (quantile-of-normal-distribution p)
                       ; Note: don't panic! --- -3.0 negates subtraction
                       (* -3.0 (sqrt alpha))))
                 3)))

#|
(quantile-of-gamma-distribution 1.0 0.975) 
;==> 3.6691637921673883
|#

;-------------------------------------------------------------------------------
(defun quantile-of-exponential-distribution
       (p)
  "given
   [1] p (required)
       ==> a number > 0 and < 1
returns
   [1] p-th quantile of exponential distribution with mean 1
---
Note: Table 6.4, page 225, ``Graphical Methods for Data Analysis''
      by Chambers, Cleveland, Kleiner, & Tukey"
  (- (log (- 1.0 p))))

#|
(quantile-of-exponential-distribution 0.975)
;==> 3.6888794541139354
|#

;-------------------------------------------------------------------------------
(defun quantile-of-chi-square-2-distribution
       (p)
  "given
   [1] p (required)
       ==> a number > 0 and < 1
returns
   [1] p-th quantile of chi-square distribution with 2 degrees of freedom
---
Note: Equation (31), P. 640 of Chave, Thomson, and Ander (1987)"
  (* -2.0 (log (- 1.0 p))))

#|
(quantile-of-chi-square-2-distribution 0.975)
;==> 7.377758908227871
|#

;-------------------------------------------------------------------------------
(defun quantile-of-chi-square-distribution
       (nu
        p
        &key
        (accuracy-level :quick))
  "given
   [1] nu (required)
       ==> degrees of freedom
   [2] p (required)
       ==> a number > 0 and < 1
   [3] accuracy-level (keyword; :quick)
       if :quick, gives quick approximation;
       if :accurate, give accurate, but computationally
       expensive, approximation
returns
   [1] p-th quantile of chi-square distribution with nu degrees of freedom
---
Note: for :quick, see Table 6.4, page 225,
      ``Graphical Methods for Data Analysis''
      by Chambers, Cleveland, Kleiner, & Tukey;
      if :accurate, see AS 91, Applied Statistics"
  (case accuracy-level
    (:quick
     (* 2.0 (quantile-of-gamma-distribution (/ nu 2.0) p)))
    (:accurate 
     (as-91 p nu))
    (otherwise
     (error "accuracy-level set to ~A; should be either :quick or :accurate"
            accuracy-level))))

#|
(quantile-of-chi-square-distribution 2 0.975)
;==> 7.338327584334777
(quantile-of-chi-square-distribution 2 0.975 :accuracy-level :accurate)
;==> 7.377758908227873
(quantile-of-chi-square-distribution 4 0.975)
;==> 11.130220797449477
(quantile-of-chi-square-distribution 4 0.975 :accuracy-level :accurate)
;==> 11.143286781877793
(quantile-of-chi-square-distribution 8 0.975)
;==> 17.533997167548357
(quantile-of-chi-square-distribution 8 0.975 :accuracy-level :accurate)
;==> 17.534546139484654
(quantile-of-chi-square-distribution 8.3 0.975)
;==> 17.984123910122445
(quantile-of-chi-square-distribution 8.3 0.975 :accuracy-level :accurate)
;==> 17.98423969597426
(quantile-of-chi-square-distribution 9 0.975)
;==> 19.023543945139263
(quantile-of-chi-square-distribution 9 0.975 :accuracy-level :accurate)
;==> 19.02276780221112
|#

;-------------------------------------------------------------------------------
;;; The function q-q-plot is organized so to give the x and y values
;;; needed to create a q-q plot for any of the the following combinations:
;;;   empirical (y) versus theoretical (x)
;;;   empirical     versus empirical
;;;   theoretical   versus theoretical
;;;
;;; For a theoretical component, you need to supply a function
;;; which, given a value from 0 to 1, calculates the corresponding quantile;
;;; for an empirical, you need to supply either a sequence of observations
;;; or a data object from which a vector is to be extracted (it is assumed
;;; to be the vector pointed to by y-values).
;;;
;;; OPERATIONAL NOTE:  if you are doing many different Q-Q plots of data sets
;;;                    with the same number of points versus the same
;;;                    theoretical distribution, it might be faster to
;;;                    calculate the quantiles just once pass them in as
;;;                    a sequence with [xy]-sorted-p set to t.
(defun q-q-plot
       (y-vals
        x-vals
        &key
        (n (if (typep y-vals 'sequence)
             (length y-vals)
             256))
        (x-vals-sorted-p (functionp x-vals))
        (y-vals-sorted-p (functionp y-vals)))
  "given
   [1] y-vals (required)
       ==> a sequence or function
   [2] x-vals (required)
       ==> a sequence or function
   [3] n (keyword; 256 if y-vals is a function, length of y-vals otherwise)
       ==> positive integer
   [4] x-vals-sorted-p (keyword; t if x-vals is a function, nil otherwise)
       ==> a flag indicating whether x-vals is already sorted
   [5] y-vals-sorted-p (keyword; t if y-vals is a function, nil otherwise)
       ==> a flag indicating whether y-vals is already sorted
return
   [1] y values needed to create a q-q-plot for y-vals
   [2] x values
---
Note: For details, see Chapter 6 of ``Graphical Methods for Data Analysis''
      by Chambers, Cleveland, Kleiner and Tukey"
  (let* ((y-copy-already-made-p nil)
         (x-copy-already-made-p nil)
         (y-values (cond
                    ((functionp y-vals)
                     (setf y-copy-already-made-p
                           (sample-from-a-function
                            y-vals
                            :x0 (/ 0.5 n)
                            :delta-x (/ (float n))
                            :n n)))
                    ((arrayp y-vals)
                     (if y-vals-sorted-p y-vals
                         (setf y-copy-already-made-p
                               (copy-seq y-vals))))
                    (t
                     (setf y-copy-already-made-p
                           (make-array n
                                       :initial-contents
                                       y-vals)))))
         (x-values (cond
                    ((functionp x-vals)
                     (setf x-copy-already-made-p
                           (sample-from-a-function
                            x-vals
                            :x0 (/ 0.5 n)
                            :delta-x (/ (float n))
                            :n n)))
                    ((arrayp x-vals)
                     (if x-vals-sorted-p x-vals
                         (setf x-copy-already-made-p
                               (copy-seq x-vals))))
                    (t
                     (setf x-copy-already-made-p
                           (make-array n
                                       :initial-contents
                                       x-vals))))))
    (cond
     ((not x-vals-sorted-p)
      (if (not x-copy-already-made-p)
        (setf x-copy-already-made-p (setf x-values (copy-seq x-values))))
      (setf x-values (sort x-values #'<=))))
    (cond
     ((not y-vals-sorted-p)
      (if (not y-copy-already-made-p)
        (setf y-copy-already-made-p (setf y-values (copy-seq y-values))))
      (setf y-values (sort y-values #'<=))))
    (values y-values x-values)))

#|
(q-q-plot
 '(9 5 4 5 6 5 4 5 6 -100 5)
 '(8 4 3 4 5 4 3 4 5  -99 4))
;==> #(-100 4 4 5 5 5 5 5 6 6 9)
;     #(-99 3 3 4 4 4 4 4 5 5 8)

(q-q-plot
 #(9 5 4 5 6 5 4 5 6 -100 5)
 #(8 4 3 4 5 4 3 4 5  -99 4))
;==> #(-100 4 4 5 5 5 5 5 6 6 9)
;     #(-99 3 3 4 4 4 4 4 5 5 8)

(q-q-plot
 #(9 5 4 5 6 5 4 5 6 -100 5)
  #'quantile-of-normal-distribution)
;==> #(-100 4 4 5 5 5 5 5 6 6 9)
;    #(-1.6903897928628724 -1.0948565688424345 -0.7451748604784388 -0.470072369736396 -0.22795082525859311 0.0 0.22795082525859311 0.470072369736396 0.7451748604784388 1.0948565688424352 1.6903897928628742)

(q-q-plot
 #'(lambda (p) (quantile-of-chi-square-distribution 100 p))
 #'quantile-of-normal-distribution
 :n 4)
;==> #(84.02560895964486 94.94772287048265 103.85500296347429 116.39899541971275)
;    #(-1.1485487296374308 -0.3163006861455602 0.3163006861455602 1.1485487296374308
|#


;-------------------------------------------------------------------------------
;;; This is needed for standard-normal-pdf below.
(defconstant +sqrt-of-two-pi+ (sqrt (* 2.0 pi)))

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;;  The functions  standard-normal-pdf
;;;                 tail-area-of-normal-distribution
;;;  compute the pdf and the tail area for the standard normal (Gaussian)
;;;  distribution.
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
(defun standard-normal-pdf
       (x)
  "given x, returns value of standard normal pdf at x"
  (/ (exp (/ (* x x) -2.0)) +sqrt-of-two-pi+))

#|
(standard-normal-pdf  0.0)  ;==> 0.3989422804014327
(standard-normal-pdf  1.0)  ;==> 0.24197072451914337
(standard-normal-pdf -1.0)  ;==> 0.24197072451914337
|#

;-------------------------------------------------------------------------------
(defun tail-area-of-normal-distribution
       (q
        &key
        (q-to-infinity-p nil))
  "given
   [1] q (required)
       ==> a quantile
   [2] q-to-infinity-p (keyword; nil)
       ==> if t,   return integral from q to +infinity;
           if nil, return integral from -infinity to q
return
   tail area of the standard norm from
   either q to +infinity (if q-to-infinity-p is true)
   or     -infinity to q (if q-to-infinity-p is nil)
---
see  Algorithm AS-66, Applied Statistics,
1973, vol. 22, no. 3"
  (if (minusp q)
    (setf q-to-infinity-p (not q-to-infinity-p) 
          q (- q)))
  (cond
   ((or (<= q 7.0d0)
        (and q-to-infinity-p (<= q 18.66d0)))
    (let* ((y (* 0.5 q q))
           (alnorm (if (> q 1.28d0)
                     (* 0.398942280385d0
                        (/ (exp (- y))
                           (+ q -3.8052d-8
                              (/ 1.00000615302d0
                                 (+ q 3.98064794d-4
                                    (/ 1.98615381364d0
                                       (+ q -0.151679116635d0
                                          (/ 5.29330324926d0
                                             (+ q 4.8385912808d0
                                                (/ -15.1508972451d0
                                                   (+ q 0.742380924027d0
                                                      (/ 30.789933034d0 (+ q 3.99019417011d0)))))))))))))
                     (- 0.5
                        (* q
                           (- 0.398942280444d0
                              (* 0.39990348504d0
                                 (/ y
                                    (+ y 5.75885480458d0
                                       (/ -29.8213557807d0
                                          (+ y 2.62433121679d0 (/ 48.6959930692d0 (+ y 5.92885724438d0)))))))))))))
      (if q-to-infinity-p alnorm (- 1.0 alnorm))))
   ;;; too far out in one direction, so return 0.0 or 1.0 ...
   (t
    (if q-to-infinity-p 0.0 1.0))))

#|
;;; Here we compare tail-area-of-normal-distribution to values tabulated
;;; in Abramowitz and Stegun, Table 26.1, pages 966--72 -- the space
;;; in each number below indicates the point at which the computed value
;;; differs from the tabulated value.
(tail-area-of-normal-distribution 0.02)     ;==> 0.50797831371 76848
(tail-area-of-normal-distribution 1.14)     ;==> 0.8728568 399049156
(tail-area-of-normal-distribution 1.86)     ;==> 0.96855723701 83276
(tail-area-of-normal-distribution 2.54)     ;==> 0.99445737655 7139
(tail-area-of-normal-distribution 3.45)     ;==> 0.9997197067 231738
(tail-area-of-normal-distribution 4.85)     ;==> 0.9999993826 92628

;;; a few additional tests ...
(tail-area-of-normal-distribution
 (quantile-of-normal-distribution 0.025))
;==> 0.02497170911465468
(tail-area-of-normal-distribution
 (quantile-of-normal-distribution 0.975))
;==> 0.9750282908853453
(tail-area-of-normal-distribution
 (quantile-of-normal-distribution 0.975 :accuracy-level :better))
;==> 0.9750251752384552
(tail-area-of-normal-distribution
 (quantile-of-normal-distribution 0.975 :accuracy-level :accurate))
;==> 0.974999999867449
|#

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;;  The functions  median-absolute-deviation
;;;                 Thomson-weight-function
;;;                 Huber-weight-function
;;;                 m-location-estimate
;;;  are all concerned with the calcuation of robust statistics.
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
(defun median-absolute-deviation
       (the-seq
        &key
        (location-estimate (sample-median the-seq)))
  "given
   [1] the-seq (required)
       ==> a sequence of real-valued numbers
   [2] location-estimate (keyword; median of the-seq)
       ==> robust estimate of location
returns
   [1] the median absolute deviation about location-estimate
   [2] location-estimate (median of the-seq by default)"
  (let* ((n (length the-seq))
         (scratch (make-array n)))
    ;(declare (dynamic-extent scratch))
    (dotimes (i n (values (sample-median scratch)
                          location-estimate))
      (setf (elt scratch i)
            (abs (- (elt the-seq i) location-estimate))))))

#|
(median-absolute-deviation '(6.0 2.0 1.0 4.0 5.0 3.0))
;==> 1.5
;    3.5

(median-absolute-deviation ' (2.0 3.0 4.0 5.0 6.0))
;==> 1.0
;    4.0
|#

;-------------------------------------------------------------------------------
(defun Thomson-weight-function
       (x
        &key
        (beta 1.0)
        (symmetric-version t))
  "given
   [1] x (required)
       ==> value at which to evaluate Thomson's
           weight function
   [2] beta (keyword; 1.0)
       ==> tuning parameter
   [3] symmetric-version (keyword; t)
       ==> one-sided or two-sided down weightings
returns
   [1] value of Thomson's weight function at x
---
Note: Equation (27), p. 637 of Chave, Thomson, and Ander (1987)"
  (let* ((inner-argument (* beta (- (if symmetric-version (abs x) x)
                                    beta)))
         (inner-exp (if (< (abs inner-argument) 50.0)
                      (exp inner-argument)
                      50.0)))
    (if (< inner-exp 50.0)
      (exp (- inner-exp))
      0.0)))

#|
(Thomson-weight-function 0.0)
;==> 0.6922006275553464
(Thomson-weight-function 0.0 :symmetric-version nil)
;==> 0.6922006275553464
|#

;-------------------------------------------------------------------------------
(defun Huber-weight-function
       (x
        &key
        (parameter 1.5))
  "given
   [1] x (required)
       ==> value at which to evaluate Huber's weight function
   [2] parameter (keyword; 1.5)
       ==> tuning parameter
returns
   [1] value of Huber's weight function at x"
  (if (> (abs x) parameter)
      (/ parameter (abs x))
      1.0))

#|
(Huber-weight-function -100.0)
;==> 0.015
|#
;;;

;-------------------------------------------------------------------------------
(defun m-location-estimate
       (the-seq
        &key
        (weighting-functions
         `(,#'(lambda (x)
                (Huber-weight-function
                 x :parameter 2.0))
           ,#'(lambda (x)
                (Thomson-weight-function
                 x :beta (/ (- (length the-seq) 0.5)
                            (length the-seq))
                 :symmetric-version t))))
        (iterations '(10 0)))
  "given
   [1] the-seq (required)
       ==> a sequence of numbers
   [2] weighting-functions (keyword; Huber and Thomson)
       ==> a list of two weighting functions
   [3] iterations (keyword; '(10 0))
       ==> a list of nonegative integers
           giving the number of iterations
           for each weighting function
returns
   [1] m-location estimate for sequence
   [2] MAD scale estimate (based upon deviations
       from m-location estimate)
---
Note: see article by Hogg in Launer and Wilkinson"
  (let ((n (length the-seq))
        (i -1)
        scaled-deviation final-mad scale-estimate
        weight sum-of-weights sum-of-weighted-scaled-deviations)
    (multiple-value-bind (mad-scale-estimate the-answer)
                         (median-absolute-deviation the-seq)
      (setf scale-estimate (/ mad-scale-estimate 0.6745))
      (dolist (a-weighting-function weighting-functions)
        (dotimes (j (nth (incf i) iterations))
          (setf sum-of-weights 0.0
                sum-of-weighted-scaled-deviations 0.0)
          (dotimes (k n)
            (setf scaled-deviation (/ (- (elt the-seq k) the-answer)	
                                      scale-estimate)
                  weight (funcall a-weighting-function scaled-deviation))
            (incf sum-of-weights weight)
            (incf sum-of-weighted-scaled-deviations
                  (* weight scaled-deviation))))
        (incf the-answer (* (/ sum-of-weighted-scaled-deviations sum-of-weights)
                            scale-estimate)))
      (setf final-mad (/ (median-absolute-deviation
                          the-seq :location-estimate the-answer)
                         0.6745))
      (values the-answer final-mad))))

#|
(m-location-estimate '(9 5 4 5 6 5 4 5 6 -100 5))
;==> 5.0
;    1.4825796886582654

(m-location-estimate '(9 5 4 5 6 5 4 5 6 -100 5)
                     :iterations '(10 1))
;==> 5.00390370448126
;    1.4767921356838247
|#

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;;  The functions  ordinary-least-squares-Cholesky
;;;                 ordinary-least-squares-Q-R
;;;                 weighted-linear-least-squares
;;;                 Durbin-Watson-test-statistic
;;;                 predicted-y-at-x
;;;                 var-predicted-y-at-x
;;;                 var-predicted-mean-at-x
;;;  handle linear regression analysis.  The first three of these functions
;;;  are the main computational engines.  Currently here is what distinguishes
;;;  these three routines:
;;;  [1] ordinary-least-squares-Cholesky uses a Cholesky decomposition;
;;;      it accepts an arbitrary list of independent variables, but all
;;;      variables in the list must be either functions or vectors; and
;;;      the function does not currently compute any error statistics
;;;      (other than the residuals themselves).
;;;  [2] ordinary-least-squares-Q-R uses a Q-R (i.e., modified Gram-Schmidt)
;;;      decomposition; it accepts an arbitrary list of independent variables,
;;;      any one of which can be either a function or a vector; and the
;;;      function optionally returns error statistics such as residuals,
;;;      variances of parameter estimates and the correlation matrix for
;;;      these estimates.  This function is somewhat slower than
;;;      ordinary-least-squares-Cholesky.
;;;  [3] weighted-linear-least-squares only works for the simple model
;;;          y_k = alpha + beta * x_k
;;;      and uses the method outlined in Chapter 2 of 
;;;      ``Applied Regression Analysis'' by Draper and Smith, 1966,
;;;      to estimate alpha and beta.  The independent variable can be
;;;      expressed as either a vector or a function.  This function optionally
;;;      returns error statistics.  It is faster than the other two functions.
;;;  The functions predicted-y-at-x, var-predicted-y-at-x and
;;;  var-predicted-mean-at-x are intended to be used in conjunction
;;;  with the values returned by weighted-linear-least-squares.
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
(defun ordinary-least-squares-Cholesky
       (dependent-variable
        list-of-independent-variables
        &key
        (compute-residuals-p nil)
        (result (if compute-residuals-p
                  (make-array
                   (length dependent-variable)))))
  "given
   [1] dependent-variable (required)
        ==> a vector
   [2] list-of-independent-variables (required)
       ==> a list of either vectors or functions
           (must be ALL vectors or ALL functions)
   [3] compute-residuals-p (keyword; nil)
       ==> if t, residuals are computed
   [4] result (keyword; new array if compute-residuals-p is t; otherwise nil)
       <== storage space for residuals
           (not used unless compute-residuals-p is true)
returns
   [1] vector with estimated parameters
   [2] residuals
---
Note: uses Lisp versions of Cholesky factorization routines
      from linpack"
  ;;; NOTE (5/7/93): the restriction that list-of-independent-variables be
  ;;;                all vectors or all functions (and not a mixture thereof)
  ;;;                was made for programming convenience.  To correct the code
  ;;;                so that it works for a mixture (as is true for the Q-R
  ;;;                version of this function), it is only necessary to modify
  ;;;                the code in the cond clause below -- the code to compute
  ;;;                the residuals can already handle a mixture.
  (assert (plusp (length list-of-independent-variables)))
  (let* ((N (length dependent-variable))
         (p (length list-of-independent-variables))
         (X-prime-X (make-array `(,p ,p)))
         (X-prime-Y (make-array p))
         (off-diag-to-do (1- p))
         offset an-independent-variable another-independent-variable)
    (cond
     ((vectorp (car list-of-independent-variables))
      ;;; independent variables are a list of vectors
      (dotimes (i p)
        (setf (aref X-prime-X i i) (sum-of-squares
                                    (elt list-of-independent-variables i))
              (aref X-prime-Y i) (dot-product
                                  (elt list-of-independent-variables i)
                                  dependent-variable)
              offset (1+ i))
        (dotimes (j off-diag-to-do)
          (setf (aref X-prime-X i offset) (dot-product
                                           (elt list-of-independent-variables i)
                                           (elt list-of-independent-variables
                                                (+ i j 1)))
                (aref X-prime-X offset i) (aref X-prime-X i offset))
          (incf offset))
        (decf off-diag-to-do)))
     (t
      ;;; independent variables are a list of functions
      (dotimes (i p)
        (setf an-independent-variable (elt list-of-independent-variables i))
        (setf (aref X-prime-X i i)
              (let ((SS 0.0))
                (dotimes (k N SS)
                  (incf SS
                        (expt (funcall an-independent-variable k) 2))))
              (aref X-prime-Y i)
              (let ((sum 0.0))
                (dotimes (k N sum)
                  (incf sum
                        (* (funcall an-independent-variable k)
                           (aref dependent-variable k)))))
              offset (1+ i))
        (dotimes (j off-diag-to-do)
          (setf another-independent-variable
                (elt list-of-independent-variables (+ i j 1)))
          (setf (aref X-prime-X i offset)
                (let ((sum 0.0))
                  (dotimes (k N sum)
                    (incf sum
                          (* (funcall an-independent-variable k)
                             (funcall another-independent-variable k)))))
                (aref X-prime-X offset i) (aref X-prime-X i offset))
          (incf offset))
        (decf off-diag-to-do))))
    (spofa! X-prime-X)
    (sposl! X-prime-X X-prime-Y)
    ;;; X-prime-Y now contains solution vector to normal equation
    (when compute-residuals-p
      (copy-vector dependent-variable result)
      (dotimes (i p)
        (setf an-independent-variable (elt list-of-independent-variables i))
        (if (vectorp an-independent-variable)
          (dotimes (j N)
            (decf (aref result j)
                  (* (aref X-prime-Y i)
                     (aref an-independent-variable j))))
          (dotimes (j N)
            (decf (aref result j)
                  (* (aref X-prime-Y i)
                     (funcall an-independent-variable j)))))))
    (values X-prime-Y result)))

#|
(ordinary-least-squares-Cholesky
 #(1.0 1.9 3.1 3.9 5.1 5.9 7.1 7.9 9.1 9.9)
 `(,(make-array 10 :initial-element 1.0)
   ,#(0.0 0.5 1.0 1.5 2.0 2.5 3.0 3.5 4.0 4.5))
 :compute-residuals-p t)
;==> #(0.9927272727272731 1.9987878787878786)
;    #(0.0072727272727268755 -0.09212121212121249 0.1084848484848484 -0.09090909090909127 0.1096969696969694 -0.08969696969696894 0.1109090909090904 -0.08848484848484794 0.1121212121212114 -0.08727272727272783)
|#

;-------------------------------------------------------------------------------
(defun ordinary-least-squares-Q-R
       (dependent-variable
        list-of-independent-variables
        &key
        (compute-residuals-p nil)
        (compute-standard-errors-p nil)
        (compute-correlation-matrix-p nil)
        (residuals (if (or compute-residuals-p
                           compute-standard-errors-p
                           compute-correlation-matrix-p)
                     (make-array
                      (length dependent-variable)))))
  "given
   [1] dependent-variable (required)
       ==> a vector
   [2] list-of-independent-variables (required)
       ==> a list of either vectors, functions or mixture thereof
   [3] compute-residuals-p (keyword; nil)
       ==> if t, residuals are computed
   [4] compute-standard-errors-p (keyword; nil)
       ==> if t, standard errors for parameter estimates are computed,
           along with residuals (even if compute-residuals-p is nil)
   [5] compute-correlation-matrix-p (keyword; nil)
       ==> if t, correlation matrix for parameter estimates is computed,
           along with residuals and standard errors (even if either
           of their associated keywords is nil) 
   [6] residuals (keyword; nil or vector)
       <== storage space for residuals
           (nil unless either compute-residuals-p,
           compute-standard-errors-p or
           compute-correlation-matrix-p is true)
returns
   [1] vector with parameter estimates
   [2] vector with residuals
   [2] standard error of residuals
   [3] vector with standard errors of parameter estimates
   [4] correlation matrix
---
Note: this routine is based on the discussion
      in ``Nonlinear Regression Analysis and Its Applications''
      by Bates and Watts, Wiley, 1988."
  (assert (plusp (length list-of-independent-variables)))
  ;;; To avoid some tricky code, we "stack up" various options:
  (cond
   (compute-correlation-matrix-p
    (setf compute-residuals-p t
          compute-standard-errors-p t))
   (compute-standard-errors-p
    (setf compute-residuals-p t)))
  (let* ((N (length dependent-variable))
         (p (length list-of-independent-variables))
         (design-matrix (make-array `(,N ,p)))
         (standard-errors (if compute-standard-errors-p
                            (make-array p :initial-element 0.0)))
         (s-estimate nil)
         an-independent-variable)
    ;;; (declare (dynamic-extent design-matrix))
    ;;; create the design matrix ...
    (dotimes (i p)
      (setf an-independent-variable (elt list-of-independent-variables i))
      (if (vectorp an-independent-variable)
        (dotimes (j N)
          (setf (aref design-matrix j i) (aref an-independent-variable j)))
        (dotimes (j N)
          (setf (aref design-matrix j i) (funcall an-independent-variable j)))))
    ;;; Q-R factor the design matrix ...
    (multiple-value-bind (Q R) (Q-R! design-matrix)
      ;;; ... followed by computation of parameter estimates
      (let ((parameter-estimates (upper-triangular-solve!
                                  R
                                  (multiply-matrix-and-vector
                                   (transpose Q)
                                   dependent-variable))))
        (when compute-residuals-p
          (copy-vector dependent-variable residuals)
          (dotimes (i p)
            (setf an-independent-variable (elt list-of-independent-variables i))
            (if (vectorp an-independent-variable)
              (dotimes (j N)
                (decf (aref residuals j)
                      (* (aref parameter-estimates i)
                         (aref an-independent-variable j))))
              (dotimes (j N)
                (decf (aref residuals j)
                      (* (aref parameter-estimates i)
                         (funcall an-independent-variable j))))))
          (setf s-estimate (sqrt (/ (sum-of-squares residuals)
                                    (- N p)))))
        (if compute-standard-errors-p
          ;;; Here we produce standard errors for the parameter estimates
          ;;; (along with a correlation matrix).
          ;;; First, we need to produce the inverse of R:
          (let ((R-inverse (make-array `(,p ,p)))
                rhs-vector row-length)
            (dotimes (i p)
              (setf rhs-vector (make-array p :initial-element 0.0))
              (setf (aref rhs-vector i) 1.0)
              (upper-triangular-solve! R rhs-vector)
              (dotimes (j p)
                (setf (aref R-inverse j i) (aref rhs-vector j))))
            ;;; We now have the inverse of R, from which we can produce
            ;;; standard errors for the parameter estimates.
            (dotimes (i p)
              (dotimes (j p)
                (incf (svref standard-errors i)
                      (expt (aref R-inverse i j) 2)))
              (setf row-length (sqrt (elt standard-errors i)))
              (setf (svref standard-errors i) (* row-length s-estimate))
              ;;; If correlation matrix is to be produced,
              ;;; we need to normalize
              (if compute-correlation-matrix-p
                (let ((1-over-row-length (/ row-length)))
                  (dotimes (j p)
                    (multf (aref R-inverse i j) 1-over-row-length)))))
            (values
             parameter-estimates
             residuals
             s-estimate
             standard-errors
             (if compute-correlation-matrix-p
               (multiply-two-matrices R-inverse (transpose R-inverse)))))
          ;;; if compute-standard-errors-p is nil, here is what we return: 
          (values
             parameter-estimates
             residuals
             s-estimate))))))

#|
(ordinary-least-squares-Q-R
 #(1.0 1.9 3.1 3.9 5.1 5.9 7.1 7.9 9.1 9.9)
 `(,(make-array 10 :initial-element 1.0)
   ,#(0.0 0.5 1.0 1.5 2.0 2.5 3.0 3.5 4.0 4.5))
 :compute-residuals-p t)
;==> #(0.9927272727272709 1.9987878787878792)
;    #(0.007272727272729096 -0.0921212121212106 0.10848484848484996 -0.09090909090908994 0.10969696969697074 -0.08969696969696805 0.11090909090909129 -0.08848484848484706 0.11212121212121229 -0.08727272727272606)
;    0.10545715775238804

(ordinary-least-squares-Q-R
 #(1.0 1.9 3.1 3.9 5.1 5.9 7.1 7.9 9.1 9.9)
 `(,(make-array 10 :initial-element 1.0)
   ,#(0.0 0.5 1.0 1.5 2.0 2.5 3.0 3.5 4.0 4.5))
 :compute-correlation-matrix-p t)
;==> #(0.9927272727272709 1.9987878787878792)
;    #(0.007272727272729096 -0.0921212121212106 0.10848484848484996 -0.09090909090908994 0.10969696969697074 -0.08969696969696805 0.11090909090909129 -0.08848484848484706 0.11212121212121229 -0.08727272727272606)
;    0.10545715775238804
;    #(0.06198284664515573 0.023220901891718743)
;    #2a((0.9999999999999999 -0.8429272304235245) (-0.8429272304235245 1.0))
|#

;-------------------------------------------------------------------------------
(defun weighted-linear-least-squares
       (y
        &key
        (versus #'(lambda (i) (float (- i (/ (1- (length y)) 2)))))
        (weights nil)
        (compute-residuals-p t)
        (compute-covariance-matrix-p t)
        (residuals (if compute-residuals-p (make-array (length y)))))
  "given
   [1] y (required)
       ==> a sequence with values of y_k's, the dependent variable.
   [2] versus (keyword; equally spaced & centered values)
       ==> a sequence or function giving values of x_k's,
           the independent variable
   [3] weights (keyword; nil)
       ==> if vector supplied, these are used as weights
           on the observations (nil implies that all y_k's
           are equally weighted)
   [4] compute-residuals-p (keyword; nil)
       ==> if t, residuals are computed
   [5] compute-covariance-matrix-p (keyword; nil)
       ==> if t, calculates and returns 2x2 var/covar matrix
   [6] residuals (keyword; nil or vector)
       <== a sequence to be stuffed with the residuals
           (not used unless compute-residuals-p is true)
fits model y_k = alpha + beta * x_k, and
returns
   [1] estimate of intercept alpha
   [2] estimate of slope beta
   [3] estimate of residual variance
   [4] residuals (if compute-residuals-p is true)
   [5] covariance matrix for parameter estimates
       (if compute-covariance-matrix-p is true)
---
Note: see Chapter 2 of Draper and Smith, 1966"
  (let ((sum-weights 0.0)
        (sum-weights-x 0.0)
        (sum-weights-x-sq 0.0)
        (sum-weights-y 0.0)
        (sum-weights-x-y 0.0)
        (sum-weights-y-sq 0.0)
        intercept slope det residual-variance loc-w loc-x loc-y
        (n (length y))
        (var-covar (if compute-covariance-matrix-p (make-array '(2 2))))
        (get-x (if (typep versus 'sequence)
                 #'(lambda (i)
                     (elt versus i))
                 versus)))
    (cond
     ((arrayp weights)
      (dotimes (i n)
        (setf loc-w (elt weights i)
              loc-x (funcall get-x i)
              loc-y (elt y i)
              sum-weights (+ sum-weights loc-w)
              sum-weights-x (+ sum-weights-x (* loc-w loc-x))
              sum-weights-x-sq (+ sum-weights-x-sq (* loc-w loc-x loc-x))
              sum-weights-y (+ sum-weights-y (* loc-w loc-y))
              sum-weights-x-y (+ sum-weights-x-y (* loc-w loc-x loc-y))
              sum-weights-y-sq (+ sum-weights-y-sq (* loc-w loc-y loc-y)))))
     (t
      (setf sum-weights (float n))
      (dotimes (i n)
        (setf loc-x (funcall get-x i)
              loc-y (elt y i)
              sum-weights-x (+ sum-weights-x loc-x)
              sum-weights-x-sq (+ sum-weights-x-sq (* loc-x loc-x))
              sum-weights-y (+ sum-weights-y loc-y)
              sum-weights-x-y (+ sum-weights-x-y (* loc-x loc-y))
              sum-weights-y-sq (+ sum-weights-y-sq (* loc-y loc-y))))))
    (setf det (- (* sum-weights sum-weights-x-sq)
                 (* sum-weights-x sum-weights-x)))
    (cond
     ((zerop det)
      (cerror "slope, intercept, residual variance set to 0.0"
              "determinant in linear least squares is 0")
      (values 0.0 0.0 0.0))
     (t
      (setf intercept (/ (- (* sum-weights-y sum-weights-x-sq)
                            (* sum-weights-x sum-weights-x-y))
                         det))
      (setf slope (/ (- (* sum-weights sum-weights-x-y)
                        (* sum-weights-x sum-weights-y))
                     det))
      (if (arrayp residuals)
        (dotimes (i n)
          (setf (elt residuals i)
                (- (elt y i)
                   intercept (* slope (funcall get-x i))))))
      (setf residual-variance (if (< n 3) 0.0
                                  (/ (- sum-weights-y-sq
                                        (* sum-weights-y intercept)
                                        (* sum-weights-x-y slope))
                                     (- n 2))))
      (cond
       ((arrayp var-covar)
        (setf (aref var-covar 0 0)
              (/ (* residual-variance sum-weights-x-sq) det))
        (setf (aref var-covar 0 1)
              (- (/ (* residual-variance sum-weights-x) det)))
        (setf (aref var-covar 1 0) (aref var-covar 0 1))
        (setf (aref var-covar 1 1)
              (/ (* residual-variance sum-weights) det))))
      (values intercept slope residual-variance residuals var-covar)))))

#|
(weighted-linear-least-squares
 #(1.0 1.9 3.1 3.9 5.1 5.9 7.1 7.9 9.1 9.9)
 :versus
 #(0.0 0.5 1.0 1.5 2.0 2.5 3.0 3.5 4.0 4.5))
;==> 0.9927272727272727       intercept
;    1.9987878787878788       slope
;    0.011121212121210533     residual variance
;    #(0.0072727272727273196 -0.09212121212121216 0.10848484848484863 -0.09090909090909083 0.10969696969696985 -0.08969696969696894 0.11090909090909129 -0.08848484848484794 0.11212121212121229 -0.08727272727272606)
;    #2a((0.003841873278236366 -0.0012132231404956945) (-0.0012132231404956945 5.392102846647531E-4))
|#
        
;-------------------------------------------------------------------------------
(defun Durbin-Watson-test-statistic
       (residuals)
  "given a set of residuals in a seqeunce,
returns the Durbin-Watson test statistic"
  (when residuals
    (let ((top 0.0)
          (i+1 0))
      (dotimes (i (1- (length residuals))
                  (/ top (sum-of-squares residuals)))
        (incf top (expt (- (elt residuals (incf i+1))
                           (elt residuals i))
                        2))))))

#|
(Durbin-Watson-test-statistic
 #(0.0072727272727273196 -0.09212121212121216 0.10848484848484863 -0.09090909090909083 0.10969696969696985 -0.08969696969696894 0.11090909090909129 -0.08848484848484794 0.11212121212121229 -0.08727272727272606)
 )
;==> 3.7078028238791196
|#

;-------------------------------------------------------------------------------
(defun predicted-y-at-x
       (x
        intercept
        slope)
  "given
   [1] x (required)
       ==> a number
   [2] intercept (required)
       ==> intercept of a line
   [3] slope (required)
       ==> slope of a line
returns
   [1] intercept + slope*x"
  (+ intercept (* x slope)))

#|
(predicted-y-at-x 3.0 0.9927272727272727  1.9987878787878788)
;==> 6.989090909090908
(predicted-y-at-x 8.0 0.9927272727272727  1.9987878787878788)
;==> 16.983030303030304
|#

;-------------------------------------------------------------------------------
(defun var-predicted-y-at-x
       (x
        residual-variance
        covariance-matrix)
  "given
   [1] x (required)
       ==> a number
   [2] residual-variance (required)
       ==> the residual variance as returned
           by weighted-linear-least-squares
   [3] covariance-matrix (required)
       ==> covariance matrix for estimated
           intercept and slope as returned
           by weighted-linear-least-squares
returns
   [1] the variance of the predicted value of y
       at x
---
Note: see Equation (1.4.8), p. 24, Draper and Smith, 1966"
  (+ residual-variance (var-predicted-mean-at-x x covariance-matrix)))

#|
(var-predicted-y-at-x
 3.0 0.011121212121210533
 #2a((0.003841873278236366 -0.0012132231404956945) (-0.0012132231404956945 5.392102846647531E-4))
 )
;==> 0.012536639118455508
(var-predicted-y-at-x
 8.0 0.011121212121210533
 #2a((0.003841873278236366 -0.0012132231404956945) (-0.0012132231404956945 5.392102846647531E-4))
 )
;==> 0.030060973370059984
|#

;-------------------------------------------------------------------------------
(defun var-predicted-mean-at-x
       (x
        covariance-matrix)
  "given
   [1] x (required)
       ==> a number
   [2] covariance-matrix (required)
       ==> covariance matrix for estimated
           intercept and slope as returned
           by weighted-linear-least-squares
returns
   [1] the variance of the predicted mean of y
       at x
---
Note: see page 56, Draper and Smith, 1966"
  (+ (aref covariance-matrix 0 0)
     (* 2 x (aref covariance-matrix 0 1))
     (* x x (aref covariance-matrix 1 1))))

#|
(var-predicted-mean-at-x
 3.0
 #2a((0.003841873278236366 -0.0012132231404956945) (-0.0012132231404956945 5.392102846647531E-4))
 )
;==> 0.0014154269972449754
(var-predicted-mean-at-x
 8.0
 #2a((0.003841873278236366 -0.0012132231404956945) (-0.0012132231404956945 5.392102846647531E-4))
 )
;==> 0.018939761248849447
|#

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;;  The functions  kernel-pdf-estimation
;;;                 window-width-from-Silverman
;;;  handle a simple case of probability density function estimation.
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
(defun kernel-pdf-estimation
       (a-seq
        start-x
        increment-x
        n-pdf
        &key
        (window-width (window-width-from-Silverman
                       a-seq))
        (result-pdf (make-array
                     n-pdf
                     :initial-element 0.0)
                    result-pdf-supplied-p)
        (result-x (make-array n-pdf)))
  "given
   [1] a-seq (required)
       ==> a sequence of real-valued numbers
   [2] start-x (required)
       ==> first point at which pdf is
           to be estimated
   [3] increment-x (required)
       ==> increment between points at which pdf is
           to be estimated (i.e., second point is
           at start-x + increment-x, third point is
           at start-x + 2*increment-x, etc)
   [4] n-pdf (required)
       ==> number of points at which pdf is
           to be estimated
   [5] window-width (keyword; Silverman's formula)
       ==> window width of kernel pdf estimator
   [6] result-pdf (keyword; vector of length n-pdf)
       <== vector to hold pdf estimate
   [7] result-x (keyword; vector of length n-pdf)
       <== vector to hold points at which
           pdf was estimated
computes a pdf estimate using the normal (Gaussian) kernel and
returns
   [1] the pdf estimate (in result-pdf)
   [2] the points associated with the pdf estimates (in result-x)
   [3] the window width"
  (let* ((n (length a-seq))
         (current-x start-x)
         (normalization-factor (* n window-width)))
    (if result-pdf-supplied-p
      (fill result-pdf 0.0 :end n-pdf))
    (dotimes (i n-pdf (values result-pdf result-x window-width))
      (setf (elt result-x i) current-x)
      (dotimes (j n)
        (incf (elt result-pdf i)
              (standard-normal-pdf
               (/ (- (elt a-seq j) current-x)
                  window-width))))
      (setf (elt result-pdf i)
            (/ (elt result-pdf i) normalization-factor))
      (incf current-x increment-x))))

#|
;;; Now we repeat the above, but now scale the histogram as a density:
(kernel-pdf-estimation
 '(9 1 9 9 1 8 1 9 6 7 9 3 9)
 0.5 1.0 11)
;==> #(0.040920458611761634 0.047495773125185846 0.053665548402843676 0.060012914127614225 0.06698186657888049 0.07432092485055371 0.08082908107969788 0.08464308352211156 0.08399125416137254 0.07803398106539552 0.06732806858217144)
;    #(0.5 1.5 2.5 3.5 4.5 5.5 6.5 7.5 8.5 9.5 10.5)
;    3.308586641170241

(* (sum (kernel-pdf-estimation
         '(9 1 9 9 1 8 1 9 6 7 9 3 9)
         -10.0 0.1 300))
   0.1)
;==> 0.9996684916122942
|#

;-------------------------------------------------------------------------------
(defun window-width-from-Silverman
       (a-seq)
  "given a-seq,
returns the window width for kernel pdf estimation
given on page 48, Equation (3.31), of Silverman, 1986"
  (min (sqrt (sample-variance a-seq))
       (/ (interquartile-range a-seq) 1.34)))

#|
(window-width-from-Silverman '(9 1 9 9 1 8 1 9 6 7 9 3 9))
;==> 3.308586641170241
|#

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;;  Everything below here consists of internal symbols in the SAPA package
;;;  and should be regarded as "dirty laundry" ...
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
(defconstant +ppnd-a0+ 2.50662823884d0)
(defconstant +ppnd-a1+ -18.61500062529d0)
(defconstant +ppnd-a2+ 41.39119773534d0)
(defconstant +ppnd-a3+ -25.44106049637d0)
(defconstant +ppnd-b1+ -8.4735109309d0)
(defconstant +ppnd-b2+ 23.08336743743d0)
(defconstant +ppnd-b3+ -21.06224101826d0)
(defconstant +ppnd-b4+ 3.13082909833d0)
(defconstant +ppnd-c0+ -2.78718931138d0)
(defconstant +ppnd-c1+ -2.29796479134d0)
(defconstant +ppnd-c2+ 4.85014127135d0)
(defconstant +ppnd-c3+ 2.32121276858d0)
(defconstant +ppnd-d1+ 3.54388924762d0)
(defconstant +ppnd-d2+ 1.63706781897d0)

;-------------------------------------------------------------------------------
(defun as-111 (p)
  "adapted from AS 111 --- FORTRAN routine ppnd"
  (assert (and (plusp p) (< p 1.0)))
  (let ((q (- p 0.5))
         ppnd r)
    (cond
     ((<= (abs q) 0.42)
      (setf r (* q q))
      (* q (/ (+ (* (+ (* (+ (* +ppnd-a3+
                                r)
                             +ppnd-a2+)
                          r)
                       +ppnd-a1+)
                    r)
                 +ppnd-a0+)
              (+ (* (+ (* (+ (* (+ (* +ppnd-b4+
                                      r)
                                   +ppnd-b3+)
                                r)
                             +ppnd-b2+)
                          r)
                       +ppnd-b1+)
                    r)
                 1.0))))
                                     
     (t
      (setf r (sqrt (- (log (if (> q 0.0) (- 1.0 p) p))))
            ppnd (/ (+ (* (+ (* (+ (* +ppnd-c3+
                                      r)
                                   +ppnd-c2+)
                                r)
                             +ppnd-c1+)
                          r)
                       +ppnd-c0+)
                    (+ (* (+ (* +ppnd-d2+
                                r)
                             +ppnd-d1+)
                          r)
                       1.0)))
      (if (< q 0.0) (- ppnd) ppnd)))))

;-------------------------------------------------------------------------------
(defconstant +ppchi2-c1+ 0.01)
(defconstant +ppchi2-c2+ 0.222222)
(defconstant +ppchi2-c3+ 0.32)
(defconstant +ppchi2-c4+ 0.4)
(defconstant +ppchi2-c5+ 1.24)
(defconstant +ppchi2-c6+ 2.2)
(defconstant +ppchi2-c7+ 4.67)
(defconstant +ppchi2-c8+ 6.66)
(defconstant +ppchi2-c9+ 6.73)
(defconstant +ppchi2-c10+ 13.32)
(defconstant +ppchi2-c11+ 60.0)
(defconstant +ppchi2-c12+ 70.0)
(defconstant +ppchi2-c13+ 84.0)
(defconstant +ppchi2-c14+ 105.0)
(defconstant +ppchi2-c15+ 120.0)
(defconstant +ppchi2-c16+ 127.0)
(defconstant +ppchi2-c17+ 140.0)
(defconstant +ppchi2-c18+ 175.0)
(defconstant +ppchi2-c19+ 210.0)
(defconstant +ppchi2-c20+ 252.0)
(defconstant +ppchi2-c21+ 264.0)
(defconstant +ppchi2-c22+ 294.0)
(defconstant +ppchi2-c23+ 346.0)
(defconstant +ppchi2-c24+ 420.0)
(defconstant +ppchi2-c25+ 462.0)
(defconstant +ppchi2-c26+ 606.0)
(defconstant +ppchi2-c27+ 672.0)
(defconstant +ppchi2-c28+ 707.0)
(defconstant +ppchi2-c29+ 735.0)
(defconstant +ppchi2-c30+ 889.0)
(defconstant +ppchi2-c31+ 932.0)
(defconstant +ppchi2-c32+ 966.0)
(defconstant +ppchi2-c33+ 1141.0)
(defconstant +ppchi2-c34+ 1182.0)
(defconstant +ppchi2-c35+ 1278.0)
(defconstant +ppchi2-c36+ 1740.0)
(defconstant +ppchi2-c37+ 2520.0)
(defconstant +ppchi2-c38+ 5040.0)
(defconstant +ppchi2-aa+ 0.6931471806d0)
(defconstant +ppchi2-e+ 0.0000005)
(defconstant +ppchi2-pmin+ 0.000002)
(defconstant +ppchi2-pmax+ 0.999998)

;-------------------------------------------------------------------------------
(defun as-91 (p degrees-of-freedom)
  (let ((g (log-of-gamma (/ degrees-of-freedom 2.0)))
        xx c ch)
    ;;; p is outside range where approximation is valid ---
    ;;; might want to change this to a cerror call with the continue option
    ;;; of just going ahead and accepting a bad approximation ...
    (assert (and (>= p +ppchi2-pmin+) (<= p +ppchi2-pmax+)))
    ;;; the degrees of freedom should be positive ...
    (assert (plusp degrees-of-freedom))
    (setf xx (* 0.5 degrees-of-freedom)
          c (- xx 1.0))
    (cond
     ((>= degrees-of-freedom (* (- +ppchi2-c5+) (log p)))
      (setf ch
            (if (> degrees-of-freedom +ppchi2-c3+)
              (let* ((x (quantile-of-normal-distribution
                         p
                         :accuracy-level :accurate))
                     (p1 (/ +ppchi2-c2+ degrees-of-freedom))
                     (temp-ch (* degrees-of-freedom (expt (+ (* x (sqrt p1))
                                                             (- 1.0 p1))
                                                          3))))
                (if (> temp-ch (+ (* +ppchi2-c6+ degrees-of-freedom) 6.0))
                  (* (- 2.0) (+ (- (log (- 1.0 p))
                                   (* c (log (* 0.5 temp-ch))))
                                g))
                  temp-ch))
              (as-91-block-1 +ppchi2-c4+ (log (- 1.0 p)) g c)
              ))
      (as-91-block-4 ch xx c p g)
      ;;; label 1 stuff goes here ...
      )
     (t
      (setf ch (expt (* (* p xx)
                        (exp (+ g (* xx +ppchi2-aa+))))
                     (/ 1.0 xx)))
      (if (< ch +ppchi2-e+) ch
          (as-91-block-4 ch xx c p g))))))

;-------------------------------------------------------------------------------
(defun as-91-block-1 (ch a g c)
  (let* ((q ch)
         (p1 (+ 1.0 (* ch (+ +ppchi2-c7+ ch))))
         (p2 (* ch (+ +ppchi2-c9+ (* ch (+ +ppchi2-c8+ ch)))))
         (temp (+ (- 0.5) (- (/ (+ +ppchi2-c7+ (* 2.0 ch))
                                  p1)
                             (/ (+ +ppchi2-c9+
                                   (* ch
                                      (+ +ppchi2-aa+ 10
                                         (* 3.0
                                            ch))))
                                p2)))))
    (decf ch (/ (- 1.0 (* (exp (+ (+ (+ a g)
                                     (* 0.5 ch))
                                  (* c +ppchi2-aa+)))
                          (/ p2 p1)))
                temp))
    (if (> (abs (- (/ q ch) 1.0)) +ppchi2-c1+)
      (as-91-block-1 ch a g c))))

;-------------------------------------------------------------------------------
(defun as-91-block-4 (ch xx c p g)
  (let* ((q ch)
         (p1 (* 0.5 ch))
         (p2 (- p (incomplete-gamma xx p1)))
         (temp (* p2
                  (exp
                   (+ (+ (* xx +ppchi2-aa+) g)
                      (- p1
                         (* c (log ch)))))))
         (b (/ temp ch))
         (a (- (* 0.5 temp)
               (* b c)))
         (s1 (/ (+ +ppchi2-c19+
                   (* a
                      (+ +ppchi2-c17+
                         (* a
                            (+ +ppchi2-c14+
                               (* a
                                  (+ +ppchi2-c13+
                                     (* a
                                        (+ +ppchi2-c12+
                                           (* +ppchi2-c11+
                                              a))))))))))
                +ppchi2-c24+))
         (s2 (/ (+ +ppchi2-c24+
                   (* a
                      (+ +ppchi2-c29+
                         (* a
                            (+ +ppchi2-c32+
                               (* a
                                  (+ +ppchi2-c33+
                                     (* +ppchi2-c35+
                                        a))))))))
                +ppchi2-c37+))
         (s3 (/ (+ +ppchi2-c19+
                   (* a
                      (+ +ppchi2-c25+
                         (* a
                            (+ +ppchi2-c28+
                               (* +ppchi2-c31+ a))))))
                +ppchi2-c37+))
         (s4 (/ (+ (+ +ppchi2-c20+
                      (* a
                         (+ +ppchi2-c27+ (* +ppchi2-c34+ a))))
                   (* c
                      (+ +ppchi2-c22+
                         (* a
                            (+ +ppchi2-c30+
                               (* +ppchi2-c36+ a))))))
                +ppchi2-c38+))
         (s5 (/ (+ (+ +ppchi2-c13+ (* +ppchi2-c21+ a))
                   (* c (+ +ppchi2-c18+ (* +ppchi2-c26+ a))))
                +ppchi2-c37+))
         (s6 (/ (+ +ppchi2-c15+
                   (* c (+ +ppchi2-c23+ (* +ppchi2-c16+ c))))
                +ppchi2-c38+)))
    (setf ch (+ ch
                (* temp
                   (+ 1.0
                      (- (* (* 0.5
                               temp)
                            s1)
                         (* (* b c)
                            (- s1
                               (* b
                                  (- s2
                                     (* b
                                        (- s3
                                           (* b
                                              (- s4
                                                 (* b
                                                    (- s5
                                                       (* b
                                                          s6))))))))))))))))
    (if (> (abs (- (/ q ch) 1.0)) +ppchi2-e+)
      (as-91-block-4 ch xx c p g)
      ch)))

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;; incomplete-gamma is based upon the Numerical Recipes routine gammp
;;; with the following extension:
;;; (1) it returns three values instead of one
;;;     (see discussion on page 162, Numerical Recipes, First Edition)
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
(defun incomplete-gamma (a x)
  "arguments:
a  --- parameter value > 0
x  --- variable >= 0
returns three values:
value of incomplete gamma function at a and x;
the numerator (little gamma of a and x);
the denominator (gamma of a);
see Section 6.2 of NR"
  (assert (and (plusp a) (or (plusp x) (zerop x))))
  (if (< x (1+ a))
    (series-representation-for-incomplete-gamma a x)
    (continued-fraction-representation-for-incomplete-gamma a x)))

;-------------------------------------------------------------------------------
;;; incomplete-gamma-q is based upon the NR routine gammq with the following
;;; extension:
;;; (1) it returns three values instead of one (see discussion on page 162)
(defun incomplete-gamma-q (a x)
  "arguments:
a  --- parameter value > 0
x  --- variable >= 0
returns three values:
value of incomplete gamma function at a and x;
the numerator (big gamma of a and x);
the denominator (gamma of a);
see Section 6.2 of NR"
  (multiple-value-bind (ratio top bottom)
                       (incomplete-gamma a x)
    (values  (- 1.0 ratio)
             (- bottom top)
             bottom)))

;-------------------------------------------------------------------------------
;;; used internally by series-representation-for-incomplete-gamma and
;;;                    continued-fraction-representation-for-incomplete-gamma
(defvar +NR-gser-itmax+ 100)
(defvar +NR-gser-eps+ 3.E-7)

;-------------------------------------------------------------------------------
;;; series-representation-for-incomplete-gamma is based upon the NR routine
;;; gser and  is intended to be called from incomplete-gamma --- checking
;;; the validity of a and x is assumed to be already done.
(defun series-representation-for-incomplete-gamma (a x)
  (let* ((gln (log-of-gamma a))
         (gamma-a (exp gln))
         ratio)
    (if (zerop x)
      (values 0.0 0.0 gamma-a)
      (let* ((ap a)
             (sum (/ a))
             (del sum))
        (dotimes (i +NR-gser-itmax+
                    (error "a too large, +NR-gser-itmax+ too small"))
          (incf ap)
          (setf del (/ (* del x) ap))
          (incf sum del)
          (if (< (abs del) (* (abs sum) +NR-gser-eps+))
            (return (values (setf ratio (* sum (exp (- (* a (log x))
                                                       x
                                                       gln))))
                            (* ratio gamma-a)
                            gamma-a))))))))

;-------------------------------------------------------------------------------
;;; continued-fraction-representation-for-incomplete-gamma is based upon
;;; the NR routine gcf (but it returns P(a,x) instead of Q(a,x) and is intended
;;; to be called from incomplete-gamma --- checking the validity of a and x
;;; is assumed to be already done.
(defun continued-fraction-representation-for-incomplete-gamma (a x)
  (let* ((gln (log-of-gamma a))
         (gamma-a (exp gln))
         (gold 0.0)
         (a0 1.0)
         (a1 x)
         (b0 0.0)
         (b1 1.0)
         (fac 1.0)
         an ana anf g ratio)
    (dotimes (i +NR-gser-itmax+
                (error "a too large, +NR-gser-itmax+ too small"))
      (setf an (float (1+ i))
            ana (- an a)
            a0 (* fac (+ a1 (* a0 ana)))
            b0 (* fac (+ b1 (* b0 ana)))
            anf (* fac an)
            a1 (+ (* x a0) (* anf a1))
            b1 (+ (* x b0) (* anf b1)))
      (cond
       ((not (zerop a1))
        (setf fac (/ a1)
              g (* b1 fac))
        (if (< (abs (/ (- g gold) g)) +NR-gser-eps+)
          (return (values (setf ratio (- 1.0
                                         (* g (exp (- (* a (log x))
                                                      x
                                                      gln)))))
                          (* ratio gamma-a)
                          gamma-a)))
        (setf gold g))))))
