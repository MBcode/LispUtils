;;;-*- Mode: LISP; Package: :SAPA; Syntax: COMMON-LISP -*-
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;
;  filtering.lisp
;
;  a collection of Lisp functions to filter a time series ...
;  Note:  before compiling and loading filtering.lisp,
;         you should compile and load (in the order listed)
;            sapa-package.lisp, utilities.lisp, basic-statistics.lisp,
;            dft-and-fft.lisp and tapers.lisp
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
(if (not (find-package :SAPA))
  (defpackage :SAPA (:USE :COMMON-LISP)))

(in-package :SAPA)

(export '(;;; general functions for filtering a time series ...
          filter-time-series-direct
          filter-time-series-fft
          filter-time-series

          ;;; creation of filters ...
          ideal-low-pass-filter-irs
          ideal-high-pass-filter-irs
          ideal-band-pass-filter-irs
          create-least-squares-low-pass-filter
          triangular-convergence-factors
          create-dpss-low-pass-filter
          compose-symmetric-filters

          ;;; calculation of the transfer function for a filter ...
          transfer-function-for-filter

          ;;; specialized functions for filtering a time series ...
          three-point-smoother
          n-applications-of-three-point-smoother
          exponential-smoothing
          running-median

          ;;; specialized functions for filtering a time series ...
          center&prewhiten&taper-time-series
          ))

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;;  The functions  filter-time-series-direct
;;;                 filter-time-series-fft
;;;                 filter-time-series
;;;  all take a time series and a filter and return a filtered
;;;  time series.  The function filter-time-series-direct uses
;;;  the direct ``time domain'' definition of filtering to compute
;;;  the filtered series; filter-time-series-fft uses fft operations;
;;;  and filter-time-series call one of these two functions based upon
;;;  a crude assessment of which of the two methods is the fastest.
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;; Note: filter-time-series-direct is primarily intended to be used
;;;       with short filters for which use of fft's would not be effecient.
(defun filter-time-series-direct
       (time-series
        the-filter
        &key
        (start 0)
        (end (length time-series))
        (result (make-array (- (- end start) (1- (length the-filter))))))
  "given
   [1] time-series (required)
       ==> a vector containing a time series
           x_0, x_1, ..., x_{N-1}
   [2] the-filter (required)
       ==> a vector containing the filter coefficients
           g_0, g_1, ..., x_{K-1}
   [3] start (keyword; 0)
       ==> start index of time-series to be used
   [4] end (keyword; length of time-series)
       ==> 1 + end index of time-series to be used
   [5] result (keyword; vector of appropriate length)
       <== vector to contain filtered time series
                 K-1
           y_t = SUM g_k x_{t+K-1-k},  t = 0, ..., N-K+1
                 k=0
 returns
   [1] result, a vector containing the filtered time series
   [2] the number of values in the filtered time series
---
Note: result can be the same as time-series"
  (let* ((N-filter (length the-filter))
         (N-filter-1 (1- N-filter))
         (N-output (- (- end start) N-filter-1)))
    ;;; reverse needed to conform to convention for
    ;;; filtering (a convolution)
    (setf the-filter (reverse the-filter))
    (dotimes (i N-output (values result N-output))
      (setf (aref result i)
            (* (aref time-series (+ i start))
               (aref the-filter 0)))
      (dotimes (j N-filter-1)
        (incf (aref result i)
              (* (aref time-series (+ i start j 1))
                 (aref the-filter (1+ j))))))))

#|
(filter-time-series-direct #(1 2 3 4 5 -5 -7 -9) #(1 -1))
;==> #(1 1 1 1 -10 -2 -2)
;    7

(filter-time-series-direct #(1 2 3 4 5 -5 -7 -9) #(1 -1) :start 2 :end 6)
;==> #(1 1 -10)
;    3

(let ((test #(1.1 2.2 3.3 4.4 5.4 -5 -7 -9)))
  (filter-time-series-direct test #(1 -1) :result test))
;==> #(1.1 1.0999999999999996 1.1000000000000005 1.0 -10.4 -2 -2 -9)
;    7
|#

;-------------------------------------------------------------------------------
(defun filter-time-series-fft
       (time-series
        the-filter
        &key
        (start 0)
        (end (length time-series))
        (fft-size (* 4 (next-power-of-2
                        (length the-filter))))
        (verbose-p nil)
        (result (make-array (- (- end start) (1- (length the-filter)))
                            :initial-element 0.0)
                result-supplied-p))
  "given
   [1] time-series (required)
       ==> a vector containing a time series
           x_0, x_1, ..., x_{N-1}
   [2] the-filter (required)
       ==> a vector containing the filter coefficients
           g_0, g_1, ..., x_{K-1}
   [3] start (keyword; 0)
       ==> start index of time-series to be used
   [4] end (keyword; length of time-series)
       ==> 1 + end index of time-series to be used
   [5] fft-size (keyword; 4 * power of 2 that is ceiling for filter length)
       ==> size of fft's to be used (must be a power of 2)
   [6] verbose-p (keyword; nil)
       ==> if t, prints line after each block of data is processed;
           if nil, prints nothing
   [7] result (keyword; vector of appropriate length)
       <== vector to contain filtered time series
                 K-1
           y_t = SUM g_k x_{t+K-1-k},  t = 0, ..., N-K+1
                 k=0
 returns
   [1] result, a vector containing the filtered time series
   [2] the number of values in the filtered time series
---
Note: result can be the same as time-series"
  (assert (power-of-2 fft-size))
  (let* ((N-time-series (- end start))
         (N-filter (length the-filter))
         (N-filter-1 (1- N-filter))
         (fft-of-filter (make-array fft-size :initial-element 0.0))
         fft-of-data
         (N-output (1+ (- N-time-series N-filter)))
         (N-block (1+ (- fft-size N-filter)))
         (i-result -1))
    (if result-supplied-p
      (fill result 0.0 :end N-output))
    (copy-vector the-filter fft-of-filter)
    (fft! fft-of-filter)
    (do* ((k start (+ k N-block))
          (still-to-go N-output (- still-to-go N-block))
          (m (+ still-to-go N-filter-1) (+ still-to-go N-filter-1)))
         ((not (plusp still-to-go)) . nil)
      (setf fft-of-data
            (if (>= m fft-size)
              (subseq time-series k (+ k fft-size))
              (concatenate 'array
                           (subseq time-series k (+ k m))
                           (make-array (- fft-size m)
                                       :initial-element 0.0))))
      (if verbose-p (format t "~&k = ~D, still-to-go = ~D" k still-to-go))
      (fft! fft-of-data)
      (dotimes (i fft-size)
        (setf (aref fft-of-data i)
              (/ (conjugate (* (aref fft-of-data i) (aref fft-of-filter i)))
                 fft-size)))
      (fft! fft-of-data)
      (dotimes (i (min still-to-go N-block))
        (setf (aref result (incf i-result))
              (realpart (aref fft-of-data (+ N-filter-1 i))))))
    (values result N-output)))

#|
(filter-time-series-fft #(1 2 3 4 5 -5 -7 -9) #(1 -1))
;==> #(0.9999999999999991 1.0000000000000018 0.9999999999999993 1.0
       -10.0 -2.0000000000000018 -1.9999999999999993)
;    7

(filter-time-series-fft #(1 2 3 4 5 -5 -7 -9) #(1 -1) :start 2 :end 6)
;==> #(1.0000000000000004 0.9999999999999996 -10.0)
;    3

(let* ((a-cosine (sample-from-a-function 
                  #'cos :delta-x (/ pi 100) :N 788))
       (direct-filtered
        (filter-time-series-direct a-cosine #(1 -1)))
       (fft-filtered
        (filter-time-series-fft a-cosine #(1 -1))))
  (compare-seqs direct-filtered fft-filtered))
;==> 4.894807734342944E-17
;    1.97758476261356E-16
|#

;-------------------------------------------------------------------------------
(defun filter-time-series
       (time-series
        the-filter
        &key
        (start 0)
        (end (length time-series))
        (technique :fastest)
        (result (make-array (- (- end start) (1- (length the-filter))))))
  "given
   [1] time-series (required)
       ==> a vector containing a time series
           x_0, x_1, ..., x_{N-1}
   [2] the-filter (required)
       ==> a vector containing the filter coefficients
           g_0, g_1, ..., x_{K-1}
   [3] start (keyword; 0)
       ==> start index of time-series to be used
   [4] end (keyword; length of time-series)
       ==> 1 + end index of time-series to be used
   [5] technique (keyword; :fastest)
       ==> if :fastest, tries to pick fastest method;
           if :fft, uses fft-based method;
           if :direct, uses direct method
   [6] result (keyword; vector of appropriate length)
       <== vector to contain filtered time series
                 K-1
           y_t = SUM g_k x_{t+K-1-k},  t = 0, ..., N-K+1
                 k=0
 returns
   [1] result, a vector containing the filtered time series
   [2] the number of values in the filtered time series
---
Note: result can be the same as time-series"
  ;;; If fastest method is chosen, we do a ROUGH count of the number
  ;;; of floating point operations for the direct and fft methods.
  (if (eq technique :fastest)
    (setf technique (which-is-faster? (length the-filter)
                                      (- end start))))
  (case technique
    (:direct (filter-time-series-direct
              time-series
              the-filter
              :start start
              :end end
              :result result))
    (:fft (filter-time-series-fft
           time-series
           the-filter
           :start start
           :end end
           :result result))
    (otherwise
     (error "technique (~A) should be either fft, direct or fastest"
            technique))))

#|
(let* ((a-cosine (sample-from-a-function 
                  #'cos :delta-x (/ pi 100) :N 788))
       (direct-filtered
        (filter-time-series-direct a-cosine #(1 -1)))
       (fft-filtered
        (filter-time-series-fft a-cosine #(1 -1)))
       (filtered
        (filter-time-series a-cosine #(1 -1))))
  (print (compare-seqs direct-filtered fft-filtered))
  (print (compare-seqs direct-filtered filtered))
  (print (compare-seqs filtered fft-filtered))
  (values))
;==> 4.894807734342944E-17 
;    4.894807734342944E-17 
;    0.0 
|#

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;;  The functions  ideal-low-pass-filter-irs
;;;                 ideal-high-pass-filter-irs
;;;                 ideal-band-pass-filter-irs
;;;                 create-least-squares-low-pass-filter
;;;                 triangular-convergence-factors
;;;                 create-dpss-low-pass-filter
;;;                 compose-symmetric-filters
;;;  can be used to create a filter (by which we mean a vector containing
;;;  the filter coefficients).  The first three of these functions
;;;  return a single member of the impulse response sequence for an ideal
;;;  low-pass, high-pass or band-pass filter.  The next three functions
;;;  can be used to create one of the approximations to an ideal low-pass
;;;  filter discussed in Sections 5.8 and 5.9 of the SAPA book.  The
;;;  final function takes any number of symmetric filters of odd length
;;;  and returns the equivalent composite filter.
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
(defun ideal-low-pass-filter-irs (k W)
  "given
   [1] k (required)
       ==> index of member of impulse response sequence (irs)
           to be calculated (must be an integer)
   [2] W (required)
       ==> the cutoff frequency, standardized such that
            0 < W < 0.5 = Nyquist frequency
returns
   [1] kth member of the impulse response sequence
       for an ideal low-pass filter with cutoff frequency W
---
Note: see Section 5.8 of the SAPA book"
  ;(assert (and (integerp k) (plusp W) (< W 0.5)))
  (if (zerop k)
    (* 2 W)
    (/ (sin (* 2 pi W k)) (* pi k))))

#|
(ideal-low-pass-filter-irs 0 0.1)   ;==> 0.2
(ideal-low-pass-filter-irs 1 0.1)   ;==> 0.1870978567577278
(ideal-low-pass-filter-irs -1 0.1)  ;==> 0.1870978567577278
|#

;-------------------------------------------------------------------------------
(defun ideal-high-pass-filter-irs (k W)
  "given
   [1] k (required)
       ==> index of member of impulse response sequence (irs)
           to be calculated (must be an integer)
   [2] W (required)
       ==> the cutoff frequency, standardized such that
            0 < W < 0.5 = Nyquist frequency
returns
   [1] kth member of the impulse response sequence
       for an ideal low-pass filter with cutoff frequency W"
  (assert (and (integerp k) (plusp W) (< W 0.5)))
  (if (zerop k)
    (- 1.0 (* 2 W))
    (- (/ (sin (* 2 pi W k)) (* pi k)))))

#|
(ideal-high-pass-filter-irs 0 0.1)   ;==> 0.8
(ideal-high-pass-filter-irs 1 0.1)   ;==> -0.1870978567577278
(ideal-high-pass-filter-irs -1 0.1)  ;==> -0.1870978567577278
|#

;-------------------------------------------------------------------------------
;;; Note: by setting W-high = 0.5, this routine produces the irs for
;;;       a high-pass filter;
;;;       by setting W-low = 0.5, this routine produces the irs for
;;;       a low-pass filter
(defun ideal-band-pass-filter-irs (k W-low W-high)
  "given
   [1] k (required)
       ==> index of member of impulse response sequence (irs)
           to be calculated (must be an integer)
   [2] W-low (required)
       ==> the low frequency cutoff (in standardized units)
   [3] W-high (required)
       ==> the high frequency cutoff (in standardized units
           so that 0 <= W-low < W-high <= 0.5, the assumed
           Nyquist frequency).
returns
   [1] k-th member of the impulse response sequence
       for an ideal band-pass filter"
  (assert (and (integerp k) (/= W-low W-high) (<= 0.0 W-low W-high 0.5)))
  (let ((width (- W-high W-low)))
    (if (zerop k)
      (* 2 width)
      (/ (* 2 (cos (* pi (+ W-high W-low) k)) (sin (* pi width k)))
         (* pi k)))))

#|
;;; low-pass
(ideal-band-pass-filter-irs  0 0.0 0.1)   ;==>  0.2
(ideal-band-pass-filter-irs  1 0.0 0.1)   ;==>  0.1870978567577278
(ideal-band-pass-filter-irs -1 0.0 0.1)   ;==>  0.1870978567577278
;;; high-pass
(ideal-band-pass-filter-irs  0 0.3 0.5)   ;==>  0.4
(ideal-band-pass-filter-irs  1 0.3 0.5)   ;==> -0.3027306914562628
(ideal-band-pass-filter-irs -1 0.3 0.5)   ;==> -0.3027306914562628
|#

;-------------------------------------------------------------------------------
(defun create-least-squares-low-pass-filter
       (filter-length
        W
        &key
        (convergence-factors nil)
        (Nyquist-frequency 0.5)
        (result (make-array filter-length)))
  "given
   [1] filter-length (required)
       ==> an odd positive integer = 2L +1
   [2] W (required)
       ==> a cutoff frequency greater than 0
           and less than the Nyquist frequency
   [3] convergence-factors (keyword; nil)
       ==> a one-argument function that maps
           an integer to a convergence factor;
           nil is also acceptable, in which case
           no convergence factors are used
   [4] Nyquist-frequency (keyword; 0.5)
       ==> the Nyquist frequency
   [5] result (keyword; vector of length filter-length)
       <== vector of length filter-length
           into which filter coefficients
           are placed (returned by the function)
uses a least squares approximation to a low-pass filter and
returns
   [1] a symmetric low-pass filter of length 2L+1
       and a gain of unity at zero frequency;
       i.e., result(0) = result(2L)
             result(1) = result(2L-1)
             etc., and
             (sum result) = 1.0
---
Note: see Section 5.8 of the SAPA book;
      (aref result 0)       corresponds to element -L of the filter;
      (aref result L)       corresponds to element  0 of the filter;
      (aref result (* 2 L)) corresponds to element  L of the filter"
  (assert (and (plusp filter-length)
               (oddp filter-length)
               (plusp W)
               (plusp Nyquist-frequency)
               (< 0.0 W Nyquist-frequency)))
  ;;; convert W from user units to standardized units ...
  (if (not (= Nyquist-frequency 0.5))
    (divf W (* 2 Nyquist-frequency)))
  (let* ((L (/ (1- filter-length) 2))
         (minus-L-to-L (iota (- L) L)))
    (transform-a-sequence!
     #'(lambda (k) (ideal-low-pass-filter-irs k W))
     minus-L-to-L
     :result result)
    (if convergence-factors (x*y! result (map-into
                                          minus-L-to-L
                                          convergence-factors
                                          minus-L-to-L)))
    (a*x! (/ (sum result)) result)))

#|
(create-least-squares-low-pass-filter 5 0.1)
;==> #(0.1726089497020141 0.21335639535653148 0.22806930988290877 0.21335639535653148 0.1726089497020141)

(sum (create-least-squares-low-pass-filter 5 0.1))
;==> 1.0
|#

;-------------------------------------------------------------------------------
(defun triangular-convergence-factors (k filter-length)
  (let ((abs-k (abs k)))
    (if (> abs-k (truncate filter-length 2))
      0.0
      (- 1.0 (/ (* 2 abs-k) (1+ filter-length))))))

#|
(triangular-convergence-factors -2 3)  ;==> 0.0
(triangular-convergence-factors -1 3)  ;==> 0.5
(triangular-convergence-factors  0 3)  ;==> 1.0
(triangular-convergence-factors  1 3)  ;==> 0.5
(triangular-convergence-factors  2 3)  ;==> 0.0

(create-least-squares-low-pass-filter
 5 0.1
 :convergence-factors
 #'(lambda (k) (triangular-convergence-factors k 5)))
;==> #(0.09167422811028572 0.22663115545827048 0.3633892328628876 0.22663115545827048 0.09167422811028572)

(sum (create-least-squares-low-pass-filter
      5 0.1
      :convergence-factors
      #'(lambda (k) (triangular-convergence-factors k 5))))
;==> 1.0
|#

;-------------------------------------------------------------------------------
(defun create-dpss-low-pass-filter
       (filter-length
        delta
        W
        &key
        (Nyquist-frequency 0.5)
        (result (make-array filter-length)))
  "given
   [1] filter-length (required)
       ==> an odd positive integer = 2L +1
   [2] delta (required)
       ==> ``W'' parameter for dpss in user
           units (see page 182 of the SAPA book)
   [2] W (required)
       ==> a cutoff frequency greater than 0
           and less than the Nyquist frequency
   [3] Nyquist-frequency (keyword; 0.5)
       ==> the Nyquist frequency
   [4] result (keyword; vector of length filter-length)
       <== vector of length filter-length
           into which filter coefficients
           are placed (returned by the function)
uses a dpss as convergence factors in least squares approximation
to a low-pass filter and
returns
   [1] a symmetric low-pass filter of length 2L+1
       and a gain of unity at zero frequency;
       i.e., result(0) = result(2L)
             result(1) = result(2L-1)
             etc., and
             (sum result) = 1.0
---
Note: see Section 5.9 of the SAPA book;
      (aref result 0)       corresponds to element -L of the filter;
      (aref result L)       corresponds to element  0 of the filter;
      (aref result (* 2 L)) corresponds to element  L of the filter"
  (assert (and (plusp filter-length)
               (oddp filter-length)
               (plusp delta)               
               (plusp W)
               (plusp Nyquist-frequency)
               (< 0.0 W Nyquist-frequency)))
  ;;; convert delta and W from user units to standardized units ...
  (when (not (= Nyquist-frequency 0.5))
    (divf delta (* 2 Nyquist-frequency))
    (divf W (* 2 Nyquist-frequency)))
  (let* ((L (/ (1- filter-length) 2))
         (minus-L-to-L (iota (- L) L)))
    (transform-a-sequence!
     #'(lambda (k) (ideal-low-pass-filter-irs k W))
     minus-L-to-L
     :result result)
    (x*y! result (dpss-data-taper! (make-array filter-length
                                               :initial-element 1.0)
                                   :taper-parameter
                                   (* filter-length delta)))
    (a*x! (/ (sum result)) result)))

#|
(create-dpss-low-pass-filter 5 0.04 0.1)
;==> #(0.16887701282533799 0.21504897804256012 0.23214801826420375 0.21504897804256012 0.16887701282533799)

(sum (create-dpss-low-pass-filter 5 0.04 0.1))
;==> 0.9999999999999999
|#

;-------------------------------------------------------------------------------
(defun compose-symmetric-filters
       (&rest filters)
  "given any number of symmetric filters
(each of odd length and each represented
by a vector of coefficients),
returns the composite filter
that will produce an output
identical to that obtained
by cascading the individual filters"
  (let* ((first-filter (car filters))
         (second-filter (if (= (length filters) 2)
                          (nth 1 filters)
                          (apply 'compose-symmetric-filters (cdr filters))))
         (half-length-first-filter
          (/ (1- (array-total-size first-filter)) 2))
         (half-length-second-filter
          (/ (1- (array-total-size second-filter)) 2)))
    (assert (integerp half-length-first-filter))
    (assert (integerp half-length-second-filter))
    (let* ((K (min half-length-first-filter half-length-second-filter))
           (L (max half-length-first-filter half-length-second-filter))
           (K+L (+ K L))
           (2-times-K+L (* 2 K+L))
           (short-length (1+ (* 2 K)))
           (index-of-last-short (1- short-length))
           (composite-filter (make-array (1+ 2-times-K+L)
                                         :initial-element 0.0))
           (short-filter (if (<= half-length-first-filter
                                 half-length-second-filter)
                           first-filter second-filter))
           (long-filter (if (eq short-filter first-filter)
                          second-filter first-filter)))
      (dotimes (j (1+ K+L) composite-filter)
        (setf (aref composite-filter (- 2-times-K+L j))
              (dotimes (m (min (1+ j) short-length) (aref composite-filter j))
                (incf (aref composite-filter j)
                      (* (aref short-filter (- index-of-last-short m))
                         (aref long-filter (- j m))))))))))

#|
(compose-symmetric-filters #(1/4 1/2 1/4) #(1/4 1/2 1/4))
;==> #(0.0625 0.25 0.375 0.25 0.0625)

(compose-symmetric-filters #(1/8 1/4 1/4 1/4 1/8) #(1/4 1/2 1/4))
;==> #(0.03125 0.125 0.21875 0.25 0.21875 0.125 0.03125)

(compose-symmetric-filters #(1/4 1/2 1/4) #(1/4 1/2 1/4) #(1/4 1/2 1/4))
;==> #(0.015625 0.09375 0.234375 0.3125 0.234375 0.09375 0.015625)

(compose-symmetric-filters #(0.0625 0.25 0.375 0.25 0.0625) #(1/4 1/2 1/4))
;==> #(0.015625 0.09375 0.234375 0.3125 0.234375 0.09375 0.015625)

(compose-symmetric-filters #(1/4 1/2 1/4) #(0.0625 0.25 0.375 0.25 0.0625))
;==> #(0.015625 0.09375 0.234375 0.3125 0.234375 0.09375 0.015625)
|#

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;;  The function   transfer-function-for-filter
;;;  can be used to compute the transfer function for a filter
;;;  represented by a vector of coefficents.
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
(defun transfer-function-for-filter
       (the-filter
        &key
        (tf-transformation #'(lambda (x)
                               (careful-convert-to-dB
                                (expt (abs x) 2)
                                -100.0)))
        (actual-index-of-1st-filter-coeff 0)
        (N-fft (* 4 (next-power-of-2 (length the-filter))))
        (return-frequencies-p nil)
        (Nyquist-frequency 0.5)
        (result-tf (make-array (1+ (/ N-fft 2))))
        (result-freq (if return-frequencies-p (make-array (1+ (/ N-fft 2))))))
  "given
   [1] the-filter (required)
       ==> a vector of filter coefficients
   [2] tf-transformation (keyword; mod squared in dB with 0 mapped to -100 dB)
       ==> a function of one argument or nil;
           if bound to a function, the function is used
           to transform all of the elements of the transfer function
   [3] actual-index-of-1st-filter-coeff (keyword; 0)
       ==> the filter coefficient in (aref the-filter 0) is assumed
           to have an index equal to whatever is given here
           (this is only needed if the phase of the transfer function
           is needed)
   [4] N-fft (keyword; 4 * (next-power-of-2 (length the-filter)))
       ==> the length of vector to be used in the fast Fourier transform 
           -- the larger this is, the finer the grid of frequencies
           over which the transfer function is computed; this number
           MUST be a power of 2.
   [5] return-frequencies-p (keyword; nil)
       ==> if t, the frequencies associated with the transfer function
           are computed and returned in result-freq
   [6] Nyquist-frequency (keyword; 0.5)
       ==> the Nyquist frequency
   [7] result-tf (keyword; vector of length (1+ (/ N-fft 2)))
       <== vector of length (1+ (/ N-fft 2))
           into which the properly transformed transfer function
           is placed (returned by the function)
   [8] result-freq (keyword; vector of length (1+ (/ N-fft 2)) if return-frequencies-p t)
       <== vector of length (1+ (/ N-fft 2))
           into which the frequencies associated with the values
           in result-tf are placed if return-frequencies-p is true
           (returned by the function)
returns
   [1] result-tf, a vector holding
       the properly transformed transfer function
   [2] nil (if return-frequencies-p is nil) or
       result-freq (if return-frequencies-p is t),
       where result-freq is a vector holding
       the frequencies associated with values
       in  result-tf
---
Note: see Section 5.3 of the SAPA book"
  (let* ((fft-scratch (make-array N-fft :initial-element 0.0))
         (N-freq (1+ (/ N-fft 2))))
    ;(declare (dynamic-extent fft-scratch))
    (copy-vector the-filter fft-scratch)
    (fft! fft-scratch)
    (if (not (zerop actual-index-of-1st-filter-coeff))
      (let ((mult-factor
             (exp (complex
                   0.0
                   (/ (* -2 pi actual-index-of-1st-filter-coeff) N-fft))))
            (freq-factor 1.0))
        (dotimes (i N-freq)
          (multf (svref fft-scratch i) freq-factor)
          (multf freq-factor mult-factor))))
    (copy-vector fft-scratch result-tf :end N-freq)
    (if tf-transformation
      (map-into result-tf tf-transformation result-tf))
    (cond
     (return-frequencies-p
      (let ((fund-freq (float (/ (* N-fft (Nyquist-frequency->sampling-time
                                           Nyquist-frequency))))))
        (dotimes (i N-freq)
          (setf (aref result-freq i) (* i fund-freq))))
      (values result-tf result-freq))
     (t
      (values result-tf)))))

#|
;;; This examples uses the 3 point filter discussed in Section 5.7
;;; of the SAPA book -- a plot of the squared modulus of the transfer function
;;; is given in Figure 172.
;;; First example: squared modulus of transfer function in dB
;;;                (associated frequences are j/16, j = 0, 1, ..., 8)
(transfer-function-for-filter
 #(1/4 1/2 1/4))
;==> #( 0.0
       -0.33704242622473096
       -1.3753861631621742
       -3.2061447321250065
       -6.020599913279622
      -10.210441257596415
      -16.686413576676696
      -28.390570977011226
       -100.0)

;;; Second example: squared modulus of transfer function
(transfer-function-for-filter
 #(1/4 1/2 1/4)
 :tf-transformation #'(lambda (x) (expt (abs x) 2)))
;==> #(1.0
       0.9253281139039617
       0.7285533905932737
       0.4779533685342263
       0.25 0.09526993616913669
       0.02144660940672623
       0.0014485813926750574
       0.0)

;;; Third example: transfer function itself (untransformed)
(transfer-function-for-filter
 #(1/4 1/2 1/4)
 :actual-index-of-1st-filter-coeff -1
 :tf-transformation nil)
;==> #(  1.0
      #c(0.9619397662556433 0.0)
      #c(0.8535533905932737 5.551115123125783E-17)
      #c(0.6913417161825448 2.7755575615628914E-17)
      #c(0.5 0.0)
      #c(0.30865828381745514 0.0)
      #c(0.14644660940672624 -8.326672684688674E-17)
      #c(0.03806023374435655 -3.2959746043559335E-17)
      #c(-0.0 -0.0))

;;; Page 171 of the SAPA book says that this transfer function
;;; is equal to the following:
(sample-from-a-function #'(lambda (f) (expt (cos (* pi f)) 2))
                        :delta-x 1/16
                        :n 9)
;==> #(1.0
       0.9619397662556434
       0.8535533905932737
       0.6913417161825449
       0.5000000000000001
       0.3086582838174552
       0.1464466094067263
       0.038060233744356645
       3.7491518045553436E-33)
;    #(0.0 0.0625 0.125 0.1875 0.25 0.3125 0.375 0.4375 0.5)
;;; ... quite good agreement
|#

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;;  The functions  three-point-smoother
;;;                 n-applications-of-three-point-smoother
;;;                 exponential-smoothing
;;;                 running-median
;;;  implement some specialized filters, all of a (more or less)
;;;  low-pass form (the running median filter is nonlinear).
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
(defun three-point-smoother (a-seq)
  "given a sequence of length N,
returns a sequence of length N-2 formed by filtering
the input sequence with a three-point filter
with coefficients 1/4, 1/2 and 1/4
---
Note: see Section 5.7 of the SAPA book"
  (let* ((n (length a-seq))
         (n-2 (- n 2))
         (smoothed-seq (subseq a-seq 1 (1- n))))
    (dotimes (i n-2 smoothed-seq)
      (multf (elt smoothed-seq i) 0.5)
      (incf (elt smoothed-seq i)
            (* 0.25 (+ (elt a-seq i) (elt a-seq (+ i 2))))))))

#|
(three-point-smoother #(71.0 63.0 70.0 88.0 99.0 90.0 110.0))
;==> #(66.75 72.75 86.25 94.0 97.25)
(three-point-smoother #(66.75 72.75 86.25 94.0 97.25))
;==> #(74.625 84.8125 92.875)
|#

;-------------------------------------------------------------------------------
(defun n-applications-of-three-point-smoother (a-seq n)
  "given
   [1] a-seq (required)
       ==> a sequence of length 2*n + 1 or greater
   [2] n (required)
       ==> positive integer indicating the number
           of times the three-point smoother is
           to be applied
returns
   [1] the result of repetitively smoothing a-seq
       using the function three-point-smoother"
  (dotimes (i n a-seq)
    (setf a-seq (three-point-smoother a-seq))))

#|
(n-applications-of-three-point-smoother
  #(71.0 63.0 70.0 88.0 99.0 90.0 110.0)
  2)
;==> #(74.625 84.8125 92.875)
|#

;-------------------------------------------------------------------------------
(defun exponential-smoothing
       (time-series
        alpha
        &key
        (initial-prediction (elt time-series 0))
        (return-smoothed-values-p t)
        (n-predictions 0)
        (result-smoothed
         (if return-smoothed-values-p
           (make-array (length time-series))))
        (result-predictions
         (if (plusp n-predictions)
           (make-array n-predictions))))
  "given
   [1] time-series (required)
       ==> a sequence of numbers
   [2] alpha (required)
       ==> parameter controlling the degree of exponential smoothing
           (usually  0 < alpha < 1)
   [3] initial-prediction (keyword; first element of time-series)
       ==> to get the smoother going, we need a ``prediction'' for
           the first element in the sequence time-series -- two
           common hacks are the first element itself (the default)
           or 0 (if the time series has a zero sample mean)
   [4] return-smoothed-values-p (keyword; t)
       ==> if t, returns smoothed (i.e., filtered) time series
   [5] n-predictions (keyword; 0)
       ==> number of predictions desired
   [6] result-smoothed (keyword; vector of length of time-series or nil)
       <== a sequence to hold smoothed (filtered) time series -- used
           only if return-smoothed-values-p is true
   [7] result-predictions (keyword; vector of length n-predictions or nil)
       <== a sequence to hold predicted values of the time series --
           used only if n-predictions is positive
returns
   [1] sum-of-squares of prediction errors
   [2] either one step ahead prediction (if n-predictions is 0)
       or vector of length n-predictions if n-predictions > 0)
   [3] vector with smoothed values
       (nil if return-smoothed-values-p is true)
---
Note: see Section 7.3 of ``Time Series: A Biostatistical Introduction''
by Diggle, 1990"
  (let ((n-ts (length time-series))
        (sum-of-squares-of-prediction-errors 0.0)
        (current-prediciton initial-prediction)
        current-prediction-error)
    (dotimes (i n-ts)
      (if return-smoothed-values-p
        (setf (elt result-smoothed i) current-prediciton))
      (setf current-prediction-error (- (elt time-series i) current-prediciton)
            current-prediciton (+ current-prediciton
                                  (* alpha current-prediction-error)))
      (incf sum-of-squares-of-prediction-errors
            (* current-prediction-error current-prediction-error)))
    (if (plusp n-predictions)
      (let ((last-value-in-time-series (elt time-series (1- n-ts)))
            (1-alpha (- 1.0 alpha)))
        (dotimes (i n-predictions (values
                                   sum-of-squares-of-prediction-errors
                                   result-predictions
                                   result-smoothed))
          (setf (elt result-predictions i) current-prediciton
                current-prediciton (+ (* alpha last-value-in-time-series)
                                      (* 1-alpha current-prediciton)))))
      (values
       sum-of-squares-of-prediction-errors
       current-prediciton
       result-smoothed))))

#|
(exponential-smoothing 
 #(71.0 63.0 70.0 88.0 99.0 90.0 110.0)
 0.2)
;==> 2064.469974937598
;    86.558592
;    #(71.0 71.0 69.4 69.52000000000001 73.21600000000001 78.37280000000001 80.69824000000001)

(exponential-smoothing 
 #(71.0 63.0 70.0 88.0 99.0 90.0 110.0)
 0.9)
;==> 1043.8179815444005
;    108.077138
;    #(71.0 71.0 63.8 69.38 86.138 97.7138 90.77138)

(exponential-smoothing 
 #(71.0 63.0 70.0 88.0 99.0 90.0 110.0)
 0.9
 :n-predictions 10)
;==> 1043.8179815444005
;    #(108.077138 109.8077138 109.98077138 109.998077138 109.9998077138 109.99998077138 109.999998077138 109.99999980771379 109.99999998077138 109.99999999807713)
;    #(71.0 71.0 63.8 69.38 86.138 97.7138 90.77138)
|#

;-------------------------------------------------------------------------------
(defun running-median
       (time-series
        K
        &key
        (result (make-array (- (length time-series) (1- K)))))
  "given
   [1] time-series (required)
       ==> a sequence of numbers
   [2] K (required)
       ==> positive integer giving the number of points
           in the running median
   [3] result (keyword; vector of appropriate length)
       <== a sequence to hold running medians
           of time series
return
   [1] result, the vector of running medians
        of K consecutive points"
  (dotimes (i (- (length time-series) (1- K)) result)
    (setf (aref result i)
          (sample-median (subseq time-series i (+ i K))))))

#|
(running-median #(71.0 63.0 70.0 88.0 99.0 90.0 110.0) 3)
;==> #(70.0 70.0 88.0 90.0 99.0)

(running-median #(71.0 63.0 70.0 88.0 99.0 90.0 110.0) 4)
;==> #(70.5 79.0 89.0 94.5)
|#

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;;  The function   center&prewhiten&taper-time-series
;;;  handles centering, prewhitening and tapering of a time series
;;;  (if just centering and tapering are needed, it is probably
;;;  best to use center&taper-time-series in tapering.lisp).
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
(defun center&prewhiten&taper-time-series
       (time-series
        &key
        (center-data t)  ;t, nil or value to be subtracted off ...
        (start 0)
        (end (length time-series))
        (prewhitening-filter nil)
        (recenter-after-prewhitening-p t)
        (data-taper nil)
        (data-taper-parameters)
        (recenter-after-tapering-p t)
        (restore-power-option-p t)
        (result (make-array
                 (if prewhitening-filter
                   (- end start (1- (length prewhitening-filter)))
                   (- end start)))))
  "given
   [1] time-series (required)
       ==> a sequence of time series values
   [2] center-data (keyword; t)
       ==> if t, subtract sample mean from time series;
           if a number, subtracts that number from time series;
           if nil, time-series is not centered
   [3] start (keyword; 0)
       ==> start index of sequence to be used
   [4] end (keyword; length of time-series)
       ==> 1 + end index of sequence to be used
   [5] prewhitening-filter (keyword; nil)
       ==> vector with coeffients of prewhitening filter
           or nil (if no prewhitening is to be done)
   [6] recenter-after-prewhitening-p (keyword; t)
       ==> if t, prewhitened series is centered using sample mean
           (not used if  prewhitening-filter is nil)
   [7] data-taper (keyword; nil)
       ==> nil or a tapering function
   [8] data-taper-parameters (keyword)
       ==> parameters for tapering function (not used
           if data-taper is nil)
   [9] recenter-after-tapering-p (keyword; t)
       ==> if t and data-taper is a function,
           centers tapered series by subtracting
           off its sample mean
  [10] restore-power-option-p (keyword; t)
       ==> if t and data-taper is a function,
           normalizes tapered series to have same
           sum of squares as before tapering
  [11] result (keyword; vector of size (- end start))
       <== sequence to hold centered, prewhitened
           and tapered time series
returns
   [1] result, the sequence containing the centered, prewhitened
       and tapered time series (time-series are unaltered unless
       it is bound to result)
   [2] the number used to center the time series
       (this is nil if center-data is nil)
   [3] C_h, the variance inflation factor due to the data taper
---
Note: see also center&taper-time-series in tapers.lisp"
  (cond
   ((not prewhitening-filter)
    (center&taper-time-series
     time-series
     :center-data center-data
     :start start
     :end end
     :data-taper data-taper
     :data-taper-parameters data-taper-parameters
     :recenter-after-tapering-p recenter-after-tapering-p
     :restore-power-option-p restore-power-option-p
     :result result))
   (t
    ;;; Because result might not be long enough to hold the
    ;;; the centered time series, we must create an intermediate
    ;;; array (since we don't want to trash time-series. The call
    ;;; to subseq thus does two things at once for us:
    ;;;       [1] subsets the time series and
    ;;;       [2] creates a NEW sequence so that
    ;;;           time-series is NOT trashed.
    (setf time-series (subseq time-series start end))
    (let ((center-factor nil)
          (C_h 1.0)
          (N (- end start (1- (length prewhitening-filter)))))
      ;;; subtract the sample mean from time series if required
      (when center-data
        (setf center-factor (if (numberp center-data)
                              center-data
                              (sample-mean time-series)))
        (x+b! time-series (- center-factor)))
      ;;; prewhiten time series ...
      (filter-time-series time-series prewhitening-filter :result result)
      ;;; Note: prewhitening can reintroduce a nonzero sample mean,
      ;;;       so here we remove it if so desired.
      (if recenter-after-prewhitening-p
        (let ((recenter-factor (sample-mean result :end N)))
          (dotimes (i N)
            (decf (elt result i) recenter-factor))))
      ;;; taper the time series if required
      (when data-taper
        (multiple-value-bind (junk more-junk local-C_h)
                             (center&taper-time-series
                              result
                              :center-data nil
                              :end N
                              :data-taper data-taper
                              :data-taper-parameters data-taper-parameters
                              :recenter-after-tapering-p recenter-after-tapering-p
                              :restore-power-option-p restore-power-option-p
                              :result result)
          (declare (ignore junk more-junk))
          (setf C_h local-C_h)))
      (values result center-factor C_h)))))

#|
(defvar *20-point-time-series*
  #(71.0 63.0 70.0 88.0 99.0 90.0 110.0 135.0 128.0 154.0
    156.0 141.0 131.0 132.0 141.0 104.0 136.0 146.0 124.0 129.0))
(sample-mean-and-variance *20-point-time-series*)
;==> 117.4
;    782.6399999999998

(center&prewhiten&taper-time-series
 *20-point-time-series*)
;==> #(-46.400000000000006 -54.400000000000006 -47.400000000000006
       -29.400000000000006 -18.400000000000006 -27.400000000000006
        -7.400000000000006 17.599999999999994 10.599999999999994
        36.599999999999994 38.599999999999994 23.599999999999994
        13.599999999999994 14.599999999999994 23.599999999999994
       -13.400000000000006 18.599999999999994 28.599999999999994
         6.599999999999994 11.599999999999994)
;    117.4
;    1.0
(sum (center&prewhiten&taper-time-series *20-point-time-series*))
;==> -1.1368683772161603E-13

(center&prewhiten&taper-time-series
 *20-point-time-series*
 :start 5 :end 15)
;==> #(-41.80000000000001 -21.80000000000001 3.1999999999999886
       -3.8000000000000114 22.19999999999999 24.19999999999999
       9.199999999999989 -0.8000000000000114 0.19999999999998863
       9.199999999999989)
;     131.8
;     1.0
(sum (center&prewhiten&taper-time-series
      *20-point-time-series*
      :start 5 :end 15))
;==> -1.1368683772161603E-13

(center&prewhiten&taper-time-series
 *20-point-time-series*
 :start 5 :end 15 :prewhitening-filter #(-1 1)
 :recenter-after-prewhitening-p nil)
;==> #(-20.0 -25.0 7.0 -26.0 -2.0 15.0 10.0 -1.0 -9.0)
;    131.8
;    1.0
(sample-mean (center&prewhiten&taper-time-series
              *20-point-time-series*
              :start 5 :end 15 :prewhitening-filter #(-1 1)
              :recenter-after-prewhitening-p nil))
;==> -5.666666666666667

(center&prewhiten&taper-time-series
 *20-point-time-series*
 :start 5 :end 15 :prewhitening-filter #(-1 1))
;==> #(-14.333333333333332 -19.333333333333332 12.666666666666668
       -20.333333333333332 3.666666666666667 20.666666666666668
        15.666666666666668 4.666666666666667 -3.333333333333333)
;    131.8
;    1.0
(sample-mean-and-variance (center&prewhiten&taper-time-series
                           *20-point-time-series*
                           :start 5 :end 15 :prewhitening-filter #(-1 1)))
;==> 5.921189464667501E-16
;    208.0

(center&prewhiten&taper-time-series
 *20-point-time-series*
 :start 5 :end 15 :prewhitening-filter #(-1 1)
 :data-taper #'dpss-data-taper!
 :data-taper-parameters 1.0)
;==> #(-8.029535056948914 -15.939710264707552 11.111941111527308
       -25.99193293330027 2.981287578952983 23.07865115777016
       14.13595435534983 1.7916496140314773 -3.13830556267502)
;    131.8
;    1.327454606904972
(sample-mean-and-variance (center&prewhiten&taper-time-series
                           *20-point-time-series*
                           :start 5 :end 15 :prewhitening-filter #(-1 1)
                           :data-taper #'dpss-data-taper!
                           :data-taper-parameters 1.0))
;==> 0.0
;    207.99999999999997

(center&prewhiten&taper-time-series
 *20-point-time-series*
 :start 5 :end 15 :prewhitening-filter #(-1 1)
 :data-taper #'dpss-data-taper!
 :data-taper-parameters 1.0
 :restore-power-option-p nil)
;==> #(-8.525293352136908 -16.92385735797956 11.798012838297177
       -27.596722783305598 3.1653577694589425 24.503569620927365
       15.00873423393055 1.902269363733894 -3.3320703329258587)
;    131.8
;    1.327454606904972
(sample-mean-and-variance (center&prewhiten&taper-time-series
                           *20-point-time-series*
                           :start 5 :end 15 :prewhitening-filter #(-1 1)
                           :data-taper #'dpss-data-taper!
                           :data-taper-parameters 1.0
                           :restore-power-option-p nil))
;==> 5.427757009278544E-16
;    234.4775142934783

(center&prewhiten&taper-time-series
 *20-point-time-series*
 :start 5 :end 15 :prewhitening-filter #(-1 1)
 :data-taper #'dpss-data-taper!
 :data-taper-parameters 1.0
 :recenter-after-tapering-p nil
 :restore-power-option-p nil)
;==> #(-6.766926964426519 -15.165490970269172 13.556379226007566
       -25.83835639559521 4.923724157169332 26.261936008637754
        16.76710062164094 3.6606357514442833 -1.5737039452154695)
;    131.8
;    1.327454606904972
(sample-mean-and-variance (center&prewhiten&taper-time-series
                           *20-point-time-series*
                           :start 5 :end 15 :prewhitening-filter #(-1 1)
                           :data-taper #'dpss-data-taper!
                           :data-taper-parameters 1.0
                           :recenter-after-tapering-p nil
                           :restore-power-option-p nil))
;==> 1.7583663877103892
;    234.4775142934783
|#

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;;  Everything below here consists of internal symbols in the SAPA package
;;;  and should be regarded as "dirty laundry" ...
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;; Here we attempt to ROUGHLY count the number of operations to do
;;; filtering using the direct and fft methods for different combinations
;;; of filter length and sample size.  This function is used
;;; by filter-time-series to choose between the fft and direct way
;;; of computing a filtered time series.
(defun which-is-faster? (N-filter N-ts)
  (let* ((N-output (-  N-ts (1- N-filter)))
         (fft-size (* 4 (next-power-of-2 N-filter)))
         (fft-cost (* 1.5 fft-size (log fft-size 2))))
    (if (< (* N-output N-filter)
           (+ fft-cost
              (* 2 fft-cost
                 (/ (1+ (- fft-size N-filter)) N-output))))
      :direct
      :fft)))
