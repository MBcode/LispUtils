;;;-*- Mode: LISP; Package: :SAPA; Syntax: COMMON-LISP -*-
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;
;  nonparametric.lisp
;
;  a collection of Lisp functions for nonparametric spectral estimation ...
;  Note:  before compiling and loading nonparametric.lisp,
;         you should compile and load (in the order listed)
;            sapa-package.lisp, utilities.lisp, basic-math.lisp,
;            basic-statistics.lisp, dft-and-fft.lisp, tapers.lisp,
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
;;; (compile-file "ccl:SAPA;nonparametric.lisp")
;;; (load "ccl:SAPA;nonparametric.fasl")
;-------------------------------------------------------------------------------
(if (not (find-package :SAPA))
  (defpackage :SAPA (:USE :COMMON-LISP)))

(in-package :SAPA)

(export '(;;; functions to compute nonparametric spectral estimates ...
          periodogram
          direct-spectral-estimate
          lag-window-spectral-estimate
          wosa-spectral-estimate

          ;;; function to correct a spectral estimate for prewhitening ...
          postcolor-spectral-estimate

          ;;; functions to compute spectral and smoothing windows ...
          spectral-window-for-direct-spectral-estimate
          spectral-window-for-lag-window-spectral-estimate
          smoothing-window-for-lag-window-spectral-estimate

          ;;; functions to bandwidths, degrees of freedom, etc. ...
          Grenander-smoothing-window-bandwidth
          Jenkins-smoothing-window-bandwidth
          equivalent-degrees-of-freedom
          bandwidth&confidence-intervals-for-sdf-dB
          
          ;;; functions useful for objectively choosing lag window parameter ...
          time-series-bandwidth
          sample-cepstrum
          cepstrum->I_m          

          ;;; functions to compute the cumulative periodogram test statistic ...
          cumulative-periodogram
          quantile-of-Kolmogorov-test-statistic

          ;;; functions to convert from one- to two-sided representation ...
          one-sided-freq->two-sided-freq
          one-sided-sdf->two-sided-sdf

          ;;; functions concerning specific lag windows ...
          bartlett-lag-window
          bartlett-m->bandwidth
          bartlett-bandwidth->m
          bartlett-N-m->degrees-of-freedom
          daniell-lag-window
          daniell-m->bandwidth
          daniell-bandwidth->m
          daniell-N-m->degrees-of-freedom
          parzen-lag-window
          parzen-m->bandwidth
          parzen-bandwidth->m
          parzen-N-m->degrees-of-freedom
          papoulis-lag-window
          papoulis-m->bandwidth
          papoulis-bandwidth->m
          papoulis-N-m->degrees-of-freedom
          ))

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;;  The functions  periodogram
;;;                 direct-spectral-estimate
;;;                 lag-window-spectral-estimate
;;;                 wosa-spectral-estimate
;;;  compute four common nonparametric spectral density estimates.
;;;  The first two functions take a time series as input and return
;;;  a direct spectral estimate (the periodogram is a special case of such 
;;;  an estimate).  The function lag-window-spectral-estimate takes an estimate
;;;  of the autocovariance sequence (acvs) and returns a lag window spectral
;;;  estimate.  Note that there is an option in direct-spectral-estimate for
;;;  returning the acvs estimate corresponding to a direct spectral estimate
;;;  (the acvs estimate corresponding to the periodogram is just the usual
;;;  biased estimator of the acvs, which can be computed using the Lisp
;;;  function acvs).  The function wosa-spectral-estimate computes
;;;  a nonparametric estimate using Welch's overlapped segment averaging (wosa)
;;;  technique.   Chapter 6 of the SAPA book is devoted to nonparametric
;;;  spectral estimation.
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
(defun periodogram
       (time-series
        &key
        (center-data t)
        (start 0)
        (end (length time-series))
        (N-nonzero-freqs :half-next-power-of-2)
        (return-est-for-0-freq-p nil)
        (sampling-time 1.0)
        (scratch-dft (make-array (get-N-dft N-nonzero-freqs (- end start))))
        (sdf-transformation #'convert-to-dB)
        (result-sdf (make-array (get-N-freqs N-nonzero-freqs (- end start)
                                             return-est-for-0-freq-p)))
        (return-frequencies-p t)
        (freq-transformation nil)
        (result-freq (if return-frequencies-p
                       (make-array (get-N-freqs N-nonzero-freqs (- end start)
                                                return-est-for-0-freq-p)))))
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
   [5] N-nonzero-freqs (keyword; :half-next-power-of-2)
       ==> specifies at how many nonzero frequencies
           periodogram is to be computed -- choices are:
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
   [6] return-est-for-0-freq-p (keyword; nil)
       ==> if t, periodogram is computed at zero frequency;
           otherwise, it is not computed.
   [7] sampling-time (keyword; 1.0)
       ==> sampling time (called delta t in the SAPA book)
   [8] scratch-dft (keyword; vector of correct length)
       ==> vector in which the in-place dft is done
   [9] sdf-transformation (keyword; #'convert-to-dB)
       ==> a function of one argument or nil;
           if bound to a function, the function is used
           to transform all elements of result-sdf
  [10] result-sdf (keyword; vector of correct length)
       <== vector into which periodogram is placed;
           it must be exactly of the length dictated
           by N-nonzero-freqs and return-est-for-0-freq-p
  [11] return-frequencies-p (keyword; t)
       ==> if t, the frequencies associated with the periodogram
           are computed and returned in result-freq
  [12] freq-transformation (keyword; nil)
       ==> a function of one argument or nil;
           if bound to a function, the function is used
           to transform all elements of result-freq
           (ignored unless return-frequencies-p is true)
  [13] result-freq (keyword; nil or vector of correct length)
       <== not used if return-frequencies-p nil; otherwise,
           vector of length dictated by
           N-nonzero-freqs and return-est-for-0-freq-p
           into which the frequencies associated with the values
           in result-sdf are placed
returns
   [1] result-sdf, a vector holding
       the properly transformed periodogram
   [2] result-freq (if return-frequencies-p is t),
       a vector holding the properly transformed
       frequencies associated with values in result-sdf
        -- or --
       nil (if return-frequencies-p is nil)
   [3] the length of the vector result-sdf
---
Note: see Section 6.3 of the SAPA book"
  (let* ((sample-size (- end start))
         (N-freqs (get-N-freqs N-nonzero-freqs sample-size
                               return-est-for-0-freq-p))
         (N-dft (get-N-dft N-nonzero-freqs sample-size))
         (offset (if return-est-for-0-freq-p 0 1))
         (fiddle-factor-sdf (/ sampling-time sample-size))
         (fiddle-factor-freq (/ (* N-dft sampling-time))))
    ;;; put centered series into first part of scratch-dft
    (center&taper-time-series time-series
                              :center-data center-data
                              :start start
                              :end end
                              :result scratch-dft)
    ;;; zero the rest of scratch-dft ...
    (fill scratch-dft 0.0 :start sample-size :end N-dft)
    (dft! scratch-dft :N N-dft)
    (dotimes (i N-freqs)
      (setf (aref result-sdf i)
            (* fiddle-factor-sdf
               (expt (abs (aref scratch-dft (+ i offset))) 2))))
    (if sdf-transformation
      (transform-a-sequence! sdf-transformation result-sdf))
    (when return-frequencies-p
      (dotimes (i N-freqs)
        (setf (aref result-freq i)
              (* fiddle-factor-freq (float (+ i offset)))))
      (if freq-transformation
        (transform-a-sequence! freq-transformation result-freq)))
    (values result-sdf result-freq N-freqs)))

#|
;;; Here we test periodogram via the Parseval result stated
;;; in Exercise [6.6c] of the SAPA book ...
(dolist (N '(5 8 9 22 32 63 64 65) (values))
  (let* ((time-series (x+b! (ranorms N) 100))
         (sigma^2 (sample-variance time-series))
         (sampling-time 0.25)
         (the-periodogram (periodogram
                           time-series
                           :N-nonzero-freqs :Fourier
                           :sdf-transformation nil
                           :sampling-time sampling-time))
         (N-f (length the-periodogram))
         (Parseval-sum (if (evenp N)
                         (/ (+ (* 2 (sum the-periodogram :end (1- N-f)))
                               (svref the-periodogram (1- N-f)))
                            (* N sampling-time))
                         (/ (* 2 (sum the-periodogram))
                            (* N sampling-time)))))
    (format t "~& N = ~2D, N-f = ~2D: ~11,8F ~11,8F ~F"
            N N-f sigma^2 Parseval-sum (/ Parseval-sum sigma^2))))
;==>
 N =  5, N-f =  2:  0.51511055  0.51511055 1.0000000000000004
 N =  8, N-f =  4:  0.68129255  0.68129255 0.9999999999999999
 N =  9, N-f =  4:  0.97793576  0.97793576 0.9999999999999989
 N = 22, N-f = 11:  0.92517636  0.92517636 1.000000000000006
 N = 32, N-f = 16:  0.74719322  0.74719322 0.999999999999999
 N = 63, N-f = 31:  0.96915207  0.96915207 1.0000000000000078
 N = 64, N-f = 32:  1.12571507  1.12571507 1.0000000000000024
 N = 65, N-f = 32:  0.64325949  0.64325949 1.000000000000028
;;; Note: the important thing here is that the last column of numbers
;;;       be close to unity -- since we are using random numbers,
;;;       these three columns of numbers will change each time
;;;       the above Lisp form is evaluated.

;;; For this example we use the 20 point time series of Section 6.16
;;; of the SAPA book:
(let ((20-pt-ts #(71.0  63.0  70.0  88.0  99.0  90.0 110.0 135.0 128.0 154.0
                  156.0 141.0 131.0 132.0 141.0 104.0 136.0 146.0 124.0 129.0)))
  (multiple-value-bind (the-periodogram freqs N-f)
                       (periodogram 
                        20-pt-ts
                        :N-nonzero-freqs :Fourier
                        :sampling-time 0.25)
    (dotimes (i N-f)
      (format t "~&~6,4F: ~8,4F"
              (svref freqs i)
              (svref the-periodogram i)))
    (values)))
;==>
0.2000:  30.8586
0.4000:  24.9217
0.6000:  21.0531
0.8000:  19.1324
1.0000:  12.5527
1.2000:  18.2452
1.4000:  12.7906
1.6000:  19.9132
1.8000:  11.6759
2.0000:   5.0515
|#

;-------------------------------------------------------------------------------
(defun direct-spectral-estimate
       (time-series
        &key
        (center-data t)
        (start 0)
        (end (length time-series))
        (N-nonzero-freqs :half-next-power-of-2)
        (return-est-for-0-freq-p nil)
        (sampling-time 1.0)
        (scratch-dft (make-array (get-N-dft N-nonzero-freqs (- end start))))
        (data-taper nil)
        (data-taper-parameters)
        (recenter-after-tapering-p t)
        (restore-power-option-p t)
        (return-acvs-p nil)
        (result-acvs (make-array (- end start)))
        (sdf-transformation #'convert-to-dB)
        (result-sdf (make-array (get-N-freqs N-nonzero-freqs (- end start)
                                             return-est-for-0-freq-p)))
        (return-frequencies-p t)
        (freq-transformation nil)
        (result-freq (if return-frequencies-p
                       (make-array (get-N-freqs N-nonzero-freqs (- end start)
                                                return-est-for-0-freq-p)))))
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
   [5] N-nonzero-freqs (keyword; :half-next-power-of-2)
       ==> specifies at how many nonzero frequencies
           direct spectral estimate is to be computed -- choices are:
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
   [6] return-est-for-0-freq-p (keyword; nil)
       ==> if t, sdf is computed at zero frequency;
           otherwise, it is not computed.
   [7] sampling-time (keyword; 1.0)
       ==> sampling time (called delta t in the SAPA book)
   [8] scratch-dft (keyword; vector of correct length)
       ==> vector in which the in-place dft is done
   [9] data-taper (keyword; nil)
       ==> nil or a tapering function
  [10] data-taper-parameters (keyword)
       ==> parameters for tapering function (not used
           if data-taper is nil)
  [11] recenter-after-tapering-p (keyword; t)
       ==> if t and data-taper is a function,
           centers tapered series by subtracting
           off its sample mean
  [12] restore-power-option-p (keyword; t)
       ==> if t and data-taper is a function,
           normalizes tapered series to have same
           sum of squares as before tapering
  [13] return-acvs-p (keyword; nil)
       ==> if t, computes acvs corresponding
           to direct spectral estimate
  [14] result-acvs (keyword; vector of correct length)
       <== vector into which acvs values are placed
           (not used if return-acvs-estimate-p is nil)
  [15] sdf-transformation (keyword; #'convert-to-dB)
       ==> a function of one argument or nil;
           if bound to a function, the function is used
           to transform all elements of result-sdf
  [16] result-sdf (keyword; vector of correct length)
       <== vector into which direct spectral estimates are placed;
           it must be exactly of the length dictated
           by N-nonzero-freqs and return-est-for-0-freq-p
  [17] return-frequencies-p (keyword; t)
       ==> if t, the frequencies associated with the spectral estimate
           are computed and returned in result-freq
  [18] freq-transformation (keyword; nil)
       ==> a function of one argument or nil;
           if bound to a function, the function is used
           to transform all elements of result-freq
           (ignored unless return-frequencies-p is true)
  [19] result-freq (keyword; nil or vector of correct length)
       <== not used if return-frequencies-p nil; otherwise,
           vector of length dictated by
           N-nonzero-freqs and return-est-for-0-freq-p
           into which the frequencies associated with the values
           in result-sdf are placed
returns
   [1] result-sdf, a vector holding
       the properly transformed sdf
   [2] result-freq (if return-frequencies-p is t),
       where result-freq is a vector holding
       the properly transformed frequencies
       associated with values in result-sdf
        -- or --
       nil (if return-frequencies-p is nil)
   [3] the length of the vector result-sdf
   [4] C_h, the variance inflation factor due to the data taper
   [5] result-acvs (if return-acvs-p is t),
       a vector holding the acvs corresponding
       to the direct spectral estimate
        -- or --
       nil (if return-acvs-p is nil)
---
Note: see Section 6.3 of the SAPA book"
  (let* ((sample-size (- end start))
         (N-freqs (get-N-freqs N-nonzero-freqs sample-size
                               return-est-for-0-freq-p))
         (N-dft (get-N-dft N-nonzero-freqs sample-size))
         (offset (if return-est-for-0-freq-p 0 1))
         (fiddle-factor-sdf (/ sampling-time sample-size))
         (fiddle-factor-freq (/ (* N-dft sampling-time))))
    ;;; put centered & tapered series into first part of scratch-dft
    (multiple-value-bind (junk more-junk C_h)
                         (center&taper-time-series
                          time-series
                          :center-data center-data
                          :start start
                          :end end
                          :data-taper data-taper
                          :data-taper-parameters data-taper-parameters
                          :recenter-after-tapering-p recenter-after-tapering-p
                          :restore-power-option-p restore-power-option-p
                          :result scratch-dft)
      (declare (ignore junk more-junk))
      (if return-acvs-p
        (acvs scratch-dft
              :end sample-size
              :center-data-p nil
              :result result-acvs))
      ;;; zero the rest of scratch-dft ...
      (fill scratch-dft 0.0 :start sample-size :end N-dft)
      (dft! scratch-dft :N N-dft)
      (dotimes (i N-freqs)
        (setf (aref result-sdf i)
              (* fiddle-factor-sdf
                 (expt (abs (aref scratch-dft (+ i offset))) 2))))
      (if sdf-transformation
        (transform-a-sequence! sdf-transformation result-sdf))
      (when return-frequencies-p
        (dotimes (i N-freqs)
          (setf (aref result-freq i)
                (* fiddle-factor-freq (float (+ i offset)))))
        (if freq-transformation
          (transform-a-sequence! freq-transformation result-freq)))
      (values result-sdf result-freq N-freqs C_h result-acvs))))

#|
;;; Here we test the direct spectral estimate using the Parseval result
;;; analogous to the one we used to check the periodogram ... 
(dolist (N '(5 8 9 22 32 63 64 65) (values))
  (let* ((time-series (x+b! (ranorms N) 100))
         (sigma^2 (sample-variance time-series))
         (sampling-time 0.25)
         (the-sdf-est (direct-spectral-estimate
                       time-series
                       :data-taper #'dpss-data-taper!
                       :data-taper-parameters 4.0
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
 N =  5, N-f =  2:  0.42023448  0.42023448 1.0000000000000004
 N =  8, N-f =  4:  0.66547264  0.66547264 1.0000000000000004
 N =  9, N-f =  4:  0.46234261  0.46234261 0.9999999999999981
 N = 22, N-f = 11:  0.58047776  0.58047776 1.0000000000000089
 N = 32, N-f = 16:  1.46881228  1.46881228 0.9999999999999989
 N = 63, N-f = 31:  1.03414687  1.03414687 1.0000000000000056
 N = 64, N-f = 32:  0.53556515  0.53556515 1.000000000000004
 N = 65, N-f = 32:  1.12152795  1.12152795 1.0000000000000302
;;; Note: again, the important thing is that the last column of numbers
;;;       be close to unity -- since we are using random numbers,
;;;       these three columns of numbers will change each time
;;;       the above Lisp form is evaluated.
|#

;-------------------------------------------------------------------------------
(defun lag-window-spectral-estimate
       (acvs
        lag-window-function
        &key
        (max-lag (1- (length acvs)))
        (N-ts (length acvs))
        (N-nonzero-freqs :half-next-power-of-2)
        (return-est-for-0-freq-p nil)
        (sampling-time 1.0)
        (scratch-dft (make-array (get-N-dft N-nonzero-freqs (1+ max-lag))))
        (C_h 1.0)
        (sdf-transformation #'convert-to-dB)
        (result-sdf (make-array (get-N-freqs N-nonzero-freqs (1+ max-lag)
                                             return-est-for-0-freq-p)))
        (return-frequencies-p t)
        (freq-transformation nil)
        (result-freq (if return-frequencies-p
                       (make-array (get-N-freqs N-nonzero-freqs (1+ max-lag)
                                                return-est-for-0-freq-p)))))
  "given
   [1] acvs (required)
       ==> vector containing autocovariance sequence
   [2] lag-window-function (required)
       ==> function of a single variable that computes the value
           of the lag window for a given lag
   [3] max-lag (keyword; (1- (length acvs)))
       ==> maximum lag in acvs to be used
   [4] N-ts (keyword; length of acvs)
       ==> length of the time series from which acvs was constructed;
           this is needed to compute equivalent degrees of freedom
   [5] N-nonzero-freqs (keyword; :half-next-power-of-2)
       ==> specifies at how many nonzero frequencies
           direct spectral estimate is to be computed -- choices are:
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
   [6] return-est-for-0-freq-p (keyword; nil)
       ==> if t, sdf is computed at zero frequency;
           otherwise, it is not computed.
   [7] sampling-time (keyword; 1.0)
       ==> sampling time (called delta t in the SAPA book)
   [8] scratch-dft (keyword; vector of correct length)
       ==> vector in which the in-place dft is done
   [9] C_h (keyword; 1.0)
       ==> variance inflation factor due to tapering
  [10] sdf-transformation (keyword; #'convert-to-dB)
       ==> a function of one argument or nil;
           if bound to a function, the function is used
           to transform all elements of result-sdf
  [11] result-sdf (keyword; vector of correct length)
       <== vector into which lag window spectral estimates are placed;
           it must be exactly of the length dictated
           by N-nonzero-freqs and return-est-for-0-freq-p
  [12] return-frequencies-p (keyword; t)
       ==> if t, the frequencies associated with the spectral estimate
           are computed and returned in result-freq
  [13] freq-transformation (keyword; nil)
       ==> a function of one argument or nil;
           if bound to a function, the function is used
           to transform all elements of result-freq
           (ignored unless return-frequencies-p is true)
  [14] result-freq (keyword; nil or vector of correct length)
       <== not used if return-frequencies-p nil; otherwise,
           vector of length dictated by
           N-nonzero-freqs and return-est-for-0-freq-p
           into which the frequencies associated with the values
           in result-sdf are placed
returns
   [1] result-sdf, a vector holding
       the properly transformed sdf
   [2] result-freq (if return-frequencies-p is t),
       where result-freq is a vector holding
       the properly transformed frequencies
       associated with values in  result-sdf
        -- or --
       nil (if return-frequencies-p is nil)
   [3] the length of the vector result-sdf
   [4] the equivalent degrees of freedom
   [5] the smoothing window bandwidth
---
Note: see Section 6.7 of the SAPA book"
  ;;; Note: in what follows, we assume that the lag window
  ;;;       at lag 0 is unity (see Equation (240a) of the SAPA book)
  (let* ((N-acvs (1+ max-lag))
         (N-freqs (get-N-freqs N-nonzero-freqs N-acvs
                               return-est-for-0-freq-p))
         (N-dft (get-N-dft N-nonzero-freqs N-acvs))
         (offset (if return-est-for-0-freq-p 0 1))
         (fiddle-factor-freq (/ (* N-dft sampling-time)))
         (B_W-bot 1.0))
    (cond
     ((<= N-dft (* 2 max-lag))
      (setf (aref scratch-dft 0) (aref acvs 0))
      (let ((tau 1))
        (dotimes (i max-lag)
          (let ((lag-window-value (funcall lag-window-function tau)))
            (incf B_W-bot (* 2 (expt lag-window-value 2)))
            (setf (aref scratch-dft tau)
                  (* 2 lag-window-value (aref acvs tau))))
          (incf tau)))
      (fill scratch-dft 0.0 :start max-lag :end N-dft))
     (t
      (let ((tau 1)
            (tau-backward (1- N-dft)))
        (dotimes (i max-lag)
          (let ((lag-window-value (funcall lag-window-function tau)))
            (incf B_W-bot (* 2 (expt lag-window-value 2)))
            (setf (aref scratch-dft tau)
                  (setf (aref scratch-dft tau-backward)
                        (* lag-window-value (aref acvs tau))))
            (incf tau)
            (decf tau-backward)))
        (fill scratch-dft 0.0 :start tau :end (1+ tau-backward)))))
    (dft! scratch-dft :N N-dft)
    (dotimes (i N-freqs)
      (setf (aref result-sdf i)
            (* sampling-time
               (realpart (aref scratch-dft (+ i offset))))))
    (if sdf-transformation
      (transform-a-sequence! sdf-transformation result-sdf))
    (when return-frequencies-p
      (dotimes (i N-freqs)
        (setf (aref result-freq i)
              (* fiddle-factor-freq (float (+ i offset)))))
      (if freq-transformation
        (transform-a-sequence! freq-transformation result-freq)))
    (let ((B_W (/ (* sampling-time B_W-bot))))
      (values result-sdf
              result-freq
              N-freqs
              (/ (* 2 N-ts B_W sampling-time) C_h)  ;equivalent dof
              B_W                                   ;smoothing window bandwidth
              ))))

#|
;;; For this example we use the 20 point time series of Section 6.16
;;; of the SAPA book:
(let* ((20-pt-ts #(71.0  63.0  70.0  88.0  99.0  90.0 110.0 135.0 128.0 154.0
                   156.0 141.0 131.0 132.0 141.0 104.0 136.0 146.0 124.0 129.0))
       (sampling-time 0.25)
       (the-acvs (acvs 20-pt-ts))
       (Parzen-15-sdf-est (lag-window-spectral-estimate
                           the-acvs 
                           #'(lambda (tau)
                               (parzen-lag-window
                                tau 15))
                           :N-nonzero-freqs :Fourier
                           :sampling-time sampling-time))
       (Parzen-10-sdf-est (lag-window-spectral-estimate
                           the-acvs 
                           #'(lambda (tau)
                               (parzen-lag-window
                                tau 10))
                           :N-nonzero-freqs :Fourier
                           :sampling-time sampling-time)))
  (multiple-value-bind (Parzen-5-sdf-est freqs N-f)
                       (lag-window-spectral-estimate
                        the-acvs 
                        #'(lambda (tau)
                            (parzen-lag-window
                             tau 5))
                        :N-nonzero-freqs :Fourier
                        :sampling-time sampling-time)
    (dotimes (i N-f)
      (format t "~&~6,4F: ~8,4F ~8,4F ~8,4F"
              (svref freqs i)
              (svref Parzen-5-sdf-est i)
              (svref Parzen-10-sdf-est i)
              (svref Parzen-15-sdf-est i)))
    (values)))
;==>
0.2000:  27.0227  28.2599  28.8918
0.4000:  26.0828  26.1869  25.8978
0.6000:  24.5508  22.3966  20.3750
0.8000:  22.5325  18.8636  18.0633
1.0000:  20.2961  17.8869  17.7836
1.2000:  18.2975  17.5004  17.8190
1.4000:  16.8731  16.8297  16.3072
1.6000:  15.9210  15.8348  16.4136
1.8000:  15.2629  14.0008  13.5807
2.0000:  15.0010  12.3206   9.7972
|#

;-------------------------------------------------------------------------------
(defun wosa-spectral-estimate
       (time-series
        block-size
        &key
        (proportion-of-overlap 0.5)
        (oversampling-factor 1)
        (center-data t)
        (start 0)
        (end (length time-series))
        (return-est-for-0-freq-p t)
        (sampling-time 1.0)
        (scratch-dft (make-array (* oversampling-factor block-size)))
        (data-taper #'Hanning-data-taper!)
        (data-taper-parameters nil)
        (restore-power-option-p t)
        (sdf-transformation #'convert-to-dB)
        (result-sdf (make-array (wosa-get-N-freqs block-size
                                                  oversampling-factor
                                                  return-est-for-0-freq-p)))
        (return-sdf-estimates-for-each-block-p t)
        (return-frequencies-p t)
        (freq-transformation nil)
        (result-freq (if return-frequencies-p
                       (make-array (wosa-get-N-freqs block-size
                                                     oversampling-factor
                                                     return-est-for-0-freq-p)))))
  "given
   [1] time-series (required)
       ==> a vector of real-valued numbers
   [2] block-size (required)
       ==> a power of two
   [3] proportion-of-overlap (keyword; 0.5)
       ==> number greater than 0 and less than 1
   [4] oversampling-factor (keyword; 1)
       ==> a factor that controls the number of frequencies
           at which the wosa spectral estimate is computed;
           this factor should be an integer power of two
           such as 1, 2, 4, etc; for example,
           1 yields Fourier frequencies for block-size;
           2 yields grid twice as fine as Fourier frequencies;
           4 yields griid 4 times as fine as Fourier frequencies;
           etc.
   [5] center-data (keyword; t)
       ==> if t, subtract sample mean from time series;
           if a number, subtracts that number from time series;
           if nil, time-series is not centered
   [6] start (keyword; 0)
       ==> start index of vector to be used
   [7] end (keyword; length of time-series)
       ==> 1 + end index of vector to be used
   [8] return-est-for-0-freq-p (keyword; nil)
       ==> if t, sdf is computed at zero frequency;
           otherwise, it is not computed.
   [9] sampling-time (keyword; 1.0)
       ==> sampling time (called delta t in the SAPA book)
  [10] scratch-dft (keyword; vector of correct length)
       ==> vector in which the in-place dft is done
  [11] data-taper (keyword; #'Hanning-data-taper!)
       ==> a tapering function or nil
  [12] data-taper-parameters (keyword; nil)
       ==> parameters for tapering function (not used
           if data-taper is nil); the default of nil
           is appropriate for the Hanning data taper
           because it does not have any parameters
  [13] restore-power-option-p (keyword; t)
       ==> if t and data-taper is non-nil,
           normalizes tapered series to have same
           sum of squares as before tapering
  [14] sdf-transformation (keyword; #'convert-to-dB)
       ==> a function of one argument or nil;
           if bound to a function, the function is used
           to transform all elements of result-sdf
  [15] result-sdf (keyword; vector of correct length)
       <== vector into which wosa sdf estimate is placed;
           it must be EXACTLY of the length dictated
           by block-size, oversampling-factor and return-est-for-0-freq-p
  [16] return-sdf-estimates-for-each-block-p (keyword; t)
       ==> if t, individual spectra for each block are returned in a list;
           note that these spectra are untransformed
           (i.e., the option sdf-transformation applies only
           to the final wosa estimate)
  [17] return-frequencies-p (keyword; t)
       ==> if t, the frequencies associated with the spectral estimate
           are computed and returned in result-freq
  [18] freq-transformation (keyword; nil)
       ==> a function of one argument or nil;
           if bound to a function, the function is used
           to transform all elements of result-freq
           (ignored unless return-frequencies-p is true)
  [19] result-freq (keyword; nil or vector of correct length)
       <== not used if return-frequencies-p nil; otherwise,
           vector of length dictated by
           block-size, oversampling-factor and return-est-for-0-freq-p
           into which the frequencies associated with the values
           in result-sdf are placed
returns
   [1] wosa spectral estimate
   [2] associated frequencies
   [3] equivalent degrees of freedom
   [4] list of individual direct spectral estimates"
  (let* ((centered-time-series (center&taper-time-series
                                time-series
                                :center-data center-data
                                :start start
                                :end end))
         (N-dft (* oversampling-factor block-size))
         (N-freqs (wosa-get-N-freqs block-size
                                    oversampling-factor
                                    return-est-for-0-freq-p))
         (offset-freq (if return-est-for-0-freq-p 0 1))
         (sample-size (- end start))
         (number-of-blocks (calculate-number-of-blocks
                            sample-size block-size
                            proportion-of-overlap))
         (list-of-individual-sdfs '())
         (vector-with-data-taper (if data-taper
                                   (center&taper-time-series
                                    (make-array block-size
                                                :initial-element 1.0)
                                    :center-data nil
                                    :data-taper data-taper
                                    :data-taper-parameters data-taper-parameters
                                    :recenter-after-tapering-p nil
                                    :restore-power-option-p nil)
                                   (make-array block-size
                                               :initial-element 1.0)))
         (fiddle-factor-sdf (/ sampling-time block-size))
         (fiddle-factor-freq (/ (* N-dft sampling-time)))
         offset-block)
    (fill result-sdf 0.0)
    (dotimes (k number-of-blocks)
      (setf offset-block
            (get-offset-to-kth-block sample-size block-size number-of-blocks k))
      (replace scratch-dft
               centered-time-series
               :start2 offset-block
               :end2 (+ offset-block block-size))
      (fill scratch-dft 0.0 :start block-size :end N-dft)
      (when data-taper
        (let ((sum-of-squares-before (if restore-power-option-p
                                       (sum-of-squares
                                        scratch-dft
                                        :end block-size))))
          (map-into scratch-dft #'* scratch-dft vector-with-data-taper)
          (if restore-power-option-p
            (let ((mult-factor (sqrt (/ sum-of-squares-before
                                        (sum-of-squares
                                         scratch-dft
                                         :end block-size)))))
              (dotimes (i block-size)
                (multf (aref scratch-dft i) mult-factor))))))
      (dft! scratch-dft :N N-dft)
      (dotimes (i N-freqs)
        (setf (aref scratch-dft i)
              (* fiddle-factor-sdf
                 (expt (abs (aref scratch-dft (+ i offset-freq))) 2))))
      (if return-sdf-estimates-for-each-block-p
        (push (subseq scratch-dft 0 N-freqs) list-of-individual-sdfs))
      ;;; Note: here we make use of the assumption that result-sdf is
      ;;;       of exactly the correct size
      (map-into result-sdf #'+ result-sdf scratch-dft))
    (a*x! (/ number-of-blocks) result-sdf)
    (if sdf-transformation
      (transform-a-sequence! sdf-transformation result-sdf))
    (when return-frequencies-p
      (dotimes (i N-freqs)
        (setf (aref result-freq i)
              (* fiddle-factor-freq (float (+ i offset-freq)))))
      (if freq-transformation
        (transform-a-sequence! freq-transformation result-freq)))
    (values result-sdf
            result-freq
            (equivalent-dof-for-wosa sample-size
                                     block-size
                                     number-of-blocks
                                     vector-with-data-taper)
            (reverse list-of-individual-sdfs)
            )))

#|
;;; For this example we use the 20 point time series of Section 6.16
;;; of the SAPA book:
(let* ((20-pt-ts #(71.0  63.0  70.0  88.0  99.0  90.0 110.0 135.0 128.0 154.0
                   156.0 141.0 131.0 132.0 141.0 104.0 136.0 146.0 124.0 129.0))
       (sampling-time 0.25)
       (wosa-4 (wosa-spectral-estimate
                20-pt-ts
                4
                :oversampling-factor 4
                :sampling-time sampling-time)))
  (multiple-value-bind (wosa-8 freqs)
                       (wosa-spectral-estimate
                        20-pt-ts
                        8
                        :oversampling-factor 2
                        :sampling-time sampling-time)
    (dotimes (i (length freqs))
      (format t "~&~6,4F: ~8,4F ~8,4F"
              (svref freqs i)
              (svref wosa-4 i)
              (svref wosa-8 i))))
  (values))
;==>
0.0000:  26.9058  28.3938
0.2500:  26.4416  27.3650
0.5000:  25.0489  23.7518
0.7500:  22.7732  17.1140
1.0000:  19.9187  16.5887
1.2500:  17.4236  17.5460
1.5000:  16.1819  16.2470
1.7500:  15.7165  14.1435
2.0000:  15.5530  12.5478
|#

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;;  The function   postcolor-spectral-estimate
;;;  takes a spectral estimate for a prewhitened time series and corrects
;;;  it for the effect of prewhitening.  One caveat here is that the spectral
;;;  estimate is assumed to be unconverted (e.g., not expressed in decibel
;;;  units), so care must be taken to insure that this is true by setting
;;;  the sdf-transformation keyword to nil in whatever function is used
;;;  to compute the spectral estimate for the prewhitened time series.
;;;  Sections 6.5 and 9.10 of the SAPA book discuss prewhitening and
;;;  postcoloring.
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
(defun postcolor-spectral-estimate
       (precolored-sdf-estimate
        prewhitening-filter
        sample-size
        &key
        (N-nonzero-freqs :half-next-power-of-2)
        (includes-est-for-0-freq-p nil)
        (scratch-dft (make-array (get-N-dft N-nonzero-freqs sample-size)))
        (sdf-transformation #'convert-to-dB)
        (result-sdf (make-array (length precolored-sdf-estimate))))
  "given
   [1] precolored-sdf-estimate (required)
       ==> vector containing sdf estimate to be postcolored;
           note that this estimate is assumed to be untransformed
           (i.e., not expressed in dB, etc)
   [2] prewhitening-filter (required)
       ==> vector with coefficients of prewhitening filter
   [3] sample-size (required)
       ==> length of the time series from which precolored-sdf-estimate
           was constructed; this is needed to get corresponding
           grid of frequencies
   [4] N-nonzero-freqs (keyword; :half-next-power-of-2)
       ==> specifies at how many nonzero frequencies
           precolored-sdf-estimate was computed -- choices are:
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
   [5] includes-est-for-0-freq-p (keyword; nil)
       ==> if t, first element of precolored-sdf-estimate
           corresponds to zero frequency
   [6] scratch-dft (keyword; vector of correct length)
       ==> vector in which the in-place dft is done
   [7] sdf-transformation (keyword; #'convert-to-dB)
       ==> a function of one argument or nil;
           if bound to a function, the function is used
           to transform all elements of result-sdf
   [8] result-sdf (keyword; vector of correct length)
       <== vector into which postcolored sdf estimate is placed;
           it must be exactly the same length as precolored-sdf-estimate
returns
   [1] result-sdf, a vector holding
       the postcolored sdf estimate
---
Note: see Equation (438) of the SAPA book"
  (let ((N-freqs (get-N-freqs N-nonzero-freqs sample-size
                              includes-est-for-0-freq-p))
        (N-dft (get-N-dft N-nonzero-freqs sample-size))
        (offset (if includes-est-for-0-freq-p 0 1)))
    ;;; put prewhitening filter into first part of scratch-dft
    (copy-vector prewhitening-filter scratch-dft)
    ;;; zero the rest of scratch-dft ...
    (fill scratch-dft 0.0 :start (length prewhitening-filter) :end N-dft)
    (dft! scratch-dft :N N-dft)
    (dotimes (i N-freqs)
      (setf (aref result-sdf i)
            (/ (aref precolored-sdf-estimate i)
               (expt (abs (aref scratch-dft (+ i offset))) 2))))
    (if sdf-transformation
      (transform-a-sequence! sdf-transformation result-sdf))
    (values result-sdf)))

#|
;;; For this example we use the 20 point time series of Section 6.16
;;; of the SAPA book:
(let* ((20-pt-ts #(71.0  63.0  70.0  88.0  99.0  90.0 110.0 135.0 128.0 154.0
                   156.0 141.0 131.0 132.0 141.0 104.0 136.0 146.0 124.0 129.0))
       (N-ts (length 20-pt-ts))
       (sampling-time 0.25)
       (pgram-20-pt-ts-dB (periodogram 20-pt-ts :sampling-time sampling-time)))
  (multiple-value-bind (pgram-differenced-20-pt-ts freqs N-f)
                       (periodogram (difference 20-pt-ts)
                                    :sampling-time sampling-time
                                    :sdf-transformation nil)
    (let ((pgram-differenced-20-pt-ts-dB
           (postcolor-spectral-estimate pgram-differenced-20-pt-ts
                                        #(-1 1)
                                        (1- N-ts))))
      (dotimes (i N-f)
        (format t "~&~6,4F: ~8,4F ~8,4F"
                (svref freqs i)
                (svref pgram-20-pt-ts-dB i)
                (svref pgram-differenced-20-pt-ts-dB i)))
      (values))))
;==>
0.1250:  31.2344  30.7002
0.2500:  29.0872  26.2775
0.3750:  26.0741  20.3138
0.5000:  -8.9025  14.7938
0.6250:  21.0714  15.2171
0.7500:  16.4107   4.0106
0.8750:  18.5221  15.2630
1.0000:  12.5527   6.9111
1.1250:  20.5795  20.3538
1.2500:  16.3219  20.0302
1.3750:  15.2798   7.3147
1.5000:  14.1776  14.0639
1.6250:  19.3791  16.7867
1.7500:   8.9029  13.9247
1.8750:  10.9060   2.5244
2.0000:   5.0515   4.4350
|#

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;;  The functions  spectral-window-for-direct-spectral-estimate
;;;                 spectral-window-for-lag-window-spectral-estimate
;;;                 smoothing-window-for-lag-window-spectral-estimate
;;;  compute the spectral windows associated with direct and lag window
;;;  spectral estimates and the smoothing window associated with
;;;  a lag window spectral estimate.
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
(defun spectral-window-for-direct-spectral-estimate
       (sample-size
        &key
        (N-nonzero-freqs :twice-next-power-of-2)
        (sampling-time 1.0)
        (scratch-dft (make-array (get-N-dft N-nonzero-freqs sample-size)))
        (data-taper nil)
        (data-taper-parameters)
        (spec-wind-transformation #'careful-convert-to-dB)
        (result-spec-wind
         (make-array (get-N-freqs N-nonzero-freqs sample-size t)))
        (return-frequencies-p t)
        (freq-transformation nil)
        (result-freq
         (if return-frequencies-p
           (make-array (get-N-freqs N-nonzero-freqs sample-size t)))))
  "given
   [1] sample-size (required)
       ==> sample size for which spectral window is to be computed
   [2] N-nonzero-freqs (keyword; :twice-next-power-of-2)
       ==> specifies at how many nonzero frequencies
           spectral window is to be computed -- choices are:
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
   [3] sampling-time (keyword; 1.0)
       ==> sampling time (called delta t in the SAPA book)
   [4] scratch-dft (keyword; vector of correct length)
       ==> vector in which the in-place dft is done
   [5] data-taper (keyword; nil)
       ==> nil or a tapering function
   [6] data-taper-parameters (keyword)
       ==> parameters for tapering function (not used
           if data-taper is nil)
   [7] spec-wind-transformation (keyword; #'convert-to-dB)
       ==> a function of one argument or nil;
           if bound to a function, the function is used
           to transform all elements of result-spec-wind
   [8] result-spec-wind (keyword; vector of correct length)
       <== vector into which spectral window values are placed;
           it must be exactly of the length dictated by N-nonzero-freqs
   [9] return-frequencies-p (keyword; t)
       ==> if t, the frequencies associated with the spectral window
           are computed and returned in result-freq
  [10] freq-transformation (keyword; nil)
       ==> a function of one argument or nil;
           if bound to a function, the function is used
           to transform all elements of result-freq
  [11] result-freq (keyword; nil or vector of correct length)
       <== not used if return-frequencies-p nil; otherwise,
           vector of N-nonzero-freqs +1 into which
           the frequencies associated with the values
           in result-spec-wind are placed
returns
   [1] result-spec-wind, a vector holding
       the properly transformed spectral window
   [2] result-freq (if return-frequencies-p is t),
       where result-freq is a vector holding
       the properly transformed frequencies
       associated with values in result-spec-wind
        -- or --
       nil (if return-frequencies-p is nil)
   [3] the length of the vector result-spec-wind
---
Note: see equation below Equation (207a) of the SAPA book"
  (let* ((N-freqs (get-N-freqs N-nonzero-freqs sample-size t))
         (N-dft (get-N-dft N-nonzero-freqs sample-size))
         (fiddle-factor-freq (/ (* N-dft sampling-time))))
    ;;; put data taper into first part of scratch-dft
    (fill scratch-dft (float (/ (sqrt sample-size))) :end sample-size)
    (if data-taper
      (funcall data-taper (make-array
                           sample-size
                           :displaced-to scratch-dft)
               :taper-parameter data-taper-parameters
               :normalization :N))
    ;;; zero the rest of scratch-dft ...
    (fill scratch-dft 0.0 :start sample-size :end N-dft)
    (dft! scratch-dft :N N-dft)
    ;;; Note: here is where we use the assumption that
    ;;;       result-spec-wind is exactly of the size
    ;;;       N-nonzero-freqs +1
    ;;; Note: Allegro Common Lisp does not properly handle
    ;;;       map-into, and it will not allow a redefinition
    ;;;       of that function, so we must resort to this hack
    ;;;       (for details, see hacks.lisp).
    #-allegro
    (map-into result-spec-wind
                   (if spec-wind-transformation
                     #'(lambda (x)
                         (funcall spec-wind-transformation
                                  (* sampling-time
                                     (expt (abs x) 2))))
                     #'(lambda (x)
                         (* sampling-time
                            (expt (abs x) 2))))
                   scratch-dft)
    #+allegro
    (sapa-map-into result-spec-wind
                   (if spec-wind-transformation
                     #'(lambda (x)
                         (funcall spec-wind-transformation
                                  (* sampling-time
                                     (expt (abs x) 2))))
                     #'(lambda (x)
                         (* sampling-time
                            (expt (abs x) 2))))
                   scratch-dft)
    (when return-frequencies-p
      (map-into result-freq
                (if freq-transformation
                  #'(lambda (k)
                      (funcall freq-transformation
                               (* k fiddle-factor-freq)))
                  #'(lambda (k)
                      (* k fiddle-factor-freq)))
                (iota 0 (1- N-freqs))))
    (values result-spec-wind result-freq N-freqs)))

#|
;;; yields Fejer's kernel (in dB) for N = 4
;;; (cf. top plot of Figure 200 of the SAPA book)
(spectral-window-for-direct-spectral-estimate 4)
;==> #(6.020599913279622 5.16438561858617 2.322606875058723 -3.9256792411212302
      -100.0 -7.427827503856934 -5.332906831698538 -8.862378656807074 -100.0)
;    #(0.0 0.0625 0.125 0.1875 0.25 0.3125 0.375 0.4375 0.5)
;    9

;;; spectral window (in dB) for N = 4, NW=1 dpss data taper
(spectral-window-for-direct-spectral-estimate
 4
 :data-taper #'dpss-data-taper!
 :data-taper-parameters  1.0)
;==> #(5.774834924698505 5.089606295677452 2.904268902242502
      -1.3159448282420236 -9.584646039813563 -25.51680903895323
     -13.166120020034743 -15.171623681945857 -100.0)
;    #(0.0 0.0625 0.125 0.1875 0.25 0.3125 0.375 0.4375 0.5)
;    9
|#

;-------------------------------------------------------------------------------
(defun spectral-window-for-lag-window-spectral-estimate
       (sample-size
        lag-window-function
        &key
        (N-nonzero-freqs :twice-next-power-of-2)
        (sampling-time 1.0)
        (scratch-dft (make-array (get-N-dft N-nonzero-freqs sample-size)))
        (data-taper nil)
        (data-taper-parameters)
        (spec-wind-transformation #'careful-convert-to-dB)
        (result-spec-wind
         (make-array (get-N-freqs N-nonzero-freqs sample-size
                                  t)))
        (return-frequencies-p t)
        (freq-transformation nil)
        (result-freq
         (if return-frequencies-p
           (make-array (get-N-freqs N-nonzero-freqs sample-size t)))))
  "given
   [1] sample-size (required)
       ==> sample size for which spectral window is to be computed
   [2] lag-window-function (required)
       ==> function of a single variable that computes the value
           of the lag window for a given lag
   [3] N-nonzero-freqs (keyword; :twice-next-power-of-2)
       ==> specifies at how many nonzero frequencies
           spectral window is to be computed -- choices are:
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
   [4] sampling-time (keyword; 1.0)
       ==> sampling time (called delta t in the SAPA book)
   [5] scratch-dft (keyword; vector of correct length)
       ==> vector in which the in-place dft is done
   [6] data-taper (keyword; nil)
       ==> nil or a tapering function
   [7] data-taper-parameters (keyword)
       ==> parameters for tapering function (not used
           if data-taper is nil)
   [8] spec-wind-transformation (keyword; #'convert-to-dB)
       ==> a function of one argument or nil;
           if bound to a function, the function is used
           to transform all elements of result-spec-wind
   [9] result-spec-wind (keyword; vector of correct length)
       <== vector into which spectral window values are placed;
           it must be exactly of the length dictated by N-nonzero-freqs
  [10] return-frequencies-p (keyword; t)
       ==> if t, the frequencies associated with the spectral window
           are computed and returned in result-freq
  [11] freq-transformation (keyword; nil)
       ==> a function of one argument or nil;
           if bound to a function, the function is used
           to transform all elements of result-freq
           (ignored unless return-frequencies-p is true)
  [12] result-freq (keyword; nil or vector of correct length)
       <== not used if return-frequencies-p nil; otherwise,
           vector of N-nonzero-freqs +1 into which
           the frequencies associated with the values
           in result-spec-wind are placed
returns
   [1] result-spec-wind, a vector holding
       the properly transformed spectral window
   [2] result-freq (if return-frequencies-p is t),
       where result-freq is a vector holding
       the properly transformed frequencies
       associated with values in result-spec-wind
        -- or --
       nil (if return-frequencies-p is nil)
   [3] the length of the vector result-spec-wind
---
Note: see equation below Equation (244a) of the SAPA book"
  ;;; put data taper into first part of scratch-dft
  (fill scratch-dft 1.0 :end sample-size)
  (if data-taper
    (funcall data-taper (make-array
                         sample-size
                         :displaced-to scratch-dft)
             :taper-parameter data-taper-parameters
             :normalization :N))
  (acvs scratch-dft :end sample-size :center-data-p nil :result scratch-dft)
  (lag-window-spectral-estimate
   scratch-dft
   lag-window-function
   :max-lag (1- sample-size)
   :N-ts sample-size
   :N-nonzero-freqs N-nonzero-freqs
   :return-est-for-0-freq-p t
   :sampling-time sampling-time
   :scratch-dft scratch-dft
   :sdf-transformation spec-wind-transformation
   :result-sdf result-spec-wind
   :return-frequencies-p return-frequencies-p
   :freq-transformation freq-transformation
   :result-freq result-freq)
  (values result-spec-wind result-freq
          (get-N-freqs N-nonzero-freqs sample-size t)))

#|
(multiple-value-bind (the-spec-wind-5 freqs N-f)
                     (spectral-window-for-lag-window-spectral-estimate 
                      20
                      #'(lambda (tau)
                          (parzen-lag-window tau 5))
                      :N-nonzero-freqs :Fourier)
  (let ((the-spec-wind-10 (spectral-window-for-lag-window-spectral-estimate 
                             20
                             #'(lambda (tau)
                                 (parzen-lag-window tau 10))
                             :N-nonzero-freqs :Fourier))
        (the-spec-wind-15 (spectral-window-for-lag-window-spectral-estimate 
                             20
                             #'(lambda (tau)
                                 (parzen-lag-window tau 15))
                             :N-nonzero-freqs :Fourier)))
    (dotimes (i N-f)
      (format t "~&~6,4F: ~8,4F ~8,4F ~8,4F"
              (svref freqs i)
              (svref the-spec-wind-5  i)
              (svref the-spec-wind-10 i)
              (svref the-spec-wind-15 i)))
    (values)))
;==>
0.0000:   5.4920   8.2174   9.6800
0.0500:   5.0695   6.6304   6.4004
0.1000:   3.7838   1.7435  -2.6115
0.1500:   1.5802  -5.7033  -8.2604
0.2000:  -1.6189 -10.3761 -10.2127
0.2500:  -5.8104 -11.9382 -12.6393
0.3000: -10.3570 -12.6772 -13.8851
0.3500: -13.1849 -14.3975 -14.7067
0.4000: -13.9394 -15.3618 -15.4563
0.4500: -14.2726 -15.5651 -15.7582
0.5000: -14.4370 -15.3760 -15.8875
|#

;-------------------------------------------------------------------------------
(defun smoothing-window-for-lag-window-spectral-estimate
       (sample-size
        lag-window-function
        &key
        (N-nonzero-freqs :twice-next-power-of-2)
        (sampling-time 1.0)
        (scratch-dft (make-array (get-N-dft N-nonzero-freqs sample-size)))
        (smooth-wind-transformation #'careful-convert-to-dB)
        (result-smooth-wind
         (make-array (get-N-freqs N-nonzero-freqs sample-size
                                  t)))
        (return-frequencies-p t)
        (freq-transformation nil)
        (result-freq
         (if return-frequencies-p
           (make-array (get-N-freqs N-nonzero-freqs sample-size t)))))
  "given
   [1] sample-size (required)
       ==> sample size for which smoothing window is to be computed
   [2] lag-window-function (required)
       ==> function of a single variable that computes the value
           of the lag window for a given lag
   [3] N-nonzero-freqs (keyword; :twice-next-power-of-2)
       ==> specifies at how many nonzero frequencies
           smoothing window is to be computed -- choices are:
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
   [4] sampling-time (keyword; 1.0)
       ==> sampling time (called delta t in the SAPA book)
   [5] scratch-dft (keyword; vector of correct length)
       ==> vector in which the in-place dft is done
   [6] smooth-wind-transformation (keyword; #'convert-to-dB)
       ==> a function of one argument or nil;
           if bound to a function, the function is used
           to transform all elements of result-smooth-wind
   [7] result-smooth-wind (keyword; vector of correct length)
       <== vector into which smoothing window values are placed;
           it must be exactly of the length dictated by N-nonzero-freqs
   [8] return-frequencies-p (keyword; t)
       ==> if t, the frequencies associated with the smoothing window
           are computed and returned in result-freq
   [9] freq-transformation (keyword; nil)
       ==> a function of one argument or nil;
           if bound to a function, the function is used
           to transform all elements of result-freq
           (ignored unless return-frequencies-p is true)
  [10] result-freq (keyword; nil or vector of correct length)
       <== not used if return-frequencies-p nil; otherwise,
           vector of N-nonzero-freqs +1 into which
           the frequencies associated with the values
           in result-smooth-wind are placed
returns
   [1] result-smooth-wind, a vector holding
       the properly transformed smoothing window
   [2] result-freq (if return-frequencies-p is t),
       where result-freq is a vector holding
       the properly transformed frequencies
       associated with values in result-smooth-wind
        -- or --
       nil (if return-frequencies-p is nil)
   [3] the length of the vector result-smooth-wind
---
Note: see equation below Equation (237c) of the SAPA book"
  (fill scratch-dft 1.0 :end sample-size)
  (lag-window-spectral-estimate
   scratch-dft
   lag-window-function
   :max-lag (1- sample-size)
   :N-ts sample-size
   :N-nonzero-freqs N-nonzero-freqs
   :return-est-for-0-freq-p t
   :sampling-time sampling-time
   :scratch-dft scratch-dft
   :sdf-transformation smooth-wind-transformation
   :result-sdf result-smooth-wind
   :return-frequencies-p return-frequencies-p
   :freq-transformation freq-transformation
   :result-freq result-freq)
  (values result-smooth-wind result-freq
          (get-N-freqs N-nonzero-freqs sample-size t)))

#|
(multiple-value-bind (the-smooth-wind-5 freqs N-f)
                     (smoothing-window-for-lag-window-spectral-estimate 
                      20
                      #'(lambda (tau)
                          (parzen-lag-window tau 5))
                      :N-nonzero-freqs :Fourier)
  (let ((the-smooth-wind-10 (smoothing-window-for-lag-window-spectral-estimate 
                             20
                             #'(lambda (tau)
                                 (parzen-lag-window tau 10))
                             :N-nonzero-freqs :Fourier))
        (the-smooth-wind-15 (smoothing-window-for-lag-window-spectral-estimate 
                             20
                             #'(lambda (tau)
                                 (parzen-lag-window tau 15))
                             :N-nonzero-freqs :Fourier)))
    (dotimes (i N-f)
      (format t "~&~6,4F: ~8,4F ~8,4F ~8,4F"
              (svref freqs i)
              (svref the-smooth-wind-5  i)
              (svref the-smooth-wind-10 i)
              (svref the-smooth-wind-15 i)))
    (values)))
;==>
0.0000:   5.7426   8.7506  10.5116
0.0500:   5.2934   6.9265   6.2889
0.1000:   3.9171   0.9068 -10.3965
0.1500:   1.5182 -12.1526 -27.9226
0.2000:  -2.1035 -149.5459 -16.4171
0.2500:  -7.3518 -20.9691 -35.2827
0.3000: -15.0060 -18.0163 -29.3197
0.3500: -22.4607 -26.4962 -27.4921
0.4000: -21.0266 -148.5768 -35.3403
0.4500: -20.6780 -29.5773 -31.7854
0.5000: -20.9691 -23.9794 -35.2827
|#

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;;  The functions  Grenander-smoothing-window-bandwidth
;;;                 Jenkins-smoothing-window-bandwidth
;;;                 equivalent-degrees-of-freedom
;;;                 bandwidth&confidence-intervals-for-sdf-dB
;;;  compute the two measures of the smoothing window bandwidths for a lag
;;;  window estimator, the equivalent degrees of freedom for a lag window
;;;  estimator, and a point-wise confidence interval for the true sdf at
;;;  given frequency.
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
(defun Grenander-smoothing-window-bandwidth
       (lag-window-function
        max-lag
        &key
        (sampling-time 1.0))
  "given a lag window function, a maximum lag and the sampling time,
returns Grenander's measure of smoothing window bandwidth
---
Note: see Equations (241c) and (241b) of the SAPA book;
      unfortunately, because of the square root operation,
      this measure can be complex-valued for certain lag windows"
  (let ((sum 0.0)
        (tau 1))
    (dotimes (i max-lag (/ (sqrt (1+ (* (/ 12 (* pi pi)) sum)))
                           sampling-time))
      (incf sum (/ (* (expt -1 tau) (funcall lag-window-function tau))
                   (* tau tau)))
      (incf tau))))

#|
(dolist (m '(5 10 13) (values))
  (print (Grenander-smoothing-window-bandwidth
          #'(lambda (tau)
              (parzen-lag-window tau m))
          20
          :sampling-time 1/4)))
;==>
1.4444046775094332 
0.7450539900333778 
0.5761648410683915 
|#

;-------------------------------------------------------------------------------
(defun Jenkins-smoothing-window-bandwidth
       (lag-window-function
        max-lag
        &key
        (sampling-time 1.0))
  "given a lag window function, a maximum lag and the sampling time,
returns Jenkins' measure of smoothing window bandwidth
---
Note: see Equation (242c) of the SAPA book"
  (let ((sum 0.0)
        (tau 1))
    (dotimes (i max-lag (/ (* sampling-time (+ 1.0 (* 2.0 sum)))))
      (incf sum (expt (funcall lag-window-function tau) 2))
      (incf tau))))

#|
(dolist (m '(5 10 13) (values))
  (print (Jenkins-smoothing-window-bandwidth
          #'(lambda (tau)
              (parzen-lag-window tau m))
          20
          :sampling-time 1/4)))
;==>
1.4822720265623148 
0.7417022065640646 
0.5705456589192851 
|#

;-------------------------------------------------------------------------------
(defun equivalent-degrees-of-freedom
       (lag-window-function
        max-lag
        &key
        (sampling-time 1.0)
        (sample-size (1+ max-lag))
        (C_h 1.0))
  "given a lag window function, a maximum lag, the sampling time,
the sample size and the variance inflation factor C_h,
returns the corresponding equivalent degrees of freedom
---
Note: see Equation (255a) of the SAPA book"
  (/ (* 2 sample-size sampling-time
        (Jenkins-smoothing-window-bandwidth
         lag-window-function max-lag :sampling-time sampling-time))
     C_h))

#|
;;; two examples discussed on page 302 of the SAPA book ...
(equivalent-degrees-of-freedom
 #'(lambda (tau) (parzen-lag-window tau 150))
 150
 :sampling-time 1/4
 :sample-size 1024
 :C_h 1.96)
;==> 12.917060857770348

(equivalent-degrees-of-freedom
 #'(lambda (tau) (parzen-lag-window tau 55))
 55
 :sampling-time 1/4
 :sample-size 1024
 :C_h 1.96)
;==> 35.22834596647285
|#

;-------------------------------------------------------------------------------
(defun bandwidth&confidence-intervals-for-sdf-dB
       (sdf-dB
        freq
        sample-size
        &key
        (confidence-level 0.95)
        (lag-window-function nil)
        (max-lag (1- sample-size))
        (sampling-time 1.0)
        (C_h 1.0))
  "given
   [1] sdf-dB (required)
       ==> an sdf estimate (in decibels) for a single frequency
   [2] freq (required)
       ==> the frequency associated with sdf-dB
   [3] sampling-size (required)
       ==> the sample size of the time series
           from which sdf-dB was computed
   [4] confidence-level (keyword; 0.95)
       ==> the level of the confidence interval
           (e.g., 0.95 yields a 95% confidence interval)
   [5] lag-window-function (keyword; nil)
       ==> the lag window function used to create
           sdf-dB (nil means a lag window function was NOT used)
   [6] max-lag (keyword; sample-size - 1)
       ==> the maximum lag used in conjunction with lag-window-function
           to create sdf-dB (not used in lag-window-function is nil)
   [7] sampling-time (keyword; 1.0)
       ==> sampling time (called delta t in the SAPA book)
   [8] C_h (keyword; 1.0)
       ==> the variance inflation factor due to tapering
           (see Table 248 of the SAPA book); the default value
           of 1.0 is appropriate for a rectangular data taper
returns
   [1] left-hand size of interval describing the bandwidth of
       the spectral estimate (interval is centered on freq)
   [2] right-hand size of bandwidth interval
   [3] lower limit (in decibels) of confidence interval for
       true sdf based upon sdf-dB
   [4] upper limit (in decibels) of confidence interval
---
Note: see Sections 6.7 and 6.10 of the SAPA book"
  (let* ((half-B_W (* 0.5 (if lag-window-function
                            (Jenkins-smoothing-window-bandwidth
                             lag-window-function max-lag
                             :sampling-time sampling-time)
                            (/ C_h (* sampling-time sample-size)))))
         (nu (if lag-window-function
               (equivalent-degrees-of-freedom
                lag-window-function
                max-lag
                :sampling-time sampling-time
                :sample-size sample-size
                :C_h C_h)
               2))
         (acc-level (if (>= nu 150) :quick :accurate))
         (for-upper (/ (- 1.0 confidence-level) 2))
         (lower-dB (convert-to-dB (/ nu
                                     (quantile-of-chi-square-distribution
                                      nu (+ confidence-level for-upper)
                                      :accuracy-level acc-level))))
         (upper-dB (convert-to-dB (/ nu
                                     (quantile-of-chi-square-distribution
                                      nu for-upper
                                      :accuracy-level acc-level)))))
    (values (- freq half-B_W)
            (+ freq half-B_W)
            (+ sdf-dB lower-dB)
            (+ sdf-dB upper-dB))))

#|
;;; example discussed on page 302 of the SAPA book ...
(bandwidth&confidence-intervals-for-sdf-dB
 6.3
 1.0
 1024
 :lag-window-function #'(lambda (tau) (parzen-lag-window tau 150))
 :max-lag 150
 :sampling-time 1/4
 :C_h 1.96)
;==>  0.9752759382019239
;     1.024724061798076
;     3.4987218467878627
;    10.458588688308273
;;; Note that (- 1.024724061798076 0.9752759382019239) ==> 0.0494,
;;; in agreement with the result stated on page 301 of the SAPA book

;;; example discussed on page 299 of the SAPA book ...
(bandwidth&confidence-intervals-for-sdf-dB
 0
 1.0
 1024
 :sampling-time 1/4
 :C_h 1.96)
;==> 0.996171875
;    1.003828125
;   -5.668944635031615
;   15.965738982351615
;;; Note that (- 1.003828125 0.996171875) ==> 0.008,
;;; in agreement with the result stated on page 299 of the SAPA book
|#

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;;  The functions  time-series-bandwidth
;;;                 sample-cepstrum
;;;                 cepstrum->I_m
;;;  can be used to objectively determine an appropriate value for
;;;  the lag window parameter m.  The function time-series-bandwidth
;;;  computes an estimate of the spectral bandwidth for a time series
;;;  (see the discussion in Section 6.14 of the SAPA book).  The functions
;;;  sample-cepstrum and cepstrum->I_m can be used to compute an approximation
;;;  to the mean integrated squared error (see the discussion in Section 6.15
;;;  of the SAPA book).
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
(defun time-series-bandwidth (acvs &key (sampling-time 1.0))
  "given
   [1] acvs (required)
       ==> a vector of length N with values of
           the acvs (or acs) from lag 0 to N-1
   [2] sampling-time (keyword; 1.0)
       ==> the sampling time
returns
   [1] unbiased estimate of trace bandwidth
       (\tilde B_T in Equation (280))
   [2] biased estimate of trace bandwidth
       (\hat B_T in equation just above Equation (28)))
---
Note: see Section 6.14 of the SAPA book"
  (let ((N (length acvs))
        (tau 1)
        (sum 0.0)
        (s_0^2 (* (elt acvs 0) (elt acvs 0))))
    (dotimes (i (1- N) )
      (incf sum (* (elt acvs tau) (elt acvs tau)
                   (- 1.0 (/ tau N))))
      (incf tau))
    (let ((hat-B_T (/ s_0^2 (* 2.0 sampling-time
                               (+ s_0^2 (* 2.0 sum))))))
      (values (- (* 5/3 hat-B_T)
                 (/ (* N sampling-time)))
              hat-B_T))))

#|
;;; For this example we use the 20 point time series of Section 6.16
;;; of the SAPA book:
(let* ((20-pt-ts #(71.0  63.0  70.0  88.0  99.0  90.0 110.0 135.0 128.0 154.0
                   156.0 141.0 131.0 132.0 141.0 104.0 136.0 146.0 124.0 129.0))
       (sampling-time 0.25)
       (the-acvs (acvs 20-pt-ts)))
  (time-series-bandwidth the-acvs :sampling-time sampling-time))
;==> 0.7889256471629704
;    0.5933553882977822A
|#

;-------------------------------------------------------------------------------
(defun sample-cepstrum
       (log-spectrum
        &key
        (sampling-time 1.0)
        (zero-frequency-p nil)
        (Nyquist-frequency-p t)
        (result (make-array (if zero-frequency-p
                              (length log-spectrum)
                              (1+ (length log-spectrum))))))
  "given
   [1] log-spectrum (required)
       ==> a vector containing log spectrum
   [2] sampling-time (keyword; 1.0)
       ==> the sample time (i.e., delta t)
   [3] zero-frequency-p (keyword; nil)
       ==> if t, the first element of log-spectrum
           is associated with 0 frequency;
           if nil, first element goes with
           lowest nonzero frequency
   [4] Nyquist-frequency-p (keyword; nil)
       ==> if t, the last element of log-spectrum
           is associated with Nyquist frequency;
           if nil, last element goes with
           a frequency just less than Nyquist
   [5] result (keyword; vector of appropriate length)
       <== vector to hold sample cepstrum
returns
   [1] result, i.e., the sample cepstrum from lag 0 to lag N_U
       where N_U = length of log-spectrum - 1 if zero-frequency-p is t
       or        = length of log-spectrum     if zero-frequency-p is nil
---
Note: see Equation 282 of the SAPA book"
  (let* ((N_U (if zero-frequency-p (1- (length log-spectrum))
                  (length log-spectrum)))
         (N_L (if Nyquist-frequency-p (1- N_U) N_U))
         (N-prime (+ N_U N_L 1))
         (for-DFT (make-array N-prime))
         (k (if zero-frequency-p 1 0))
         (i+1 1))
    #+mcl(declare (dynamic-extent for-DFT))
    ;;; NOTE: if the log spectrum is not supplied for 0 frequency,
    ;;;       this sets it equal to that of the closest non-zero frequency.
    (setf (aref for-DFT 0) (elt log-spectrum 0))
    (dotimes (i N_U)
      (setf (aref for-DFT i+1) (elt log-spectrum k))
      (incf i+1)
      (incf k))
    (setf k (if zero-frequency-p N_L (1- N_L)))
    (dotimes (i N_L)
      (setf (aref for-DFT i+1) (elt log-spectrum k))
      (incf i+1)
      (decf k))
    (inverse-dft! for-DFT :sampling-time sampling-time)
    (dotimes (i (1+ N_U) result)
      (setf (aref result i) (realpart (aref for-DFT i))))))

#|
;;; For this example we use the 20 point time series of Section 6.16
;;; of the SAPA book:
(let* ((20-pt-ts #(71.0  63.0  70.0  88.0  99.0  90.0 110.0 135.0 128.0 154.0
                   156.0 141.0 131.0 132.0 141.0 104.0 136.0 146.0 124.0 129.0))
       (sampling-time 0.25)
       (pgram-20-pt-ts-log (periodogram 20-pt-ts
                                          :sampling-time sampling-time
                                          :N-nonzero-freqs :Fourier
                                          :sdf-transformation #'log))
       (pgram-19-pt-ts-log (periodogram 20-pt-ts
                                          :end 19
                                          :sampling-time sampling-time
                                          :N-nonzero-freqs :Fourier
                                          :sdf-transformation #'log)))
  (let ((cepstrum-20 (sample-cepstrum pgram-20-pt-ts-log))
        (cepstrum-19 (sample-cepstrum pgram-19-pt-ts-log
                                      :Nyquist-frequency-p nil)))
    (dotimes (i (length cepstrum-20))
      (format t "~&~2D: ~8,4F"
              i
              (svref cepstrum-20 i)))
    (format t "~&...")
    (dotimes (i (length cepstrum-19))
      (format t "~&~2D: ~8,4F"
              i
              (svref cepstrum-19 i)))
    (values)))
;==>
 0:   4.3542
 1:   0.9286
 2:   0.2987
 3:   0.3236
 4:  -0.1946
 5:   0.2022
 6:  -0.1170
 7:   0.1663
 8:  -0.2263
 9:  -0.1352
10:   0.2587
...
 0:   4.4931
 1:   0.9671
 2:   0.1895
 3:   0.0822
 4:   0.2625
 5:   0.0137
 6:  -0.0898
 7:   0.1136
 8:  -0.1625
 9:  -0.0512
|#

;-------------------------------------------------------------------------------
(defun cepstrum->I_m
       (cepstrum
        &key
        (sampling-time 1.0)
        (N-prime (- (* 2 (length cepstrum)) 2))
        (lag-window-function #'parzen-lag-window)
        (lambda-factor 1.0)
        (result (make-array (length cepstrum))))
  "given
   [1] cepstrum (required)
       ==> a vector of length N_U+1 with values of the cepstrum
           from lag 0 to N_U
   [2] sampling-time (keyword; 1.0)
       ==> the sample time (i.e., delta t)
   [3] N-prime (keyword; 2 * N_U)
       ==> effective sample size, as discussed in Section 6.15
           of the SAPA book (default value ok if N-prime is even,
           but must be replaced if N-prime is odd)
   [4] lag-window-function (keyword; #'parzen-lag-window)
       ==> lag window function of 2 parameters,
           tau (the lag) and m (the window parameter)
   [5] lambda-factor (keyword; 1.0)
       ==> lambda, as discussed in Section 6.15 of the SAPA book
   [6] result (keyword; vector of same length as cepstrum)
       <== vector to hold I_m
returns
   [1] the value m at which I_m is minimized
   [2] result, i.e., I_m for m = 0 to N_U
   [3] the minimum value of I_m for m = 0 to N_U
---
Note: see Equation (283b) of the SAPA book"
  (let* ((N_U (1- (length cepstrum)))
         (N_L (- N-prime N_U 1))
         (a-constant (/ (* lambda-factor lambda-factor
                           +euler-constant+ +euler-constant+)
                        sampling-time))
         (another-constant (/ (* lambda-factor lambda-factor pi pi)
                              (* 6.0 N-prime sampling-time)))
         (yet-another-constant (/ another-constant
                                  sampling-time))
         first-sum second-sum wtm tau)
    ;;; loop over all possible lag window parameters
    (dotimes (m (1+ N_U))
      (setf first-sum 0.0
            second-sum 0.0
            tau 1)
      (dotimes (i N_L)
        (setf wtm (funcall lag-window-function tau m))
        (incf first-sum (* (- (expt (aref cepstrum tau) 2)
                              yet-another-constant)
                           (expt (- 1.0 wtm) 2)))
        (incf second-sum (* wtm wtm))
        (incf tau))
      ;;; double these ...
      (incf first-sum first-sum)
      (incf second-sum second-sum)
      ;;; add in the odd term if need be ...
      (when (not (= N_L N_U))
        (setf wtm (funcall lag-window-function tau m))
        (incf first-sum (* (- (expt (aref cepstrum tau) 2)
                              yet-another-constant)
                           (expt (- 1.0 wtm) 2)))
        (incf second-sum (* wtm wtm)))
      (setf (aref result m) (+ a-constant
                            (* sampling-time first-sum)
                            (* another-constant second-sum))))
    (multiple-value-bind (min-I_m min-m)
                         (min-of-seq result)
      (values min-m result min-I_m))))

#|
;;; For this example we use the 20 point time series of Section 6.16
;;; of the SAPA book:
(let* ((20-pt-ts #(71.0  63.0  70.0  88.0  99.0  90.0 110.0 135.0 128.0 154.0
                   156.0 141.0 131.0 132.0 141.0 104.0 136.0 146.0 124.0 129.0))
       (sampling-time 0.25)
       (pgram-20-pt-ts-log (periodogram 20-pt-ts
                                          :sampling-time sampling-time
                                          :N-nonzero-freqs :Fourier
                                          :sdf-transformation #'log))
       (pgram-19-pt-ts-log (periodogram 20-pt-ts
                                          :end 19
                                          :sampling-time sampling-time
                                          :N-nonzero-freqs :Fourier
                                          :sdf-transformation #'log)))
  (let ((cepstrum-20 (sample-cepstrum pgram-20-pt-ts-log))
        (cepstrum-19 (sample-cepstrum pgram-19-pt-ts-log
                                      :Nyquist-frequency-p nil)))
    (multiple-value-bind (m-min I_m-20 I_m-min)
                         (cepstrum->I_m cepstrum-20)
      (print m-min)
      (print I_m-min)
      (dotimes (i (length I_m-20))
        (format t "~&~2D: ~8,4F"
                i
                (svref I_m-20 i))))
      (format t "~&...")
      (multiple-value-bind (m-min I_m-19 I_m-min)
                           (cepstrum->I_m cepstrum-19 :N-prime 19)
        (print m-min)
        (print I_m-min)
        (dotimes (i (length I_m-19))
          (format t "~&~2D: ~8,4F"
                  i
                  (svref I_m-19 i))))
    (values)))
;==>
5 
-0.05104128407495598 
 0:   1.3292
 1:   1.3292
 2:   0.6569
 3:   0.1269
 4:  -0.0210
 5:  -0.0510
 6:  -0.0336
 7:   0.0100
 8:   0.0715
 9:   0.1449
10:   0.2252
...
4 
-0.4396526809044076 
 0:   0.9690
 1:   0.9690
 2:   0.2372
 3:  -0.3244
 4:  -0.4397
 5:  -0.4118
 6:  -0.3428
 7:  -0.2564
 8:  -0.1608
 9:  -0.0586
|#

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;;  The functions  cumulative-periodogram
;;;                 quantile-of-Kolmogorov-test-statistic
;;;  can be used to compute and evaluate the normalized cumulative periodogram,
;;;  a useful way of evaluating the hypothesis that a time series can be
;;;  regarded as white noise.
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
(defun cumulative-periodogram
       (time-series
        &key
        (sampling-time 1.0)
        (center-data t)
        (start 0)
        (end (length time-series))
        (cumulative-periodogram-test-p t)
        (significance-level 0.95)
        (scratch-dft (make-array (- end start)))
        (return-frequencies-p t)
        (result-freq (if return-frequencies-p
                       (make-array (truncate (1- (- end start)) 2))))
        (result (make-array (truncate (1- (- end start)) 2))))
  "given
   [1] time-series (required)
       ==> a vector containing time series values
   [2] sampling-time (keyword; 1.0)
       ==> the sample time (i.e., delta t)
   [3] center-data (keyword; t)
       ==> if t, subtract sample mean from time series;
           if a number, subtracts that number from time series;
           if nil, time-series is not centered
   [4] start (keyword; 0)
       ==> start index of vector to be used
   [5] end (keyword; length of time-series)
       ==> 1 + end index of vector to be used
   [6] cumulative-periodogram-test-p (keyword; t)
       ==> if t, returns values associated with
           Kolmogorov test statistic
   [7] significance-level (keyword; 0.95)
       ==> level of significance at which Kolmogorov test
           is performed
   [8] scratch-dft (keyword; vector of size (- end start))
       ==> scratch vector used for in-place dft calculations
   [9] return-frequencies-p (keyword; t)
       ==> if t, the frequencies associated with the cumulative
           periodogram are computed and returned in result-freq
  [10] result-freq (keyword; nil or vector of correct length)
       <== not used if return-frequencies-p nil; otherwise,
           vector of correct length into which the frequencies
           associated with the values in result are placed
  [11] result (keyword; vector of correct length)
       <== vector to hold normalized cumulative periodogram
returns
   [1] vector with normalized cumulative periodogram 
   [2] vector with associated frequencies
 and -- if cumulative-periodogram-test-p is true --
   [3] Kolmogorov test statistic
   [4] index of maximum deviation
   [5] frequency of maximum deviation
   [6] quantile of Kolmogorov test statistic (under null hypothesis)
   [7] either :reject or :fail-to-reject, depending on whether
       or not we reject or fail to reject the null hypothesis
   [8] slope of upper and lower lines
   [9] intercept of upper line
  [10] intercept of lower line
---
Note: see Section 6.6 of the SAPA book"
  (let* ((N (- end start))
         ;;; M represents the number of Fourier frequencies
         ;;; that are greater than zero and less than the Nyquist frequency
         (M (truncate (/ (1- N) 2)))
         ;;; M-1 is the effective number of samples for
         ;;; the Kolmogorov test statistic
         (M-1 (1- M))
         (sampling-time-over-N (/ sampling-time N)))
    ;;; Here we center the time series ...
    (center&taper-time-series time-series
                              :center-data center-data
                              :start start
                              :end end
                              :result scratch-dft)
    (dft! scratch-dft :N N)
    (dotimes (i M)
      (setf (aref scratch-dft i)
            (* (expt (abs (aref scratch-dft (1+ i)))
                     2)
               sampling-time-over-N)))
    (cumulative-sums scratch-dft :end M :result result)
    (let ((factor (/ (elt result (1- M)))))
      (a*x! factor result)
      (if return-frequencies-p
        (let ((1-over-N-sampling-time (/ (* N sampling-time))))
          (dotimes (i M)
            (setf (aref result-freq  i)
                  (* (1+ i) 1-over-N-sampling-time)))))
      (if cumulative-periodogram-test-p
        (let* ((D+ (- (/ M-1) (elt result 0)))
               (D- (elt result 0))
               (D (max D+ D-))
               (j-D 0)
               (j 0)
               (k 1)
               D+current D-current KS-quantile)
          (dotimes (i (1- M-1))
            (incf j)
            (incf k)
            (setf D+current (- (/ k M-1) (elt result j))
                  D-current (- (elt result j) (/ j M-1)))
            (if (< D (max D+current D-current))
              (setf j-D j
                    D (max D+current D-current))))
          (setf KS-quantile (quantile-of-Kolmogorov-test-statistic
                             M-1
                             :significance-level significance-level))
          (values result
                  result-freq
                  D
                  j-D
                  (aref result-freq j-D)
                  KS-quantile
                  (if (> D KS-quantile) :reject :fail-to-reject)
                  ;;; slope of upper/lowe lines
                  (/ (* N sampling-time) M-1)
                  ;;; intecept of upper line
                  (- KS-quantile (/ M-1))
                  ;;; intecept of lower line
                  (- KS-quantile)))
        (values result result-freq)))))

#|
;;; For this example we use the 20 point time series of Section 6.16
;;; of the SAPA book:
(let* ((20-pt-ts #(71.0  63.0  70.0  88.0  99.0  90.0 110.0 135.0 128.0 154.0
                   156.0 141.0 131.0 132.0 141.0 104.0 136.0 146.0 124.0 129.0))
       (fd (difference 20-pt-ts))
       (sampling-time 0.25))
  (let* ((cum-per-results (multiple-value-list
                           (cumulative-periodogram
                            20-pt-ts
                            :sampling-time sampling-time)))
         (cum-per (elt cum-per-results 0))
         (cum-per-freqs (elt cum-per-results 1))
         (KS-results (subseq cum-per-results 2)))
    (print KS-results)
    (dotimes (i (length cum-per))
      (format t "~&~6,4F: ~8,4F"
              (svref cum-per-freqs i)
              (svref cum-per i))))
  (format t "~&...")
  (let* ((cum-per-results (multiple-value-list
                           (cumulative-periodogram
                            fd
                            :sampling-time sampling-time)))
         (cum-per (elt cum-per-results 0))
         (cum-per-freqs (elt cum-per-results 1))
         (KS-results (subseq cum-per-results 2)))
    (print KS-results)
    (dotimes (i (length cum-per))
      (format t "~&~6,4F: ~8,4F"
              (svref cum-per-freqs i)
              (svref cum-per i))))
    (values))
;==>
(0.6571803654140053 1 0.4 0.454 :reject 0.625 0.329 -0.454) 
0.2000:   0.6233
0.4000:   0.7822
0.6000:   0.8474
0.8000:   0.8893
1.0000:   0.8985
1.2000:   0.9326
1.4000:   0.9423
1.6000:   0.9925
1.8000:   1.0000
...
(0.2987185709446501 4 1.0526315789473684 0.454 :fail-to-reject 0.59375 0.329 -0.454) 
0.2105:   0.0977
0.4211:   0.1381
0.6316:   0.1800
0.8421:   0.2368
1.0526:   0.3263
1.2632:   0.6725
1.4737:   0.7619
1.6842:   0.9916
1.8947:   1.0000
|#

;-------------------------------------------------------------------------------
(defun quantile-of-Kolmogorov-test-statistic
       (sample-size
        &key
        (significance-level 0.95))
  "given
   [1] sample-size (required)
       ==> a positive integer
   [2] significance-level (keyword; 0.95)
       ==> 0.99, 0.98, 0.95, 0.90, or 0.80;
           0.75 is also ok if sample-size > 40
returns
   [1] quantile of two-sided Kolmogorov test statistic
---
Note: see Table 14, page 462, of Conover (2nd edition, 1980);
      Stephens (1974); and Diggle (1990, page 55);
      significance-level is currently limited to one of
      these values: 0.99, 0.98, 0.95, 0.90, or 0.80;
      if sample-size is greater than 40, 0.75 is also ok"
  (cond
   ((> sample-size 40) (let ((sqrt-sample-size (sqrt sample-size)))
                         (/ (case significance-level
                              (0.99 1.628)
                              (0.98 1.52)
                              (0.95 1.358)
                              (0.90 1.224)
                              (0.80 1.07)
                              (0.75 1.02))
                            (+ sqrt-sample-size
                               0.12
                               (/ 0.11 sqrt-sample-size)))))
   (t
    (nth (position significance-level
                   '(0.80 0.90 0.95 0.98 0.99) :test #'=)
         (nth (1- sample-size)
              '((0.900 0.950 0.975 0.990 0.995)   ;;;  1 = sample size
                (0.684 0.776 0.842 0.900 0.929)   ;;;  2
                (0.565 0.636 0.708 0.785 0.829)   ;;;  3
                (0.493 0.565 0.624 0.689 0.734)   ;;;  4
                (0.447 0.509 0.563 0.627 0.669)   ;;;  5
                (0.410 0.468 0.519 0.577 0.617)   ;;;  6
                (0.381 0.436 0.483 0.538 0.576)   ;;;  7
                (0.358 0.410 0.454 0.507 0.542)   ;;;  8
                (0.339 0.387 0.430 0.480 0.513)   ;;;  9
                (0.323 0.369 0.409 0.457 0.489)   ;;; 10
                (0.308 0.352 0.391 0.437 0.468)   ;;; 11
                (0.296 0.338 0.375 0.419 0.449)   ;;; 12
                (0.285 0.325 0.361 0.404 0.432)   ;;; 13
                (0.275 0.314 0.349 0.390 0.418)   ;;; 14
                (0.266 0.304 0.338 0.377 0.404)   ;;; 15
                (0.258 0.295 0.327 0.366 0.392)   ;;; 16
                (0.250 0.286 0.318 0.355 0.381)   ;;; 17
                (0.244 0.279 0.309 0.346 0.371)   ;;; 18
                (0.237 0.271 0.301 0.337 0.361)   ;;; 19
                (0.232 0.265 0.294 0.329 0.352)   ;;; 20
                (0.226 0.259 0.287 0.321 0.344)   ;;; 21
                (0.221 0.253 0.281 0.314 0.337)   ;;; 22
                (0.216 0.247 0.275 0.307 0.330)   ;;; 23
                (0.212 0.242 0.269 0.301 0.323)   ;;; 24
                (0.208 0.238 0.264 0.295 0.317)   ;;; 25
                (0.204 0.233 0.259 0.290 0.311)   ;;; 26
                (0.200 0.229 0.254 0.284 0.305)   ;;; 27
                (0.197 0.225 0.250 0.279 0.300)   ;;; 28
                (0.193 0.221 0.246 0.275 0.295)   ;;; 29
                (0.190 0.218 0.242 0.270 0.290)   ;;; 30
                (0.187 0.214 0.238 0.266 0.285)   ;;; 31
                (0.184 0.211 0.234 0.262 0.281)   ;;; 32
                (0.182 0.208 0.231 0.258 0.277)   ;;; 33
                (0.179 0.205 0.227 0.254 0.273)   ;;; 34
                (0.177 0.202 0.224 0.251 0.269)   ;;; 35
                (0.174 0.199 0.221 0.247 0.265)   ;;; 36
                (0.172 0.196 0.218 0.244 0.262)   ;;; 37
                (0.170 0.194 0.215 0.241 0.258)   ;;; 38
                (0.168 0.191 0.213 0.238 0.255)   ;;; 39
                (0.165 0.189 0.210 0.235 0.252)   ;;; 40
                ))))))

#|
(quantile-of-Kolmogorov-test-statistic 17)
;==> 0.318
(quantile-of-Kolmogorov-test-statistic 17 :significance-level 0.9)
;==> 0.286
|#

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;;  The functions  one-sided-freq->two-sided-freq
;;;                 one-sided-sdf->two-sided-sdf
;;;  allow conversion of a one-sided sdf estimate (or spectral/smoothing window)
;;;  into its two-sided representation (i.e., involving negative as well as
;;;  positive frequencies).
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
(defun one-sided-freq->two-sided-freq
       (one-sided-freq
        &key
        (result (make-array (1- (* 2 (length one-sided-freq))))))
  "given a vector of N frequencies
0, f_1, ..., f_{N-1},
returns the vector of 2N-1 frequencies
-f_{N-1}, ..., -f_1, 0, f_1, ..., f_{N-1}"
  (let* ((n (length one-sided-freq))
         (j-up (1- n))
         (j-down (1- n)))
    (setf (aref result j-up) (aref one-sided-freq 0))
    (dotimes (i (1- n) result)
      (incf j-up)
      (decf j-down)
      (setf (aref result j-down)
            (- (setf (aref result j-up)
                     (aref one-sided-freq (1+ i))))))))

#|
(one-sided-freq->two-sided-freq #(0 1 2 3 4))
;==> #(-4 -3 -2 -1 0 1 2 3 4)
|#

;-------------------------------------------------------------------------------
(defun one-sided-sdf->two-sided-sdf
       (one-sided-sdf
        &key
        (result (make-array (1- (* 2 (length one-sided-sdf))))))
  "given a one-sided sdf
S(0), S(f_1), ..., S_(f_{N-1}) of length N,
returns the two-sized sdf
S_(-f_{N-1}), ..., S(-f_1), S(0), S(f_1), ..., S_(f_{N-1})
of length 2N-1, where S_(-f_k) = S_(f_k)"
  (let* ((n (length one-sided-sdf))
         (j-up (1- n))
         (j-down (1- n)))
    (dotimes (i n result)
      (setf (aref result j-up)
            (setf (aref result j-down)
                  (aref one-sided-sdf i)))
      (incf j-up)
      (decf j-down))))

#|
(one-sided-sdf->two-sided-sdf #(1 7 2 5))
;==> #(5 2 7 1 7 2 5)
|#

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;;  The functions  bartlett-lag-window
;;;                 bartlett-m->bandwidth
;;;                 bartlett-bandwidth->m
;;;                 bartlett-N-m->degrees-of-freedom
;;;                 daniell-lag-window
;;;                 daniell-m->bandwidth
;;;                 daniell-bandwidth->m
;;;                 daniell-N-m->degrees-of-freedom
;;;                 parzen-lag-window
;;;                 parzen-m->bandwidth
;;;                 parzen-bandwidth->m
;;;                 parzen-N-m->degrees-of-freedom
;;;                 papoulis-lag-window
;;;                 papoulis-m->bandwidth
;;;                 papoulis-bandwidth->m
;;;                 papoulis-N-m->degrees-of-freedom
;;;  implement four of the lag windows discussed in Section 6.10
;;;  of the SAPA book and allow computation of the smoothing window bandwidth
;;;  and equivalent degrees of freedom associated with these windows.
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;;
;;; Bartlett lag window ...
;;;
(defun bartlett-lag-window (tau m)
  "given the lag tau and window parameter m,
returns the value of the Bartlett lag window
---
Note: see Equation (260) of the SAPA book
      or Priestley, page 439, Equation (6.2.65)"
  (assert (not (minusp m)))
  (if (zerop tau)
    1.0
    (let ((abs-tau (abs tau)))
      (if (< abs-tau m)
        (- 1.0 (float (/ abs-tau m)))
        0.0))))

#|
(map 'vector #'(lambda (lag) (bartlett-lag-window lag 4))
     (iota -5 5))
;==> #(0.0 0.0 0.25 0.5 0.75 1.0 0.75 0.5 0.25 0.0 0.0)
|#

;-------------------------------------------------------------------------------
(defun bartlett-m->bandwidth (m &key (sampling-time 1.0))
  "given window parameter m and sampling time,
returns bandwidth B_W for the Bartlett smoothing window
---
Note: see Table 269 of the SAPA book"
  (/ 1.5 (* m sampling-time)))

#|
(map 'vector #'bartlett-m->bandwidth #(1 10 100 1000))
;==> #(1.5 0.15 0.015 0.0015)
|#

;-------------------------------------------------------------------------------
(defun bartlett-bandwidth->m (B_W &key (sampling-time 1.0))
  "given desired smoothing window bandwidth B_W and sampling time,
returns 
   [1] window parameter m required to approximately achieve B_W
       using the Bartlett lag window
   [2] actual B_W achieved
---
Note: see Table 269 of the SAPA book"
  (let ((m (max 1 (round (/ 1.5 (* B_W sampling-time))))))
    (values m (bartlett-m->bandwidth m))))

#|
(map 'vector #'bartlett-bandwidth->m #(0.1 0.01 0.001 0.0001))
;==> #(15 150 1500 15000)
|#

;-------------------------------------------------------------------------------
(defun bartlett-N-m->degrees-of-freedom (N m &key (C_h 1.0))
  "given sample size N, window parameter m and variance inflation factor C_h,
  returns equivalent degrees of freedom nu for Bartlett lag window
---
Note: see Table 269 of the SAPA book"
  (/ (* 3.0 N) (* m C_h)))

#|
(bartlett-N-m->degrees-of-freedom 512 20)
;==> 76.8
|#

;-------------------------------------------------------------------------------
;;;
;;; Daniell lag window ...
;;;
(defun daniell-lag-window (tau m)
  "given the lag tau and window parameter m,
returns the value of the Daniell lag window
---
Note: see equation between Equations (264a) and (264b) of the SAPA book
      or Priestley, page 441, Equation (6.2.73)"
  (assert (plusp m))
  (cond
   ((zerop tau) 1.0)
   (t
    (let ((ratio (/ (* pi tau) m)))
      (/ (sin ratio) ratio)))))

#|
(map 'vector #'(lambda (lag) (daniell-lag-window lag 4))
     (iota -5 5))
;==> #(-0.18006326323142122 3.898043091051478E-17 0.3001054387190354
        0.6366197723675814 0.9003163161571061
        1.0
        0.9003163161571061 0.6366197723675814
        0.3001054387190354 3.898043091051478E-17 -0.18006326323142122)
|#

;-------------------------------------------------------------------------------
(defun daniell-m->bandwidth (m &key (sampling-time 1.0))
  "given window parameter m and sampling time,
returns bandwidth B_W for the Daniell smoothing window
---
Note: see Table 269 of the SAPA book"
  (/ (* m sampling-time)))

#|
(map 'vector #'daniell-m->bandwidth #(1 10 100 1000))
;==> #(1.0 0.1 0.01 0.001)
|#

;-------------------------------------------------------------------------------
(defun daniell-bandwidth->m (B_W &key (sampling-time 1.0))
  "given desired smoothing window bandwidth B_W and sampling time,
returns 
   [1] window parameter m required to approximately achieve B_W
       using the Daneill lag window
   [2] actual B_W achieved (in fact, this is always equal to B_W,
       but we return it anyway for consistency with other lag windows)
---
Note: see Table 269 of the SAPA book"
  (values (/ (* B_W sampling-time)) B_W))

#|
(map 'vector #'daniell-bandwidth->m #(0.1 0.01 0.001 0.0001))
;==> #(10.0 100.0 1000.0 10000.0)
|#

;-------------------------------------------------------------------------------
(defun daniell-N-m->degrees-of-freedom (N m &key (C_h 1.0))
  "given sample size N, window parameter m and variance inflation factor C_h,
  returns equivalent degrees of freedom nu for Daniell lag window
---
Note: see Table 269 of the SAPA book"
  (/ (* 2.0 N) (* m C_h)))

#|
(daniell-N-m->degrees-of-freedom 512 20)
;==> 51.2
|#

;-------------------------------------------------------------------------------
;;;
;;; Parzen lag window ...
;;;
(defun parzen-lag-window (tau m)
  "given the lag tau and window parameter m,
returns the value of the Parzen lag window
---
Note: see the equation on page 265 of the SAPA book
      or Priestley, page 443, Equation (6.2.82)"
  (assert (not (minusp m)))
  (cond
   ((zerop tau) 1.0)
   ((zerop m) 0.0)
   (t
    (let* ((abs-tau (abs tau))
           (ratio (float (/ abs-tau m))))
      (cond
       ((<= abs-tau (/ m 2))
        (1+ (* 6 (- (expt ratio 3) (* ratio ratio)))))
       ((<= abs-tau m)
        (* 2.0 (expt (- 1.0 ratio) 3)))
       (t
        0.0))))))

#|
(map 'vector #'(lambda (lag) (parzen-lag-window lag 4))
     (iota -5 5))
;==> #(0.0 0.0 0.03125 0.25 0.71875 1.0 0.71875 0.25 0.03125 0.0 0.0)
|#

;-------------------------------------------------------------------------------
(defun parzen-m->bandwidth (m &key (sampling-time 1.0))
  "given window parameter m and sampling time,
returns bandwidth B_W for the Parzen smoothing window
---
Note: see Table 269 of the SAPA book"
  (/ 1.85 (* m sampling-time)))

#|
(map 'vector #'parzen-m->bandwidth #(1 10 100 1000))
;==> #(1.85 0.185 0.018500000000000003 0.00185)
|#

;-------------------------------------------------------------------------------
(defun parzen-bandwidth->m (B_W &key (sampling-time 1.0))
  "given desired smoothing window bandwidth B_W and sampling time,
returns
   [1] window parameter m required to approximately achieve B_W
       using the Parzen lag window
   [2] actual B_W achieved
---
Note: see Table 269 of the SAPA book"
  (let ((m (max 1 (round (/ 1.85 (* B_W sampling-time))))))
    (values m (parzen-m->bandwidth m))))

#|
(map 'vector #'parzen-bandwidth->m #(0.1 0.01 0.001 0.0001))
;==> #(18 185 1850 18500)
|#

;-------------------------------------------------------------------------------
(defun parzen-N-m->degrees-of-freedom (N m &key (C_h 1.0))
  "given sample size N, window parameter m and variance inflation factor C_h,
  returns equivalent degrees of freedom nu for Parzen lag window
---
Note: see Table 269 of the SAPA book"
  (/ (* 3.71 N) (* m C_h)))

#|
(parzen-N-m->degrees-of-freedom 512 20)
;==> 94.976
|#

;-------------------------------------------------------------------------------
;;;
;;; Papoulis lag window ...
;;;
(defun papoulis-lag-window (tau m)
  "given the lag tau and window parameter m,
returns the value of the Papoulis lag window
---
Note: see equation near bottom of page 266 of the SAPA book"
  (assert (not (minusp m)))
  (cond
   ((zerop tau) 1.0)
   ((zerop m) 0.0)
   ((< (abs tau) m)
    (let* ((ratio (float (/ tau m)))
           (prod (* pi ratio)))
      (+ (/ (abs (sin prod)) pi)
         (* (- 1.0 (abs ratio))
            (cos prod)))))
   (t
    0.0)))

#|
(map 'vector #'(lambda (lag) (papoulis-lag-window lag 4))
     (iota -5 5))
;==> #(0.0 0.0 0.048302383742639676 0.31830988618379075 0.7554091649291872
       1.0
       0.7554091649291872 0.31830988618379075 0.048302383742639676 0.0 0.0)
|#

;-------------------------------------------------------------------------------
(defun papoulis-m->bandwidth (m &key (sampling-time 1.0))
  "given window parameter m and sampling time,
returns bandwidth B_W for the Papoulis smoothing window
---
Note: see Table 269 of the SAPA book"
  (/ 1.70 (* m sampling-time)))

#|
(map 'vector #'papoulis-m->bandwidth #(1 10 100 1000))
;==> #(1.7 0.16999999999999998 0.017 0.0017)
|#

;-------------------------------------------------------------------------------
(defun papoulis-bandwidth->m (B_W &key (sampling-time 1.0))
  "given desired smoothing window bandwidth B_W and sampling time,
returns
   [1] window parameter m required to approximately achieve B_W
       using the Papoulis lag window
   [2] actual B_W achieved
---
Note: see Table 269 of the SAPA book"
  (let ((m (max 1 (round (/ 1.70 (* B_W sampling-time))))))
    (values m (papoulis-m->bandwidth m))))

#|
(map 'vector #'papoulis-bandwidth->m #(0.1 0.01 0.001 0.0001))
;==> #(17 170 1700 17000)
|#

;-------------------------------------------------------------------------------
(defun papoulis-N-m->degrees-of-freedom (N m &key (C_h 1.0))
  "given sample size N, window parameter m and variance inflation factor C_h,
  returns equivalent degrees of freedom nu for Papoulis lag window
---
Note: see Table 269 of the SAPA book"
  (/ (* 3.41 N) (* m C_h)))

#|
(papoulis-N-m->degrees-of-freedom 512 20)
;==> 87.296

;-------------------------------------------------------------------------------
;;; Pathology checks.
(bartlett-lag-window 0 0)  ;1.0
(bartlett-lag-window 1 0)  ;0.0
(daniell-lag-window 0 0)   ;assert error
(daniell-lag-window 1 0)   ;assert error
(parzen-lag-window 0 0)    ;1.0
(parzen-lag-window 1 0)    ;0.0
(papoulis-lag-window 0 0)  ;1.0
(papoulis-lag-window 1 0)  ;0.0
|#

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;;  Everything below here consists of internal symbols in the SAPA package
;;;  and should be regarded as "dirty laundry" ...
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
(defun get-N-dft (N-nonzero-freqs sample-size)
  (case N-nonzero-freqs
    (:half-next-power-of-2
     (next-power-of-2 sample-size))
    (:next-power-of-2
     (* 2 (next-power-of-2 sample-size)))
    (:twice-next-power-of-2
     (* 4 (next-power-of-2 sample-size)))
    (:Fourier sample-size)
    (otherwise
     (if (and (integerp N-nonzero-freqs)
              (power-of-2 N-nonzero-freqs)
              (>= (* 2 N-nonzero-freqs) sample-size))
       (* 2 N-nonzero-freqs)
       (error "N-nonzero-freqs (~A) is should be either
[a] a power of 2 greater than or equal to ~F (half the sample size)
or
[b] one of the following keywords: half-next-power-of-2,
    next-power-of-2, twice-next-power-of-2 or Fourier"
              N-nonzero-freqs
              (float (/ sample-size 2))
              )))))

#|
(get-N-dft :half-next-power-of-2 77)  ;==> 128
(get-N-dft :next-power-of-2 77)       ;==> 256
(get-N-dft :twice-next-power-of-2 77) ;==> 512
(get-N-dft :Fourier 76)               ;==> 76
(get-N-dft :Fourier 77)               ;==> 77
(get-N-dft 64 77)                     ;==> 128
(get-N-dft 32 77)                     ;==> error
(get-N-dft :foobar 78)                ;==> error
|#

;-------------------------------------------------------------------------------
(defun get-N-freqs
       (N-nonzero-freqs
        sample-size
        return-est-for-0-freq-p)
  (let ((temp (truncate (get-N-dft N-nonzero-freqs sample-size) 2)))
    (if return-est-for-0-freq-p
      (1+ temp)
      temp)))

#|
(get-N-freqs :half-next-power-of-2 77 t)     ;==> 65
(get-N-freqs :half-next-power-of-2 77 nil)   ;==> 64
(get-N-freqs :next-power-of-2 77 t)          ;==> 129
(get-N-freqs :next-power-of-2 77 nil)        ;==> 128
(get-N-freqs :twice-next-power-of-2 77 t)    ;==> 257
(get-N-freqs :twice-next-power-of-2 77 nil)  ;==> 256
(get-N-freqs :Fourier 76 nil)                ;==> 38 
(get-N-freqs :Fourier 77 nil)                ;==> 38
(get-N-freqs :Fourier 78 nil)                ;==> 39
(get-N-freqs :Fourier 76 t)                  ;==> 39 
(get-N-freqs :Fourier 77 t)                  ;==> 39
(get-N-freqs :Fourier 78 t)                  ;==> 40
(get-N-freqs :foobar 78 t)                   ;==> error
(get-N-freqs 64 78 t)                        ;==> 65
(get-N-freqs 63 78 t)                        ;==> error
(get-N-freqs 32 78 t)                        ;==> error
|#

;-------------------------------------------------------------------------------
;;; The next 5 functions support wosa-spectral-estimate
(defun wosa-get-N-freqs
       (block-size
        oversampling-factor
        return-est-for-0-freq-p)
  (let ((temp (* oversampling-factor (/ block-size 2))))
    (if return-est-for-0-freq-p
      (1+ temp)
      temp)))

#|
(wosa-get-N-freqs 256 1 t)         ;==> 129
(wosa-get-N-freqs 256 1 nil)       ;==> 128
(wosa-get-N-freqs 256 2 t)         ;==> 257
(wosa-get-N-freqs 256 2 nil)       ;==> 256
|#

;-------------------------------------------------------------------------------
(defun calculate-number-of-blocks (sample-size block-size proportion-of-overlap)
  (+ 1 (truncate
        (/ (float (- sample-size block-size))
           (* (float block-size) (- 1.0 proportion-of-overlap))))))

;-------------------------------------------------------------------------------
(defun get-offset-to-kth-block (sample-size block-size number-of-blocks k)
  (if (>  number-of-blocks 1)
    (truncate (* k (- sample-size block-size))
              (1- number-of-blocks))
    0))

#|
(calculate-number-of-blocks 1024 256 0.5)  ;==> 7
(dotimes (k 7 (values))
  (print (get-offset-to-kth-block 1024 256 7 k)))
;==> 0 128 256 384 512 640 768

(calculate-number-of-blocks 1024 128 0.5)  ;==> 15
(dotimes (k 15 (values))
  (print (get-offset-to-kth-block 1024 128 15 k)))
;==> 0 64 128 192 256 320 384 448 512 576 640 704 768 832 896
|#

;-------------------------------------------------------------------------------
(defun equivalent-dof-for-wosa
       (N N_S N_B vector-with-data-taper)
  "given the sample size N, the block size N_S, the number of blocks N_B,
and the values of the data taper,
returns the equivalent degrees of freedom for wosa
using Equation (292b) of the SAPA book"
  (let ((acs-taper (acvs vector-with-data-taper
                         :center-data-p nil :acs-p t))
        (n-shift (round (if (= N_B 1)
                          0
                          (/ (- N N_S) (1- N_B))))))
    (transform-a-sequence! #'(lambda (x) (expt x 2)) acs-taper)
    (let ((sum 0.0)
          (m 0))
      (dotimes (m-1 (1- N_B))
        (incf m)
        (if (< (* m n-shift) N_S)
          (incf sum (* (- N_B m)
                       (aref acs-taper (* m n-shift))))
          (return)))
      (values (/ (* 2 N_B) (1+ (/ (* 2.0 sum) N_B)))))))

#|
(dolist (N_S '(64 16 8) (values))
  (print
   (equivalent-dof-for-wosa
    1024
    N_S
    (calculate-number-of-blocks 1024 N_S 0.5)
    (Hanning-data-taper! (make-array N_S :initial-element 1.0)))))
;==>  58.45100403500567
;    233.79011528097186
;    453.18366286133556
|#

;-------------------------------------------------------------------------------
(defun equivalent-dof-for-wosa-standard-case (N_S)
  "given number of blocks N_S,
returns the equivalent degrees of freedom for wosa
with 50% overlap and Hanning data taper
using Equation (294) of the SAPA book"
  (float (/ (* 36 N_S N_S)
            (1- (* 19 N_S)))))
#|
(dolist (N_S '(64 16 8) (values))
  (print (equivalent-dof-for-wosa-standard-case
          (calculate-number-of-blocks 1024 N_S 0.5))))
;==>  58.83673469387755
;    240.73134328358208
;    483.2576383154418
;;; Note: comparison of these results with the above
;;;       indicates that Equation (294) agrees with
;;;       Equation (292b) to within about 10%.
|#
