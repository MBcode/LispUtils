;;;-*- Mode: LISP; Package: :SAPA; Syntax: COMMON-LISP -*-
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;
;  hacks.lisp
;
;  a hack to correct a deficiency in map-into as implemented in Genera 8.1
;  and in Allegro Common Lisp ...
;  Note:  before compiling and loading hacks.lisp,
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
;;; (compile-file "ccl:SAPA;hacks.lisp")
;;; (load "ccl:SAPA;hacks.fasl")
;-------------------------------------------------------------------------------
(if (not (find-package :SAPA))
  (defpackage :SAPA (:USE :COMMON-LISP)))

(in-package :SAPA)

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;; For some unknown reason, the function map-into is NOT defined
;;; in the package Common-Lisp in the version of Genera under which
;;; the SAPA package was tested (Symbolics Genera 8.1.1 on a Symbolics
;;; MacIvory model 3).  In that Lisp environment, there is a function
;;; called map-into in the packages Symbolics-Common-Lisp
;;; and Future-Common-Lisp, but these do not act in all respects like
;;; map-into as defined in Steele2 (and as implemented in Macintosh Common
;;; Lisp) -- an example of its deficiency is ilustrated in the commented-out
;;; material following the function definition.  Here is a hack that attempts
;;; to provide a map-into that is close enough to the definition in Steele2
;;; so that the functions in SAPA package will run properly.
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
#+genera
(defun map-into (result function &rest rest)
  (if (= (length result) (length (car rest)))
    (apply #'scl:map-into result function rest)
    (let ((temp (make-array (length (car rest)))))
      (apply #'scl:map-into temp function rest)
      (dotimes (i (min (length result) (length temp)) result)
        (setf (elt result i) (svref temp i))))))
#|
;;; Evaluation of the following form in Genera causes an error
;;; because this version of map-into does not like the sequences
;;; to have different lengths -- Steele2 explicitly says that
;;; this is allowed.
(scl:map-into #(1 2 3) #'log #(1 10 100 1000))
|#

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;; For some unknown reason, the function map-into is incorrectly defined
;;; in the package Common-Lisp in the version of Allegro Common Lisp under which
;;; the SAPA package was tested -- an example of its deficiency is ilustrated
;;; in the commented-out material.  Unfortunately, Allegro Common Lisp
;;; does not allow a redefinition of map-into, so I have defined the following
;;; hack to be used in situations where the incorrect definition makes a
;;; difference (currently just in spectral-window-for-direct-spectral-estimate).
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
#+allegro
(defun sapa-map-into (result function &rest rest)
  (if (= (length result) (length (car rest)))
    (apply #'map-into result function rest)
    (let ((temp (make-array (length (car rest)))))
      (apply #'map-into temp function rest)
      (dotimes (i (min (length result) (length temp)) result)
        (setf (elt result i) (svref temp i))))))

#|
;;; Evaluation of the following form in Allegro Common Lisp causes an error
;;; because this version of map-into does not allow the first sequence to be
;;; shorter than the second sequence -- Steele2 explicitly says that
;;; this is ok.
(map-into #(1 2 3) #'log #(1 10 100 1000))
|#
