;;;-*- Mode: LISP; Package: :CL-USER; Syntax: COMMON-LISP -*-
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;
;  sapa-package.lisp
;
;  In some implementations of Lisp, the first line of a Lisp file (the so-called
;  mode line) is parsed automatically when opened by an editor, and,
;  if the package indicated by the mode line does not yet exist,
;  all sorts of strange things can happen.   To circumvent this annoyance,
;  we have created this short file that does nothing more than define
;  the SAPA package from within the CL-USER package, which -- according to
;  Steele2 -- is ALWAYS supposed to exist.  The idea is that you should
;  compile and load this file PRIOR to attempting to do anything with
;  any of the other files in sapaclisp contribution to StatLib.
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
;;; (compile-file "home:SAPA;sapa-package.lisp")
;;; (load "home:SAPA;sapa-package.fasl")
;-------------------------------------------------------------------------------
(in-package :CL-USER)

(if (not (find-package :SAPA))
  (defpackage :SAPA (:USE :COMMON-LISP)))
