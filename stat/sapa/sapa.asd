(defpackage :SAPA (:USE :COMMON-LISP))

(defsystem sapa
 :name "sapa"
 :version "0.0.1"
 :maintainer "bobak"
 :author "orig"
 :description "adding asd" 
 :components (
  (:module sapa 
    :components (
    (:file "utilities")
    (:file "acvs")
    (:file "basic-math")
    (:file "basic-statistics")
    (:file "dft-and-fft")
    (:file "filtering")
    (:file "hacks")
    (:file "harmonic")
    (:file "matrix")
    (:file "multitaper")
    (:file "nonparametric")
    (:file "parametric")
    (:file "random")
    (:file "sapa-package")
    (:file "tapers")
  ; (:file "examples")
    )  :serial t)))

#+ignore ;in load file
(defun test (&optional (fn "sapa/examples.lisp"))
  (load fn :print t))
