(defsystem ml
 :name "ml"
 :version "0.0.1"
 :maintainer "bobak"
 :author "orig"
 :description "adding asd" 
 :components (
  (:module ml :components (  
    (:file "data-utilities")
    (:file "deduce")
    (:file "theory-utilities")
    (:file "binary-encoder")
    (:file "t-test")
    (:file "aq")
    (:file "backprop-multi")
    (:file "backprop")
    (:file "bayes-indp")
    (:file "c4.5-multi")
    (:file "c4.5")
    (:file "cobweb")
    (:file "data-generator")
    (:file "dlist")
    (:file "dna-standard")
    (:file "id3-all-multi")
    (:file "id3-all")
    (:file "knn")
    (:file "labor-neg")
    ;(:file "learn-curves") ;needs clasp
    (:file "perceptron")
    (:file "pfoidl")
    (:file "pfoil-cnf")
    (:file "pfoil")
    (:file "pgolem")
    ;(:file "sample-dna-saved-tests")  ;check on
    (:file "univ-tester-multi")
    (:file "universal-tester")
    )  :serial t)))
