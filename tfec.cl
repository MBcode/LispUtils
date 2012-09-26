;Hack Night at Heroku, neo4j example, in Lisp, bobak@computer.org
;http://www.meetup.com/Data-Mining/events/80275492/t/cr1_grp/?rv=cr1 went over:
; https://github.com/akollegger/FEC_GRAPH &I coudn't get2boxIcompiled on, so rewrote
; Also wanted an excuse to try cl-neo4j, and compare w/say:allegrograph,vivace-graph-v2..&even KM
; so not only test of basic utils, but of the km utils as well.
; (load "util_mb") (load "km_2-5-33") (load "u2") ;play w/
(defvar *c2h* '(
 ("aic" (CONTRIBKEY commID contribDate contribSearchDate contribAmt CONTRIBTYPE CONTRIBID))
 ("can" (candidateID candidateNAME name candidatePARTY candidateELECTIONYEAR candidateOFFICESTATE candidateOFFICE candidateDISTRICT candidateICI candidateSTATUS candidatePCC candidateST1 candidateST2 candidateMAILCITY candidateMAILST candidateMAILPOSTAL))
 ("com" (commID commNAME name commTREAS commADDR1 commADDR2 commCITY commSTATE commZIP commDSG commTYPE commPARTYAFFIL commFILING))
 ("spd" (RECEIPT_TYPE SUPER_PAC SUPER_PAC_ID DONATING_ORG DONOR_LAST DONOR_FIRST DONOR_CITY DONOR_STATE DONOR_OCCUPATION DONOR_EMPLOYER DONOR_AMOUNT DONATION_DATE TOTAL_AMT TRANS_ID)) ;lines w/^M
 ("spe" (SPENDING_COMM SPENDING_COMM_ID SUPERPAC ELECTION_TYPE CANDIDATE SUPPORT_OPPOSE CANDIDATE_ID CANDIDATE_PARTY CANDIDATE_OFFICE CANDIDATE_DISTRICT CANDIDATE_STATE EXPEND_AMT EXPENDITURE_STATE EXPEND_DATE ELE))
 ))
;load&dump w/cl-neo4j &sv-al 2km &cmp ;mb
;(load "c2h.cl") ;has csv header info, incl above now
(load-kb "c1.km") ;/FEC_GRAPH/DATA> wc c1.km 6      24     194 c1.km
;map&convert just n lines
(defun i-lt-n-p (i n)
  (if (numberp n) (< i n)
    t)) 
(defun apply-lines-n (filename linefnc &optional (n nil))
 (let ((tot 0))
  (with-open-file (stream filename :direction :input)
    (loop for line = (read-line stream nil)
        while (and line (i-lt-n-p tot n))
        do 
        (incf tot)
        (funcall linefnc line)))))
;want to either collect csv lines, or mk km ins w/o collecting, which is preferable for large n
(defun csv-bar (l) (csv_parse-str l :separator #\|))

 
;-redone, to give class.txt mkclskm
(defun 2l2alst (l1 l2) (mapcar #'cons l1 l2))
;(defun mkhl (h l) (2l2alst h l))
(defun mkhl (h l) 
  "alst of:csv header&list of values for a line"
  (rm-nil (mapcar #'(lambda (a b) (when b (cons a b))) h l)))
(defun first-nonnil (l) (first (rm-nil l)))
               ;(i (or (first l) (second l)))
(defun assoc2 (a b) 
  "val/2nd of assoc"
  (let ((as (assoc a b :test #'equal)))
    (when as (second as))))

(defun mkclskm (cls &optional (n 5)) 
  "was prs-barfile cls.txt, &optional n, &map flet/lambda over it"
  (let ((f (str-cat cls ".txt"))
       ;(h (assoc cls *c2h* :test #'equal))
        (h (assoc2 cls *c2h*))
        )
    (apply-lines-n f
      #'(lambda (s)
         (let* ((l (csv-bar s))
                (i (first-nonnil l))) ;might pass in attr for ID
            (sv-cls i cls)
            (sv-al i (mkhl h l))))
      n)))
;
;(trace csv-bar sv-al)
;(trace mkhl sv-al assoc2)
;
(defun init-fec ()
  (mkclskm "aic")
  (mkclskm "can")
  (mkclskm "com")
  )
;taxonomy to look at it
;Thing
;   fec
;      aic
;I        *C00000885
;I        *C00000901
;I        *C00000935
;      can
;I        *H0AK00089
;I        *H0AK00097
;I        *H0AL00016
;I        *H0AL01030
;I        *H0AL02087
;      com
;I        *C00000042
;I        *C00000059
;I        *C00000422
;I        *C00000489
;I        *C00000547
;      spd
;      spe
 
