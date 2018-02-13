;bayes example bobak@balisp.org ;use: mike.bobak@gmail
;cl-bayesnet example asia(2).dne
(time (defparameter *jt* (bn:use-join-tree *join-tree*))) 
(format t "~%~a" (bn:node-order *join-tree*))
;(bn:node-order *join-tree*)
;#(:VISITASIA :TUBERCULOSIS :SMOKING :CANCER :TBORCA :XRAY :BRONCHITIS :DYSPNEA) ;so
;(defvar *nds* '(:VISITASIA :TUBERCULOSIS :SMOKING :CANCER :TBORCA :XRAY :BRONCHITIS :DYSPNEA))
(defvar *nds* (coerce  (bn:node-order *join-tree*) 'list)) ;now can work w/any loaded net
(defun gn (n) 
  "get-node" 
  (bn:node n *jt*))
;(defun qn (n) "query-node" (bn:query (gn n))) ;opt qry
;(defun qni (ni) "query-node" (bn:query ni)) ;opt qry
(defun qn (n) 
  "query-node" 
  (let ((ni (if (symbolp n) (gn n) n)))
    (bn:query ni))) ;opt qry
;(defvar *nis* (mapcar #'gn *nds*)) ;no longer needed 
;(defun gs (n) "get-states"
;  (if (symbolp n) (bn:states (gn n))
;    (bn:states n)))
(defun gs (n) 
  "get-states"
  (let ((ni (if (symbolp n) (gn n) n)))
    (bn:states ni)))
(defun gqs (n) 
  "get-qry+states"
  (let ((ni (if (symbolp n) (gn n) n)))
    (cons (bn:states ni) (qn ni))))
;;(defun all-states () (mapcar #'(lambda (ni nn) (cons nn (bn:states ni))) *nis* *nds*))
;(defun all-states () (mapcar #'(lambda (ni) (cons (bn:name ni) (bn:states ni))) *nis*))
(defun all_states () (mapcar #'(lambda (n) (cons n (bn:states (gn n)))) *nds*))
(defun all-qry () (mapcar #'(lambda (n) (cons n (qn n))) *nds*)) ;want prob near state names:
;(defun all-qrys () (mapcar #'(lambda (n) (list n (qn n) (bn:states (gn n)))) *nds*))
(defun all-qrys () (mapcar #'(lambda (n) (list n (qn n) (gs n))) *nds*))
(defun all_qrys () (mapcar #'(lambda (n) (cons n (gqs n))) *nds*))
(defun pqn (sa pa rn) 
  ;format t " ~a=~,3f~a" 
  (format t " ~a=~$~a" 
          (aref sa rn) (aref pa rn) #\Tab)
  (format t " ~a=~$ /~,4f~a" (aref sa rn) (aref pa rn) (aref pa rn) #\Tab)
  )
(defun pq (q) 
  "print query, start w/2state assumption";then use: (array-dimension ary 0)
  (let ((n (first q))
        (pa (second q))
        (sa (third q)))
    (format t "~%~a~a" n #\Tab) (pqn sa pa 0) (pqn sa pa 1) ;&loop over states
  n))
(defun paq () "print all-qrys" (mapcar #'pq (all-qrys)))
; (bn:with-evidence (*jt* :visitasia :visit) (paq))
(defun tq () (print "(bn:with-evidence (*jt* :visita :visit) (paq))")) ;ld3.cl now
; (bn:with-evidence (*jt* :visitasia :visit) (all_qrys))
; just do by hand for now
;(defun tq () (print "(bn:with-evidence (*jt* :visitasia :visit) (all_qrys))"))
;(defun qwe (pll)
;  "all-qrys with-evidence plist"
; (bn:with-evidence (*jt* pll) (all_qrys)))
;;defmacro with-evidence ((net &rest evidence) &body body)
;(defmacro we ((evidence &optional (net *jt*)) &body body)
;  "Saves the existing evidence in the net and sets the evidence to
;evidence. Restores the net to its previous state after leaving the
;with-evidence block."
;  (let ((gnet (gensym)))
;    `(let ((,gnet ,net))
;       (bn:save-evidence ,gnet
;     (setf (evidence ,gnet) ',evidence)
;     ,@body))));would need another macro&more from src/bn.lisp 
;(defun qwe (pll)
;  "all-qrys with-evidence plist"
;  (we (pll) (all_qrys)))
