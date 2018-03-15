;test of distributing files among nodes, to be redone in C&maybe Python;  mike.bobak@gmail.com
;(load "util_mb") ;this is another test of the use of my util_mb.lisp but only uses a few fncs
;USE:  sbcl --eval '(progn (load "util_mb") (load "tbl.cl") (setf *dbg* nil) (tst))'
(lu)
(defvar *dbg* t)

(defun split2n (txt2)
  "split txt-pair, change str->num on 2nd"
  (let ((tl (split txt2)))
    (list (first tl) (numstr (second tl)) 
          (len (second tl)) ;quickly see the size of a number
          )))

(defun txtfile2alst (fn)
  "split 2col fn.txt"
  (rest (mapcar #'split2n (list-lines (str-cat fn ".txt")))))

(defun txtfile2srt-alst (fn)
  (sort (txtfile2alst fn) #'> :key #'second))

;local accessors
(defun name (pr)
  "a-lst has name in 1st position"
  (first pr))
(defun size (pr)
  "a-lst has size in 2nd position"
  (second pr))

(defun assign-f2n (f1 n1)
  "file to node, alter node size" ;should remove file so can't reassign
  ;if (< (size f1) (size n1))  ;decf node size
  (rplaca (rest n1) (- (size n1) (size f1)))
  (cons (name f1) (name n1))) ;ret: SizedNameAssignment pair

(defun f-per-n (sf sn)
  "file per node"
  ;(mapcar #'(lambda (f n) (list (name f) (name n))) sf sn)
  (mapcar #'(lambda (f n) (assign-f2n f n) (list (name f) (name n))) sf sn)
  ) ;get rid of &easy test, as the more general should handle it


(defun distribute2 (sf2 sn2) 
  "all in one distribute helper, makes as many passes over the nodes as needed"
 (let ((out sf2)  ;;ran-out(not yet placed in a pass)sized-Files ;set to sf2  for test below
       (fstp 0)) ;files set this pass
   (labels ((adapt-f2n-pass (sf sn) ;flet doesn't allow rec calls,right away
     (let* ((f1 (first sf)) ;try to match the 2 largest 1st
            (n1 (first sn))); from the sorted lists of files&nodes
       (cond ;make >1 pass now, so it now just what wasn't placed on this pass
         ((null f1) (setf out nil) ;no files w/o nodes
            nil)   
         ((null n1)        ;went through all the nodes 
            (if (eq fstp 0)  ;This will catch a pass that can't assign any files, w/the asked Warning
              (progn (format t "~%This Distribution Ran OUT of Nodes:~%~a ~a" sf fstp) (setf out nil))
              (progn (when *dbg* (format t "~%this distrib-PASS ran out of nodes:~%~a ~a" sf fstp))
                     (setf out sf)))
            nil) ;test data only has files left after run/pass, not in the end
        (t    
          (if (<= (size f1) (size n1)) ;maybe also pop/ (remove f1 sf)
            (progn (incf fstp) 
             (cons (assign-f2n f1 n1) (adapt-f2n-pass (rest sf) sn)) )
            (adapt-f2n-pass sf (rest sn)))))))) ;try other nodes 
     ;go for a pass until either out is nil (=run out of files)
    (if (full out)   ;have some but not all assignments yet
     (let ((sna (adapt-f2n-pass out sn2))) ;generated node assignments for this pass
       (when *dbg*
        (format t "~%cur-snAssigned:~a" sna) ;tmp 
        (format t "~%cur-out_of-pass:~a" out)) ;tmp
      (if (> fstp 0)
        (cons sna (distribute2 out sn2))  ;so send undistributed files to another distribution pass
        sna))
     nil))))
    ;need a way to know that not even 1 of the out=sf files could be put in any of the nodes ;fstp

 
(defun sum-2nd (l) (reduce #'+ (mapcar #'second l)))
(defun pct (a b) (/ (- b a) (* 0.01 b)))

#+ignore ;original
(defun distribute (f-fn n-fn)
  "get input &start doling out the files"
  (let* ((sf (txtfile2srt-alst f-fn))
         (sn (txtfile2srt-alst n-fn))
         (lf (len sf))
         (ln (len sn))
         (tf (sum-2nd sf))
         (tn (sum-2nd sn))
         (easy (>= ln lf)))
    (when *dbg*
     (format t "~%~a ~d:file-sz than ~d:node-sz so ~a~%" 
            (pct tf tn) tf tn (if (> tn tf) 'ok 'bad))
     (format t "~%~a ~d:files than ~d:nodes so ~a~%"
            (if easy 'fewer 'more) lf ln (if easy 'easy 'gather)))
    (if easy (f-per-n sf sn) ;could get rid of this case, but ok to leave 
      (let ((sna (flat1 (distribute2 sf sn))))
       (mapcar #'(lambda (fn-pr) (format t "~%~a ~a" (first fn-pr) (rest fn-pr))) sna)
       sna))))
;split apart so can call w/o files:

;defun distribute (f-fn n-fn)
(defun distribute (sf sn)
  "get input &start doling out the files"
  (let* (;(sf (txtfile2srt-alst f-fn))    ;;;These did a sort, so will/might want2do again
         ;(sn (txtfile2srt-alst n-fn))
         (lf (len sf))
         (ln (len sn))
         (tf (sum-2nd sf))
         (tn (sum-2nd sn))
         (easy (>= ln lf)))
    (when *dbg*
     (format t "~%~a ~d:file-sz than ~d:node-sz so ~a~%" 
            (pct tf tn) tf tn (if (> tn tf) 'ok 'bad))
     (format t "~%~a ~d:files than ~d:nodes so ~a~%"
            (if easy 'fewer 'more) lf ln (if easy 'easy 'gather)))
    (if easy (f-per-n sf sn) ;could get rid of this case, but ok to leave 
      (let ((sna (flat1 (distribute2 sf sn))))
       (mapcar #'(lambda (fn-pr) (format t "~%~a ~a" (first fn-pr) (rest fn-pr))) sna)
       sna))))

(defun distribute-fn (f-fn n-fn)
  "get input &start doling out the files"
  (let* ((sf (txtfile2srt-alst f-fn))
         (sn (txtfile2srt-alst n-fn))
         )
    (distribute sf sn)))

(defun tst () 
  "try it out"
   (distribute "files" "nodes"))
 
;can easily (trace distribute2) to see the Size(of the)NodeAssignments, drop
;=had the start of the C version in the last commit, &have a Python started offline ;lsp-like
;USER(1): (tst)
;
;11.175601 433984592140:file-sz than 488587138990:node-sz so OK
;
;MORE 24:files than 10:nodes so GATHER
;
;this distrib-PASS ran out of nodes:
;((file18 6609806629 10) (file11 6348867697 10) (file15 5942107928 10)
; (file9 4495356117 10) (file10 3118866364 10) (file17 2424678728 10)
; (file14 1293428979 10) (file8 170858581 9)) 16
;cur-snAssigned:((file16 . node5) (file6 . node5) (file21 . node0)
;                (file3 . node0) (file0 . node6) (file1 . node6)
;                (file13 . node9) (file4 . node9) (file20 . node7)
;                (file7 . node7) (file23 . node8) (file19 . node8)
;                (file2 . node4) (file22 . node4) (file12 . node1)
;                (file5 . node3))
;cur-snAssigned:((file18 . node5) (file11 . node0) (file15 . node6)
;                (file9 . node6) (file10 . node9) (file17 . node9)
;                (file14 . node9) (file8 . node9))
;==those not assigned on 1st pass got assigned in the 2nd  
;;;use next line to just get file-assignments:
;sbcl --noinform --eval '(progn (load "util_mb") (load "tbl.cl") (setf *dbg* nil) (tst))'
;file16 node5
;file6 node5
;file21 node0
;file3 node0
;file0 node6
;file1 node6
;file13 node9
;file4 node9
;file20 node7
;file7 node7
;file23 node8
;file19 node8
;file2 node4
;file22 node4
;file12 node1
;file5 node3
;file18 node5
;file11 node0
;file15 node6
;file9 node6
;file10 node9
;file17 node9
;file14 node9
;file8 node9 
;========================================tlb.cl above t.cl(or knap.cl) below:
;use above but w/1 node, and just give the weights as file sizes
;(distribute  nw (list capacity))
;when resort need to save orig ordering
;(lu)
;(defvar *dbg* t) (defvar *dbg* nil)
(defun lr (n2)  ;can use this as the :key in the sort directly 
  (let ((nv (first n2))
        (nw (second n2)))
    (if (and (numberp nv) (numberp nw)) (/ nv nw) 0)))

(defun numberpairs1 (l)
  "add numbering to list so can get ordering back after a sort"
  (loop for i to (1- (len l)) collect (list (nth i l) i)))
; (numberpairs1 '(a b c)) ; ((A 0) (B 1) (C 2))
(defun numberpairs (l)
  "add numbering to list so can get ordering back after a sort"
  (loop for i to (1- (len l)) collect (append (list+ (nth i l)) (list+ i))))
;USER(4): (numberpairs '(a b c)) ((A 0) (B 1) (C 2))
;USER(5): (numberpairs '((1 a) (2 b) (3 c))) ((1 A 0) (2 B 1) (3 C 2))

(defun line-nums (str) 
 (when (> (len str) 2) (mapcar #'numstr (remove "" (explode str) :test #'equal))))

;(trace numberpairs)
(trace map-lines distribute)

(defun knap2 (&optional (fn "tmp.data"))
  (let* ((in (map-lines fn #'line-nums))
         (ic (first in)) ;the pair: items,capacity
         (capacity (second ic))
         (it (rest in)) ;was 'is' below, but distribute does it's own sort
         ;(wtl (mapcar #'first (rest in)))
         (wtl (rest in))
         ;(wtl (mapcar #'(lambda (pr) (cons (first pr) (second pr))) (rest in))) ;no,as lists
         (fwt (mapcar #'first ;as only1node, just want file-sz/weights
                      (distribute  wtl 
                                   ;(list (cons 'node1 capacity)) ;also not alst
                                   (list (list 'node1 capacity))
                                   )))
         (val (sum-l fwt)))
    ;go down 'it' list and find each wt(assuming no dups,which could happen),&set the vals list
    ;positions would find dups, just use member right now, for the bool
  (format t "~%~a 0" val)
  (format t "~%~{~a~^ ~}" ;taken
          (mapcar #'(lambda (wt) (if (member wt fwt) 1 0)) it)
          )
  ));did not yet find the taken vector from tlb findings /FIX/finish

(defun knap (&optional (fn "tmp.data"))
  (let* ((in (map-lines fn #'line-nums))
         (ic (first in)) ;the pair: items,capacity
         (is (sort 
               (numberpairs (rest in)) ;(copy-list (rest in)) 
                   #'> :key #'lr)))
;I could also add the original order in here, so after sorted I could recover the final knapsacked
    (let ((val 0)
          (wt 0)
          (items (first ic)) ;could limit initial read/list-len
          (capacity (second ic))
          (taken '()))   ;use pair, from all but 1st line
      (when *dbg* (format t "~%items=~a cap=~a" items capacity))
  (loop for n2 in is do
        (let  ((vi (first n2))
               (wi (second n2)))
          (when *dbg* (format t "~%~a,~a " vi wi))
          (if (<= (+ wt wi) capacity) (progn (push 1 taken) (incf wt wi) (incf val vi)
                                             (when *dbg* (format t "~%take:~a,~a " wt val)))
            (push 0 taken))))
  (format t "~%~a 0" val)
  ;now pair last of is w/taken, and resort on the orig ordering# before putting out
  (format t "~%~{~a~^ ~}" (mapcar #'first
          (sort
            (mapcar #'(lambda (tk i) (list tk (last i))) taken is)
            #'> :key #'(lambda (x) (caar (last x))) ;#'last
            )))
  ;(format t "~%~{~a~^ ~}" taken)
  );only problem is if order of taken is supposed to line up w/the un-sorted list
    ))
;(knap) (exit)
;=new for tlb2.cl:
(knap2)
(exit)
