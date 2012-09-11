;test of distributing files among nodes, to be redone in C&maybe Python;  mike.bobak@gmail.com
;this is another test of the use of my util_mb.lisp but can be loaded w/just a few of them
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
   (labels ((adapt-f2n-pass (sf sn) ;flet doesn't allow rec calls
     (let* ((f1 (first sf)) ;try to match the 2 largest 1st
            (n1 (first sn))); from the sorted lists of files&nodes
       (cond ;make >1 pass now, so it now just what wasn't placed on this pass
         ((null f1) (setf out nil) ;no files w/o nodes
            nil)   
         ((null n1)        ;went through all the nodes 
            (if (eq fstp 0)  ;This will catch a pass that can't assign any files, w/the asked Warning
              (progn (format t "~%This Distribution Ran OUT of Nodes:~%~a ~a" sf fstp) (setf out nil))
              (progn (format t "~%this distrib-PASS ran out of nodes:~%~a ~a" sf fstp) (setf out sf)))
            nil) ;test data only has files left after run/pass, not in the end
        (t    
          (if (<= (size f1) (size n1)) ;maybe also pop/ (remove f1 sf)
            (progn (incf fstp) 
             (cons (assign-f2n f1 n1) (adapt-f2n-pass (rest sf) sn)) )
            (adapt-f2n-pass sf (rest sn)))))))) ;try other nodes 
     ;go for a pass until either out is nil (=run out of files)
    (if (full out)   ;have some but not all assignments yet
     (let ((sna (adapt-f2n-pass out sn2))) ;generated node assignments for this pass
      (format t "~%cur-snAssigned:~a" sna) ;tmp ;(format t "~%cur-out:~a" out) ;tmp
      (if (> fstp 0)
          ;(and (full sna)  ;have some but not all assignments yet
          ;    (full out)) ;could have been reset in latest pass
        (cons sna (distribute2 out sn2))  ;so send undistributed files to another distribution pass
        sna))
     nil))))
    ;need a way to know that not even 1 of the out=sf files could be put in any of the nodes ;fstp

 
(defun sum-2nd (l) (reduce #'+ (mapcar #'second l)))
(defun pct (a b) (/ (- b a) (* 0.01 b)))

(defun distribute (f-fn n-fn)
  "get input &start doling out the files"
  (let* ((sf (txtfile2srt-alst f-fn))
         (sn (txtfile2srt-alst n-fn))
         (lf (len sf))
         (ln (len sn))
         (tf (sum-2nd sf))
         (tn (sum-2nd sn))
         (easy (>= ln lf)))
    (format t "~%~a ~d:file-sz than ~d:node-sz so ~a~%" 
            (pct tf tn) tf tn (if (> tn tf) 'ok 'bad))
    (format t "~%~a ~d:files than ~d:nodes so ~a~%"
            (if easy 'fewer 'more) lf ln (if easy 'easy 'gather))
    (if easy (f-per-n sf sn) ;could get rid of this case, but ok to leave 
      (let ((sna (flat1 (distribute2 sf sn))))
       (mapcar #'(lambda (fn-pr) (format t "~%~a ~a" (first fn-pr) (rest fn-pr))) sna)
       sna))))

(defun tst () 
  "try it out"
   (distribute "files" "nodes"))
 
;can easily (trace distribute2) to see the Size(of the)NodeAssignments, drop
;=had the start of the C version in the last commit, &have a Python started offline ;lsp-like
;I will incl 1 or both after I clean up this file /getting rid of un-needed code/fncs above

;had another pass of getting rid of unneeded code, &some 1st exploratory prints
;;skip ;several lines deleted from 1st attempt ;as ;can't just go w/the 1st plan 
;;got rid of older file assignment as well
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
