;test of distributing files among nodes, to be redone in C&maybe Python;  mike.bobak
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
  "file to node, alter node size"
  ;if (< (size f1) (size n1))  ;decf node size
  (rplaca (rest n1) (- (size n1) (size f1)))
  (cons (name f1) (name n1)))

(defun f-per-n (sf sn)
  "file per node"
  ;(mapcar #'(lambda (f n) (list (name f) (name n))) sf sn)
  (mapcar #'(lambda (f n) (assign-f2n f n) (list (name f) (name n))) sf sn)
  ) ;get rid of &easy test, as the more general should handle it

(defun distribute2 (sf sn) ;consider this further generalization/being able to handle not yet seen data
  "all in one distribute helper, makes as many passes over the nodes as needed"
 (let ((out sf))  ;;ran-out(not yet placed in a pass) sized-files
   (labels ((adapt-f2n-pass (sf sn) ;flet doesn't allow rec calls
     (let* ((f1 (first sf))
            (n1 (first sn)))
       (cond
         ((null f1) nil)   ;make >1 pass now, so it now just what wasn't place on this pass
         ((null n1)  (when (equal out sf) (format t "~%this distribution ran out of nodes:~%~a" sf)) 
                     (setf out sf) (format t "~%this distrib-run ran out of nodes:~%~a" sf) nil)
        (t    
          (if (< (size f1) (size n1)) ;maybe also pop/ (remove f1 sf)
            (cons (assign-f2n f1 n1) (adapt-f2n-pass (rest sf) sn))
            (adapt-f2n-pass sf (rest sn)))))))) ;try other nodes 
     ;go for a pass until either out is nil (=run out of files)
    (let ((sna (adapt-f2n-pass out sn))) ;generated node assignments for this pass
      (format t "~%cur-sna:~a" sna) ;tmp ;(format t "~%cur-out:~a" out) ;tmp
      (if (and (full out) (full sna))  ;have some but not all assignments yet
        (cons sna (distribute2 out sn))  ;so send undistributed files to another distribution pass
        sna)
    ))))
    ;need a way to know that not even 1 of the out=sf files could be put in any of the nodes
    ; one bad way is to see that out hasn't changed ;well print above catches on 1st try

(defun sum-2nd (l) (reduce #'+ (mapcar #'second l)))
(defun pct (a b) (/ (- b a) (* 0.01 b)))

(defun distribute (f-fn n-fn)
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
    (if easy (f-per-n sf sn) 
      (mapcar #'(lambda (fn-pr) (format t "~%~a ~a" (first fn-pr) (rest fn-pr)))
          (flat1 (distribute2 sf sn)))
      )))

(defun tst () 
  "try it out"
   (distribute "files" "nodes"))

;had another pass of getting rid of unneeded code, &some 1st exploratory prints
;;skip ;several lines deleted from 1st attempt ;as ;can't just go w/the 1st plan 
;;got rid of older file assignment as well
;USER(1): (tst)
;
;11.175601 433984592140:file-sz than 488587138990:node-sz so OK
;
;MORE 24:files than 10:nodes so GATHER
;
;this distrib-run ran out of nodes:
;((file18 6609806629 10) (file11 6348867697 10) (file15 5942107928 10)
; (file9 4495356117 10) (file10 3118866364 10) (file17 2424678728 10)
; (file14 1293428979 10) (file8 170858581 9))
;cur-sna:((file16 . node5) (file6 . node5) (file21 . node0) (file3 . node0)
;         (file0 . node6) (file1 . node6) (file13 . node9) (file4 . node9)
;         (file20 . node7) (file7 . node7) (file23 . node8) (file19 . node8)
;         (file2 . node4) (file22 . node4) (file12 . node1) (file5 . node3))
;cur-sna:((file18 . node5) (file11 . node0) (file15 . node6) (file9 . node6)
;         (file10 . node9) (file17 . node9) (file14 . node9) (file8 . node9))
;cur-sna:((file18 . node9) (file11 . node7) (file15 . node7) (file9 . node8)
;         (file10 . node8) (file17 . node1) (file14 . node1) (file8 . node1))
;this distribution ran out of nodes:
;((file18 6609806629 10) (file11 6348867697 10) (file15 5942107928 10)
; (file9 4495356117 10) (file10 3118866364 10) (file17 2424678728 10)
; (file14 1293428979 10) (file8 170858581 9))
;this distrib-run ran out of nodes:
;((file18 6609806629 10) (file11 6348867697 10) (file15 5942107928 10)
; (file9 4495356117 10) (file10 3118866364 10) (file17 2424678728 10)
; (file14 1293428979 10) (file8 170858581 9))
;cur-sna:NIL
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
;file18 node9
;file11 node7
;file15 node7
;file9 node8
;file10 node8
;file17 node1
;file14 node1
;file8 node1 
;=had the start of the C version in the last commit, &have a Python started offline
;I will incl 1 or both after I clean up this file /getting rid of un-needed code/fncs above
