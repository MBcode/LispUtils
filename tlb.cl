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

(defun round-up (x) ;from lhstats  ;not needed now
    (multiple-value-bind (rounded ignore) (ceiling x)
          (declare (ignore ignore))
              rounded))

;local accessors
(defun name (pr)
  "a-lst has name in 1st position"
  (first pr))
(defun size (pr)
  "a-lst has size in 2nd position"
  (second pr))


(defun assign-f2n (f1 n1)
  ;if (< (size f1) (size n1))  ;decf node size
  (rplaca (rest n1) (- (size n1) (size f1)))
  (cons (name f1) (name n1)))

(defun f-per-n (sf sn)
  "file per node"
  ;(mapcar #'(lambda (f n) (list (name f) (name n))) sf sn)
  (mapcar #'(lambda (f n) (assign-f2n f n) (list (name f) (name n))) sf sn)
  )
;(trace f-per-n assign-f2n)
(defvar *out1* nil) ;tmp
(defun gather-adapt-f2n (sf sn)
  (let* ((f1 (first sf))
         (n1 (first sn))
         )
    (cond
      ((null f1) nil)   ;make >1 pass now, so it now just what wasn't place on this pass
      ((null n1) (setf *out1* sf) (format t "~%this distribution ran out of nodes:~%~a" sf) nil)
     ;((null n1) (format t "~%this distribution ran out of nodes:~%~a" sf) (list sn)) ;so can try again
      (t
        (if (< (size f1) (size n1)) ;maybe also pop/ (remove f1 sf)
          (cons (assign-f2n f1 n1) (gather-adapt-f2n (rest sf) sn))
          (gather-adapt-f2n sf (rest sn))) ;try other nodes 
        ;could go back over w/smaller ones to see if they would fit now
        ;Or if doesn't fit instead of giving up on node, go down files looking4one that is small enough
      ))))

;should at least pick largest/best fit for last2put in; can use gn(frac)for ave num/bin
; consider a best1st,..
; consider(recording)pct full on each bin
; rest-below  ;&get best fit in the bin  ;;didn't even have2read bin-packing
; next pass could just be, largest2largest then best fitting of what is left

(defun sum-2nd (l) (reduce #'+ (mapcar #'second l)))
(defun pct (a b) (/ (- b a) (* 0.01 b)))

;I did code up another pass at this, but it over/re distr some files, caught both in a ;check-warn &output check
;so will revert to this messy file for a bit
#+ignore 
(defun distr-pass (sf sn)  ;make the 2 /or more if needed later/ passes at doling out the files
  "make passes at gathering assignments , need to pop files off though, or ret list of present files"
  ;while sf
    ;(sna2 (gather-adapt-f2n *out1* sn))
  ) ;could flet so sf doesn't have to be ret, but is a local pop of as files taken off

(defun distribute (f-fn n-fn)
  (let* ((sf (txtfile2srt-alst f-fn))
         (sn (txtfile2srt-alst n-fn))
         (lf (len sf))
         (ln (len sn))
         (tf (sum-2nd sf))
         (tn (sum-2nd sn))
         (easy (>= ln lf))
         ;(pass1 (f-per-n sf sn)) ;pass1
         )
    (format t "~%~a ~d:file-sz than ~d:node-sz so ~a~%" 
            (pct tf tn) tf tn (if (> tn tf) 'ok 'bad))
    (format t "~%~a ~d:files than ~d:nodes so ~a~%"
            (if easy 'fewer 'more) lf ln (if easy 'easy 'gather))
    (if easy (f-per-n sf sn) 
      (let* ((sna (gather-adapt-f2n sf sn))
             (sna2 (gather-adapt-f2n *out1* sn))
             ) 
       ;(format t "~%get:~a" (gather-adapt-f2n sf sn))
        (format t "~%get1:~a" sna)
        ;could go back over w/smaller ones to see if they would fit now
        ;nodes always there(w/smaller sizes), but file list is smaller now
        ;(format t "~%get2:~a" (gather-adapt-f2n *out1* sn))
        (format t "~%get2:~a" sna2)
      ;final answer would be append get2 get1  ;print out as requested below:
      (mapcar #'(lambda (fn-pr) (format t "~%~a ~a" (first fn-pr) (rest fn-pr))) (append sna sna2))
      ))))

;did twice as expect 2or3/bin, but if much higher&didn't distribute well right away(nice zipf/etc distr?)then might need>2passes
;could be generalized/cleaned up, but there is a desire to see it in other language/s
;could also make small mods to get into STELLA &dump in both Java&C++
;(trace gather-adapt-f2n)
;(trace gather-ln round-up name size)
;;;=get rid of these lines ;1st exploratory code ..
;(defvar *files* (txtfile2alst "files"))
;(defvar *nodes* (txtfile2alst "nodes"))
;;could just sort right away; in fact will do all in the 1main fnc
;(defvar *sf* (sort *files* #'> :key #'second))
;(defvar *sn* (sort *nodes* #'> :key #'second))
;;nice to see that there is always a larger node & len.sn>len.sf
;(defvar *sa* (sort (append *files* (copy-seq *nodes*)) #'> :key #'second))
;USER(1): *sa* 
;(("node5" 100718892926 12) ("node0" 94524502533 11) ("node6" 72343165238 11)
; ("node9" 61751021819 11) ("node7" 51885044325 11) ("file16" 45264051186 11)
; ("file6" 42822363289 11) ("node8" 42484467910 11) ("file21" 42111644867 11)
; ("file3" 40239886027 11) ("file0" 34656959152 11) ("node4" 30962561208 11)
; ("file1" 25232891343 11) ("file13" 23721335923 11) ("file4" 23650076783 11)
; ("file20" 20496566233 11) ("node1" 19405881468 11) ("file7" 19043424466 11)
; ("file23" 17332335657 11) ("file19" 16680091213 11) ("file2" 15749300904 11)
; ("file22" 14448309360 11) ("file12" 12835739361 11) ("node3" 9819857373 10)
; ("file5" 9295645353 10) ("file18" 6609806629 10) ("file11" 6348867697 10)
; ("file15" 5942107928 10) ("node2" 4691744190 10) ("file9" 4495356117 10)
; ("file10" 3118866364 10) ("file17" 2424678728 10) ("file14" 1293428979 10)
; ("file8" 170858581 9))
;USER(2): ;a file can only go to a node closer to the head of the list 
(defun tst () 
   (distribute "files" "nodes"))

;;skip ;several lines deleted from 1st attempt ;as ;can't just go w/the 1st plan 
;USER(1): (tst) 
;11.175601 433984592140:file-sz than 488587138990:node-sz so OK 
;MORE 24:files than 10:nodes so GATHER 
;this distribution ran out of nodes:  ;on the 1st pass, but gets it on the second
;((file18 6609806629 10) (file11 6348867697 10) (file15 5942107928 10)
; (file9 4495356117 10) (file10 3118866364 10) (file17 2424678728 10)
; (file14 1293428979 10) (file8 170858581 9))
;get1:((file16 . node5) (file6 . node5) (file21 . node0) (file3 . node0)
;      (file0 . node6) (file1 . node6) (file13 . node9) (file4 . node9)
;      (file20 . node7) (file7 . node7) (file23 . node8) (file19 . node8)
;      (file2 . node4) (file22 . node4) (file12 . node1) (file5 . node3))
;get2:((file18 . node5) (file11 . node0) (file15 . node6) (file9 . node6)
;      (file10 . node9) (file17 . node9) (file14 . node9) (file8 . node9))
;NIL
;USER(2):  ;final output is the combination of get1+get2 
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
;=had the start of the C version in the last commit, &have a Python started offline ;lsp-like
;I will incl 1 or both after I clean up this file /getting rid of un-needed code/fncs above 
