;test of distributing files among nodes, to be redone in C;  mike.bobak
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

(defvar *files* (txtfile2alst "files"))
(defvar *nodes* (txtfile2alst "nodes"))
;could just sort right away; in fact will do all in the 1main fnc
(defvar *sf* (sort *files* #'> :key #'second))
(defvar *sn* (sort *nodes* #'> :key #'second))
;nice to see that there is always a larger node & len.sn>len.sf
(defvar *sa* (sort (append *files* (copy-seq *nodes*)) #'> :key #'second))
(defun round-up (x) ;from lhstats
    (multiple-value-bind (rounded ignore) (ceiling x)
          (declare (ignore ignore))
              rounded))
(defun name (pr)
  "a-lst has name in 1st position"
  (first pr))
(defun size (pr)
  "a-lst has size in 2nd position"
  (second pr))

(defun gather-ln (l n)
  "gather up list in groups of n long"
  (if (len-lt l n) l
    (cons (subseq l 0 n) (gather-ln (subseq l n (len l)) n)))) 

(defun distribute (f-fn n-fn)
  (let* ((sf (txtfile2srt-alst f-fn))
         (sn (txtfile2srt-alst n-fn))
         (lf (len sf))
         (ln (len sn))
         (easy (> ln lf)))
    (format t "~%~a ~d:files than ~d:nodes so ~a~%"
            (if easy 'fewer 'more) lf ln (if easy 'easy 'gather))
    (if easy (mapcar #'(lambda (f n) (list (name f) (name n))) sf sn)
      (let* ((gn 3) ;fix(round-up (/ sf sn))
             (gsf (gather-ln sf gn))
             (f2n
              (mapcar #'(lambda (f n) (list (mapcar #'name f) (reduce #'+ (mapcar #'size f))  n)) 
                                           gsf sn)
             ))
        (format t "~%dbl-up ~a into~% ~a ~%, if dole out in groups of ~d" sf sn gn)
             (format t "~%gather-files: ~a~%" gsf)
            ;(mapcar #'(lambda (f n) (list (mapcar #'name f) (name n))) gsf sn)
            (mapcar #'(lambda (l3) (if (< (second l3) (size (third l3))) 
                                     (format t "~%put ~a at ~a" (first l3) (name (third l3)))
                                     (format t "~%warn:too-much:~a" l3)))  f2n)
      ))))

;(trace gather-ln ceiling)
;(trace gather-ln round-up name size)
;(trace gather-ln)
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

;; NAME
;; GATHER-LN
;; DISTRIBUTE
;; TST
;USER(1): (tst)
;
;MORE 24:files than 10:nodes so CANSTILLDO
;
;dbl-up ((file16 45264051186 11) (file6 42822363289 11) (file21 42111644867 11)
;        (file3 40239886027 11) (file0 34656959152 11) (file1 25232891343 11)
;        (file13 23721335923 11) (file4 23650076783 11) (file20 20496566233 11)
;        (file7 19043424466 11) (file23 17332335657 11) (file19 16680091213 11)
;        (file2 15749300904 11) (file22 14448309360 11) (file12 12835739361 11)
;        (file5 9295645353 10) (file18 6609806629 10) (file11 6348867697 10)
;        (file15 5942107928 10) (file9 4495356117 10) (file10 3118866364 10)
;        (file17 2424678728 10) (file14 1293428979 10) (file8 170858581 9)) into
; ((node5 100718892926 12) (node0 94524502533 11) (node6 72343165238 11)
;  (node9 61751021819 11) (node7 51885044325 11) (node8 42484467910 11)
;  (node4 30962561208 11) (node1 19405881468 11) (node3 9819857373 10)
;  (node2 4691744190 10)) 
;, if dole out in groups of 3
;gather-files: (((file16 45264051186 11) (file6 42822363289 11)
;                (file21 42111644867 11))
;               ((file3 40239886027 11) (file0 34656959152 11)
;                (file1 25232891343 11))
;               ((file13 23721335923 11) (file4 23650076783 11)
;                (file20 20496566233 11))
;               ((file7 19043424466 11) (file23 17332335657 11)
;                (file19 16680091213 11))
;               ((file2 15749300904 11) (file22 14448309360 11)
;                (file12 12835739361 11))
;               ((file5 9295645353 10) (file18 6609806629 10)
;                (file11 6348867697 10))
;               ((file15 5942107928 10) (file9 4495356117 10)
;                (file10 3118866364 10))
;               ((file17 2424678728 10) (file14 1293428979 10)
;                (file8 170858581 9)))
;                =====this is an initial sketch of a doling out/distribution:
;((("file16" "file6" "file21") "node5") (("file3" "file0" "file1") "node0")
; (("file13" "file4" "file20") "node6") (("file7" "file23" "file19") "node9")
; (("file2" "file22" "file12") "node7") (("file5" "file18" "file11") "node8")
; (("file15" "file9" "file10") "node4") (("file17" "file14" "file8") "node1"))
;======first 2 don't check out, so schedule/distribute more carefully:(fix)
;warn:too-much:((file16 file6 file21) 130198059342 (node5 100718892926 12))
;warn:too-much:((file3 file0 file1) 100129736522 (node0 94524502533 11))
;put (file13 file4 file20) at node6
;put (file7 file23 file19) at node9
;put (file2 file22 file12) at node7
;put (file5 file18 file11) at node8
;put (file15 file9 file10) at node4
;put (file17 file14 file8) at node1
;(NIL NIL NIL NIL NIL NIL NIL NIL)
;USER(2): 
 
