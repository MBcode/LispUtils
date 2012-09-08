;test of distributing files among nodes, to be redone in C;  mike.bobak
(defun split2n (txt2)
  "split txt-pair, change str->num on 2nd"
  (let ((tl (split txt2)))
    (list (first tl) (numstr (second tl)))))

(defun txtfile2alst (fn)
  "split 2col fn.txt"
  (rest (mapcar #'split2n (list-lines (str-cat fn ".txt")))))

(defvar *files* (txtfile2alst "files"))
(defvar *nodes* (txtfile2alst "nodes"))
 
