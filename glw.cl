;a quick ~~wiki/Conway's_Game_of_Life, that I might play w/more sometime ;bobak@balisp.org
;http://unriskinsight.blogspot.com/2014/06/fast-functional-goats-lions-and-wolves.html
;{{17, 55, 6},{17, 55, 6},{17, 55, 6}} + {{-1,-1,+1},{-1,+1,-1},{+1,-1,-1}}
;start w/a/the iteration, &maybe go further
;((17  55  6) (17  55  6) (17  55  6)) + ((-1 -1 +1) (-1 +1 -1) (+1 -1 -1))
(defvar *t1* '(((17  55  6) (17  55  6) (17  55  6))   ((-1 -1 +1) (-1 +1 -1) (+1 -1 -1))))
;defun maprn2 (lol1 lol2) ;start simply &can generalize more later
(defun map+2 (lol1 lol2)
  (if (and (numberp (first lol1)) (numberp (first lol2))) (mapcar #'+ lol1 lol2)
    (mapcar #'map+2 lol1 lol2)))

(defun t2 () (map+2 (first *t1*) (second *t1*)))
;((16 54 7) (16 56 5) (18 54 5))
;keep on adding 2nd to 1st, but use p0
(defun p02 (v1 v2)
  (max  0 (+ v1 v2)))
 
(defun map+02 (lol1 lol2)
  (if (and (numberp (first lol1)) (numberp (first lol2))) (mapcar #'p02 lol1 lol2)
    (mapcar #'map+02 lol1 lol2)))

(defun t02 () (map+02 (first *t1*) (second *t1*)))
;((16 54 7) (16 56 5) (18 54 5))
(defun map+02n (lol1 lol2 &optional (n 1)) 
  (if (> n 0) (map+02n (map+02 lol1 lol2) lol2 (1- n))
    lol1))
(defun t02n (&optional (n 1)) (map+02n (first *t1*) (second *t1*)  n))
;USER(2): (t02n)
;((16 54 7) (16 56 5) (18 54 5))
;USER(3): (t02n 2)
;((15 53 8) (15 57 4) (19 53 4))
;USER(4): (t02n 8)
;((9 47 14) (9 63 0) (25 47 0)) 
 
