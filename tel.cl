;mike bobak@balisp.org ;use: mike.bobak@gmail
(lut) ;load: https://github.com/MBcode/LispUtils/blob/master/util_mb.lisp
;;;===============================================================================
;1.      Assume the US dollar is available in denominations of $100, $50, $20, $10, $5, $1, $0.25, $0.10, $0.05 and $0.01. Write a function to return the number of different ways to exactly represent an arbitrary value less than $1,000,000 using any number or combination of these denominations.
(defvar *denom* '(100 50 20 10 5 1 0.25 0.10 0.05 0.01))
;assuming efficient enumerations, as 10**(6+2)pennies etc, will be a very very large number of combinations
(defun divf (n d)
  (floor (/ (float n) d)))

(defun mx-val-in (v n) (divf n v))

(defun denom4val (v &optional (den *denom*))
 (when (full den)
  (let* ((d (first den))
         (nd (mx-val-in d v)) ;# of this d
         (nr (mod v d)))
    (cons (cons nd d) (denom4val nr (rest den))))))

;use: (denom4val 102.26)
;((1 . 100) (0 . 50) (0 . 20) (0 . 10) (0 . 5) (2 . 1) (1 . 0.25) (0 . 0.1) (0 . 0.05) (1 . 0.01))
;Could write something to explain in English what was used
(defun denoms4val (v)
  (collect-if #'(lambda (p) (posative-p (car p))) (denom4val v)))
;use: (denoms4val 102.26)
;((1 . 100) (2 . 1) (1 . 0.25) (1 . 0.01))
;Then use formatting, for English of the 1st elt..
(defun speak-f1a (f v)
  "fnc(v)->alst, format car of cdr"
  (let ((al (funcall f v)))
    (mapcar #'(lambda (p) (format nil "~r of ~a" (car p) (cdr p))) al))) 

(defun speak-denom4val (v)
  (speak-f1a #'denoms4val v))
;get: ("one of 100" "two of 1" "one of 0.25" "one of 0.01")
;If you really want 'all' enumerations, there are standard enums for each denom in terms of lesser vals
;&you could replace any w/a tree of those enums, &all DFS paths would be all enums
;;;===============================================================================
;2. Determine the sum of all prime numbers less than an input number (the input number will be at most 10,000,000).

;common-lisp/primes.lisp" 199 lines --27%--
(defun compute-primes-to (n)
  " DO:     Compute an Eratostene cribble to find all prime numbers up to N.
RETURN: An array of prime numbers.  "
  (cond
    ((< n 2) #())
    ((= n 2) #(2))
    ((= n 3) #(2 3))
    (t
     (let (bits-max bits bit (prime-count 2) (cur-prime 3) primes)
       ;; (setq n (+ n (if (oddp n) 3 2)))
       (setq n (- n (if (oddp n) 3 2))) ; base of bits array is 3.
       (setq bits-max (/ n 2))
       ;; set the bitset to full bits;
       (setq bits (make-array (list bits-max)
                              :initial-element 1 :element-type 'bit))
       (while (< cur-prime n)
         (setq bit (+ cur-prime (/ (- cur-prime 3) 2)))
         (while (< bit bits-max)
           (setf (aref bits bit) 0)
           (incf bit cur-prime))
         (setq bit (1+ (/ (- cur-prime 3) 2)))
         ;; search next prime
         (setq bit (position 1 bits :start bit))
         (if bit
             (setq cur-prime (+ bit bit 3)
                   prime-count (1+ prime-count))
             (setq cur-prime n)))
       ;; gather the primes.
       (setq primes (make-array (list prime-count) :element-type 'integer))
       (let ((curnum 0))
         (setf (aref primes curnum) 2)
         (incf curnum)
         (setf (aref primes curnum) 3)
         (incf curnum)
         (setq cur-prime 3)
         (setq bit 0)
         (setq bit (position 1 bits :start (1+ bit)))
         (while bit
           (setq cur-prime (+ bit bit 3))
           (setf (aref primes curnum) cur-prime)
           (incf curnum)
           (setq bit (position 1 bits :start (1+ bit)))))
       primes)))) 

(defun sum-primes-to (n)
  (reduce #'+ (compute-primes-to n)))

;use: (sum-primes-to 40) 
;197
;;;===============================================================================
;3. I have a csv format table of telephone calls; each call is on a line of the format “calling #, called #”. 
;Define friends as any two people who have talked, and acquaintances as any two people who are not friends, but share a friend. 
;Thus, if A talks to B, and C talks to B, then A and B are friends, A and C are acquaintances. 
;Find who has the most acquaintances. 
;The table will have at most 1,000,000 entries, and the phone numbers will be integers with at most 15 digits. 
;(defvar *table* (csv-read-file "file.csv"))
;(defvar *table* (read-csv "file.csv"  #'convert-to-list))
(defvar *table* (read-csv "file.csv"))

;then a version of https://github.com/MBcode/LispUtils/blob/master/test.lisp w/more options
;
;Might use: http://common-lisp.net/project/cl-graph/  ;or might not need, but try:
#+ignore (progn
(ql 'cl-graph)
(use-package :cl-graph)

(defun table2graph (&optional (table *table*))
  (let ((g  (make-container 'graph-container)))
    (mapcar #'(lambda (p) (add-edge-between-vertexes g (car p) (cdr p))) table)))

(defun most-aquaint (&optional (table *table*))
  (let ((g (table2graph table)))
    ;read up on this lib
    ))
)
;if not, sort the letter/names (so don't have2have both halves of a connection matrix)
;assert friend on each pair off the csv-table, &go down list of friends,&assert aquaint4their friends
;incr aquaint count, could also format this as a hadoop job, 
;not unlike: https://github.com/MBcode/LispUtils/blob/master/ts.cl
(defvar *friends* (make-hash-table :test 'equal))
(defvar *aquaint* (make-hash-table :test 'equal))

(defun set-hash (h key val)
 ;(setf (gethash key h) val)
 (pushnew (first-lv val) (gethash key h))
 ;(pushnew (first-lv (first-lv val)) (gethash key h))
  )

(defun print-hash (h)
  (maphash #'(lambda (k v) (format t "~%~a,~a" k v)) h))

;(defun add-hash-between-vertexes (g from to)
;  (if (char-lessp from to) (set-hash g from to)
;                           (set-hash g to from)))
;actually might need friends to be set both ways, to faster find the foaf, for aquaints
(defun add-hash-between-vertexes (g from to)
  (set-hash g from to) (set-hash g to from))

(defun table2g (&optional (table *table*) (g *friends*))
  (mapcar #'(lambda (p) (add-hash-between-vertexes g (car p) (cdr p))) table))

(defun friend-p (a b &optional (h *friends*))
  (member a (gethash b h) :test #'equal))

(defun most-aquaint (&optional (table *table*)) ;fix
  (table2g table)
  (maphash #'(lambda (k v)  ;k=person v=all-friends ;aquaint aren't already friends
               (loop for friend in v 
                     unless (friend-p k friend) do (set-hash *aquaint* k friend)))  *friends*)
  (let ((mx 0) (p nil))
    (maphash #'(lambda (k v)  ;k=person v=aquaint list, find max of
                 (when (> (len v) mx) (setf mx (len v)) (setf p k))) *aquaint*)
                ;if = maybe ret whole set
    p)) ;person w/mx aquaint
;untested, but might do it, will finish tomorrow
