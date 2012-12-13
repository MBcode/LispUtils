;mike bobak@balisp.org
(lut) ;load: https://github.com/MBcode/LispUtils/blob/master/util_mb.lisp
;;;===============================================================================
;1.      Assume the US dollar is available in denominations of $100, $50, $20, $10, $5, $1, $0.25, $0.10, $0.05 and $0.01. Write a function to return the number of different ways to exactly represent an arbitrary value less than $1,000,000 using any number or combination of these denominations.
(defvar *vals* '(100 50 20 10 5 1 0.25 0.10 0.05 0.01))
;assuming efficient enumerations, as 10**(6+2)pennies etc, will be a very very large number of combinations


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
;use: (sum-primes-to 40) ;197
;;;===============================================================================
;3. I have a csv format table of telephone calls; each call is on a line of the format “calling #, called #”. Define friends as any two people who have talked, and acquaintances as any two people who are not friends, but share a friend. Thus, if A talks to B, and C talks to B, then A and B are friends, A and C are acquaintances. Find who has the most acquaintances. The table will have at most 1,000,000 entries, and the phone numbers will be integers with at most 15 digits. 
