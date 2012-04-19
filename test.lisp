;;;a test use of util_mb.lisp
;;;======================================> play.lisp <==
;all the strategy is here
;(defvar *words* (map-lines "w10" #'(lambda (wl) (intern (string-upcase wl))))) ;change to w10
(defvar *words* (map-lines "w10" #'(lambda (wl) (string-upcase wl)))) ;regex this/?
(defvar *wordls* (map-lines "w10" #'(lambda (wl) (explode2s (string-upcase wl))))) 
(defvar *alphaw10* '(E S I R A N T O L C D U G M P H B Y F V K W Z X Q J)) ;can gen from init words /ofCurrentLen
                      ;from http://www.datagenetics.com/blog/april12012/index.html
;can index all/some or since will re-get freq just go down list till contin2fall4a bit
;set_diff&implode2s added2utils   ;set_diff =complement?
;s3/pdt stuff in p1.cl, use s4 now
;-
(defvar *lg* nil)

(defun ltr-ocr-in (ltrs &optional (words *wordls*))
  (mapcar #'(lambda (l) (cons l (sum-l (mapcar #'(lambda (w) (count l w)) words)))) ltrs))

(defun mx-ltr-ocr (ltrs &optional (words *wordls*))
  (let ((lc-alst (ltr-ocr-in ltrs words)))
    ;(loop for p in lc-alst maximize (cdr p) finally (return p))
    (sort lc-alst #'> :key #'cdr)
    ))

;well game will have picked until player actually makes choice
; so@1st assume taking our suggestions
(defvar *picked* '()) 
(defun suggest (current) ;includes len, which is set to 10 on 1st pass
  "given current's constraints, find max occurance of possible words"
  (let* ((pick-l (set_diff  *alphaw10* *picked*)) ;lst2pick from&use2get freq/max-let-occur
        ;(pick (first pick-l)) ;will reorder by count of each in cur wordls
         (missed (set_diff *picked* current))
         (got (set_diff *picked* missed))
         (lg (set_diff got *lg*))
         (lg1 (first-lv lg)) ;cnstr letter
         (lgp (when lg (positions (first-lv lg) current)))
         (lgp1 (first-lv lgp)) ;cnstr position
         (wll (len *wordls*))
        ;(nwl (when (and lg lgp) (collect-if #'(lambda (w) (eq (nth lgp1 w) lg1)) *wordls*)))
         (nwl (when (and lg lgp) (collect-if #'(lambda (w) (nth-eq lgp1 w lg1)) *wordls*)))
         (pick-from (if nwl (mapcar #'car (mx-ltr-ocr pick-l nwl)) pick-l))
         (pick (first pick-from)) ;will reorder by count of each in cur wordls
         )
    (push pick *picked*)
  ;start using current2filter *wordls* &get next max-letter-occurance,search via alpha2..
  ;filter if it has lg@lgp (start w/1occur then do all)
  ;when (and lg lgp) (setf *wordls* (collect-if #'(lambda (w) (eq (nth lgp1 w) lg)) *wordls*))
  (when nwl 
    (if (len-gt nwl 1) 
      (progn (setf *wordls* nwl)
            ;(format t "~%try:~a" (mx-ltr-ocr pick-l nwl))
             (format t "~%try:~a" pick)
             )
      (format t "~%GUESS:~a" nwl))
    (format t "~%wl~a->~a" wll (len *wordls*))
    ;now get max occur letter, &suggest that now
    )
    (format t "~%m:~a,g:~a,lg:~a,~a,~a,suggest(~a)4:~a" missed got lg lgp (len nwl) pick current)
  (setf *lg* got))
  )

;;;======================================> hang.lisp <==
;a quick hangman game to play; start w/1test word and have play/strategy give suggestions; then expand MB
;status{GAME_WON, GAME_LOST, KEEP_GUESSING}
;load: uts.cl or util_mb.lisp
;when I started to rewrite the game I got rid of things the game didn't use; &only have it in player.lisp now
;started from: http://lyle.smu.edu/~mhd/5320sp02/hang.lisp
(defun update-word (word current guess)
   "Update the current guess.  We can assume that the input is always a list."
   (let ((found nil))
      (dotimes (i (length word) current) ; Word and Current have the same length
         (if (equal (nth i word) guess)
           (progn (setf found t) (setf (nth i current) guess))
             nil))
      (if found (format t "Good try, keep guessing")
                (format t "Nope, sorry~%"))
      current))

(defun hangman ()
   "Simple hangman game.  The player has to keep track of letters
    he/she has already guessed."
   (let ((maxtries 15)
         (word    '(a r t i f i c i a l ))
         (current '(- - - - - - - - - -)) 
         (letterGuesses 0)
         (wordGuesses 0) ;if this maxes then letterGuesses goes to 25
         (guess nil))
      (dotimes (i maxtries)
         (suggest current) ;does: (print current)
         (terpri)
         (format t "What is your guess?  ")  ;start by calling play.lisp to get a suggestion
         (setf guess (read))
         (terpri)
         (setf current (update-word word current guess))
         ;if (equal word current)
         (if (or (if (and (eq (len guess) 1) (equal word current)) t (progn (incf letterGuesses) nil))
                 (if (equal word (explode2s guess))                  t (progn (incf wordGuesses) nil)))
             (progn                                    ;   Here is something new
                (print current)
                (terpri)
                ;(format t "Congratulations!  You have won the game.~%")
                (format t "Congratulations!  You found ~a in ~a,~a,~a~%" word (- 25 maxtries) letterGuesses wordGuesses)
                (return nil))  ;guess letter/word in fncs &ret status2use here
             (if (equal i (- maxtries 1))
                 (format t "You lost this time.  Try again!")
                 nil)))))
