(load "util_mb") ;;;a test use of util_mb.lisp
(defvar *dbg* nil)
(defvar *auto* t)
;;============================================> play.lisp <==
;all the strategy is here
(defvar *wordls* nil)
(defvar *alphaw10* '(E S I R A N T O L C D U G M P H B Y F V K W Z X Q J)) ;use4now/finish
(defvar *alphaw-n* nil)
;can gen from init words /ofCurrentLen ;from http://www.datagenetics.com/blog/april12012/index.html
;set_diff&implode2s added2utils   ;set_diff =complement?
;-
(defvar *lg* nil)
(defvar *lm* nil)
(defvar *picked* '()) 
;(defun positivep (n) (> n 0)) ;put in utils
;-
(defun ltr-ocr-in (ltrs &optional (words *wordls*))
  "ret alst of letters w/occurance counts in present word set"
  (mapcar #'(lambda (l) (cons l (sum-l (mapcar #'(lambda (w) (count l w)) words)))) ltrs))

(defun mx-ltr-ocr (ltrs &optional (words *wordls*))
  "ret most likely letter to choose from current word list"
  (let ((lc-alst (ltr-ocr-in ltrs words)))
    ;(loop for p in lc-alst maximize (cdr p) finally (return p))
    (let* ((sl (sort lc-alst #'> :key #'cdr))
           (sl- (collect-if #'(lambda (pr) (positivep (cdr pr))) sl)))
      (when *dbg* 
       (format t "~%New pick-l:~a~%" (head sl- 8))) ;when all the same reorder2more like init distrib 
      sl-)))
;-
(defun useWordsOfLen (n)  
  "set once know length of word using"
  (let ((file (str-cat "w" n))) ;replace w/word-len filtered list from words.txt
    (setf *wordls* (map-lines file #'(lambda (wl) (explode2s (string-upcase wl)))))) 
  (setf *alphaw-n* (mapcar #'first (ltr-ocr-in *alphaw10* *wordls*)))
  (cons (len *wordls*) (head *alphaw-n*))) 

(defun initGame (cur)  
  "start game w/word of len cl"
  (let* ((cl (len cur))
         (fininit (useWordsOfLen cl)))
    (setf *lg* nil) (setf *lm* nil) (setf *picked* nil)
    (when *dbg* (format t "~%Have a game of ~a letters,~a~%" cl fininit))))

;-for missed letter word removal:
;(defun rm-if-member (m lol)
;  (remove-if #'(lambda (l) (member m l)) lol)) 
 
;-for got-letter word filtering
;(defun no-nils (l) (not (member nil l))) ;need each letter/position combo to keep that word
;(defun any-t (l) (len-gt (rm-nil l) 0))

(defun mpc (m ps lol)
  "m@position/s constraint"  ;need all to be true so reject if any nils
  (collect-if #'(lambda (l) (no-nils (mapcar #'(lambda (p) (nth-eq p l m)) ps))) 
                 ;a  word  has no occurences of m @ positions p   
              lol)) ;from wordls
;-
;well game will have picked until player actually makes choice
; so@1st assume taking our suggestions
(defun suggest (current) ;includes len, which is set to 10 on 1st pass
  "given current's constraints, find max occurance of possible words"
  (unless *wordls* (initGame current))
  (let* ((pick-l (set_diff  *alphaw10* *picked*)) ;lst2pick from&use2get freq/max-let-occur
        ;(pick (first pick-l)) ;will reorder by count of each in cur wordls
         (missed (set_diff *picked* current)) ;use2fileter wordls
         (got (set_diff *picked* missed)) ;every correct pick
         (lg (set_diff got *lg*)) ;latest 'got'/found letter
         (lg1 (first-lv lg)) ;cnstr letter
         (lgp (when lg (positions (first-lv lg) current)))
         ;(lgp1 (first-lv lgp)) ;cnstr position
         (wll (len *wordls*))
         (lm (set_diff missed *lm*)) ;latest 'missed'/found letter
        ;do for each position it is found in/finish-this
         (nwl (if (and lg lgp) ;(collect-if #'(lambda (w) (nth-eq lgp1 w lg1)) *wordls*)
                (mpc lg1 lgp *wordls*)
                (when lm (rm-if-member lm *wordls*)))) ;rm last missed letter from wordlist
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
             (when *dbg* (format t "~%try:~a" pick))
             ) ;else only 1 word left so pick it
      (progn (setf pick (implode2s (first-lv nwl)))
             (format t "~%GUESS:~a" nwl)))
    (when *dbg* (format t "~%wl~a->~a,~a" wll (len *wordls*) (head *wordls* 6)))
    ;now get max occur letter, &suggest that now
    )
    (when *dbg*
      (format t "~%m:~a,lm:~a,g:~a,lg:~a,~a,~a,suggest(~a)4:~a" 
            missed lm got lg lgp (len nwl) pick current))
    (setf *lm* missed)
    (setf *lg* got)
  pick))

;;============================================> hang.lisp <==
;a quick hangman game to play;start w/1test word and have play/strategy give suggestions; then expand MB
;status{GAME_WON, GAME_LOST, KEEP_GUESSING}
;load: uts.cl or util_mb.lisp
;when I started to rewrite the game I got rid of things the game didn't use; in play.lisp now
(defvar *status* nil) ;game had different way of printing
;started from: http://lyle.smu.edu/~mhd/5320sp02/hang.lisp
(defun update-word (word current guess)
   "Update the current guess.  We can assume that the input is always a list."
   (let ((found nil))
      (if (and (len-gt guess 1) (equal (explode2s guess) word)) 
        (progn (when *dbg* (format t "~%GOT IT HERE~%"))
               (setf current (explode2s guess))
               (setf *status* 'GAME_WON)
               (setf found 'GAME_WON))
        (dotimes (i (length word) current) ; Word and Current have the same length
          (if (equal (nth i word) guess)
            (progn (setf found 'KEEP_GUESSING) 
                   (setf *status* 'KEEP_GUESSING) 
                   (setf (nth i current) guess))
            nil)))
      (when *dbg* ;new
        (if found (when (eq found 'KEEP_GUESSING) (format t "~a Good try, keep guessing" guess))
          (format t "~a Nope, sorry~%" guess)))
      current))

(defun print-current (current score)
  (format t "~%~a; score=~a; status=~a" (implode2s current) score *status*)) 

(defun hangman (&optional (word  '(a r t i f i c i a l )) (mxWg 4))
   "Simple hangman game.  The player has to keep track of letters he/she has already guessed."
   (let* ((maxtries 25)
          (mx-current '(- - - - - - - - - - - - - - - - - - - - - - - - -)) 
          (current (subseq mx-current 0 (len word)))
          (letterGuesses 0)
          (wordGuesses 0) ;if this maxes then letterGuesses goes to 25
          (guess nil))
      (dotimes (i maxtries)
        (let ((sug (suggest current)) ;does: (print current)
              (score (+ letterGuesses wordGuesses)))
         ;(unless *dbg* (print current)) ;unless dbg off
         ;(unless *dbg* (format t "~%~a; score=~a; status=~a" current score status)) 
         (unless *dbg* (print-current current score)) 
         (terpri)
         (when  *dbg* ;new
          (format t "What is your guess?  "))  ;start by calling play.lisp to get a suggestion
         (setf guess 
               (if *auto* sug
                 (read)))
         (terpri)
         (if (len-gt guess 1)  ;incr word or letter guess count
           (progn (incf wordGuesses)
                  (when (> wordGuesses mxWg) (setf letterGuesses 25)
                    (when *dbg* (format t "~%TooManyWordGuesses~%"))))
           (incf letterGuesses)) 
         (setf current (update-word word current guess)) ;update/check if correct
         (if (equal word current)  
             (progn (print current) (terpri)
               ;(format t "Congratulations!  You have won the game:~a~%" word)
                (format t "Congratulations!  You have won the game w/~a in:~a+~a=~a~%" 
                                                    word letterGuesses wordGuesses score)
                (return nil))  ;guess letter/word in fncs &ret status2use here
             (if (or (>= letterGuesses 25) (> wordGuesses mxWg)) ;only have to test for 1st
               (progn (setf *status* 'GAME_LOST)
                      (unless *dbg* (print-current current score)) 
                      (format t "You lost this time:~a,~a  Try again!" letterGuesses wordGuesses))
                 nil) ;why nil
             ) 
         score))))

(defun tst2 () (hangman '(r e m e m b e r e d))) ;in 5 letter trys &default in 10

;;============================================> tst.lisp <==
(defvar *tsts* '(;my-score in comments sometimes doing better than original game maker's
 ("COMAKER" 25) ;14  ;(was not able to guess the word before making more than 5 mistakes)
 ("CUMULATE" 9) ;8
 ("ERUPTIVE" 5) ;9
 ("FACTUAL" 9)  ;9
 ("MONADISM" 8) ;9
 ("MUS" 25) ;9 ;(was not able to guess the word before making more than 5 mistakes)
 ("NAGGING" 7) ;7
 ("OSES" 5)    ;3
 ("REMEMBERED" 5)   ;5
 ("SPODUMENES" 4)   ;5
 ("STEREOISOMERS" 2);3
 ("TOXICS" 11)      ;11
 ("TRICHROMATS" 5)  ;6
 ("TRIOSE" 5)       ;7
 ("UNIFORMED" 5)))  ;13 

(defun hang (&optional (r nil)) ;run fnc
  "test hangman"
  (format t "~%TestHangman w/word or nth of~%~a" *tsts*)
  (setf *wordls* nil) ;now that in 1file, to reset
  (let* ((choice (if r r (read)))
         (word (if (numberp choice) 
                 (let ((cp  (nth (min choice (len *tsts*)) *tsts*)))
                   (format t "~%Try to get it in:~a~%" (second cp))
                   (first cp))
                 (string choice))))
    (when (len-gt word 1)
      (hangman (explode2s word)))))

(defun run (&optional (n nil))
  "run 1 or all tests"
  (if n (hang n)
    (loop for i from 1 to (len *tsts*) do (hang i))))

;;===========================================end of hangman game
;;===========================================start of word reach
;;;;;;https://github.com/mikaelj/snippets/blob/master/lisp/spellcheck/spellcheck.lisp
;;could have ignore as optional, but if !dfs/etc might want as global
;#+ignore (progn (in-package :spellcheck)
;(defvar *ignore* nil) ;(defvar *in* nil) ;just get length of *ignore*
;(defvar *tmp* 0)
;(defvar *wq* nil)
;(defun useablep (wrd) (and (> (length wrd) 0) (not (member wrd *ignore* :test #'equal))))
;
;(defun edit1ql (word)
;  (push word *wq*) ;start it off
;  (labels ((editq- ()
;                 (let ((wrd (pop *wq*)))
;                   (when (useablep wrd) ;shouldn't need here
;                     (pushnew wrd *ignore* :test #'equal) 
;                     (when *dbg* (format t "[~a]~a" wrd (incf *tmp*)))
;                     (let ((try-l (known (edits-1 wrd))))  
;                       (when try-l
;                         ;(mapcar #'(lambda (tl) (when (useablep tl) (pushnew tl *wq* :test #'equal))) try-l)
;                          (loop for w in try-l do (funcall #'(lambda (tl) (when (useablep tl) (pushnew tl *wq* :test #'equal))) w))
;                         ))))
;                 ))
;    (while *wq* (editq-)))
;  *tmp*)
;;
;
;(defun answer () (length *ignore*))
;(defun get-answer (&optional (word "causes"))
;  (format t "~%social network for ~a is ~a~%" word (edit1ql word))
;  ) 
;;to run:
;;USER(1): (in-package :spellcheck)
;;
;;#<PACKAGE "SPELLCHECK">
;;SPELLCHECK(2): (get-answer)
;;social network for causes is 78768
