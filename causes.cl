;http://www.jobhost.org/jobs/viewjob/software-engineer-14266507a2d4ef34?source=indeed&medium=sponsored 
;use spell corrector code, &make a tree recursivly&get final count
;Like puzzles?
;yes mike.bobak@gmail
;Solve this problem to catch our attention! Be sure to follow the instructions exactly.
;
;Two words are friends if they have a Levenshtein distance (http://en.wikipedia.org/wiki/Levenshtein_distance) of 1. That is, you can add, remove, or substitute exactly one letter in word X to create word Y. A word’s social network consists of all of its friends, plus all of their friends, and all of their friends’ friends, and so on. Write a program to tell us how big the social network for the word “causes” is, using this word list (https://github.com/causes/puzzles/raw/master/word_friends/word.list).
;
;Include your answer, along with your thought process, notes, and any code with your resume.
;
;get&load: ;;;;https://github.com/mikaelj/snippets/blob/master/lisp/spellcheck/spellcheck.lisp
;  which uses: http://www.quicklisp.org/ to load other libs;Start w/quicklisp load this&call last fnc
(in-package :spellcheck)
;a few from my: https://github.com/MBcode/LispUtils/blob/master/util_mb.lisp
(defun full (a)
  "clips leftover not needed in lsp"
  (if (stringp a) (> (length a) 0)
          (not (null a))))

(defun collect-if-not (predicate sequence)
  (when (full sequence)
    (remove-if predicate sequence)))

(defun set_diff (a b)
  "set-difference that doesn't unorder the list"
  (collect-if-not #'(lambda (e) (member e b)) a)) 

;;could have ignore as optional, but if !dfs/etc might want as global
(defvar *ignore* nil) ;(defvar *in* nil) ;just get length of *ignore*
(defvar *tmp* 0)
;defun edit1reach (wrd  &optional (ignore nil))
(defun edit1reach (wrd)
 (when (and (> (length wrd) 0) (not (member wrd *ignore* :test #'equal))) ;shouldn't need here
  (let* ((nw1-l (known (edits-1 wrd)))
         ;(try-l (set_diff nw1-l *ignore*))
         (try-l nw1-l)
         )
    ;new-ignore
    (pushnew wrd *ignore* :test #'equal) 
    (format t "[~a]~a" wrd (incf *tmp*))
    ;if try-l (cons wrd (mapcar #'(lambda (w) (edit1reach w )) try-l)) ;only# not graph
    (if try-l ;(mapcar #'(lambda (w) (edit1reach w)) try-l)
    ;(loop for w in try-l do (funcall  #'(lambda (w) (edit1reach w)) w))
      (loop for w in try-l do (funcall #'edit1reach w))
      nil ;wrd
      ))))
(defun edit1r (wrd)
 (when (and (> (length wrd) 0) (not (member wrd *ignore* :test #'equal))) ;shouldn't need here
    (pushnew wrd *ignore* :test #'equal) 
    (format t "[~a]~a" wrd (incf *tmp*))
    (loop for w in (known (edits-1 wrd)) do (funcall #'edit1r w))))

(defun edit1rec (wrd &optional (ignore nil))
 (when (and (> (length wrd) 0) (not (member wrd ignore :test #'equal))) ;shouldn't need here
    (pushnew wrd ignore :test #'equal) 
    (format t "[~a]~a" wrd (incf *tmp*))
   ;(loop for w in (known (edits-1 wrd)) do (funcall #'edit1r w)) ;below prob mk control stack worse
    (let ((try-l (known (edits-1 wrd))))  (when try-l (mapcar #'(lambda (w) (edit1rec w ignore)) try-l)))
    ))
;try w/letrec&loop/?

(defun answer () (length *ignore*))
(defun get-answer (&optional (word "causes"))
  (edit1rec word) ;(edit1reach word)
  (format t "~%social network for ~a is ~a~%" word (answer)))

;to run:
;USER(1): (in-package :spellcheck)
;
;#<PACKAGE "SPELLCHECK">
;SPELLCHECK(2): (get-answer)
