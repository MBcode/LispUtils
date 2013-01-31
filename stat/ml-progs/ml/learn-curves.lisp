(defun read-gnuplot-file (filename)
  (let (line lines)
    (with-open-file (input filename :direction :input :if-does-not-exist nil)
      (loop (unless (setf line (read-line input nil nil)) (return (nreverse lines)))
            (unless (blank-line line)
		    (push 
		     (read-from-string (concatenate 'string "(" line ")" ))
		     lines))))))

(defun acc-to-error (points)
  (mapcar #'(lambda (pair) (list (first pair) (- 100 (second pair))))
	  points))
  
(defun blank-line (line)
  (dotimes (i (length line) t)
           (unless (eq (char line i) #\space)
                   (return nil))))

(defun linear-fit (points)
  (clasp:linear-regression-brief
   (mapcar #'second points) (mapcar #'first points)))

(defun transform-x (points fn)
  (mapcar #'(lambda (pair) (list (funcall fn (first pair)) (second pair)))
	  points))

(defun transform-y (points fn)
  (mapcar #'(lambda (pair) (list (first pair) (funcall fn (second pair))))
	  points))

  
