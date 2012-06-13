(defvar *dbg* nil)
;-streaming:   ;can open file, but also useful for (hadoop)streaming
(defun handle-stream (&optional (in *standard-input*))
  (let ((tot 0))
    (loop for line = (read-line in nil nil) while line  do
          (let ((n (num-str line)))
            (when (numberp n) (incf tot n))
            (when *dbg* (format t "~%~a" n))))
    (format t "~%tot=~a~%" tot)))

(handle-stream) 

;could have used cl-json to parse out, but cut works during a streaming job as well
;mongo> cat zips.json | cut -d',' -f5 | cut -d':' -f2 | sbcl --core km.core --script ts.cl 
;tot=248709873 
