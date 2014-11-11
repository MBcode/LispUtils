;bobak read new tweet timeline output script captures/parse into KM
;Example:
;[1m[33m   @RWW[0m
;   iPhone users, do you trust iOS8? http://t.co/3Nhw9RzIxM
;
(defparameter +tm1+ "[1m[33m") 
(defparameter +tm2+ "[0m")
(defparameter +nl+ "")
(defvar *dbg* " ")

(defun parse-tw-strm (&optional  (in  *standard-input*))
  "pipe input in  2the-lsp-exe"
  (loop for line =  (read-line in nil nil) do
     ;parse here:
     (when *dbg*  (format t "~% line: ~a " line))
     (sleep 1)
  )
)

(parse-tw-strm)
