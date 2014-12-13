;bobak read new tweet timeline output script captures/parse into KM
;Example:
;[1m[33m   @RWW[0m
;   iPhone users, do you trust iOS8? http://t.co/3Nhw9RzIxM
;
(defparameter +tm1+ "[1m[33m") 
(defparameter +tm2+ "[0m")
;(defparameter +tc1a+ #\[1m) 
;(defparameter +tc1b+ #\[33m) 
;(defparameter +tc2+ #\[0m)
(defparameter +nl+ "")
(defvar *dbg* " ")

(lu)

(defun parse-tw-strm (&optional  (in  *standard-input*))
  "pipe input in  2the-lsp-exe"
  (loop for line =  (read-line in nil nil) do
     ;parse here:  ;use a cond
     (when *dbg*  (format t "~% line: ~a " line))
     (if (len-lt line 2)  (format t "  newline: ~a" (equal line +nl+))
          (let (
                ;(n (position +tc2+ line))
                (n (when  (prefixp +tm1+ line) (len line)))
                (h (position #\: line)) ;http
                ) 
             (when *dbg*  (format t "~% n: ~a h: ~a" n h))
             (when n (format t "~% name: ~a " (substr line 2 n)))
          )
     )
     (sleep 1)
  )
)

(parse-tw-strm)
;test w/:
;cat o8t | sbcl --load t.cl
 
;can also sed stream to stnd csv like break, like:
;(defun csv-bar (l) (csv_parse-str l :separator #\|))
(defun csv-slash (l) (csv_parse-str l :separator #\/))
 
