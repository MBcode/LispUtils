;get&compile: https://github.com/tialaramex/sparql-query   ;bobak@balisp.org ;use: mike.bobak@gmail
(lu)
(ql 's-xml)
;-had this, but trivial-shell is easier
(defun run-xml (cmd &rest args)
  "run-ext&prs ret xml2s-expr"
  ;STRING-OUTPUT-STREAM {100AAC9DC1}> is not a character input stream
  (let ((str (make-string-output-stream))) ;just tried
      (sb-ext:run-program cmd args :search t :output str)
              (s-xml:start-parse-xml str))) 
;-or
(require :trivial-shell)
(defun tshell-command (str)
 (trivial-shell:shell-command (to-str str))) 

(defun tshell-cmnd-sxml (str)
   (s-xml:parse-xml-string (tshell-command str)))

(defvar *i2* "sparql-query -np http://dbpedia.org/sparql < i2.txt")

(defun t2 (&optional (str *i2*))
  "make sparql qry of dbpedia, and parse resulting xml into an s-exp"
  ;(tshell-command str)
  (tshell-cmnd-sxml str))
