;playing w/some xml 2json files,  bobak
(ql 'xmls)
(defun parse-xmlsfile (filename &key (output-type nil))
  "might have better output than s-xml"
  (when output-type (warn "this is xmls not s-xml"))
  (with-open-file (in filename :direction :input)
    (xmls:parse in)))
(defvar *ix* (parse-xmlsfile "index.xml"))

(ql 's-xml)
(defvar *i-x* (s-xml:parse-xml-file "index.xml"))
 
(defun save-l (l filename)
 (when (fulll l)
  (with-open-file (strm filename :direction :output)
   ;(write l strm)
    (format strm "~%~a~%" l)
    )))
(defun save-s (s filename)
  (with-open-file (strm filename :direction :output)
    (format strm "~%~a~%" s)
    ))

(defun len0 (l)
  (if (listp l) (length l)
    0))

(defun restn (l n)
  (if (or (null l) (eq n 0)) l
    (restn (rest l) (1- n))))

(defun depth-gt (l n)
  (> (tree_depth l) n))

(defun depth-gt2 (l &optional (n 2))
  (depth-gt l n))

(defun first-lvv (l)
  "just ret a single Val"
  (let ((lv (first-lv l)))
    (if (listp lv) (first-lvv lv)
      lv)))

(defun second-lvv (l)
  "just ret a single Val"
  (let ((lv (second-lv l)))
    (if (listp lv) (second-lvv lv)
      lv)))

#+ignore
(defun save-lines (l filename)
 (when (fulll l)
  (with-open-file (stream filename :direction :output)
    (mapcar #'(lambda (x) (write-line x stream)) l))))
 
(defun rwl (r wx)
  (mapcar #'(lambda (a) (if (listp a) (rwl a wx) ;
                          (funcall wx a))) r)) ;only print vals not lists
(defun saverlines (l filename)
 (when (fulll l)
  (with-open-file (stream filename :direction :output)
   (flet ((wx (p) (write-line p stream)))
         ;;(wl (r) (mapcar #'wx r))
         ;(wl (r) (mapcar #'(lambda (a) (if (listp a) (wl a) (wx a))) r)) (wl l)
          (rwl l #'wx)
          ))))

(require :cl-json) 

(defun encode-json2str (js)
  (if (equal js '(nil)) "[]"
    (json:encode-json-to-string js)))
 
(defun save-js (js filename)
  (save-s (encode-json2str js) filename))
 
;also looked at xml2json xmlstarlet xsltproc xml2json-xslt jq underscore jsonpp ppjson
; some output already csv or similar
;(defun csv-bar (l) (csv_parse-str l :separator #\|))
(defun csv-slash (l) (csv_parse-str l :separator #\/))

(defun prs-slash-file (fn) (mapcar #'csv-slash (list-lines fn)))
(defun t1 () (prs-slash-file "af/a1.txt"))
 
