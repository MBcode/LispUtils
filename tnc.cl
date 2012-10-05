;parse some blog posts bobak@balisp.org
;cat ld.cl s.cl t.cl >tnc.cl
(ql '(puri drakma chtml-matcher phtml cxml-stp closure-html))
(use-package :puri)
(lut) ;(load "/Users/bobak/lsp/util_mb")  ;https://github.com/MBcode/LispUtils
(lkm2) ;(load "/Users/bobak/lsp/km_2-5-33") ;(load "/Users/bobak/lsp/u2") ;~=ukm2 ;https://github.com/MBcode/kmb
(setq drakma:*header-stream* *standard-output*)

(ql 'cl-json)
(defun encode-js2s (l &optional (strm *standard-output*)) 
  (json:encode-json l strm))

(defun encode-js2str (l &optional (tag "")) 
  (let ((str (make-string-output-stream)))
    (json:encode-json l str)
    (str-cat tag "," (get-output-stream-string str))))

(defun logjsonl (l &optional (tag "") (outf "log.txt"))
  (with-open-file (out outf :direction :output :if-exists :supersede)
    (when (and (stringp tag) (full tag)) (format out "~%~a," tag))
    (if (listp tag) (mapcar #'(lambda (l1 t1)  ;l=lol tag=assoc l o tags
                                (format out "~%~a," t1)
                                (encode-js2s l out)) l tag)
      (enconde-js2s l out))))

;(load "s.cl" :print t) ;now below
(load "cu.cl" :print t) ;domain specific /blogs
(lt) ;rest in t.cl for now
;blog-post scraper bobak@balisp.org 
(defun hr (u)
  "get the page as 1str"
  (drakma:http-request (if (s-prefixp "http" u) u (str-cat "http://" u))))

;git rid of unused from: ;http://codereview.stackexchange.com/questions/2277/simple-web-scraper-in-common-lisp-sbcl
;-now in s-.cl, found struct more difficult to dbg/get used2html struct/ures
;not scraping what I want right now, but can still study it
(defun scrape-xhtml-type (xhtml-tree typ)
  "Scrapes data from an XHTML tree, returning a list of lists of values."
  (let ((results nil))
    (stp:do-recursively (element xhtml-tree)
            (when (and (typep element 'stp:element)
                   (equal (stp:local-name element) typ))
              ;(if (scrape-row element)
              ;    (setq results (append results (list (scrape-row element)))))
              (setq results (append results (list element)))
              ))
    results))

(defun scrape-xhtml-types (xhtml-tree typl)
  "Scrapes data from an XHTML tree, returning a list of lists of values."
  (let ((results nil))
    (stp:do-recursively (element xhtml-tree)
            (when (and (typep element 'stp:element)
                   (member (stp:local-name element) typl :test #'equal))
              (setq results (append results (list element)))
              ))
    results))

;not scraping what I want right now, but can still study it 
; -moved2 chtml:parse list easier2debug than structs @1st

(defun scrape-body (body)
  "Scrapes data from a potentially invalid HTML document, returning a list of lists of values."
  (let ((valid-xhtml (chtml:parse body (cxml:make-string-sink))))
    (let ((xhtml-tree (chtml:parse valid-xhtml (cxml-stp:make-builder))))
      (scrape-xhtml-types xhtml-tree '("img" "i"))))) 

(defun scrape-fn (fn)
  (scrape-body (read-file-to-string fn)))

(defun scrape-uri (uri)
  (scrape-body (hr uri)))
;-prob won't use above scrape2structs
; -moved2 chtml:parse list easier2debug than structs @1st
;(lt) ;rest in t.cl for now
(defun s-crape-fn (fn)
  (chtml:parse (read-file-to-string fn) (chtml:make-lhtml-builder))) 
(defvar *i* (s-crape-fn "index.html"))
;(defun find-lh (tag attrib &optional (n 1) (lhtml *i*))
;  (chtml-matcher:find-in-lhtml lhtml tag attrib n))
(defun find-lh (tag &optional (n 1) (attrib nil) (lhtml *i*))
  (chtml-matcher:find-in-lhtml lhtml tag attrib n))
(defun get-post (n &optional (at-str "post hentry") (lh *i*))
  (find-lh "div" n `((:CLASS ,at-str)) lh))
;soon alst of plc & ..  ;pull out into problem specific part
;now in cu.cl

;start w/1,&expand to get all for the file
(defun gp1 (fn pt)
  "get 1st post from a file"
  (get-post 1 pt (s-crape-fn fn))) 

(defun gp-ff0 (fn pt)
  "get post from file"
  (let ((s (s-crape-fn fn)))
    (loop for i from 1 to 99 
          for p = (get-post i pt s)
          while p collect p)))

(defun gp-ff (fn pt)
  "get post/s from file &log-js"
  (let* ((s (s-crape-fn fn))
         (la (loop for i from 1 to 99 
                  for p = (get-post i pt s)
                  while p collect (cons (str-cat fn i) p)))
         (l2 (alst2 la))
         (tl (first l2))
         (l (second l2)))
    (logjsonl l tl))) ;1st use work/but dbg/fix here

;use this one  ;will also parse each post more, &(km)assert interesting bits
(defun gp-ff2 (fn pt) 
  "get post/s from file &log-js2sep files"
  (let* ((s (s-crape-fn fn))
         (la (loop for i from 1 to 99 
                  for p = (get-post i pt s)
                  while p collect (cons (str-cat fn i) p)))
         (l2 (alst2 la))
         (tl (first l2))
         (l (second l2)))
    (logjsonl l tl (str-cat "log/" fn)))) ;1st use work/but dbg/fix here
;ld.cl will use (lt) to load this, in the beginning
;several lines into cu.cl
;load-km c.km  could also give these :|keywords|
(defvar *pw* (string-list->keyword-vector '("jpg" "jpeg" "gif" "png")))
(defvar *dp* '(".jpg" ".jpeg" ".gif" ".png"))
(defun lk () (load-kb "c2.km")) ;then can do a (taxonomy)
(defun lk2 () (lk) (taxonomy))
;rest in t-.cl ;;pull out into problem specific part
(defun htm-p (s) (suffixp ".htm" s))
;domain-specific rest now in cu.cl
;can use in load&find in s.cl
