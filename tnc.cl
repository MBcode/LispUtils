;parse some blog posts bobak@balisp.org
;was;cat ld.cl s.cl t.cl >tnc.cl (ql '(puri drakma chtml-matcher phtml cxml-stp closure-html))
(ql '(puri drakma chtml-matcher))
(use-package :puri)
(lut) ;(load "~/lsp/util_mb")  ;https://github.com/MBcode/LispUtils
(lkm2) ;(load "~/lsp/km_2-5-33") ;(load "~/lsp/u2") ;~=ukm2 ;https://github.com/MBcode/kmb
(setq drakma:*header-stream* *standard-output*)
;blog-post scraper bobak@balisp.org 
(defun hr (u)
  "get the page as 1str"
  (drakma:http-request (if (s-prefixp "http" u) u (str-cat "http://" u))))

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

(defun logjsonal (lal &optional (outf "log.txt")) ;maybe still a dflt tag
  ;this v as alst of l1 tg1 ..
  (with-open-file (out outf :direction :output :if-exists :supersede)
   ;(when (and (stringp tag) (full tag)) (format out "~%~a," tag))
    (mapcar #'(lambda (al)  ;lst of vals or (assoc if a tag)
                (when (listp al) (format out "~%~a," (cdr al)))
                (encode-js2s (first-lv al) out)) lal)))

(load "cu2.cl" :print t) ;domain specific /blogs
;;moved50lines2 tx.cl
(defun s-crape-str (str)
  (chtml:parse str (chtml:make-lhtml-builder))) 
(defun s-crape-fn (fn)
 ;(chtml:parse (read-file-to-string fn) (chtml:make-lhtml-builder))
  (s-crape-str (read-file-to-string fn))
  )
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
        ;(l2 (alst2 la))
        ;(tl (first l2))
        ;(l (second l2))
         )
   ;(logjsonl l tl)
    (logjsonal la)
    )) ;1st use work/but dbg/fix here

(defun p-lh (lh tag)
  "w/in1post,get all of a tag-type"
    (loop for i from 1 to 99 
          for p = (find-lh tag i nil lh)
          while p collect p))

;ct=city (sf or ny)now; used to subclass blogPost s &more
;-use this one  ;will also parse each post more, &(km)assert interesting bits
(defun lt-assert (lf tf fn &optional (ct nil))
  "km assert"
  ;will parse lf more too
  ;let ((il (p-lh lf "img"))
  ;     (ii (p-lh lf "i")))
    (sv-cls tf (if ct (str-cat ct "BlogPost") "BlogPost"))
   ;(sv-al tf '((img i1) (i stuff))) ;make this part real/soon -finish
    (sv-al tf (list ;(cons "img" (p-lh (p-lh lf "img") "src"))
                (cons "img" (p-lh lf "img"))
                (cons "i" (p-lh lf "i"))))
    )  ;pull out less/make cleaner..
;(trace p-lh)

;moved other try to tx.cl
;-fix above to replace: &allow for a str vs filename version
(defun gp-ffns2 (fn s pt &optional (ct nil))  ;use this on sf&sf-pt
  "get post/s from sexpr w/filename-tag  assert&log-js2sep files"
  (let* (;(s (s-crape-fn fn))
         (la (loop for i from 1 to 99 
                  for p = (get-post i pt s)
                  while p collect (cons (str-cat fn i) p)))
         (l2 (alst2 la)) ;or collect2&ret values
         (tl (first l2))
         (l (second l2)))
    ;log(for doc-store instert?)/etc, and assert now, for indx/inference/..
    (logjsonl l tl (str-cat "log/" fn ".js")) ;seperate&supercede now ;already takes 2lists
    (mapcar #'(lambda (lf tf) (lt-assert lf tf fn ct)) l tl) ;do for each blog post
    ))  
(defun gp-ff2 (fn pt &optional (ct nil))  ;use this on sf&sf-pt
  "get post/s from file assert&log-js2sep files"
   (gp-ffns2 fn (s-crape-fn fn) pt ct))
;-if give fn also for now, then can have a vers that gets a str from a uri/&go right from that
;==was in cu2.cl
(defun scrape-tag (tg) (scrape-uri (t2rss tg)))

(defun rss-t (tg)
  (hr (t2rss tg)))
(defun rss_t (tg)
  (s-crape-str (rss-t tg)))
#+ignore
(defun gp-ff2 (fn pt &optional (ct nil))  ;use this on sf&sf-pt
  "get post/s from file assert&log-js2sep files"
  (let* ((s (s-crape-fn fn))
         (la (loop for i from 1 to 99 
                  for p = (get-post i pt s)
                  while p collect (cons (str-cat fn i) p)))
         (l2 (alst2 la)) ;or collect2&ret values
         (tl (first l2))
         (l (second l2)))
    ;log(for doc-store instert?)/etc, and assert now, for indx/inference/..
    (logjsonl l tl (str-cat "log/" fn ".js")) ;seperate&supercede now ;already takes 2lists
    (mapcar #'(lambda (lf tf) (lt-assert lf tf fn ct)) l tl) ;do for each blog post
    ))  
;could also get&dump2file either the str or chtml(sexpr) so could revisit
;  then pick it up from fn if str in same names files; or ... /generalizing is better though
;ld.cl will use (lt) to load this, in the beginning
;several lines into cu.cl
;load-km c.km  could also give these :|keywords|
(defvar *pw* (string-list->keyword-vector '("jpg" "jpeg" "gif" "png")))
(defvar *dp* '(".jpg" ".jpeg" ".gif" ".png"))
(defun lk () (load-kb "c2.km")) ;(BlogPost has (superclasses (Thing))) &much more
(defun lk2 () (lk) (taxonomy))    ;then can do a (taxonomy)
;rest in t-.cl ;;pull out into problem specific part
(defun htm-p (s) (suffixp ".htm" s))
;domain-specific rest now in cu.cl
;can use in load&find in s.cl
(lk)
;i miss my mapcar that re-uses an arg if not a list, but can just lambda
  ;ctp=*sf* *ny*  pt might go to cfg, &/or run through list of them 1st time/? 
(defun do-city (city &optional (ctp *ct*) (pt *pt*)) 
  "parse posts from htm files"
  (let ((ct (assoc2nd city ctp))  ;use assoc_v
        (pt (assoc2nd city pt)))
   ;(mapcar #'gp-ff2 *sf* *sf-pt*)
    ;mapcar #'gp-ff2 ct pt city
    (mapcar #'(lambda (c p) (gp-ff2 c p city)) ct pt)))

(defun tst (&optional (cts '("sf" "ny"))) 
 ;(mapcar #'gp-ff2 *sf* *sf-pt*)
 ;(mapcar #'gp-ff2 *ny* *ny-pt*)
  (mapcar #'do-city cts)
  (taxonomy))
#+ignore ;works, but it's in cu2.cl
(defun do_city (city &optional (ctp *rt2*) (pt *pt*))
  "parse from rss"
  (let* ((fut (assoc2nd city ctp))  ;use assoc_v
         (ft (mapcar #'first fut))
         (ut (mapcar #'cdr fut))
         (pt (assoc2nd city pt)))
    (mapcar #'(lambda (f u p) (gp-ffns2 f (rss_t f) p city)) ft ut pt)
    )) 
