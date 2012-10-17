;parse/scrape  blog-posts bobak@balisp.org
; wish more bloggers used rdfa or similar ;thought experiment on how2add it, might help here/?
(ql '(puri drakma chtml-matcher)) ;(use-package :puri)
(lut) ;(load "~/lsp/util_mb")  ;https://github.com/MBcode/LispUtils
(lkm2) ;(load "~/lsp/km_2-5-33") ;(load "~/lsp/u2") ;~=ukm2 ;https://github.com/MBcode/kmb
;(when (find-package :km) (use-package :km)) ;should just import what needed
;(when (find-package :km)
;  (defun load-kb (kb) (km:load-kb (kb)))
;  (defun km () (km:km)))
(defun str-cat_2 (a b)
      (format nil "~a ~a" a b))
(defun str-cat+ (&rest args)
      (reduce #'str-cat_2 args))
(defun butfirst-n (s n)
  "everything after1st n in a seq"
    (subseq s n (len s)))
(setq drakma:*header-stream* *standard-output*)

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
  "log lst of posts to json lines in a file"  ;used
  (with-open-file (out outf :direction :output :if-exists :supersede)
    (when (and (stringp tag) (full tag)) (format out "~%~a," tag))
    (if (listp tag) (mapcar #'(lambda (l1 t1)  ;l=lol tag=assoc l o tags
                                (format out "~%~a," t1)
                                (encode-js2s l out)) l tag)
      (enconde-js2s l out))))

(defun logjsonal (lal &optional (outf "log.txt")) ;maybe still a dflt tag
  "alst of to a json log" ;this v as alst of l1 tg1 ..  ;was going2mv2this
  (with-open-file (out outf :direction :output :if-exists :supersede)
   ;(when (and (stringp tag) (full tag)) (format out "~%~a," tag))
    (mapcar #'(lambda (al)  ;lst of vals or (assoc if a tag)
                (when (listp al) (format out "~%~a," (cdr al)))
                (encode-js2s (first-lv al) out)) lal)))

(load "cu2.cl" :print t) ;domain specific /blogs

(defun s-crape-str (str)
  "htm str -> lhtml"
  (chtml:parse str (chtml:make-lhtml-builder))) 

(defun s-crape-fn (fn)
  "htm file -> lhtml"
  (s-crape-str (read-file-to-string fn)))

(defvar *i* (s-crape-fn "index.html")) ;as a default for testing parser fncs

;defun find-lh (tag attrib &optional (n 1) (lhtml *i*))
(defun find-lh (tag &optional (n 1) (attrib nil) (lhtml *i*))
  "get nth sub-sexpr w/tag" ;try to only do in smaller branchs
  (chtml-matcher:find-in-lhtml lhtml tag attrib n))

(defun get-post (n &optional (at-str "post hentry") (lh *i*))
  "pull lhtml branch for one blogPost" ;pass everything if can't catch it
  (format nil "~%g-p:~a" (len lh))
  (or (find-lh "div" n `((:CLASS ,at-str)) lh)
      lh)) ;if can't find pass through w/o filtering out just the nth blogPost

(defun get-post- (n &optional (at-str "post hentry") (lh *i*))
  "pull lhtml branch for one blogPost"
  (format nil "~%g-p-:~a" (len lh))
  (find-lh "div" n `((:CLASS ,at-str)) lh)) ;need ver to ret nil, so can get-pt

;might make a version that tries all the pt(post-tag) strings  ;get_post
(defvar *trypt* '("post hentry" "format_text entry-content" "blog-content"
                  "journal-entry-tag journal-entry-tag-post-body" "pin-it-btn-shortcode-wrapper"
                  "post-meta"))
(defun get-pt (lh)
  "return pt(post-tag) for lhtml, blogPost"
  (first-lv (rm-nil (mapcar #'(lambda (tp) (when (get-post- 1 tp lh) tp)) *trypt*))))
;so can now come up w/versions that don't have to remember site metadata of PostTag
; could still remember it, but later can defgeneric,&have it try2look it up 1st

#+ignore ;only check get-pt once/page not /post
(defun get_post (n &optional (lh *i*))
  "get any post w/o knowing PostTag in adv"
  (format nil "~%g_p:~a" (len lh))
     ;or (get-post n (get-pt lh) lh)  ;better to get-pt once/blog vs.during iteration
     (or (get-post- n (get-pt lh) lh)  ;better to get-pt once/blog vs.during iteration
         lh)) ;if can't find pass through w/o filtering out just the nth blogPos

(defun p-lh (lh tag)
  "w/in1post,get all of a tag-type"
    (loop for i from 1 to 99 
          for p = (find-lh tag i nil lh)
          while p collect p))
;(trace p-lh)
(defun get-body (lh) (p-lh lh "body"))

;ct=city (sf or ny)now; used to subclass blogPost s &more
;-use this one  ;will also parse each post more, &(km)assert interesting bits
(defun lt-assert (lf tf fn &optional (ct nil))
  "km assert" ;will parse lf more too
    (sv-cls tf (if ct (str-cat ct "BlogPost") "BlogPost"))
    (sv-al tf (list ;(cons "img" (p-lh (p-lh lf "img") "src"))
                (cons "img" (p-lh lf "img"))
                (cons "i" (p-lh lf "i"))))
    )  ;pull out less/make cleaner..
;going to make ins for img/etc ;will be in c2.km
;can more easily group/see same/similar imgs/descriptions/etc

;;;GP(get-post)fns
(defun gp-ffns2 (fn s pt &optional (ct nil))  ;use this on sf&sf-pt
  "get post/s from sexpr w/filename-tag  assert&log-js2sep files"
  (format nil "~%gp-ffns2p:~a ~a ~a" fn (len s) pt)
  (let* (;(s (s-crape-fn fn))
         (la (loop for i from 1 to 89 
                 ;for p = (get-post i pt s)
                  for p = (get-post- i pt s) ;need so it will stop
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
;-try ver w/o pt, by using get-pt
(defun gp_ffns2- (fn s  &optional (ct nil))  ;pt not needed
  "get post/s from sexpr w/filename-tag  assert&log-js2sep files"
  (let* (;(s (s-crape-fn fn))   ;s=sexpr
         (pt (get-pt s)) ;get PostTag once/blog 
         (la (loop for i from 1 to 89 
                 ;for p = (get_post i  s)
                 ;for p = (get-post i pt s)
                  for p = (get-post- i pt s) ;need so it will stop
                  while p collect (cons (str-cat fn i) p)))
         (l2 (alst2 la)) ;or collect2&ret values
         (tl (first l2))
         (l (second l2)))
    ;log(for doc-store instert?)/etc, and assert now, for indx/inference/..
    (logjsonl l tl (str-cat "log/" fn ".js")) ;seperate&supercede now ;already takes 2lists
    (mapcar #'(lambda (lf tf) (lt-assert lf tf fn ct)) l tl) ;do for each blog post
    ))  
(defun gp_ffns2 (fn s  &optional (ct nil))  ;pt not needed  ;for rss ;rename-soon
  "log&assert rss lh"
  (let ((tl (list (str-cat fn "-date-or-similar")))
        (l (list s)))
    (logjsonl l tl (str-cat "log/" fn ".js")) ;seperate&supercede now ;already takes 2lists
    (mapcar #'(lambda (lf tf) (lt-assert lf tf fn ct)) l tl) ;do for each blog post
    ))
(defun gp_ff2 (fn  &optional (ct nil))  ;use this on sf&sf-pt
  "get post/s from file assert&log-js2sep files"
   (gp_ffns2 fn (s-crape-fn fn)  ct))
;-
(defun lk () (load-kb "c2.km")) ;(BlogPost has (superclasses (Thing))) &much more
(defun lk2 () (lk) (taxonomy))    ;then can do a (taxonomy)
(lk)
;-try ver os docity fncs w/o pt
  ;ctp=*sf* *ny*  pt might go to cfg, &/or run through list of them 1st time/? 
(defun do-city (city &optional (ctp *ct*) (pt *pt*)) 
  "parse posts from htm files"
  (let ((ct (assoc2nd city ctp))  ;use assoc_v
        (pt (assoc2nd city pt)))
   ;(mapcar #'gp-ff2 *sf* *sf-pt*)
    ;mapcar #'gp-ff2 ct pt city
    (mapcar #'(lambda (c p) (gp-ff2 c p city)) ct pt)))

(defun do-city_ (city &optional (ctp *ct*) ) 
  "parse posts from htm files"
  (let ((ct (assoc2nd city ctp)))
    (mapcar #'(lambda (c ) (gp_ff2 c  city)) ct )))

(defun tst (&optional (cts '("sf" "ny"))) 
 ;(mapcar #'gp-ff2 *sf* *sf-pt*)
 ;(mapcar #'gp-ff2 *ny* *ny-pt*)
  (mapcar #'do-city cts)
  (taxonomy))
;-last v had these working, but also caught in loop now
(defun tst2 (&optional (cts '("sf" "ny"))) 
  (mapcar #'do-city_ cts)
  (taxonomy))
;==was in cu2.cl
(defun hl95 () (s-crape-str (h95)))
(defun scrape-tag (tg) (scrape-uri (t2rss tg)))

(defun rss-t (tg)
  "get str of rss for a tag"
  (hr (t2rss tg)))

(defun rss_t (tg)
  "get sexpr from rss for tg"
  (s-crape-str (rss-t tg)))
;-
;(defvar *pw* (string-list->keyword-vector '("jpg" "jpeg" "gif" "png")))
;(defvar *dp* '(".jpg" ".jpeg" ".gif" ".png"))
;-
#+ignore ;~works, but it's in cu2.cl ;finish
(defun do_city (city &optional (ctp *rt2*) (pt *pt*))
  "parse from rss"
  (let* ((fut (assoc2nd city ctp))  ;use assoc_v
         (ft (mapcar #'first fut))
         (ut (mapcar #'cdr fut))
         (pt (assoc2nd city pt)))
    (mapcar #'(lambda (f u p) (gp-ffns2 f (rss_t f) p city)) ft ut pt)
    )) ;does the rss version even need the pt anyway?
;still fix/finish these
(defun gp_fc (f city) (gp_ffns2 f (rss_t f)  city))
(defun gp_tg (tg) (gp_fc tg (subseq tg 0 2)))
;-fix not parsing body:
(defun body-lh (lh)
  "get unparsed body&parse it"
  (s-crape-str (apply #'str-cat+ (butfirst-n (first (get-body lh)) 5))))
(defun prs-lh-body (lh &optional (tg "dflttg"))
  "get lh w/unparsed body, get&parse it"  ;assume :body &4more are junk /but could check
  (let (;(lhb (s-crape-str (apply #'str-cat+ (butfirst-n (first (get-body lh)) 5))))
        (lhb (body-lh lh))
        (ct (subseq tg 0 2)))
    (gp_ffns2 tg lhb ct)))

(defun gp_fc2 (f city) 
  "parse an rss update" ;hopefully only 1 /check  ;make sure not getting all the posts at once/could be
  (let ((lh (rss_t f)))
    (gp_ffns2 f lh  city) ;try to parse the whole thing
    (prs-lh-body lh f))) ;assume body needs to be broken out&parsed
(defun gp_tg2 (tg) (gp_fc2 tg (subseq tg 0 2))) ;eg. (gp_tg2 "sf6") ;gets rss&when bodyMissed parses it
;next go for parsing out time, &more..
;
(defun do_city_ (city &optional (ctp *rt2*) ) ;still needs help even w/o pt
  "parse from rss"
  (let* ((fut (assoc2nd city ctp))  ;use assoc_v
         (ft (mapcar #'first fut))
         (ut (mapcar #'cdr fut)))
    (mapcar #'(lambda (f u ) (gp_fc f city) ;(gp_ffns2 f (rss_t f)  city)
                ) ft ut )))
;if can't find pass through w/o filtering out just the nth blogPos 
; poss through kept get-post from stopping the loop
