;parse/scrape  blog-posts bobak@balisp.org ;https://sites.google.com/site/mikebobak/ip
; wish more bloggers used rdfa or similar ;thought experiment on how2add it, might help here/?
(ql '(puri drakma chtml-matcher)) ;(use-package :puri)
(lut) ;(load "~/lsp/util_mb")  ;https://github.com/MBcode/LispUtils
(lkm2) ;(load "~/lsp/km_2-5-33") ;(load "~/lsp/u2") ;~=ukm2 ;https://github.com/MBcode/kmb
;(when (find-package :km) (use-package :km)) ;should just import what needed
; loading the file directly w/o package info instead
(setq drakma:*header-stream* *standard-output*)
(defun hr (u) ;should not have to redefine, worry about later
  "get the page as 1str"
  (drakma:http-request (if (s-prefixp "http" u) u (str-cat "http://" u))))
;(load "queue.lisp" :print t) ;unpkgd rsm for:
;(load "filter.lisp" :print t) ;(al 'rsm-filter) ;try2use
(lrsm) ;in .sbclrc like lut&lkm2
(defun hr (u)
  "http-request:get the page as 1str" ;if file:// skip hr
  (drakma:http-request (if (s-prefixp "http" u) u (str-cat "http://" u))))

(ql 'cl-json)
(defun decode-file (path) ;not used right now
  (with-open-file (stream path :direction :input)
    (json:decode-json-strict stream)))

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

(load "cu2.cl" :print t) ;domain specific /blogs ;will replace w/site.csv

(defun s-crape-str (str)
  "htm str -> lhtml"
  (chtml:parse str (chtml:make-lhtml-builder))) 

(defun s-crape-fn (fn)
  "htm file -> lhtml"
  (s-crape-str (read-file-to-string fn)))

(defgeneric s-crape (s))
;(defmethod s-crape ())

(defvar *i* (s-crape-fn "index.html")) ;as a default for testing parser fncs
(defvar *r1* (s-crape-fn "cu/sf1.htm")) ;find rss still has posts/go to old parsing of rss as well
;(load "x.cl" :print t) ;(ql 's-xml) (defvar *ix* (parse-xmlsfile "index.xml"))
;(al 'phtml) ;so can compare w/parse-html
;(defun s-crape-str (a) (phtml:parse-html a))
;(defun s-crape-fn (a) (phtml:parse-html (read-file-to-string a)))
;(defvar *ip* (s-crape "index.html")) ;as a default for testing parser fncs

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
                  "post-meta")) ;after figure out url<->pt could cache it&use vs. trying again
(defun get-pt (lh)
  "return pt(post-tag) for lhtml, blogPost"
  (first-lv (rm-nil (mapcar #'(lambda (tp) (when (get-post- 1 tp lh) tp)) *trypt*))))
;so can now come up w/versions that don't have to remember site metadata of PostTag
; could still remember it, but later can defgeneric,&have it try2look it up 1st

#+ignore ;only check get-pt once/page not /post
(defun get_post (n &optional (lh *i*))
  "get any post w/o knowing PostTag in adv"
  (format t "~%g_p:~a" (len lh))
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
(defun rec-len (l)
  (when (full l)
    (format t " ~a:~a " (len l) (len (second-lv l)))
    (rec-len (cddr l)))) ;but-first-n 2
;(rec-len (caddr *i*))
; 28:0  26:8  24:2  22:2  20:2  18:2  16:2  14:3  12:2  10:2  8:3  6:2  4:2  2:3 
 ;try2get chtml-matcher going, it might speed some of the work up
;chtml pairs w/cxml -
;(name (attributes) children*) ;xmls /etc, should go dwn&parse this way
;ct=city (sf or ny)now; used to subclass blogPost s &more
;-use this one  ;will also parse each post more, &(km)assert interesting bits
(defun gentmp (n) (if n (gentemp (str-trim n)) (gentemp)))
#+ignore
(defun mk-name (al &optional (c nil)) ;might have a slot name2use for cls
  (let ((np (second-lv (second-lv al)))) ;c2sn ->  (assoc2nd al c2sn)
    (gentmp (when (atom np) np)))) ;for now
(defun mk-name (al &optional (c nil))
  ;might have a slot name2use for cls
  (let ((np (second-lv (second-lv al)))) ;c2sn ->  (assoc2nd al c2sn)
    (gentmp (if (and (atom np) (full np)) 
              (ki_ np) ;(under_ (str-trim (str-cat np)))
              c)) ;for now
  ))
(defun sv-al (i al)   ;SetValue s from alist ;wrk on here1st, was mapcar&car
  "set km values from alst"  ;other tests worth thinking about
  (mapcar_ #'(lambda (pr) (sv i (first-lv pr) (cdr pr)))
          al))
;in all uses of sv-al below, might iterate over &/or look@ svs[2]  ;or try:
(defun svs-al (i al)   ;SetValue s from alist ;wrk on here1st, was mapcar&car
  "set km values from alst"  ;other tests worth thinking about
  (mapcar_ #'(lambda (pr) (svs i (first-lv pr) (cdr pr)))
          al))
(defun sv_ (i sn v) (if (listp v) (svs i sn v) (sv i sn v)))
;(defun sv_ (i sn v) (if (listp v) (svs2 i sn v) (sv i sn v)))
(defun sv-s-al (i al) 
  (let ((all (if (or (<= (tree-depth al) 1)  (not (first-lv al)))
               (list (list (or (first-lv al) 'val) (second-lv al))) ;* (list al) 
               al)))
    (mapcar_ #'(lambda (pr) (let ((sn (first-lv pr))) 
                              (when sn (sv_ i sn (cdr pr)))))
          all)))
;set up as (:cls * ...
(defun sv-al_ (i al) 
  (let ((all (collect-if #'listp al)))
    (mapcar_ #'(lambda (pr) (let ((sn (first-lv pr))) 
                              (when sn (sv_ i sn (cdr pr)))))
          all)))
(defun mk-cls (i cls als)
  ;when i 
  (when (and i (or cls als)) 
    (when cls
      (sv-cls i cls))
    (when (and als (len_gt als 1))
      (sv-s-al i als));if just one of cls ;might want a ver that svs if al val is a list ;tried sv-al_
    (ki i)))
(defun mk-clses (als) ;assoc of cls name &values, which can be used to make the ins name mabye w/gentemp
  ;(sv-cls i cls)
  ;(sv-als i als);if just one of cls, map over just that 1
  (rec-len als) ;try
  (mapcar #'(lambda (cal) 
              (let ((c (first cal))
                    (al (cdr cal)))
                ;when (eq c cls) 
                (mk-cls (mk-name al c) c al) ;(sv-al i al)
                  ))
          (if (listp als) als
            (collect-if #'listp als)))
 ;(ki i) ;should ret list of ins-names
  )
(trace mk-clses)
(defun mk-body-clses (als) 
  (let* ((bdy (get-body als))
         (td (tree-depth bdy)))
    (when bdy 
      (format t "~%body-depth:~a" td)
      (when (< td 8) ;not aligned to iterate properly/fix
        (mk-clses bdy)))))
;
(defun rec-mk (l)
  (when (full l)
    (format t "~%~a:~a:~a " (first-lv l) (len l) (len (second-lv l)))
    (rec-mk (cddr l)))) 
;get the (reg)struct of this thing down/so can easily parse it  ;maybe try rsm mappers as well
(defvar *ignore-tgs* '(:SCRIPT :STYLE))
(defun ignorable-tg-p (a) (member (first-lv a) *ignore-tgs*)) ;not yet/fix
(defun filter-tgs (l) (filter l #'ignorable-tg-p)) ;lrsm above
(defun rec-mk2 (l) ;;get the (reg)struct of this thing down fix/finish
  (when (full l)
    (let ((fl (first-lv)))
      (format t "~%~a:~a:~a:~a " (first-lv fl) (len l) (len (second-lv l)) (len (second-lv fl)))
      (rec-mk2 (cddr l)))))
;(defun sv-p-lh (i tg) (sv-al i (p-lh lf tg)))
(defun mk-si (lh tg)
  "get ins for slots"
  (cons tg (mk-clses (p-lh lh tg))))

(defun lt-assert (lf tf fn &optional (ct nil))
  "km assert" ;will parse lf more too
    (sv-cls tf (if ct (str-cat ct "BlogPost") "BlogPost"))
    ;(mapcar #'(lambda (tg) (sv-p-lh tf tg)) '("img" "i" "strong"))
    ;or (sv-p-lh tf "img") ... or:
    ;(mapcar #'(lambda (tg) (sv-al tf (p-lh lf tg))) '("img" "i" "strong"))
    (sv-al tf (mapcar #'(lambda (tg) (mk-si lf tg)) '("img" "i" "strong")))
    ;;svs-al tf 
    #+ignore
    (sv-al tf 
           (list ;(cons "img" (p-lh (p-lh lf "img") "src"))
                (cons "img" (p-lh lf "img"))
                (cons "i" (p-lh lf "i"))
                (cons "strong" (p-lh lf "strong"))
                ))
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
;same file read just different inputs
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

;===below is all for rss
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
    ;(mk-body-clses lhb) ;or just mk-clses  ;try this as well
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
    (mapcar #'(lambda (f u ) (gp_fc2 f city) ;gp_fc;(gp_ffns2 f (rss_t f)  city)
                ) ft ut )))
;if can't find pass through w/o filtering out just the nth blogPos 
; poss through kept get-post from stopping the loop
(defun tst3 (&optional (cts '("sf" "ny"))) 
  (mapcar #'do_city_ cts)
  (taxonomy))
