;bobak@computer.org load bson2json dump of emails into KM
(lkm3) ;setup KM env (in my .sbclrc)
(load-kb "em.km") ;*Email* &re .km class files ; http://www.cs.utexas.edu/users/mfkb/RKF/tree/ 
;helper code at: https://github.com/MBcode/LispUtils &soon only updates of this there too.
; https://github.com/MBcode/LispUtils/blob/master/em.cl u2.lisp util_mb.lisp
;------------------
;bobak@computer.org
;after loading Email classes, try to make some instances
(ql 'cl-date-time-parser)
(use-package :cl-date-time-parser)

(defun td () (parse-date-time "Fri, 8 Dec 2000 08:53:00 -0800 (PST)"))

(defun t1 ()
 ;(sv-cls "efn5" "Email")
  (sv-cls "efn5" "Email-Header")
  (sv "efn5" "email-To" "bert")
  (show "efn5"))
;------------------
;from bobak@computer.org 's j.cl
(ql 'cl-json)
;excerpt
(defun jsonsp (s) (prefixp "{" s))  ;after running 'strings' on an avro file
(defun decode-jsonsp (a) (when (jsonsp a) (json:decode-json-from-string a)))
(defun decode-jsonsp2 (a) (or (json:decode-json-from-string a) a))
(defun collect-jsonsp (l) (mapcar_ #'json:decode-json-from-string (collect-if #'jsonsp l)))
(defun load-jsonsp (f) (collect-jsonsp (list-lines f))) 
;---
(defun decode-json (s)
  (if (stringp s) (decode-jsonsp s)
    (format t "~%Not a json str:~a" s)))

;load some bson dump (after small sed cleanup), then turn into KM instances
;(defun tj (&optional (file "h10.json")) (load-jsonsp file))
(defun tj (&optional (file "h1.json")) (load-jsonsp file))
(defvar *j* (tj))
;testing this by loading whole file, but want to do it via streaming, given number coming through
;so set up to process one line at a time, starting w/json accessors, then build instances
(defvar *j1* (first *j*))
(defvar *j8* (elt *j* 8))
(defun get-fn (l1) (assoc_v :FILENAME l1))
(defun get-body (l1) (assoc_v :BODY l1))
;(defun get-header (l1) (assoc_v :HEADERS l1))
(defun get-header (l1) (let ((ha (assoc :HEADERS l1))) (when (consp ha) (rest ha))))
(defun get-head-key (l1 k) (let ((h (get-header l1))) (when h (assoc_v k h))))
;(defun get-subj (l1) (let ((h (get-header l1))) (when h (assoc :*SUBJECT h))))
;(defun get-subj (l1) (get-head-key l1 :*SUBJECT)) ;might rm re/fw/etc &put in a thread slot
(defun get-subj (l1) (replace-all (get-head-key l1 :*SUBJECT) "\"" "_")) 
(defun get-from (l1) (get-head-key l1 :*FROM))
;could be serveral, also need b/cc
;(defun get-to (l1) (split-string (get-head-key l1 :*TO) #\Space)) 
;(defun get-heads-key (l1 k) (csv_parse-str (get-head-key l1 k) :separator #\,)) 
 ;want way to skip setting if nil
(defun get-heads-key (l1 k) 
  (let ((hs (get-head-key l1 k))) 
    (when hs (csv_parse-str hs :separator #\,))))
(defun get-to (l1) (get-heads-key l1 :*TO)) 
(defun get-cc (l1) (get-heads-key l1 :*Cc)) 
(defun get-bcc (l1) (get-heads-key l1 :*Bcc)) 
(defun get-date (l1) (parse-date-time (get-head-key l1 :*DATE)))
(defun get-msgid (l1) (get-heads-key l1 :*Message-ID)) 
;could just attach key to slot-name, &if a plural/multi-slot
#+no ;get all header fields from header once:
(defun get-sft (l1)
  (let ((h (get-header l1)))
    (list (assoc_v :*SUBJECT h) (assoc_v :*FROM h) (assoc_v :*TO h))))
;---put in u2.lisp
(defun svs-if (i sn vals)  
    "sv :seq"
    (let ((wsv (words-seq vals)))
      (when (full wsv) (sv i sn wsv nil t))))
;---from tel.cl:
(defun set-hash (h key val)
   ;(setf (gethash key h) val)
    (pushnew (first-lv val) (gethash key h))
   ;(pushnew (first-lv (first-lv val)) (gethash key h))
   )
;---
(ql 'mime4cl) ;so can write a better email_person
(defun mime (addr) (first-lv (mime4cl:parse-addresses addr)))
;(defun get-user (mbx) (mime4cl:mbx-user mbx))
(defun get-user (mbx) (when mbx (mime4cl:mbx-user mbx)))
;-----
;(defun email-person (atstr) (str-trim (first-lv (split-string atstr #\@))))
;(defun email-person (atstr) (str-trim 
;                              (string-trim '(#\< #\> #\' #\.)
;                                  ;rm-str "e-mail "
;                                  (rm-strs '("e-mail " "e-<")
;                                      (first-lv (split-string atstr #\@))))))
(defun get-usr (atstr) (first-lv (split-string atstr #\@)))
(defun clean-usr (usr) 
  (str-trim (string-trim '(#\< #\> #\' #\.)
                          ;rm-str "e-mail "
                              (rm-strs '("e-mail " "e-<") usr))))
(defun email-person (atstr)  (clean-usr (get-usr atstr)))
;(defun email_person (atstr) (replace-all (email-person atstr) "." "_"))
(defun email_person1 (atstr) (replace-all (email-person atstr) "." "_"))
;(defun email_person (atstr) (get-user (mime atstr)))
(defun email_person (atstr)  ;try new&fallback w/old
  (let ((usr (get-user (mime atstr))))
    (if usr (clean-usr usr)
      (email_person1 atstr))))
(trace split-string)
;---
(load-kb "Person.km")
(defun mk-person (name)
  (sv-cls name "Person")
 ;(sv name "name" name)
  (sv name "hasFullName" name) ;from Person.km
  )
;---might want to check if exists 1st
;thread prob needs more than Subj similarity(past re/fw/etc), but not doing quoted-txt/etc now
;from --~subj--> all-of: to/cc/bcc (could put b/cc svs's into to) ;could just pull out cl-graph 
;Easier to start w/ ~subj/threads from a top candidate list sketch, so have less to load;(if easy to filter)
;each thread/~subj could have it's own graph/set of instances; len could have re/loops
(ql 'md5) ;md5 of ~body as key,& msg-id as val
(defvar *b2mid* (make-hash-table :test #'equal)) ;body to msd-id
(defvar *bl2mid* (make-hash-table :test #'equal)) ;body-len(key):blk to msd-id
;(ql 'cl-graph)
;;(use-package :cl-graph)
;(defvar *g*  (cl-graph::make-container 'graph-container))
;-----
;(load "tg.cl" :print t)
(ql 'cl-graph)
(defvar *g*  (cl-containers:make-container 'cl-graph:graph-container))
(defun add-edge-between-v (from to) 
  (cl-graph:add-edge-between-vertexes *g* from to))
(defun dpth (&optional (g *g*)) (cl-graph:depth g))
;-----
(defun js2mh (&optional (l1 *j1*) (out t))
  "make js mail header KM instance"
  (when (full l1)
    (let* ((fn (get-fn l1))
          ;(efn (str-cat "efn" (remove #\. fn))) ;ok in short runs, but not full guid
           (efn (gentemp (str-cat "efn" fn))) ;guid
           (subj (get-subj l1))
           (thread (rm-strs '("RE_" "RE_" "Re_"  "FWD_" "FW_" "Fwd_" "fw_") subj)) 
           (date (get-date l1))
           (mid (get-msgid l1)) 
           (from (get-from l1))
           (from_ (email_person from))
           (to (get-to l1))
           (cc (get-cc l1))
           (bcc (get-bcc l1))
           (ta (flatten- (list to cc bcc))) ;toall=to+cc+bcc
           (tap (mapcar- #'email_person ta)) ;just ppl names
           (body (get-body l1))
           (om-p (has-om-p body)) ;where/if there is an 'original msg' in the body
           (past-body (when (numberp om-p) (subseq body (+ om-p 26)))) ;orig-msg body
           (body-use (or past-body body))    ;orig-body if available
           (m5 (md5:md5sum-string body-use)) ;will use body after om-p
           (blk (when body-use (round (/ (len body-use) 2)))) ;body-len-key w/fzy match
            ) ;esp. w/fzy round, have to make 2nd pass to be sure same body, but if same thread, might be enough
       (format t "~%bl:~a om-p:~a prev-body:~a:~a blk:~a:~a~%" (len body) om-p (len past-body) (gethash m5 *b2mid*) blk (gethash blk *bl2mid*))
       ;could test hash, or just store all nil key's vals together:
       ;(setf (gethash m5 *b2mid*) efn)
       (set-hash *b2mid* m5 efn)
       (set-hash *bl2mid* blk efn)
   ;(sv efn "email-Date" (get-date l1))
   ;(sv efn "email-Message-ID" (get-msgid l1)) 
      (sv efn "email-Date" date)
      (sv efn "email-Message-ID" mid) 
       ;
    ;let* ((fn (get-fn l1))
    ;      (efn (str-cat "efn" (remove #\. fn))))
      (format t "~%New-ins:~a" efn)
      (sv-cls efn "Email-Header") ;might have to use part of msgid, as efn not unique (beyond this test)
      ;(sv efn "email-Subject" (get-subj l1))
       (sv efn "email-Subject" subj) 
      ;(sv efn "email-thread" (rm-strs '("RE_" "RE_" "Re_"  "FWD_" "FW_" "Fwd_" "fw_") subj)) 
       (sv efn "email-thread" thread) 
     ;let ((es (get-subj l1)))
      ; (sv efn "email-Subject" es) ;might incl flag for existance of 'fwd' as well as om-p
      ;;(sv efn "email-thread" (rm-strs '("RE:" "RE" "Re:" "Re" "FWD:" "FW:" "Fwd:" "fw") es))  ;mk case insensative
      ;;(sv efn "email-thread" (rm-strs '("RE_" "RE" "Re_" "Re" "FWD_" "FW_" "Fwd_" "fw") es)) 
      ; (sv efn "email-thread" (rm-strs '("RE_" "RE_" "Re_"  "FWD_" "FW_" "Fwd_" "fw_") es)) 
      ; ;would rather have a fnc that splits all fwd etc strs, and orig subj, &store seperately
      ; ; if thread works can just diff w/orig subj
      ; )
     ;let* ((from (get-from l1))
           ;(from_ (email_person from))
           ;(to (get-to l1))
           ;(cc (get-cc l1))
           ;(bcc (get-bcc l1))
           ;(ta (flatten- (list to cc bcc)))
           ;(tap (mapcar- #'email_person ta))
           ;)
       (format t "~%to:~a" tap)
       (svs efn "email-To" tap)
       ;(mapcar #'(lambda (ato) (cl-graph:add-edge-between-vertexes *g* from_ ato)) tap)
        (mapcar #'(lambda (ato) (add-edge-between-v from_ ato)) tap)
        ;could just dump the pairs here&read in elsewhere
        ;(mapcar #'(lambda (ato) (format out "~%~a, ~a" from_ ato)) tap) ;could incl date ..
        ;(mapcar #'(lambda (ato) (format out "~%~a, ~a, ~a, ~a" from_ ato date om-p)) tap) 
        ;(mapcar #'(lambda (ato) (format out "~%~a, ~a, ~a" date from_ ato)) tap) 
      ; (mapcar #'(lambda (ato) (format out "~%~a, ~a, ~a, ~a" date from_ ato subj)) tap) 
        (mapcar #'(lambda (ato) (format out "~%~a, ~a, ~a, ~a" date from_ ato thread)) tap) 
       ;(sv efn "email-From" (get-from l1)) 
       ;(svs efn "email-From" from) 
       ;(svs efn "email-From" (email_person from)) 
       (svs efn "email-From" from_) 
       ;(mapcar- #'mk-person (append (list (email_person from)) tap))
       (mapcar- #'mk-person (append (list from_) tap))
       ;;(svs efn "email-To" (get-to l1))
       ;(svs efn "email-To" to)
       ;;(svs efn "email-Cc" (get-cc l1))
       ;;(svs efn "email-Bcc" (get-bcc l1))
       ;(svs-if efn "email-To" cc)
       ;(svs-if efn "email-To" bcc)
      ;
      (show efn)
      efn)))  ;clean up old commented code/in a bit
;what is enough for linking, msgid needed?
;look for -----Original Message----- in body
(defun has-om-p (tx &optional (om "-----Original Message-----"))
  (search om tx))
; if om-p hash past it, if not the whole thing, assoc w/ the email-id
;  if not full-txt then hash-check/like from dwnlded sofware/later
;  hash-table will need :test #'equal ;also push(new?) all IDs
;Do so can have further sense of that subj(thread)is being passed -&sv chain
;..
;(trace get-head-key assoc_v get-to)
;(trace get-to str-trim)
(defun tjl (&optional (l *j*)) (mapcar- #'js2mh l))
(defun tjlo (&optional (l *j*)) (with-open-file (strm "pairs.csv" :direction :output) (mapcar- #'(lambda (x) (js2mh x strm)) l)))
  ;could inhibit pairs.csv now that cl-graph loaded, but keep for a bit; even though it can do a dot/etc dump
(defun tj1 (&optional (l1 *j1*)) (js2mh l1))
(defun tj8 (&optional (l1 *j8*)) (js2mh l1))
;Have rule that said if same thread, &no send w/a from, then you are the start
;  (actually send has starter as the from, but no send to that person)
;  after that chain down to find length, taxonomy cmnd over re slots should do it.
;------------------can view in Protege &then use Jess/CLIPS after using this sed:
;/^(./s/ has/]/
;/^(./s//([/
;/^  (instance-of (/s/))//
;/^  (instance-of (/s//    of /
;/^  (email-/s/))/)/
;/^  (email-/s/((:seq ./ [/
;/^  (email-Subject (/s/t (/t /
;/^  (email-thread (/s/d (/d /
;/^  (email-Date (/s/e (/e /
;/^  (email-[FT]/s/))/])/
;/^  (hasFullName/s/))/)/
;/^  (hasFullName/s/e (/e / 
;/ \*/s//] [/g
;--then on (save-kb "output") run: agrep -i -d '^('  'instance-of' 
;which can be used in em.pins for protege&clips
;Have taken it from 1 to 10 to 100, then 1124 of the hopefully longest email thread, so can hook up the links to get the length.
;Jess> (mapclass USER) ;get: :THING  ;then could try a rule like:
;Jess> (defrule apass (object (is-a Email-Header) (email-From ?from1) (email-To ?to1) (email-Date ?date1)) 
;                     (object (is-a Email-Header) (email-From ?to1) (email-To ?To2) (email-Date ?date2)) 
;=> (printout t "from=" ?from1 " to-from=" ?to1 " and " ?date2 " > " ?date1) ) ;can put in a test
;use http://www.cliki.net/email sometime  http://wcp.sdf-eu.org/software/
