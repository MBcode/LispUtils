;threec utilities
(defun km-seqp+ (s)  ;mv2 kmb/u2.lisp
  (km-seqp (list+ s)))
 ;====start of sys.lisp
#|
Copyright (c) 2000-2006, Sunil Mishra All rights reserved. for99lines
|# ;;; $Id: system-standalone.lisp 97 2006-08-31 06:00:10Z smishra $

(defvar *source-file-extensions*
  (list "lisp" "cl" "lsp"))
(defvar *compiled-file-extension*
  #.(pathname-type (compile-file-pathname "foo")))

(defun file-newer-p (file1 file2)
  (> (or (file-write-date file1) 0) (file-write-date file2)))

(defun find-file-with-type (pathname types
			    &optional (if-does-not-exist :error))
  (or (cond ((pathname-type pathname)
	     ;; Must be the full file path
	     (when (probe-file pathname)
	       pathname))
	    ((stringp types)
	     (let ((pathname (make-pathname :type types :defaults pathname)))
	       (when (probe-file pathname)
		 pathname)))
	    (t (dolist (source-type types)
		 (let ((pathname
			(make-pathname :type source-type :defaults pathname)))
		   (when (probe-file pathname)
		     (return pathname))))))
      (ecase if-does-not-exist
	((nil :soft) nil)
	(:error (error "Source file corresponding to ~S does not exist."
		       pathname)))))

#-allegro
(defun compile-file-if-needed (source fasl verbose verbose-supplied-p
			       print print-supplied-p external-format force)
  #+clisp (declare (ignore external-format))
  (when (or force
	    (not (probe-file fasl))
	    (and source (file-newer-p source fasl)))
    (loop
     (multiple-value-bind (output-truename warnings-p failure-p)
	 (compile-file source :output-file fasl
		       :verbose (if verbose-supplied-p
				    verbose
				  *compile-verbose*)
		       :print (if print-supplied-p
				  print
				*compile-print*)
		       #-clisp :external-format #-clisp external-format)
       (if (or failure-p (and warnings-p (eql *break-on-signals* t)))
	   ;; Todo: Also need a skip compilation restart.
	   (cerror "Retry compile." "Problems compiling ~S." source)
	 (return output-truename))))))

(defun compile-load (file &key (verbose nil verbose-supplied-p)
		     (print nil print-supplied-p) (if-does-not-exist :error)
		     (external-format :default) output-file force)
  "Compile a file if source newer, and load."
  (let* ((source (find-file-with-type (pathname file) *source-file-extensions*
				      nil))
	 (fasl (apply #'compile-file-pathname (or source file)
		      (when output-file
			`(:output-file ,output-file)))))
    (cond ((or source (probe-file fasl))
	   (let ((compile-result
		  #+allegro
		  (apply #'compile-file-if-needed source
			 `(,@(when verbose-supplied-p `(:verbose ,verbose))
			     ,@(when print-supplied-p `(:print ,print))
			     ,@(when output-file `(:output-file ,output-file))
			     :external-format ,external-format
			     :force-recompile ,force))
		  #-allegro
		  (compile-file-if-needed
		   source fasl verbose verbose-supplied-p print
		   print-supplied-p external-format force)))
	     (values
	      (load fasl :print (if print-supplied-p print *load-print*)
		    :verbose (if verbose-supplied-p verbose *load-verbose*)
		    :if-does-not-exist if-does-not-exist)
	      compile-result)))
	  (t
	   (case if-does-not-exist
	     (:error (error "Could not locate source or fasl for ~S." file))
	     ((nil :soft) nil))))))
;rest in: ai/sw/sf/public/mcpat/trunk/system-standalone.lisp
;
;new
(defun lsp-p (f) 
  (member (pathname-type f) *source-file-extensions* :test #'equal))
(defun ls-lsp (&optional (d nil))
  (collect-if #'lsp-p (ls d)))
;(defun asd-p (f) (suffixp ".asd" f))
(defun asd-p (f) (equal "asd" (pathname-type f)))
(defun ls-asd (&optional (d nil))  
  (collect-if #'asd-p (ls d)))
(defun asd1 (&optional (d nil))
  "1st/smallest asd file in dir"
  (first-lv (sort (ls-asd d) #'len<))) 
;need2 str-cat the d in
(defun a1 (&optional (d nil)) 
  "get root/sym of smallest asd file in dir"
  (let ((as (asd1 d)))
    (when as (intern (car-lv (split-strs2at as "."))))))
(defun la1 () 
  "load most likely .asd file in this dir"
 ;(al (a1))
  (let ((a1 (a1)))
    (format t "~%(al '~a)" a1)
    (al a1)))
;-
;(load-bps) ;mine is much nicer
(defun c-load (f) (compile-load f))
;(trace c-load)
(defun load-lsp (&optional (dir "tools/bps/"))
 (let ((lf (ls-lsp dir)))
  (when (full lf)
    (mapcar #'c-load (mapcar #'(lambda (f) (strcat dir (pathname-name f))) lf)))))
;(setq *reasoner-loaded* t)
 ;====end of sys.lisp
;(in-package "LISA")
;util_mb.cl (in v3) =util-mb.cl + path.cl (from v2);  Written/Collected by bobak@computer.org
;use: adjoin/pushnew, SET-EXCLUSIVE-OR, SUBSETP, getf/plist
(defun subl> (a b) (subsetp b a))
;==> /Users/bobak/Documents/downloads/lang/lsp/ai/ut/mfkb/km/run/two/ut/path.cl <== 
;;; *****************************************************************************

(defun list-directory (dirname)
  "Return a list of the contents of the directory named by dirname.
Names of subdirectories will be returned in `directory normal
form'. Unlike CL:DIRECTORY, LIST-DIRECTORY does not accept
wildcard pathnames; `dirname' should simply be a pathname that
names a directory. It can be in either file or directory form."
  (when (wild-pathname-p dirname)
    (error "Can only list concrete directory names."))

  (let ((wildcard (directory-wildcard dirname)))

    #+(or sbcl cmu lispworks)
    ;; SBCL, CMUCL, and Lispworks return subdirectories in directory
    ;; form just the way we want.
    (directory wildcard)
    
    #+openmcl
    ;; OpenMCl by default doesn't return subdirectories at all. But
    ;; when prodded to do so with the special argument :directories,
    ;; it returns them in directory form.
    (directory wildcard :directories t)
            
    #+allegro
    ;; Allegro normally return directories in file form but we can
    ;; change that with the :directories-are-files argument.
    (directory wildcard :directories-are-files nil)
            
    #+clisp
    ;; CLISP has a particularly idiosyncratic view of things. But we
    ;; can bludgeon even it into doing what we want.
    (nconc 
     ;; CLISP won't list files without an extension when :type is
     ;; wild so we make a special wildcard for it.
     (directory wildcard)
     ;; And CLISP doesn't consider subdirectories to match unless
     ;; there is a :wild in the directory component.
     (directory (clisp-subdirectories-wildcard wildcard)))

    #-(or sbcl cmu lispworks openmcl allegro clisp)
    (error "list-directory not implemented")))


(defun file-exists-p (pathname)
  "Similar to CL:PROBE-FILE except it always returns directory names
in `directory normal form'. Returns truename which will be in
`directory form' if file named is, in fact, a directory."

  #+(or sbcl lispworks openmcl)
  ;; These implementations do "The Right Thing" as far as we are
  ;; concerned. They return a truename of the file or directory if it
  ;; exists and the truename of a directory is in directory normal
  ;; form.
  (probe-file pathname)

  #+(or allegro cmu)
  ;; These implementations accept the name of a directory in either
  ;; form and return the name in the form given. However the name of a
  ;; file must be given in file form. So we try first with a directory
  ;; name which will return NIL if either the file doesn't exist at
  ;; all or exists and is not a directory. Then we try with a file
  ;; form name.
  (or (probe-file (pathname-as-directory pathname))
      (probe-file pathname))

  #+clisp
  ;; Once again CLISP takes a particularly unforgiving approach,
  ;; signalling ERRORs at the slightest provocation.

  ;; pathname in file form and actually a file      -- (probe-file file)      ==> truename
  ;; pathname in file form and doesn't exist        -- (probe-file file)      ==> NIL
  ;; pathname in dir form and actually a directory  -- (probe-directory file) ==> truename
  ;; pathname in dir form and doesn't exist         -- (probe-directory file) ==> NIL

  ;; pathname in file form and actually a directory -- (probe-file file)      ==> ERROR
  ;; pathname in dir form and actually a file       -- (probe-directory file) ==> ERROR
  (or (ignore-errors
        ;; PROBE-FILE will return the truename if file exists and is a
        ;; file or NIL if it doesn't exist at all. If it exists but is
        ;; a directory PROBE-FILE will signal an error which we
        ;; ignore.
        (probe-file (pathname-as-file pathname)))
      (ignore-errors
        ;; PROBE-DIRECTORY returns T if the file exists and is a
        ;; directory or NIL if it doesn't exist at all. If it exists
        ;; but is a file, PROBE-DIRECTORY will signal an error.
        (let ((directory-form (pathname-as-directory pathname)))
          (when (ext:probe-directory directory-form)
            directory-form))))


    #-(or sbcl cmu lispworks openmcl allegro clisp)
    (error "list-directory not implemented"))

(defun no-such-file (fn)
  "maybe more readable"
  (not (file-exists-p fn)))

(defun directory-wildcard (dirname)
  (make-pathname 
   :name :wild
   :type #-clisp :wild #+clisp nil
   :defaults (pathname-as-directory dirname)))

#+clisp
(defun clisp-subdirectories-wildcard (wildcard)
  (make-pathname
   :directory (append (pathname-directory wildcard) (list :wild))
   :name nil
   :type nil
   :defaults wildcard))


#-CCL
(defun directory-pathname-p (p)
  "Is the given pathname the name of a directory? This function can
usefully be used to test whether a name returned by LIST-DIRECTORIES
or passed to the function in WALK-DIRECTORY is the name of a directory
in the file system since they always return names in `directory normal
form'."
  (flet ((component-present-p (value)
           (and value (not (eql value :unspecific)))))
    (and 
     (not (component-present-p (pathname-name p)))
     (not (component-present-p (pathname-type p)))
     p)))


(defun file-pathname-p (p)
  (unless (directory-pathname-p p) p))

;; (pathname-as-directory "foo")
#+sbcl ;otherwise think already defined
(defun pathname-as-directory (name)
  "Return a pathname reperesenting the given pathname in
`directory normal form', i.e. with all the name elements in the
directory component and NIL in the name and type components. Can
not be used on wild pathnames because there's not portable way to
convert wildcards in the name and type into a single directory
component. Returns its argument if name and type are both nil or
:unspecific."
  (let ((pathname (pathname name)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (if (not (directory-pathname-p name))
      (make-pathname 
       :directory (append (or (pathname-directory pathname) (list :relative))
                          (list (file-namestring pathname)))
       :name      nil
       :type      nil
       :defaults pathname)
      pathname)))

#+sbcl ;otherwise think already defined
(defun pathname-as-file (name)
  "Return a pathname reperesenting the given pathname in `file form',
i.e. with the name elements in the name and type component. Can't
convert wild pathnames because of problems mapping wild directory
component into name and type components. Returns its argument if
it is already in file form."
  (let ((pathname (pathname name)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (if (directory-pathname-p name)
      (let* ((directory (pathname-directory pathname))
             (name-and-type (pathname (first (last directory)))))
        (make-pathname 
         :directory (butlast directory)
         :name (pathname-name name-and-type)
         :type (pathname-type name-and-type)
         :defaults pathname))
      pathname)))

;; (walk-directory *logs-root* #'pprint :directories t :test #'directory-p)
(defun walk-directory (dirname fn &key directories (test (constantly t)))
  "Walk a directory invoking `fn' on each pathname found. If `test' is
supplied fn is invoked only on pathnames for which `test' returns
true. If `directories' is t invokes `test' and `fn' on directory
pathnames as well."
  (labels
      ((walk (name)
         (cond
           ((directory-pathname-p name)
            (when (and directories (funcall test name))
              (funcall fn name))
            (dolist (x (list-directory name)) (walk x)))
           ((funcall test name) (funcall fn name)))))
    (walk (pathname-as-directory dirname))))

(defun directory-p (name)
  "Is `name' the name of an existing directory."
  (let ((truename (file-exists-p name)))
    (and truename (directory-pathname-p name))))

(defun file-p (name)
  "Is `name' the name of an existing file, i.e. not a directory."
  (let ((truename (file-exists-p name)))
    (and truename (file-pathname-p name))))

;"home/local/bank-a-trail/code/web-access/elf-database3a.lisp" 1610 lines --64%--    
;
(defun pathname-lessp (pathname1 pathname2)
  (string-lessp (princ-to-string pathname1)
                (princ-to-string pathname2)))
 

(defun subdirs-of (dirname &key recursive-p)
  (loop
      for pathname in (list-directory dirname)
      when (directory-p pathname)
      collect pathname
      into subdirs
      finally
        (return
          (if subdirs
            (if recursive-p
                (cons dirname
                      (loop for subdir in (sort subdirs #'pathname-lessp)
                          collect (let ((sub-subdirs
                                         (subdirs-of subdir
                                                     :recursive-p recursive-p)))
                                    (if sub-subdirs
                                        (list subdir sub-subdirs)
                                      subdir))))
              (sort subdirs #'pathname-lessp))
            (sort subdirs #'pathname-lessp)))))

(defvar *logs-root* "")
(defun recursive-dirs (&optional (base-dir *logs-root*))
  (let (pathnames)
    (flet ((push-dir (dir)
             (push dir pathnames)))
      (walk-directory base-dir #'push-dir
                      :directories t
                      :test #'directory-p)
      (sort pathnames #'pathname-lessp)
      )))
;-http://www.koders.com/lisp/fidFB7070D914D164945DCC6128CF2A5307A8C34731.aspx?s=common#L1
(defmacro in-directory ((dir) &body body)
    `(progn 
            (#+sbcl sb-posix:chdir #+cmu unix:unix-chdir #+openmcl ccl:cwd
	     #+allegro excl:chdir #+lispworks hcl:change-directory ,dir)
	         ,@body))
(defun launch-background-program (directory program &key (args nil))
  "Launch a program in a specified directory - not all shell interfaces
   or OS's support this"
  #+(and allegro (not mswindows))
  (multiple-value-bind (in out pid)
      (excl:run-shell-command (concat-separated-strings " " (list program) args)
			      :wait nil
			      :directory directory)
    (declare (ignore in out))
    pid)
  #+(and sbcl unix)
  (in-directory (directory)
    (sb-ext:run-program program args :wait nil))
  #+cmu 
  (in-directory (directory)
      (ext:run-program program args :wait nil))
  #+openmcl
  (in-directory (directory)
    (ccl:run-program program args :wait nil))
  #+lispworks
  (funcall #'sys::call-system
	 (format nil "~a~{ '~a'~} &" program args)
	 :current-directory directory
	 :wait nil)
  ) 
(defun kill-background-program (process-handle)
  #+(and allegro (not mswindows))
  (progn (excl.osi:kill process-handle 9)
	 (system:reap-os-subprocess :pid process-handle))
  #+(and sbcl unix)
  (sb-ext:process-kill process-handle 9)
  #+openmcl
  (ccl:signal-external-process process-handle 9) )
;-
#+sbcl (defun cd (path) (sb-posix:chdir path)) 
(defun ls (&optional d) 
 (break2lines
  (if (stringp d) (run-ext "ls" d)
    (run-ext "ls"))))
;==start=
;(defun head (l) (subseq l 0 4))
;(defun head (l &optional (n 4)) (subseq l 0 n))
(defun head (l &optional (n 4)) (subseq l 0 (min n (len l))))
;(defun tail (l) (last l 4))
;(defun tail (l &optional (n 4)) (last l n))
(defun tail (l &optional (n 4)) (last l (min n (len l))))
(defun last3 (l) (last l 3)) 
;==> /Users/bobak/Documents/downloads/lang/lsp/ai/ut/mfkb/km/run/two/ut/util-mb.cl <==
;utils collected/written by m.bobak
;sb- specific now, might do it w/o; &/or if want acl specific subsys like agraph, just load km/triple
;(require :sb-posix)
;-gen utils now
(defvar *dbg-ut* nil)
;from ch.cl
(defun numbers (n) ;clips/py has a more general one
  (if (eq n 1) (list 1)  ;can use phrNum from tsp.cl this time
    (append (numbers (- n 1)) (list n))))
;from csv.lsp
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (fboundp 'parse-number)
    (defun parse-number (s)
      (with-standard-io-syntax ()
        (let* ((*read-eval* nil)
               (*read-default-float-format* 'double-float)
               (n (read-from-string s)))
          (if (numberp n) n))))))  
;tsp.cl
(defun remove-nils (l) (remove-if #'null l)) 
(defun nth+ (ns ls)
  "nth from a list vs a single int"
  (remove-nils ;assume nils only at end
   (mapcar #'(lambda (n) (nth n ls)) ns)))
;
(defun nop (a) a)
;
;pass in keys, so can send: :test #'string-equal, &generlize beyond str2seq
(defun split-str2by (str by) ;can generalize, think there is something already
  "str by char get cons"
  (when (position by str)
    (let ((p (position by str)))
      (cons (subseq str 0 p) 
            (subseq str (+ p 1)))))) 
(defun last-str2by (str by) 
  "str from char on"
  (let ((p (position by str)))
    (when p (subseq str p ))))
(defun first-str2by-end (str by) 
  "str till char"
  (let ((p (positions by str)))
    (when p (subseq str 0 (1+ (last_lv p))))))
(defun between-str2by (str by by2) 
  "between first by and last by2"
  (first-str2by-end (last-str2by str by) by2))
(defun last-str2by-end (str by) 
  (let ((p (positions by str)))
    (when p (subseq str (last_lv p)))))
;or for now   ;oh could mv let up
#+ignore
(defun split-strs2at (strs by) ;mv let up
  (when (position by strs :test #'string-equal)
    (let ((p (position by strs :test #'string-equal)))
      (cons (apply #'str-cat (subseq strs 0 p)) 
            (apply #'str-cat (subseq strs (+ p 1))))))) 
(defun split-strs2at (strs by) 
  (let ((p (position by strs :test #'string-equal)))
    (when p (cons (subseq strs 0 p) (subseq strs (+ p 1))))))
;(defun split-at (seq by) 
;  (let ((p (position by seq :test #'equal)))
;    (when p (list (subseq seq 0 p) (subseq seq (+ p 1))))))
#+ignore-w-km
(defun split-at (seq by) 
  (let ((p (position by seq :test #'equal)))
    (when p (values (list (subseq seq 0 p) (subseq seq (+ p 1))) p))))
(defun split-at_p (p seq) 
  (when (< 0 p (len seq))
    (list (subseq seq 0 p) (subseq seq (+ p 1)))))
(defun split-nth (p seq) 
  (when (< 0 p (len seq))
    (list (subseq seq 0 p) (nth p seq) (subseq seq (+ p 1)))))
;from lx2.cl
(defun cdr- (cns)     
  (if (full cns) (cdr cns) ""))
(defun split-second (line &optional (splt #\:))     
   (cdr- (split-str2by line splt)))
;(split-str2by "C0030705:Patients" #\:) ;("C0030705" . "Patients") 
;yes, utl-.lsp's csv-parse-string
;started as oba parsing routines, but breaking out utils
;==> ab.cl <== ;Start using s-query so the xml can change
;     (:|obs.common.beans.AnnotationBean| (:|score| "122") 
;   (:|concept| (:|localConceptID| "CSP/C0006826") (:|preferredName| "cancer")
;    (:|synonyms| (:|string| "neoplasm/cancer")) (:|isTopLevel| "false")
;    (:|localOntologyID| "CSP")
;    (:|localSemanticTypeIDs| (:|string| "T191") (:|string| "T000")))
;   ((:|context| :|class| "obs.common.beans.MappingContextBean")
;    (:|contextName| "MAPPING") (:|isDirect| "false")
;    (:|mappedConceptID| "CST/C0006826") (:|mappingType| "inter-cui")))
;    -write some spath like code to find elts by tag, basically an assoc/but not dotted
;    I really do NOT like list acessors
;    ==I'd really like auto-mapping to CLOS obj/ins, but it is so embedded, I'll still look.
(defun secondt (tv pr)
  (when (and (listp pr) (eq (first pr) tv)) (second pr)))
(defun second-t (tv pr &optional (dflt nil))
  (if (and (listp pr) (eq (first pr) tv)) (second pr)
    dflt))
(defun rest-t (tv pr &optional (dflt nil))
  (if (and (listp pr) (eq (first pr) tv)) (rest pr)
    dflt))
;=====utl.lsp
(defun fulll (l)
  (and (listp l) (> (length l) 0)))
(defun first-lv (lv)
  (if (fulll lv) (first lv) lv))
(defun rest-lv (lv)
  (if (fulll lv) (rest lv) lv))
(defun second-lv (lv) (if (fulll lv) (second lv) lv))
(defun third-lv (lv) (if (fulll lv) (third lv) lv))
(defun fourth-lv (lv) (if (fulll lv) (fourth lv) lv)) 
(defun nth-lv (n lv)
  (if (fulll lv) (if (>= n (len lv)) (progn (format t "nth-lv:~a ~a" n lv) (last-lv lv))
		    (nth n lv)) 
      lv))
(defun last-lv (lv)
  (if (fulll lv) (last lv) lv))
(defun last_lv (lv) ;so not a list
  (first-lv (last-lv lv)))
(defun car_lv (lv) (when (consp lv) (car lv))) 
(defun car-lv (lv) (if (consp lv) (car lv) lv))
(defun car_eq (l v) (when (consp l) (when (eq (car l) v) l))) 
(defun cdr-lv (lv) (if (consp lv) (cdr lv) lv))
;
;(defun assoc-v (k a)  (let ((v (assoc k a))) (when (consp v) (cdr v))))
;(defun assoc-v (k a)  (let ((v (assoc k a :test #'equal))) (when (consp v) (cdr v))))
(defun assoc-v (k a)  (let ((v (assoc k a :test #'equal))) (if (consp v) (cdr v) v))) ;or assoc_v
;mk more gen vers of:
(defun mapcar- (f l) (when (fulll l) (mapcar f l)))
(defun mapcar_ (f l) (if (fulll l) (mapcar f l)
		       (funcall f l)))
(defun mapcar_2 (f l l2) (if (and (fulll l) (fulll l2)) (mapcar f l l2)
		       (funcall f l l2)))
;
(defun list+ (ml)
    (if (listp ml) ml (list ml)))
;
(defun flat1onlys (l)
  "get rid of all (((a)) till just (a)"
    (if (and (listp l) (eq (len l) 1) (listp (first-lv l))) (flat1onlys (first-lv l))
          l))
(defun first_lv (lv)
  (if (fulll lv) (first (flat1onlys lv)) lv))
;
(defun nn (n) (if (numberp n) n 0))
(defun nn> (&rest args) (apply #'> (mapcar #'nn args)))
(defun first-nn (lv) (nn (first-lv lv)))
(defun full (a)
  "clips leftover not needed in lsp"
  (if (stringp a) (> (length a) 0)
          (not (null a))))
(defun nul (a)
  (not (full a)))
;=====
(defun str-trim (s)
  (string-trim '(#\Space #\Tab #\Newline) s))
(defun intern-trim (s)  ;consider:  (intern (string-upcase ))
  (intern (str-trim s)))
(defun str_trim (s)
  (if (stringp s) (str-trim s) s))

(defun safe-trim (s)
      (string-trim '( #\( #\) #\tab #\newline #\space #\; #\\) s)) 
 
;;; with apologies to christophe rhodes ...
(defun split (string &optional max (ws '(#\Space #\Tab)))
  (flet ((is-ws (char) (find char ws)))
    (nreverse
     (let ((list nil) (start 0) (words 0) end)
       (loop
        (when (and max (>= words (1- max)))
          (return (cons (subseq string start) list)))
        (setf end (position-if #'is-ws string :start start))
        (push (subseq string start end) list)
        (incf words)
        (unless end (return list))
        (setf start (1+ end)))))))
 
(defun explode- (string &optional (delimiter #\Space))
  (let ((pos (position delimiter string)))
    (if (null pos)
        (list string)
        (cons (subseq string 0 pos)
              (explode- (subseq string (1+ pos))
                       delimiter)))))
(defun explode-2 (string &optional (delimiter #\Space) (offset nil))
  (let ((pos (position delimiter string)))
    (if (or (null pos) (eq pos 0)) (list string)
	(let ((npos (if (integerp offset) (+ pos offset) pos)))
	  (cons (subseq string 0 npos)
		(explode-2 (subseq string (1+ npos))
			  delimiter offset))))))
(defun str-cat2 (a b)
    (format nil "~a~a" a b))
(defun str-cat (&rest args)
    (reduce #'str-cat2 args))
;-langband:
(defun strcat (&rest args)
    (apply #'concatenate 'string args))

;when you need spaces between
(defun str-cat_2 (a b)
      (format nil "~a ~a" a b))
(defun str-cat+ (&rest args)
      (reduce #'str-cat_2 args))
;(defun implode-l (l)
;  (apply #'strcat (mapcar #'to-str l)))
(defun implode-l (l &optional (insert-spaces t))
  (let* ((f (if insert-spaces #'to-str+ #'to-str))
	 (s (apply #'strcat (mapcar f l))))
    (if insert-spaces (str-trim s) s)
  ))
(defun implode_l (l &optional (insert-spaces t))
  (string-downcase (implode-l l insert-spaces)))

(defun sym-cat (&rest args)
  ;(intern (str-cat @args))
   (intern (reduce #'str-cat2 args)))

(defun break2lines (s) 
  (explode- s #\Newline))
(defun break-by-bar (s) 
  (explode- s #\|))
;defun split-by (d s) (explode- s :delimeter d)
(defun split-by (d s) 
  (explode- s d))

(defun split-by-slash (s) 
  (explode- s #\/))
(defun split-by_ (s) 
  (explode- s #\_))

(defun rm-commas (s)
  (remove #\, s))
(defun trim-commas (s)
  (string-trim '(#\, #\Space #\Tab #\Newline #\NO-BREAK_SPACE) s))
(defun rm-space (s)
  (remove #\NO-BREAK_SPACE (remove #\space s)))

(defun rm-ws-parens (s)
  ;(when (full s) (remove-if #'(lambda (x) (member x "( )")) s))
  (when (stringp 
	  (if (listp s) (first s) s)) (string-trim '(#\Space #\Tab #\Newline #\( #\)) s)))
;had list, but can't trim everything, or can't break it
;  -there are times if 1char long you want to keep it, even make it a char-

(defun rm-star (s) (remove #\* s)) 
(defun rm-parens (s) (remove #\( (remove #\) s))) 
(defun no* (str) (csv-trim2 '( #\*) str)) ;utl
(defun noparens (str) (csv-trim2 '( #\( #\)) str)) ;utl 

;either get something 1before the split-by, or just regular explode& filter/remove-if-not prefix ed (
;  then make sure paren-strings are combined/collected into one list (no sublists) of strs
;so need prefix-p  ;use string= ;fix
(defun prefix-p (pre s) ;prefixp is better below
  (and (stringp s) ;(sequence-p s) 
       (eq 0 (position pre s))))
(defun paren-str-p (s)
  (prefix-p #\( s))
;problem is the explode doesn't keep multi word paren-strs together
;could look for postfix-p too, &terms between
;Basically for every input string only want str between the parens, or paren-on
(defun paren_on (s) 
  "only substrs w/parens"
  (remove-if-not #'paren-str-p (explode- s) ;s
  ))  ;(paren_on "a (b) (c)") ;("(b)" "(c)") 
;  probably easier to just go back one
;  or just add on where needed
(defun set-prefix (pre s)
  "prefix if a string"
  (if (stringp s) (str-cat pre s)
    s))
(defun preparen (s) 
  "balance out paren-str"
  (str_trim
   (set-prefix #\( s)
  ))

(defun paren-on (s)
  "ret from paren on in the str"
    (let ((l (split-by #\( s)))
          ;when (listp l) (preparen (rest l)) ;s
          (when (listp l) (preparen (second l)) 
          ;(when (listp l) (rm-ws-parens (rest l)) 
	          )))
;not reall a rm-
(defun rm_comma (s)
      (substitute #\Space #\, s))
(defun rm-comma (s)
      (substitute #\_ #\, s))
(defun rm-colon (s)
      (substitute #\_ #\: s))
(defun rm-dash (s)
      (substitute #\Space #\- s))
(defun rm-bslash (s)
      (substitute #\Space #\/ s))

(defun underscore (s)
  (substitute #\_ #\Space s))
;(defun underscore_ (s) (string-downcase (underscore s)))
(defun underscore_ (s-) 
  (let ((s (if (symbolp s-) (symbol-name s-) 
	     s-)))
    (string-downcase (underscore s))))
(defun under_ (s) (string-downcase (underscore s)))
(defun str-cat_ (a) (under_ (str-cat a))) ;try
(defun str-cat_l (l) (str-cat_ (implode-l l)))
(defun has-space-p (s)
  (find #\Space s))

(defun rm-pd (s) "rm post -dashed like -pos" (butlast (explode- (rm-dash s))))
(defun under-pd (s) "under_ of all but -pos" (str-cat_l (rm-pd s))) 
(defun under-pd-l (l) (str-cat_l (mapcar- #'rm-pd l))) ;could overload above

(defun rm-underscore (s)
  (substitute #\Space #\_ s))

(defun hyphenate (s)
  (substitute #\- #\Space s))

(defun under2hyphen (s)
  (substitute #\- #\_ s))
(defun under2sp (s)
  (substitute #\Space #\_ s))
(defun hyphen2under (s)
  (substitute #\_ #\- s))

(defun replace-commas (s)
  (substitute #\; #\, s))

(defun slash2hyphen (s)
  (substitute #\- #\\ s))

(defun all2hyphen (s cl)
  "all in changeList2hypens"
 (if (not (fulll cl)) s
  (all2hyphen
     (substitute #\- (first cl) s)
 	(rest cl))))

(defun plusify-str (s)
  (substitute #\+ #\Space s))

(defun no-retlines (s)
  (substitute #\Space #\Newline s))

;(defun len (l) (when (listp l) (length l)))
;(defun len (l) (when (or (listp l) (stringp l)) (length l)))
;(defun len (l) (if (or (listp l) (stringp l)) (length l)
;		 (when (arrayp l) (first (array-dimensions l)))))
(defun len (l) (typecase l 
                 (list  (length l))
                 (string  (length l))
                 (symbol  (length (symbol-name l)))
                 (array  (first (array-dimensions l)))))
(defun nnlen (l) 
  "0 vs nil, if can't get length"
  (nn (len l)))
(defun len= (l n) (when (listp l) (eq (length l) n)))
(defun tree-size (tree) ;http://tehran.lain.pl/stuff/diff-sexp.lisp
  "Computes the number of atoms contained in TREE."
  (if (atom tree) 
      1 
      (reduce #'+ tree :key #'tree-size))) 
;------
(defun lexemep (s) (or (stringp s) (symbolp s)))
(defun lens1 (s) (if (lexemep s) 1 (len s)))
;------
(defun lens (l)
  (mapcar #'len l))
;------
(defun substr (txt from to)
  "subseq for str w/test"
  (when  (>= (length txt) to from 0) (subseq txt from to)))

(defun substr- (txt from to)
  ;(substr txt (1- from) (1- to)) ;should be this
  (when (full txt)
    (substr txt (1- from) to)
  ))
;==was tree-depth but already in km
(defun tree_depth (tree)
   (cond ((atom tree) 0)
         (t (1+ (reduce #'max (mapcar #'tree-depth tree))))))

(defun list+2depth (a d)
  (if (<= d 0) a
    (list+2depth (list a) (1- d))))
(defun list2depth (a wd) ;only adds to wd, could flat-1/flat1 the other way, &/or start w/flat1onlys
  (let ((pd (tree_depth a)))
    (list+2depth a (- wd pd))))
(defun list++ (ml &optional (assure-depth 1)) ;starts out just like list+ but can modify
   ;(if (listp ml) ml (list ml))
   (let ((td (tree_depth ml)))
	 (if (< td assure-depth) (list+2depth ml (- assure-depth td))
	   ml)))
(defun list+2 (ml) (list++ ml 2))
 
(defun tree-stats (tree)
 ;(format t "~&width:~a depth:~a size:~a" (len tree) (tree-depth tree) (tree-size tree))
  (let ((wid (len tree)) (dep (tree-depth tree)) (siz (tree-size tree)))
   (format t "~&width:~a depth:~a size:~a" wid dep siz)
   (if (> siz (* wid dep)) (format t " > ~a "  (* wid dep)))
  ))
;=====
(defun str_cat (&rest args)  
  (apply #'concatenate 'simple-string args))
;rest already above
;(defun str-cat2 (a b)  
;  (format nil "~a~a" a b))
;(defun str-cat (&rest args)  
;  (reduce #'str-cat2 args))
;=====
;-in utl.lsp now
;CL-USER(7): (read-delimited-list #\, (make-string-input-stream "1,2,3"))
;(1)  ;see o13a for more
;--
;(with-input-from-string (s "6") (read s))  -> 6
;--
;(parse-integer "word" :junk-allowed t)
;--
(defun alpha-start (str)
  "does it start w/an alpha"
  (alpha-char-p (char str 0))
  )
(defun has-alpha-p (str)
  (alpha-start str);for now
  )
;--
;it might be better to alter explode str, to have numbers go to numbers; as easier to look@separated?
;find-if #'alpha-char-p  ;but don't know if it fits in there?
;--
(defun num-str (numstr)
  (let ((n (parse-integer numstr :junk-allowed t)))
    (if n n numstr))
  )
(defun numstr (numstr)
  "get num from a str"
  (if (equal numstr "") 0 
    (if (or (numberp numstr) (alpha-start numstr) ) numstr
      (read-from-string (remove #\: numstr)) ;(num-str numstr) 
      )
    ))
;--
(defun has-date-p (s) (len-gt (positions #\: s) 1))
(defun numstr- (s) 
  (if (has-date-p s) (prs-univ-time- s)  (numstr s)))
;--
;I'd like to be able to rm a : at the end of what is read..
;--
  ;whish I could READ-FROM-STRING w/a format ;look@ make-string-input-stream
;;;just USE:   READ-DELIMITED-LIST ...!!!but needs a stream, still
;-garnet
;;; Read and return the numbers at the end of a line on stream bitstream
;;;
(defun get-nums (bitstream) ;garnet/opal/mac.lisp
       (do ((ch (peek-char t bitstream) (peek-char t bitstream)))
	          ((digit-char-p ch))
		            (read-char bitstream))
            (parse-integer (read-line bitstream)))
;-langband: ;already above
;(defun strcat (&rest args)
;    (apply #'concatenate 'string args))
;----- csv.lisp simplification
(defun csv-trim (whitespace string)
    "Trim the string argument from the whitespace."
      (let ((clean (string-trim whitespace string)))
	    (if (zerop (length clean)) nil clean))
      )
(defun csv-trim2 (whitespace string)
 (let ((c2 (csv-trim whitespace string)))
   (if c2 c2 " ")))
(defvar *punct* "[]();.")
(defvar *punct2* "[]();., ")
(defun punct-p (str)
 (position (aref str 0) *punct*))
(defun punctp (str)
  (and (eq (length str) 1) (not (alphanumericp (aref str 0)))))
(defvar +whitespace+ " ")
;(defvar +whitespace+ " \0")  ;mmtx tagger puts out \0's ;but it's dangerous

(defun csv-parse-string (string &key (separator #\,) (whitespace +whitespace+))
  "Parse a string, returning a vector of strings."
  (loop :with num = (count separator string :test #'char=)
    :with res = (make-array (1+ num))
    :for ii :from 0 :to num
    :for beg = 0 :then (1+ end) 
    :for end = (or (position separator string :test #'char= :start beg)
                   (length string))
    :do (setf (aref res ii)
              (when (> end beg) ; otherwise NIL = missing
                (csv-trim whitespace (subseq string beg end))))
    :finally (return res)))  
;---(read-from-string " 1 3 5" t nil :start 2)
;==new==
;defun csv-parse-str (string &key (separator #\t) (whitespace +whitespace+))
(defun csv-parse-str (string &key (separator #\Tab) (whitespace +whitespace+))
  "Parse a string, returning a vector of strings."
  (loop :with num = (count separator string :test #'char=)
    :with res = (make-array (1+ num))
    :for ii :from 0 :to num
    :for beg = 0 :then (1+ end) 
    :for end = (or (position separator string :test #'char= :start beg)
                   (length string))
    :do (setf (aref res ii)
              (when (> end beg) ; otherwise NIL = missing
                (csv-trim whitespace (subseq string beg end))))
    :finally (return res)))  
;==
;my try
(defun read-from-csv-str (str &key (start 0) (separator #\,))
    (if (>= start (length str)) nil
      (let ((pn (position separator str)))
        (if (not pn) nil
          (cons (read-from-string str t nil :start start)
	        (read-from-csv-str str :start (+ start pn))))))
    )
;--from clhp: 
(defmacro if-bind ((&rest bindings) test if else)
    "An IF wrapped in a LET"
      `(let (,@bindings) (if ,test ,if ,else))
      )
(defmacro explode-string (string)
    "Converts a string to a list of chars, this is an aux function used
  for string processing.
  ex: (EXPLODE-STRING (\"Hello\") --> (#\H #\e #\l #\l #\o)"
		        `(concatenate 'list ,string)
			)
(defun implode-string (char-list)
    "Converts EXPLODEd CHAR-LIST into string, used as an aux function
  for string processing.
  ex: (IMPLODE-STRING '(#\H #\e #\l #\l #\o)) --> \"Hello\"
      (IMPLODE-STRING (EXPLODE-STRING \"Hello\")) --> \"Hello\""
        (coerce char-list 'string)   ;maybe allow other types?
	)
(defun implode- (cl)
  "kludge"
  (if (listp cl) (implode-string cl) 
    (numstr cl)  ;(eval cl) ;need to get it to turn "1"->1
    )
  )
;; ex:  ;this is the explode that I'm used to, but explode-str w/opt #\Space does the same
;; (mapcar #'implode-string
;;      (split-char-list #\Space
;;                       (explode-string "In God We Trust" ))) -->
;; ("In" "God" "We" "Trust")  
(defun split-char-list (char char-list)
  "Splits a char-list (EXPLODEd string) on CHAR."
  (labels
      ((split
        (char-list split-list)
        (if-bind ((position (position char char-list)))
           (null position)
              (remove nil (nreverse (cons char-list split-list)))
            (split (nthcdr (1+ position) char-list)
                   (cons (butlast char-list (- (length char-list) position))
                         split-list)))))
    (split char-list nil))
  ) 
(defun explode-str (str &key (sep #\,))
  "explode-str-by"
   (mapcar #'implode-  ;#'implode-string
        (split-char-list sep (explode-string str)))
   )
; (explode-str "1,2,3") -->("1" "2" "3")
 ;i'd like to change implode- to eval
(defun explode-bar (str) (explode-str str :sep #\|))
;--from clhttp headers:
;define-macro char-position (char string  &optional (start 0) end from-end)
(defmacro char-position (char string  &optional (start 0) end from-end)
  "Returns the position of CHAR in string from START upto END.
when FROM-END is non-null, the string is scanned backward."
  (case from-end
    ((t)
     `(let ((ch ,char))
        (with-fast-array-references ((string ,string string))
          (loop for idx fixnum downfrom (1- (the fixnum ,(or end '(length string)))) to (the fixnum ,start)
                when (eql ch (aref string idx))
                  return idx
                finally (return nil)))))
    ((nil)
     `(let ((ch ,char))
        (with-fast-array-references ((string ,string string))
          (loop for idx fixnum upfrom (the fixnum ,start) below (the fixnum ,(or end '(length string)))
                when (eql ch (aref string idx))
                  return idx
                finally (return nil)))))
    (t (if end
           `(char-position-2-case ,char ,string ,start ,end ,from-end)
           `(let ((string ,string))
              (char-position-2-case ,char ,string ,start (length string) ,from-end)))))
  ) 
(defconstant +white-space_chars+ '(#\Space #\Tab)
	     )
(defmacro with-fast-array-references (bindings &body body)
  "Declares the arrays in bindings (var value &optional type)
as type and sets speed to 3 with safety 0 within its scope."
  (loop for (var val type) in bindings
        collect `(,var ,val) into n-bindings
        when type
          collect `(type ,type ,var) into type-dcls
        finally (return `(let ,n-bindings
                          (declare (optimize (speed 3) (safety 0)) . ,type-dcls)
                          ,@body)))
  ) 
(declaim (inline white-space-charp) ;was white-space-char-p
	 ) 
;define white-space-char-p (char)
(defun white-space-charp (char)
    (member char +white-space_chars+)
    )
;define fast-position-if-not (predicate string start end from-end)
(defun fast-position-if-not (predicate string start end from-end)
  (declare (fixnum start end))
  (with-fast-array-references ((string string string))
    (if from-end
        (loop for idx fixnum downfrom (1- end) to start
              unless (funcall predicate (aref string idx))
                return idx
              finally (return nil))
        (loop for idx fixnum upfrom start below end
              unless (funcall predicate (aref string idx))
                return idx
              finally (return nil))))
  ) 
;define-macro position-if-not* (predicate string &key (start 0) (end nil end-supplied-p) from-end)
(defmacro position-if-not* (predicate string &key (start 0) (end nil end-supplied-p) from-end)
  (if end-supplied-p
      `(fast-position-if-not ,predicate ,string ,start ,end ,from-end)
      `(let ((string ,string))
         (fast-position-if-not ,predicate string ,start (length string) ,from-end)))
  ) 
(defun parse-comma-separated-header (string &optional (start 0) (end (length string)) (header-value-parser #'subseq))
  "Applies header-value-parser to each comma separated header-value in STRING.
If HEADER-VALUE-PARSER return multiple values, they are concatenated together into the returned list."
  (flet ((first-non-blank (start end)
           (position-if-not* #'white-space-charp string :start start :end end)))
    (declare (inline first-non-blank))
    (loop for s = (first-non-blank start end) then (first-non-blank (1+ idx) end)
          while s
          for idx fixnum = (or (char-position #\, string s end) end)
          for last = (position-if-not* #'white-space-charp string :start s :end idx :from-end t)
          when last
            nconc (multiple-value-list (funcall header-value-parser string s (1+ (the fixnum last))))
          while (< idx end)))
  ) 
;(parse-comma-separated-header "1,2,3") --> ("1" "2" "3") 
;==================================================== 
;(defcustom *csv-separator* character #\,
;  "The separator in the CSV file, normally the comma.")
(defvar *csv-separator*  #\,)

(defun csv-print-vector (vec &optional (out *standard-output*))
  "Print a vector as a comma-separated line."
  (declare (type vector vec) (stream out))
  (loop :with len = (length vec) :for val :across vec :and ii :from 1
        :when val :do (write val :stream out :escape nil)
        :unless (= ii len) :do (write-char *csv-separator* out))
  (terpri out))

;(defcustom *csv-whitespace* (or null string) +whitespace+
;  "The string of characters to trim from the values.")
;(defcustom *csv-progress* integer 1000
;  "*How often the progress report should be made")
(defvar *csv-whitespace* +whitespace+)
(defvar *csv-progress*  1000)
#+IGNORE ;already above
(defun csv-parse-string (string &key
                         ((:separator *csv-separator*) *csv-separator*)
                         ((:whitespace *csv-whitespace*) *csv-whitespace*))
  "Parse a string, returning a vector of strings."
  (loop :with num = (count *csv-separator* string :test #'char=)
    :with res = (make-array (1+ num))
    :for ii :from 0 :to num
    :for beg = 0 :then (1+ end)
    :for end = (or (position *csv-separator* string :test #'char= :start beg)
                   (length string))
    :do (setf (aref res ii)
              (when (> end beg) ; otherwise NIL = missing
                (csv-trim *csv-whitespace* (subseq string beg end))))
    :finally (return res)))


;;;###autoload
;(defun csv-read-file (inf)
; "Read comma-separated values into a list of vectors."
;====================================================
(defun parse-csv- (str)
 ;(mapcar #'numstr (parse-comma-separated-header str))
 ;(mapcar #'numstr (csv-parse-str str))
 (csv-parse-str str)
 )
;maybe give an alt arg of how many/which to numstr?
; or better just fix numstr to avoid anything starting w/a alphabetic-char
;====================================================fix/skip
;;;; -*-Mode:LISP; Package:LISP; Base:10; Syntax:ISLISP -*-
;;;; Date:	2003/03/19
;;;; Title:	csv.lsp
;;;; Author:	C. Jullien

;;;
;;; Read CSV (Comma Separated Value) File Format
;;;

;(defglobal *separator* #\;)
;(defglobal *separator* #\t)
(defvar *separator* #\Tab)

;defun read-csv (file &optional (cfnc #'convert-to-list))
(defun read-csv (file &optional (cfnc #'prs-csv-nums))
   ;; read a CVS into a list of lines.
   ;with-open-input-file (si file)
   (with-open-file (si file) 
	 (let ((tree nil))
	      (do ((line (read-line si () 'eof) (read-line si () 'eof)))
		   ((eq line 'eof))
		   (push (funcall cfnc line) tree)) ;broken?
	      (nreverse tree))))

;(defun format-fresh-line (strm) ;when couldn't find
;  (format strm "~&"))
;; ~&, CLTL p.397, CLtL2 p. 596
(defun format-fresh-line (stream ;colon-modifier atsign-modifier
                          &optional (count 1))
  (declare (ignore colon-modifier atsign-modifier))
  (if (null count) (setq count 1))
  (when (plusp count)
    (fresh-line stream)
    (dotimes (i (1- count)) (terpri stream))))

(defun write-csv (file tree)
   ;; write a CVS from a list of lines.
   ;with-open-output-file (so file)
   (with-open-file (so file)
	 (dolist (line tree)
		 (dolist (item line)
			 (format so "~a~c" (or item "") *separator*))
		 (format-fresh-line so))))

(defun subseq- (seq a b)
  (if (< a b) (subseq seq a b)
              (subseq seq b a)))
  ;check this!
;defun convert-to-list (line) ;still finding problem w/this
(defun convert-to-list (line &optional (sep *separator*))
   ;; convert  a  single  line with CSV into a list.  Empty items are
   ;; set to nil.
   (let ((len (length line))
	 (last- 0)
	 (res nil))
	(do ((i 0 (1+ i)))
	     ((= i len))
	     (when (char= (char line i) sep) ;*separator*
		   (if (= i last-)
		       (push nil res)
		       (push 
			   (subseq- line last- (- i last-))
			   res))
		   (setf last- (1+ i))))
	(nreverse res))) 
;====================================================
;"int/ffi/cl-objc/src/utils.lisp" 104 lines --33%--     35,8-15       72%
;defun split-string (string separator &key (test-fn #'char-equal))
(defun split-string (string &optional (separator " ") &key (test-fn #'char-equal)) ;soCanReplace other
  "Split STRING containing items separated by SEPARATOR into a list."
  (let ((ret)
        (part))
    (loop
       for el across string
       for test = (funcall test-fn el separator)
       do
       (cond
         ((not test) (setf part (if part
                                    (format nil "~a~c" part el)
                                    (format nil "~c" el))))
         (test (setf ret (append ret (list part))) (setf part nil))))
    (when part (setf ret (append ret (list part))))
    ret))
;
(defun search-str (a b) (search a b :test #'string-equal))
;
;should have save version &send fnc in
(defun len-gt (l n) (> (len l) n)) ;make sure safe to access
(defun len-lt (l n) (< (len l) n)) ;make sure safe to access
;
(defun len> (a b) (> (len a) (len b)))
(defun len< (a b) (< (len a) (len b)))
(defun sort-len-lol (lol) (sort lol #'len>))
;(defun max-len (&rest lol) (sort lol #'len>))
(defun max-len (&rest lol) (first-lv (sort-len-lol lol)))
;
(defun simple-replace-string (old new string) ;some replacements could be get it stuck/?
  "Replace OLD with NEW into STRING."
  (loop
     with changed = t
     while changed
     do (setf string
              (let ((match (search old string :test #'string-equal ;equal
				   )))
                (if match
                    (prog1
                        (concatenate 'string (subseq string 0 match) new (subseq string (+ match (length old))))
                      (setf changed t))
                    (prog1
                        string
                      (setf changed nil)))))
       finally (return string)))
;--- 
(defun simple-replace-strings (alst str)
  "alst(old . new) "
 (if (not (full alst)) str
  (let* ((aon1 (first alst))
	 (old (car aon1))
	 (new (cdr aon1))
	 (nstr (replace-substrings str old new)))
    (when *dbg-ut* (format t "~%~a" nstr))
    (simple-replace-strings (rest-lv alst) nstr))))
;(simple-replace-strings '((":" . ": ") ("\"{" . "{") ("}\"" . "}")) str)
;defun replace-substrings (string substring replacement) ;replace-string
    	 
;--- 
(defun rm-str (rs s)
  (simple-replace-string rs "" s)) 

(defun rm-strs (rsl s)
  ;reduce #'rm-str
  (if (and (full rsl) (> (len rsl) 1)) (rm-str (first rsl) (rm-strs (rest rsl) s))
    (rm-str (first rsl) s) ))
;--- 
(defun no-dbl-spaces (s)
  (simple-replace-string "  " " " s)) 
;--- 
;--- find- .. fnc also of possible use
;(defun simple-hyphenate-str (s str)
;     (simple-replace-string s (hyphenate s) str)) 
;     already worked though
(defun simple-hyphenate-str (s str &optional (dc t))
  "hyphen word w/in str, ret whole str"
  (let ((s- (if dc (string-downcase s) s)))
     (simple-replace-string s- (hyphenate s-) str))) 
;
(defun simple-hyphenated-str (s str &optional (dc t))
  "hyphen word w/in str, ret it& rest of str"
  (let* ((s- (if dc (string-downcase s) s))
	 (sh (hyphenate s-))
	 (nstr (simple-replace-string s- sh str))) 
   ;(cons sh (rm-str sh nstr))
    (list nstr sh (rm-str sh nstr))
    ))
(defun simple-hyphenated-strs (sl str)
  "take strings &hyphenate all of them in str, w/o glomming"
    (mapcar #'(lambda (s) (simple-hyphenated-str s str)) (sort sl #'len>))) ;finish, use new ret
(defun find-merge-hyphens (lwl str)
  "hyphenate all longestwords, w/removal so no glomming of hyphens"
  (let* ((shs (simple-hyphenated-strs (list+ lwl) str))
	 (allp (find "" shs :test #'equal :key #'cdr))
	;(hp (reduce #'union (mapcar #'hyphens-at shs)))
	 ) ;finish
    (if allp (progn (format t "~&hyphenated whole str:~a" allp) allp) ;check
      (let ((hp (reduce #'union (mapcar #'hyphens-at (mapcar #'first shs))))) ;still want2use cdr..?
	   (replace-chars-at str #\- hp)))
    ))
;could rm-dups after downcase but don't have to ;positions to find spaces&cmp w/- positions
; don't just remove, as it's ok if >1 hyphen comes from 1src, which is a reason to sort-len
;;len>, sort was good, but we said we'd try to anchor from the right, so sort by position/hit in str*
;; but it's position+len-of-match str, to see which is most right;I should try it by hand 1st:
;; ;; well if 2go all the way, it's the longer of the 2
;
;;w/simple-hyphenated-strs if crd=="" then done, could look@others b4 a reduce
(defun simple-hyphenate-strs (sl str)
  "take strings &hyphenate all of them in same str" ;used longest 1st, not helpful here?
    (mapcar #'(lambda (s) (simple-hyphenate-str s str)) (sort sl #'len>)))
;--;above ret >1, while below only 1
(defun find+merge-hyphens (lwl str)
  (let ((hp (reduce #'union (mapcar #'hyphens-at (simple-hyphenate-strs lwl str)))))
    (replace-chars-at str #\- hp)))
;(find+merge-hyphens '("Illicit drug use" "drug use" "Drug use" "illicit drug") "Illicit drug use")
;"Illicit-drug-use"
;--- 
(defun replace-w-hyphens (sl txt) 
  (let ((hsl (mapcar #'hyphenate sl)))
    (mapcar #'(lambda (o n) (simple-replace-string o n txt)) sl hsl)))
    ;shouldn't it be reduced though/?
;(trace replace-w-hyphens) 
;--- 
;defun hyphens (s)
(defun hyphens-at (s)
  (positions #\- s))
(defun spaces-at (s)
  (positions #\Space s))
;--- 
(defun digit-prefixp (str)
 (digit-char-p (aref str 0)))

(defun trim-whitesp (str)
 (csv-trim +whitespace+  str))

(defun trim-punct (str)
 (csv-trim *punct*  str))
(defun trim-punct2 (str)
 (csv-trim *punct2*  str))

;try to trim everything
(defun not-ALPHANUMERICP (c)
  (not (ALPHANUMERICP c)))
;use: string-downcase 

(defun alpha_char-p (c)
  (or (eq c #\_) (alpha-char-p c)))
(defun alph_char-only (s)
  (remove-if-not #'alpha_char-p s))

(defun digit_prefixp (str)
 ;(digit-char-p (aref (csv-trim +whitespace+  str) 0))
 (digit-char-p (aref (trim-whitesp str) 0))
 )
;(digit_prefixp "    123  ") ;1 

;==from mmtx parsing files originally
;(defgeneric positions (a b &key start sum test))
;--was otp.cl now tp.cl 
(defgeneric positions (a b &key start sum test))

(defmethod positions (c  b &key (start 0) (sum 0) (test #'equal))
  (let* ((p (position c b :start start :test test))
         (s (if p (subseq b (+ p 1)) nil))
         (np (if p (+ sum p) 0)))
    (if p (cons np (positions c s :sum (1+ np) :test test)) nil)))

(defmethod positions ((c string) b &key (start 0) (sum 0) (test #'prefixp))
  (let* ((p (position c b :start start :test test))
         (s (if p (subseq b (+ p 1)) ""))
         (np (if p (+ sum p) 0)))
    (if p (cons np (positions c s :sum (1+ np) :test test)) nil)))
 

(defmethod positions ((c character) b &key (start 0) (sum 0) (test #'equal))
  (let* ((p (position c b :start start :test test))
         (s (if p (subseq b (+ p 1)) ""))
         (np (if p (+ sum p) 0)))
    (if p (cons np (positions c s :sum (1+ np) :test test)) nil)))
 
(defmethod positions ((pl list) b &key (start 0) (sum 0) (test #'equal))
  (mapcar #'(lambda (x) (position x b :start start :test test)) pl))
 
(defun tst-posit () (positions #\- "Women with a positive urine beta HCG-pregnancy-test"))
 
;--
(defun positions- (a  b)
  (remove-nils (positions a b)))

(defun mapappend (fun &rest lists)
    "A non-destructive mapcan."
      (reduce #'append (apply #'mapcar fun lists)))
;-
(defun positionsl (pl l)
  "find each pl in l, &give all#s"
  (sort (mapappend #'(lambda (p) (positions- p l)) pl) #'<)) 

;better than prefix-p above
;#+sbcl ;or #-acl
#+sbcl ;already in acl
(defun prefixp (pre str)
 (and (stringp pre) (stringp str) (> (length str) (length pre)) ;(equal (subseq str 0 (length pre)) pre))
         (string= pre str :end2 (length pre))))
;-
(defun prefix (pre str &optional (trim t)) 
  "if not a prefix already, then add it"
 (let ((ret (if (prefixp pre str) str 
	      (str-cat pre str))))
   (if trim (str-trim ret)
     ret)))

;-
;tried as method, but messed up cui-p, so give as alt:
(defgeneric s-prefixp (pre str))
(defmethod s-prefixp (pre (s Symbol))
  (s-prefixp pre (symbol-name s)))
(defmethod s-prefixp (pre (str String))
; (and (> (length str) (length pre)) (equal (subseq str 0 (length pre)) pre))
 (and (stringp pre) ;(stringp str) 
      (> (length str) (length pre)) ;(equal (subseq str 0 (length pre)) pre))
         (string= pre str :end2 (length pre))
  ))
(defun sprefixp (pre s) ;will handle non-strs incl symbols  ;downcase all of these too?
  (prefixp (to_str pre) (to_str s))) ;just did this for prefixp though
(defun prefix_p (pre str)
  (or (equal pre str) (prefixp pre str)))
(defun suffixp (post str) ;postfixp
 (and (stringp post) (stringp str) (> (length str) (length post)) 
         (string= post str :start2 (- (length str) (length post)))
  ))
; (suffixp "Tagging" ">>>>> Tagging") ; T
 
 ;from sr-init.lisp
(defun list-lines (filename)
  "file2list of lines"
 (when (file-exists-p filename)
  (with-open-file (stream filename :direction :input)
    (loop for line = (read-line stream nil)
        while line
        collect line))))
(defun list_lines (filename)  ;see if can get unicode
  ;with-open-file (stream filename :direction :input :external-format :iso-8859-)
  (with-open-file (stream filename :direction :input :element-type '(unsigned-byte 8))
    (loop for line = (read-line stream nil)
        while line
        collect line)))
;-
(defun read-file-to-string (fn)
  (let ((l (list-lines fn)))
    (when (full l)  (reduce #'str-cat l))))
;-
(defun save-lines (l filename)
 (when (fulll l)
  (with-open-file (stream filename :direction :output)
    (mapcar #'(lambda (x) (write-line x stream)) l))))
 
(defun map-lines (filename linefnc)
  (with-open-file (stream filename :direction :input)
    (loop for line = (read-line stream nil)
        while line
        collect (funcall linefnc line))))

(defun map-lines2 (filename linefnc)
 (with-open-file (stream2 (str-cat filename ".out") :direction :output)
  (with-open-file (stream filename :direction :input)
    (loop for line = (read-line stream nil)
        while line
        collect (funcall linefnc line stream2)))))
 
;-from rsc2.cl /kick of parsers
#+sbcl ;or #-acl
(defun run-shell-command (txt) (asdf:run-shell-command txt))
(defun rsc (txt) (asdf:run-shell-command txt))
;(trace rsc)
 
;-from tsp.cl  /access csv parses
;(defun nth+ (ns ls) ;already above
;  (remove-nils ;assume nils only at end
;   (mapcar #'(lambda (n) (nth n ls)) ns)))
;
(defun flatten- (x) ;km has another flatten
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))
;
(defun flat-1 (l) ;check on ;was flat1 but use below
  (reduce #'nconc (copy-list l))) ;safter, 

;ben/ocelot2obo.lisp
(defun flatten1 (vals)
    (apply #'append (mapcar (lambda (x) (if (listp x) x (list x))) vals))) 

;(defun flat1 (vals) (flatten1 vals))
(defun flat1 (vals) 
  (if (full vals) (flatten1 vals)
    (progn (warn "flat1 ~a" vals) vals)))

(defun shuffle (x y)
  "Interleave two lists."   ; LMH
  (cond ((null x) y)
        ((null y) x)
        (t (list* (car x) (car y)
                  (shuffle (cdr x) (cdr y)))))) 
;
;defun r-find (s tree)
(defun r_find (s tree)
  (if (fulll tree) 
    (let ((f1 (find s tree :test #'member)))
      (if f1 f1 (mapcar- #'(lambda (tr) (r-find s tr)) tree))))) 
(defun find-member (s l) (when (fulll l) (find s l :test #'member)))
;(defun r-find-all (s tree)
;  (if (fulll tree) 
;    (let ((f1 (find s tree :test #'member)))
;      (if f1 (cons f1 (mapcar- #'(lambda (tr) (r-find-all s tr)) tree))
;	(mapcar- #'(lambda (tr) (r-find-all s tr)) tree))))) 
;(defun r-find-all (s tree)
;  (mapcar- #'(lambda (tr) (cons (find-member s tree) (r-find-all s tr))) tree))
	   
;web-access/umls-mysql070507.lisp
(defun rfind (s tree)
  (cond ((null tree) nil)
        ((atom tree)
         (if (equal s tree) tree nil))
        ((listp tree)
         (if (find s tree :test #'equal)
             (assoc s (list tree) :test #'equal)
           (or (rfind s (car tree))
               (if (cdr tree) (rfind s (cdr tree)))))))) 
;also in web-access/bat-utilities111406.lisp 
(defun rfind-all (key alst)
  (let* ((tree (copy-tree alst))
         (temp-result (rfind key tree))
         (results
          (loop while (and tree temp-result)
              collect temp-result into list-of-sub-alsts
              do (setq tree
                   (let ((test-tree (remove temp-result tree
                                            :test #'equal)))
                            (if (equal test-tree tree)
                                nil test-tree))
                     temp-result (rfind key tree))
              finally (return list-of-sub-alsts))))
    results)) 
(defun tree-depth (tree)
  (cond ((atom tree) 0)
	(t (1+ (reduce #'max (mapcar #'tree-depth tree)))))) 
(defun r-find (s tree)
  (cond ((null tree) nil)
        ((atom tree)
         (if (equal s tree) tree nil))
        ((listp tree)
         (if (find s tree :test #'prefix_p)
             (assoc s (list tree) :test #'prefix_p)
           (or (rfind s (first tree))
               (if (rest tree) (rfind s (rest tree)))))))) 
;code/msc/msc/lsp/onlisp.lisp (more2look@here)
(defun rfind-if (fn tree)
  (if (atom tree)
    (and (funcall fn tree) tree)
    (or (rfind-if fn (car tree))
	(if (cdr tree) (rfind-if fn (cdr tree)))))) 
;might also want a set of things to search &use member/etc
;also could try a prefix-p that deals w/symbol(-names)
; prefix_p only slight ext2 equal, needs symb-prefix-p/overloading
;web-access/CLOS-class-browser.lisp also has .. ;eg.
;defmethod CLOS-class-grapher ((url url:http-form) stream)
(defun symbol-name-sort (symbol-list)
    (sort (copy-list symbol-list) #'string-lessp :key #'symbol-name))

(defun symbol_name (s) (string-downcase (symbol-name s)))

;;; from Pierre R. Mai
(defun replace-substrings (string substring replacement) ;replace-string
  (declare (optimize (speed 3))
           (type simple-string string substring replacement))
  (assert (> (length substring) 0) (substring)
    "Substring ~A must be of length ~D > 0" 
    substring (length substring))
  (with-output-to-string (stream)
    (loop with substring-length = (length substring)
        for index = 0 then (+ match-index substring-length)
        for match-index = (search substring string :start2 index)
        do   
          (write-string string stream :start index :end match-index)
          (when match-index
            (write-string replacement stream)) 
        while match-index)))
 
(defun subst-at (str ch at)
  (setf (aref str at) ch)
  str)

(defun replace-chars-at (str ch atl)
  "atlist mask of where to subst ch"
   (mapc #'(lambda (a) (subst-at str ch a)) atl)
   str)
 
;look at find*comparator ben/new/choice-params-ff3-new.lisp
;ben's files
(defun safer-read-from-string (s)
  (read-from-string (string-trim '( #\( #\) #\newline #\space #\; #\\) s)))
 
;(string s) ;does the same thing
(defun to-str (s)  ;could have symbolp then symbol-name, but this ok
  (if (stringp s) s
    (format nil "~a" s)))
(defun to_str (s)
  (string-downcase (to-str s)))

(defun to-str+ (s)  
  "to str with a space"
  (str-cat s " "))
 ;(if (stringp s) s
 ;  (format nil "~a " s))

(defun safer-string (s)
 ;(if s (safer-read-from-string (to-str s)) "")
 ;(if s (string-trim '( #\( #\) #\newline #\space #\; ) (to-str s)) " ") ;was "" still stripped so:
  (if s (csv-trim2 '( #\( #\) #\newline #\space #\; #\\) (to-str s)) " ")
  )
;tsp.cl
;(defun remove-nils (l) (remove-if #'null l)) ;already above

;from tle.cl
(defun quote-str (s) (format nil "\"~a\"" s))
(defun quote-str2 (s) (if (stringp s) (format nil "\"~a\"" s) s))
(defun quote-str3 (s) ;new ;quote if it has space, ok?
  (if (and (stringp s) (position #\Space s)) (format nil "\"~a\"" s) 
    (quote-char s)))
;(defun quote-char (c) (format nil "\"#\\~a\"" c))
(defun quote-char (c) 
  (if  (characterp c) 
    (format nil "#\\~a" c) ;(format nil "\"#\\~a\"" c)
    (if (and (stringp c) (< (length c) 4) (prefixp "#" c))
      (quote-char (aref c (1- (length c)))) 
      c)))
(defun clean-punct (s)  ;should check len=1 or..
  (if (not (punctp s)) (quote-alpha s)
    (quote-char (aref s 0)) ;case (aref s 0)
    ))
(defgeneric quote- (s))
(defmethod quote- ((s STRING))
   (format nil "\"~a\"" s))
(defmethod quote- ((s CHARACTER))
   (format nil "#\\~a" s))
(defmethod quote- ((s LIST))
  (mapcar #'quote- s))
      

(defun quote-alpha (s) 
  (if (alpha-start s) (quote-str s) s))
(defun ins-alpha (s) 
  (if (alpha-start s) (format nil "*~a" (string-downcase s)) 
    (quote-str s)))
(defun ins-alpha2 (s) 
 (if (eq (len s) 1) (clean-punct s)
  (if (alpha-start s) (format nil "*~a" (string-downcase s)) 
    (quote-str2 (num-str s)))))

;many more intersting fncs in the km utils/etc, &in the other gigs of opensrc code I have around.
(defun split-strbybar (l)  (split-string l #\|)) 
#+sbcl ;or #-acl
(defun run-2 (cmndstr arglst)
 (let* ((sb-impl::*default-external-format* :utf-8)
       (process (run-program cmndstr arglst
                             :output :stream
                             :wait nil)))
  (prog1 (read-line (process-output process))
    (process-wait process)
    (process-close process))))

#+sbcl ;or #-acl
(defun run_2 (cmndstr arglst)
 (let* (;(sb-impl::*default-external-format* :utf-8)
       (process (run-program cmndstr arglst
                             :output :stream
                             :wait nil)))
  (prog1 (read-line (process-output process))
    (process-wait process)
    (process-close process))))

 
;/ros-0.4/core/experimental/roslisp/rosutils.lisp
#+sbcl ;or #-acl
(defun run-external (cmd &rest args)
          (let ((str (make-string-output-stream)))
            (sb-ext:run-program cmd args :search t :output str)
            (let ((s (get-output-stream-string str)))
              (delete #\Newline s))))  

;in my sbclrc now
;#+sbcl ;or #-acl
#-sbcl ;for sbcl in vicl2
(defun run-ext (cmd &rest args)
          (let ((str (make-string-output-stream)))
            #+sbcl (sb-ext:run-program cmd args :search t :output str)
	    ;#-sbcl (run-shell-command cmd args) ;finish ;rsc
	    #-sbcl (run-shell-command  (format nil "echo \"~a ~a\"|cat>.tmp" cmd args)) ;finish
            (let ((s #+sbcl (get-output-stream-string str)
		     #-sbcl (read-file-to-string ".tmp")
		     ))
              s ;(delete #\Newline s)
	      )))
#+IGNORE
(defun run-xml (cmd &rest args) 
  "run-ext&prs ret xml2s-expr"
  ;STRING-OUTPUT-STREAM {100AAC9DC1}> is not a character input stream
  (let ((str (make-string-output-stream))) ;just tried
      (sb-ext:run-program cmd args :search t :output str)
              (s-xml:start-parse-xml str)))

 
(defun eval-str2 (s) ;1st in nlp.cl
    (eval-str (str-cat "'" s)))

;(defun clean4echo (s) (rm-strs '("*") s))
;(defun clean4echo (s) (rm-strs '("(" "*" ")" "_nil" "nil_") s))
;(defun clean4echo (s) (rm-strs '("(" "*" ")" "_nil" "nil_" ">" "<") s))
(defun clean4echo (s) (rm-strs '("(" "*" ")" "_nil" "nil_" ">" "<" ":" ",") s))
(defun run_ext (cmd &rest args)
          (let ((str (make-string-output-stream)))
            #+sbcl (sb-ext:run-program (clean4echo cmd) args :search t :output str)
	    #-sbcl (run-shell-command (clean4echo cmd)) ;finish
            (let ((s (get-output-stream-string str)))
              s ;(delete #\Newline s)
	      )))  
 
;;;ros-0.4.3/core/experimental/roslisp/utils/utils.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Symbols

(defun build-symbol-name (&rest args)
  "build-symbol-name S1 ... Sn.  Each Si is a string or symbol.  Concatenate them into a single long string (which can then be turned into a symbol using intern or find-symbol."
  (apply #'concatenate 'string (mapcar (lambda (x) (if (symbolp x) (symbol-name x) x)) args)))

(defun intern-compound-symbol (&rest args)
  "intern-compound-symbol S1 ... Sn.  Interns the result of build-symbol-name applied to the S."
  (intern (apply #'build-symbol-name args))) 


(defmacro with-struct ((name . fields) s &body body)
  "with-struct (CONC-NAME . FIELDS) S &rest BODY 
 Example:
 with-struct (foo- bar baz) s ...
 is equivalent to
 let ((bar (foo-bar s)) (baz (foo-baz s)))...  
 Note that despite the name, this is not like with-accessors or with-slots in that setf-ing bar above would not change the value of the corresponding slot in s."

  (let ((gs (gensym)))
    `(let ((,gs ,s))
       (let ,(mapcar #'(lambda (f)
                         `(,f (,(intern-compound-symbol name f) ,gs)))
              fields)
         ,@body))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Strings
 
(defun tokens (string &key (start 0) (separators (list #\space #\return #\linefeed #\tab)))
  (if (= start (length string))
      '()
      (let ((p (position-if #'(lambda (char) (find char separators)) string :start start)))
        (if p
            (if (= p start)
                (tokens string :start (1+ start) :separators separators)
                (cons (subseq string start p)
                      (tokens string :start (1+ p) :separators separators)))
            (list (subseq string start))))))
;http://www.mail-archive.com/stumpwm-devel@nongnu.org/msg01195.html
;(in-package :stumpwm-user)
#+sbcl ;or #-acl
(require :sb-posix)
;;; XXX This is only a workaround for SBCLs with a unreliable
;;; run-program implementation (every version at least until
;;; 1.0.21). If someone makes run-program race-free, this should be
;;; removed! - Julian Stecklina (Oct 23th, 2008)
#+sbcl
(progn
  (defun exec-and-collect-output (name args env)
    "Runs the command NAME with ARGS as parameters and return everything
the command has printed on stdout as string."
    (flet ((to-simple-strings (string-list)
             (mapcar (lambda (x)
                       (coerce x 'simple-string))
                     string-list)))
      (let ((simplified-args (to-simple-strings (cons name args)))
            (simplified-env (to-simple-strings env))
            (progname (sb-impl::native-namestring name))
            (devnull (sb-posix:open "/dev/null" sb-posix:o-rdwr)))
        (multiple-value-bind (pipe-read pipe-write)
            (sb-posix:pipe)
          (unwind-protect
               (let ((child 
                      ;; Any nicer way to do this?
                      (sb-sys:without-gcing 
                        (sb-impl::with-c-strvec (c-argv simplified-args)
                          (sb-impl::with-c-strvec (c-env simplified-env)
                            (sb-impl::spawn  progname c-argv devnull 
                                             pipe-write ; stdout
                                             devnull 1 c-env 
                                             nil ; PTY
                                             1 ; wait? (seems to do nothing)
                                             ))))))
                 (when (= child -1)
                   (error "Starting ~A failed." name))
                 ;; We need to close this end of the pipe to get EOF when the child is done.
                 (sb-posix:close pipe-write)
                 (setq pipe-write nil)
                 (with-output-to-string (out)
                   ;; XXX Could probably be optimized. But shouldn't
                   ;; make a difference for our use case.
                   (loop 
                      with in-stream = (sb-sys:make-fd-stream pipe-read :buffering :none)
                      for char = (read-char in-stream nil nil)
                      while char
                      do (write-char char out))
                   ;; The child is now finished. Call waitpid to avoid
                   ;; creating zombies.
                   (handler-case
                       (sb-posix:waitpid child 0)
                     (sb-posix:syscall-error ()
                       ;; If we get a syscall-error, RUN-PROGRAM's
                       ;; SIGCHLD handler probably retired our child
                       ;; already. So we are fine here to ignore this.
                       nil))))
            ;; Cleanup
            (sb-posix:close pipe-read)
            (when pipe-write
              (sb-posix:close pipe-write))
            (sb-posix:close devnull))))))

 ;(defun stumpwm::run-prog-collect-output (prog &rest args)
 ;  "run a command and read its output."
 ;  #+sbcl (exec-and-collect-output prog args (cons 
 ;	(stumpwm::screen-display-string (current-screen))
 ;    (remove-if (lambda (str)
 ;		 (string= "DISPLAY=" str :end2 (min 8 (length str)))) 
 ;(sb-ext:posix-environ)))))
 (defun run2 (name args)
   "compare w/run-2 etc"
  #+sbcl (exec-and-collect-output name args nil))
  #-sbcl (run-shell-command name args)
  ) 
 
(defun eval-str (s) 
  (eval (read-from-string s))) 

(defun string-list->keyword-vector (string-list)
    (loop for string
          in (copy-list string-list)
          collect (intern (string-downcase string) :keyword)
          into keywords
          finally (return (apply #'vector keywords))))

(defun record->list (delimiter seq) ;explode could also do this
  (let ((end (length seq)))
    (loop for left = 0 then (+ right 1)
          for right = (or (position delimiter seq :start left) end)
          if (< left right)
          collect (subseq seq left right)
          until (eq right end))))

;=look at using a bit more from: choice-params-ff3e-new.cl
;; (find-elt-in-string "Age greater than 18 years" *english-arithmetic-comparators-vector*)
;; (find-elt-in-string "pulmonary hypertension due to congenital heart disease" *semantic-connectors-vector*)
(defun find-elt-in-string (string vector)
  (loop with downcase-string = (string-downcase string)
      with res = nil
      for v across vector
      thereis (when (setq res
                      (let ((s (string-downcase v)))
                        (or (and (find #\space s)
                                 (search s downcase-string
                                         :test #'string-equal)
                                 s)
                            (and (not (find #\space s))
                                 (find s (record->list #\space downcase-string)
                                       :test #'string-equal)
                                 s))))
                (return res))))

(defun find-elt-n-string (s v)
  "if key-vect in str, ret (substr str)"
  (let ((ps (find-elt-in-string s v)))
    (when ps (list ps s))))
 
(defun find-substring-in-string (string list)
  (loop with downcase-string = (string-downcase string)
      with tokens = (record->list #\space downcase-string)
      for elt in list
      thereis (when (loop for token in tokens
                        thereis (when (string-equal token elt)
                                  (return elt)))
                (return elt))))
 
(defun seq-cmp (seq1 seq2 cmp) ;this is wrong, use len
; (and (eq (type-of seq1) (type-of seq2))
;      (or (stringp seq1) (listp seq1))
;      (funcall cmp (length seq1) (length seq2)))
       (funcall cmp (nnlen seq1) (nnlen seq2)))
;broke out to allow other comparisions
(defun seq-longerp (seq1 seq2)
  (seq-cmp seq1 seq2 #'>))
(defun seq-shorterp (seq1 seq2)
  (seq-cmp seq1 seq2 #'<))
 
;; (direct-siblings 'Quantitatively_Restricted_Term)
(defun direct-siblings (class)
  (when (ignore-errors (find-class class))
    (let* ((superclasses (direct-superclasses class))
           (siblings (loop for superclass in superclasses
                       append (direct-subclasses superclass))))
      siblings)))
 
(defun list->choice-list (list-of-elements &key (index-p nil)
                                                (splice-keywords t)
                                                (space-char-replacement #\_)
                                                (max-strlen 110)
                                                )
  "elements may be pair-lists or individual strings - processed accordingly"
   (declare (optimize (speed 3))
            (type cons list-of-elements))
   (if (choice-list-p list-of-elements)
       list-of-elements
   (let (display-string)
     (loop for element in list-of-elements
         for i from 1
         do (let* ((string1 (typecase element
                              (symbol (string element))
                              (string element)
                              (list (first element))))
                   (string1-len (length string1)))
              (setq display-string
                (format nil "~a~a~a"
                        (if (not index-p) "" i)
                        (if (not index-p)
                            ""
                          (if (> i 9) "&nbsp;" "&nbsp;&nbsp;&nbsp;"))
                        (subseq string1
                                0
                                (min max-strlen (length string1 ))))))
         collect (list display-string
                       :value
                       (or (intern
                            (if (listp element)
                                (second element)
                              (if splice-keywords
                                  (substitute
                                   space-char-replacement #\space
                                   (string-trim '( #\space )
                                                display-string))
                                display-string))
                            :keyword)))
         into 3-tuples
         finally (return (remove-duplicates 3-tuples :test #'equalp))))))
 
(defun has-more-tokens-p (string1 string2)
  (when (and (stringp string1) (stringp string2))
    (cond ((and (not (search " " string1))
                (not (search " " string2)))
           nil)
          ((not (search " " string1)) nil)
          ((not (search " " string2)) t)
          (t (let ((token-list1 (record->list #\space string1))
                   (token-list2 (record->list #\space string2)))
               (> (length token-list1) (length token-list2)))))))
 
(defgeneric siblings-choice-list (s))
;; (siblings-choice-list 'Quantitatively_Restricted_Term)
(defmethod siblings-choice-list ((s symbol))
  (when (ignore-errors (find-class s))
    (list->choice-list (direct-siblings s))))
 
(defun choice-list-p (x)
  (and (listp x)
       (or (and (listp (car x))
                (eq (second (car x)) :value)
                (keywordp (third x)))
           (consp (car x)))))

(defun string->upcase-keyword (s)
  (intern (substitute #\_ #\space (string-upcase s)) :keyword))
 
;-
(defun getnum (str)
  (if (and (stringp str) (len-gt str 0)) ;new
    (numstr (trim-punct str))
    (when (numberp str) str)))

(defun numericp (sn) (numberp (getnum sn)))

(defun get_nums (line)
      (collect-if #'numberp (mapcar #'getnum (remove-if-not #'full (explode- line)))))
(defun first-num (line)
    (first-lv (get_nums line)))
(defun second-num (line)
    (second-lv (get_nums line)))


(defun subseqs (lst snl &key (start nil) (end nil))
  "start# end=t subseq  maps over offset lst of positions"
 (let ((snl1 (if start (cons start snl) snl))
       (snl2 (if end (append snl (list (len lst)))  snl)))
    (when *dbg-ut* (format nil "~&~a ~a" snl1 snl2))
    (mapcar #'(lambda (a b) (subseq lst a b)) snl1 (rest snl2))))
 
;;==from lexA.cl 2fncs:
;defgeneric wordize (fn) ;would have to differentiate str&path, which i can do later
(defun wordize (fn) ;or list of lines now
  (let* ((l (if (listp fn) fn (list-lines fn)))
         (lc (mapcar #'string-downcase l)) ;added
         (le (mapcar #'explode- lc))
         (re (reduce #'union le))
         (ret (mapcar #'(lambda (x) (string-trim " ()./[]:;,\\\"" x)) re)) ;added
        )
    ;now reduce by getting intersection (basically doing a sortu)  ;also rm all nonalphnum
(remove-duplicates ret :test #'string=)))

(defun phraseize (fn)
  "keep as phrases this time"
  (let* ((l (list-lines fn))
         (lc (mapcar #'string-downcase l)) ;added
         (ret (mapcar #'(lambda (x) (string-trim " ()./[]:;,\\\"" x)) lc)) ;added
         )
(remove-duplicates ret :test #'string=)))
 
(defun mklist (l) (list+ l))
;; (insert-into-seq 'new '((1 2) 'a ((5 6))) 1)
;; => ((1 2) NEW ((5 6)))
;; (insert-into-seq 'new '((1 2) 'a ((5 6))) 1 :replace-existing t)
;; (insert-into-seq 'new '((1 2) 'a ((5 6))) 1 :replace-existing t :splicep t)
;; (insert-into-seq 'new '((1 2) 'a ((5 6))) 1 :replace-existing nil)
;; => ((1 2) 'A NEW ((5 6)))
(defun insert-into-seq (new seq pos &key replace-existing splicep)
  (loop for item in (mklist seq)
      for i from 0 below (length seq)
      if (= i pos)
      append (if replace-existing
                 (if (and splicep (listp new))
                     new
                   (list new))
               (list new item))
      else
      collect item))
 

(defun insert-char (char string pos &key (excluded-char char))
  (when (stringp string)
    (let* ((string-len (length string))
           (preceding-char (if (= pos 0) nil (char string (1- pos))))
           (next-char (if (= pos (1- string-len)) nil (char string (1+ pos)))))
      (cond ((= (length string) 0) (string char))
            ((< pos 0)
             (coerce (cons char (coerce string 'list)) 'string))
            ((and (> pos 0)
                  (< pos string-len)
                  excluded-char
                  (eq (char string (1- pos)) excluded-char))
             string)
            ((>= pos string-len)
             (coerce (append
                      (coerce string 'list)
                      (list char))
                     'string))
            (t (coerce (insert-into-seq char (coerce string 'list) pos)
                       'string))))))
 
;try ;given month how many days to add, to get get days through year; assume same yr2start
(defparameter +month2days+ '(("Jan" .  0) ("Feb" . 31) ("Mar" . 59) 
			     ("Apr" . 90) ("May" . 120) ("Jun" . 151)
			     ("Jul" . 181) ("Aug" . 212) ("Sep" . 243)
			     ("Oct" . 273) ("Nov" . 304) ("Dec" . 334)))
(defun file-date (file)
  (subseq (explode- (run-ext "ls" "-l" file)) 8 11))

(defun month2day (mstr)
  (let ((a (assoc mstr +month2days+ :test #'equal)))
    (if (full a) (cdr a) 0)))

(defun file-day+time (file)
  (let* ((mdt-l (file-date file))
	 (m (first mdt-l))
	 (d (parse-integer (second mdt-l)))
	 (tme (third mdt-l)))
    (cons (+ (month2day m) d) tme)))

(defun time> (t1 t2)
  ;finish
  )
(defun day+time> (tp1 tp2)
  (let* ((td1 (first tp1))
	 (td2 (first tp2)))
    (if (= td1 td2) (time> (cdr tp1) (cdr tp2))
      (> td1 td2))))

(defun collect-if (predicate sequence) 
  (when (full sequence)
    (remove-if-not predicate sequence)))
(defun collect-if-not (predicate sequence) 
  (when (full sequence)
    (remove-if predicate sequence)))

(defun collect-if-eq (to seq &key (key #'nop)) 
  (collect-if #'(lambda (it) (equal (funcall key it) to)) seq))

(defun collect (fnc lst) ;== remove-nils on mapcar
    (collect-if #'nop (mapcar fnc lst))) 
;(collect #'(lambda (n) (when (evenp n) n)) '(1 2 3 4 5 6 7 8)) ;(2 4 6 8)

;from ruleed-tables1.lisp
(defun break-into-subseqs (list &key (subseq-len 2))
  (when (and (listp list)
             (numberp subseq-len))
    (let ((listlen (length list)))
      (cond ((< listlen subseq-len) nil)
            ((= listlen subseq-len) list)
            (t (cons (subseq list 0 subseq-len)
                     (break-into-subseqs
                      (subseq list subseq-len)
                      :subseq-len subseq-len)))))))
;USER(3): (break-into-subseqs '(0 0 1 1 0 0 1 1 0 1)) ;((0 0) (1 1) (0 0) (1 1) 0 1) ;last wrong?
;USER(4): (break-into-subseqs '(0 0 1 1 0 0 1 1 0 1) :subseq-len 3) ;((0 0 1) (1 0 0) (1 1 0))
 
;http://stackoverflow.com/questions/610680/linearly-recursive-list-difference-function-in-common-lisp
(defun list-diff (L1 L2)
  (cond
    ((null L1) nil)
    ((null (member (first L1) L2)) (cons (first L1) (list-diff (rest L1) L2)))
    (t (list-diff (rest L1) L2))
  )
);j8a see if this makes sense /useful for diff of nlp parses

(defun list-difference (a b) ;was in in/ssheet3
    (mapcar #'(lambda (e) (remove e b :count 1)) a)) 

;http://groups.google.com/group/comp.lang.lisp/browse_thread/thread/2d71b553b62e20b5?hl=en#
(defun mapconcat (fun list sep) 
   (when list 
      (let ((~sep (with-output-to-string (*standard-output*) 
                     (map nil (lambda (ch) (princ (if (char= #\~ ch) "~~" ch))) sep)))) 
       (format nil (format nil "~~A~~{~A~~A~~}" ~sep) 
               (funcall fun (first list)) 
               (mapcar fun (rest list))))))  
;(mapconcat 'identity '("one" "two" "three") "-") 
;--> "one-two-three" 
;(mapconcat (lambda (x) (concatenate 'string "[" x "]")) '("one" "two" "three") "~") 
;--> "[one]~[two]~[three]"
;-from: "s-acc.cl"
;#+IGNORE 
(progn ;redone elsewhere
(defun st (a b)
  "str w/in a str" ;make a str-intersection
  (search a b :key #'char-upcase))
(defun str_intersect (a b)
  (when (and a b)
    (cons (st a b) (st b a))))
(defun explode_ (s)
  (mapcar #'string-downcase (explode- s)))
(defun str-intersect (a b) ;have an eq that upcases everything 1st
  (intersection (explode_ a) (explode_ b) :test #'equal)) ;ok
(defun str+intersect (a b)
  (cons (str-intersect a b) (str_intersect a b)))
)
(defun first-numstr (l) (eval (numstr (first l)))) 
;-
(defun first_num-in-str (ps)
     (if (numberp ps) ps
           (numstr (second (explode- ps)))))
(defun first2num-in-str (l) (first_num-in-str (first l))) 
;-
(defun prs-datetime (dts)
    (let* ((dt2 (explode- dts))
           (ds (first dt2))
           (ts (second dt2))
           (dl (split-at-2nums ds))
           (tl (split-at-2nums ts ":"))
           )
      (append dl tl)))
(defun univ-time (l)
  (apply #'encode-universal-time (reverse l)))
(defun prs-univ-time (dts)
  (univ-time (prs-datetime dts)))
(defun prs-univ-time- (dts &optional (off 3155702400))
  (let ((ut (prs-univ-time dts)))
    (if (numberp off) (- ut off) ut)))
;-
(defun countInAlst (n alst)
  "fraction w/n"
  (let ((ncnct (sum-l (mapcar #'(lambda (l) (count n l)) alst)))
	(d+a (sum-l (mapcar #'len alst)))
	(a (len alst)))
    (/ ncnct (- d+a a))))
;-
(defun str-eq2 (a b) (equal (string-downcase a) (string-downcase b))) ;used in sort/ec
;could apply equal mapcar .. &call w/&rest
;-
(defun ssort (l &optional (cmp #'string<))
    "safe sort of a list of symbols"
   ;(sort (copy-list l) #'string< :key #'symbol-name)
    (sort (copy-list l) cmp :key #'symbol-name)
    )
;;printout.cl 
(defgeneric prins (a &optional s))
(defmethod prins ((arg string) &optional (strm t))
  (princ arg strm))
(defmethod prins ((arg number) &optional (strm t))
  (princ arg strm))
(defmethod prins ((arg symbol) &optional (strm t))
  (princ (symbol-name arg) strm))
(defmethod prins ((arg cons) &optional (strm t))
  ;(format nil "~{~a, ~}" arg)
  (dolist (a arg) do (princ a strm)))
(defmethod prins (arg &optional (strm t))
  (print-object arg strm)) 

(defun printout (strm &rest args)
  (if (eq strm t) (setf strm nil))
  (dolist (a args) do
    (prins a strm)))
;-eof
(defun format-list (list) (format t "~{~a~^, ~}" list))
(defun frmt-l (list) (format t "~{~a~^, ~}" list))
;;junk:
(defun first-num- (line) ;skip
  "pull 1st # from str"
  (when (full line)
    (getnum (first (remove-if-not #'full (explode- line))))))
;
(defun sum-l (the-seq &key (start 0) (end (length the-seq))) 
  (reduce #'+ the-seq :start start :end end))

(defun sum (&rest args) (sum-l args))

(defun posative-p (n) (and (numberp n) (> n 0)))
 
(defun ave-l (the-seq &key (start 0) (end (length the-seq))) 
 ;(/ (reduce #'+ the-seq :start start :end end) (- end start))
 (let ((num (- end start))) ;now get nil if input not fulll
   (when (posative-p num)
     (/ (reduce #'+ the-seq :start start :end end) num))))

(defun ave (&rest args) (ave-l args))
;-added test
(defun get1+ (key l)  ;get-key 
  "generic get value after key"
  (let ((p (position key l :test #'equal)))
    (when (and (numberp p) (< p (1- (len l)))) (elt l (1+ p)))))
(defun get2+ (key l)
  (let ((p (position key l :test #'equal)))
    (when (numberp p) (elt l (+ p 2)))))
;-http://groups.google.com/group/comp.lang.lisp/browse_thread/thread/f127c58d9ae193f2/770eb1ec95f606a5?hl=en&lnk=raot#770eb1ec95f606a5
;(show-tree '(animal (pet (cat (siamese) (tabby))  (dog (chihuahua)))(farm (horse (2)) (cow (2)))))
(defun show-tree (node) 
   (labels ((show (node prefix) 
              (format t "~S~%" (car node)) 
              (let ((next-prefix (format nil "~A |   " prefix))) 
                (mapl #'(lambda (nodes) 
                          (format t "~A +-- " prefix) 
                          (show (car nodes) 
                                (if (null (cdr nodes)) 
                                  (format nil "~A     " prefix) 
                                  next-prefix))) 
                      (cdr node))))) 
     (terpri) 
     (show node "") 
     (values)))  
(defun alst2 (al)
  "alist into 2 lists" ;op of pairlis
  (loop for e in al
	collect (car e) into a
	collect (cdr e) into b
	finally (return (list a b))))

(defun max-l (l) (apply #'max l))
(defun min-l (l) (apply #'min l))
(defun max_l (l) (max-l (flat1onlys l)))
(defun min_l (l) (min-l (flat1onlys l)))
(defun max-fl (l) (max-l (flatten- l)))
(defun min-fl (l) (min-l (flatten- l)))
;-
(defun range- (start end)
    (when (and (numberp start) (numberp end))
          (loop for i from start to end collect i)))
(defun range_ (end &optional (start 0))
    (range- start (1- end))) 
(defun range_1 (end &optional (start 0))
  (range_ (1+ end) (1+ start)))
;-
(defun date- () (run-ext "date"))
;-
;http://groups.google.com/group/comp.lang.lisp/browse_thread/thread/917ec8f0a3fe30c7?hl=en# 
(defun print-subclasses (root &optional (indent 0)) 
  (dotimes (x indent) (princ "  ")) 
  (let ((class (typecase root 
                 (class root) 
                 (symbol (find-class root)) 
                 (t (class-of root))))) 
    (princ (class-name class)) 
    (terpri) 
    (dolist (sub 
	      #+sbcl (sb-mop:class-direct-subclasses class)
	      #-sbcl (class-direct-subclasses class)
	      ) ;#-sbcl (clos:class-direct-subclasses class) ;not4 ccl
      (print-subclasses sub (1+ indent))))) 
;-
(defun butlast- (s)
  (subseq s 0 (1- (len s))))

(defun butlast-n (s n)
  (subseq s 0 (- (len s) n)))

(defun butfirst-n (s n)
  "everything after1st n in a seq"
    (subseq s n (len s))) 

(defun rm-ext (s)
  "rm extention"
  (let ((n (position #\. s :from-end t)))
    (when n (subseq s 0 n))))
;----
;-ut  ;how much better than ut/ fncs?
(defun rfind_ (s tree) ;work on
  (cond ((null tree) nil)
        ((atom tree)
         (if (equal s tree) tree nil))
        ((listp tree)
         (if (find s tree :test #'equal)
             (member s tree :test #'equal) ;(assoc s (list tree) :test #'equal)
           (or (rfind_ s (car tree))
               (if (cdr tree) (rfind_ s (cdr tree))))))))

(defun rfind_all (key alst)
  (let* ((tree (copy-tree alst))
         (temp-result (rfind_ key tree))
         (results
          (loop while (and tree temp-result)
              collect temp-result into list-of-sub-alsts
              do (setq tree
                   (let ((test-tree (remove temp-result tree
                                            :test #'equal)))
                            (if (equal test-tree tree)
                                nil test-tree))
                     temp-result (rfind_ key tree))
              finally (return list-of-sub-alsts))))
    results))
;-now not for member, but for naming of consecutive id's e.g. 1a 1b..
(defvar *alph* 
'(#\a #\b #\c #\d #\e #\f #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z))
(defvar *alphs* 
'("a" "b" "c" "d" "e" "f" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"))
;-
(defun equals (&rest l) (apply #'equal (mapcar_ #'(lambda (x) (under_ (to-str x))) l))) 
;-
(defun assocs (key alst &optional (ef #'equals))
  "assoc that can handle a str key, ret value, when it exists"
  (let ((v (assoc key alst :test ef))) ;was #'equalp
    (when v (cdr v))))
(defun assocs+ (s alst) (list+ (assocs s alst)))
;-
(defun assoc-w (l v)
  "assoc lst w/value"
  (mapcar_ #'(lambda (le) (cons le v)) l))
;-
(defun dash-p (s) (search "-" s))
;-
(defgeneric s2num (s))
(defmethod s2num ((sym SYMBOL))
  (s2num (symbol-name sym)))
(defmethod s2num ((str String))
 ;(mapcar #'string-to-number (split-at-dash str))
 (if (not (dash-p str)) (string-to-number str)
   (let ((s (rm-dash str)))
     ;(list (first-num s) (second-num s) 'pp)
     (list (first-num s) (second-num s))
     ))
  )
;(defmethod s2num (s)
;  (when *dbg-ut* (format nil "~&warn:s2num:~a,~a" s (type-of s)))
;  (s2num (to-str s)))
(defmethod s2num ((n Number))
  ;n
  (list n)
  )
(defmethod s2num ((l List))
  (mapcar_ #'s2num  l))
;-
(defun rm-nil (l)
  (if (listp l) (remove 'nil l)
    l))
;-
(defun range-list-p (l) 
  (when (eq (len l) 2)  (rm-nil l)))
(defun range-list2range (l)
  (let ((rl (range-list-p l)))
    (if (not rl) l
      (if (eq (len rl) 1) rl ;if part of range nil, 
	(loop for i from (first rl) to (second rl) collect i)))))
;-
(defun s2nums (s)
 ;(range-list2range (s2num s))
  (mapcar_ #'range-list2range (s2num s))
  )
;-
(defgeneric split-at-dash (s))
(defmethod split-at-dash ((sym SYMBOL))
  (split-at-dash (symbol-name sym)))
(defmethod split-at-dash ((str STRING))
  (let ((r (split-at1 str "-")))
    (if r (range-list2range r)  ;check:want ret ranges expanded
      (list str) ;so can map over single or ranges
      )))
;defmethod split-at- ((str STRING))
(defun split-at- (str)
  (split-at1 str "-"))
(defun split-at-2 (str &optional (by "-")) 
  (let* ((fs (split-at1 str by)) 
         (one (first fs)) 
         (dd (second fs)) 
         (lt (split-at1 dd by))) 
    (cons one lt)))
;USER(15): (split-at-2 "2010-08-18") ("2010" "08" "18")
(defun split-at-2nums (str &optional (by "-")) 
  (s2num (split-at-2 str by)))
;moved rest to p2.lisp till it settles down
(defun nth-eq (n l e)
  (when (listp l) (eq (nth n l) e)))
(defun first-eq (l e)
  (when (listp l) (eq (first-lv l) e)))

(defun list2alst (l)
  "pairlis"
  (when l 
    (let ((ln (len l)))
      (if (and (>= ln 2) (evenp ln))
	(cons
	  (cons (first l) (second l))
	  (list2alst (rest (rest l))))
	(warn "no alst of")))))

(defun s-num-str (s) (when s (num-str s)))
(defun s-to-str (s) (when s (to-str s)))

(defun csv_parse-str (string &key (separator #\Tab) (whitespace +whitespace+))
  "Parse a string, returning a vector of strings."
  (loop :with num = (count separator string :test #'char=)
    :with res = '() ;(make-array (1+ num))
    :for ii :from 0 :to num
    :for beg = 0 :then (1+ end)
    :for end = (or (position separator string :test #'char= :start beg)
                   (length string))
        ;setf (aref res ii)
    :do (push ;ref res ii
              (when (> end beg) ; otherwise NIL = missing
                (csv-trim whitespace (subseq string beg end)))
              res
              )
    :finally (return (nreverse res))))

;(defun prs-csv (str) (csv_parse-str str  :separator #\Comma))
(defun prs-csv (str) (csv_parse-str str  :separator #\,))
(defun prs-csv-nums (str) 
  (mapcar- #'numstr- (prs-csv str)))
 
(defun under_f (str) ;better than cache version ;see m16_
  (let* ((wrds (string-to-words str))
         (iname (str-cat_l wrds)))
    iname))
 
(defgeneric s2num (s))
(defmethod s2num ((sym SYMBOL))
  (s2num (symbol-name sym)))
(defmethod s2num ((str String))
 ;(mapcar #'string-to-number (split-at-dash str))
 (if (not (dash-p str)) (string-to-number str)
   (let ((s (rm-dash str)))
     ;(list (first-num s) (second-num s) 'pp)
     (list (first-num s) (second-num s))
     ))
  )
;(defmethod s2num (s)
;  (when *dbg-ut* (format nil "~&warn:s2num:~a,~a" s (type-of s)))
;  (s2num (to-str s)))
(defmethod s2num ((n Number))
  ;n
  (list n)
  )
(defmethod s2num ((l List))
  (mapcar_ #'s2num  l))
 
(defun slot->defclass-slot (spec)
  (let ((name (first spec)))
    `(,name :initarg ,(as-keyword name) :accessor ,name))) 

(defun len-eq (l n) (and (listp l) (eq (len l) n)))
(defun len_gt (l n) (when (fulll l) (len-gt l n)))
(defun len_ge (l n) (when (fulll l) (len-gt l (1- n))))
(defun rm-nils (l) (collect-if #'full l))
(defun run-ext2s (s s2)
  "input1str&it breaks4you"
  (apply #'run-ext (append (tokens s) (list s2))))
 
(defun prefix-bar (s) (prefix-p #\| s)) 

;defun iso-time (&optional (time (get-universal-time)))
(defun iso-time- (&optional (time (get-universal-time)))
  "Returns the universal time TIME as a string in full ISO format."
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time time)
    ;format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d"
    (format nil "~4,'0d-~2,'0d-~2,'0d-~2,'0d-~2,'0d"
            year month date hour minute ;second
	    )))
;prs-univ-time does the opposite 

;;from: http://www.cs.ut.ee/~isotamm/PKeeled/AllarOja_LISP/LISP/HangMan.cl
;USER(1): (explode "abc") -> (|a| |b| |c|) ;w/small cleaning 
(defun goexplode (word cur last fword)
   (cond ((equal cur last)
          (append fword
                  (list (prog1 (intern (string (char (string word) (1- cur)))))) ))
         (t  (goexplode word (+ cur 1) last
                        (append fword
                                (list (prog1 (intern (string (char (string word) (1- cur))))))) ))))

(defun explode2s (word &key (upper t))
   (let ((lenword (length (string word))))
    (cond ((null word) nil)
          (t (goexplode (if upper (string-upcase word) word) 
                        1 lenword '())))))
 
;set-difference reorders so
;need set_diff that doesn't reorder, picked small collect only if not memeber of it
(defun set_diff (a b)
  "set-difference that doesn't unorder the list"
  (collect-if-not #'(lambda (e) (member e b)) a))
;explode2s str2 list of symbol/letters
(defun implode2s (l)
  "l of sym/letters->str"
  (implode-l l nil))

(defun positivep (n) (> n 0)) ;put in utils

(defun rm-if-member (m lol)
  (remove-if #'(lambda (l) (member m l)) lol)) 

(defun no-nils (l) (not (member nil l))) 
(defun any-t (l) (len-gt (rm-nil l) 0))
 
(defmacro while (test &body body)
  `(do ()
     ((not ,test))
     ,@body))

;from my tfec.cl work
(defun i-lt-n-p (i n)
  (if (numberp n) (< i n)
    t))
(defun apply-lines-n (filename linefnc &optional (n nil))
  "apply to at most n lines"
 (let ((tot 0))
  (with-open-file (stream filename :direction :input)
    (loop for line = (read-line stream nil)
        while (and line (i-lt-n-p tot n))
        do 
        (incf tot)
        (funcall linefnc line)))))
;want to either collect csv lines, or mk km ins w/o collecting, which is preferable for large n
(defun csv-bar (l) (csv_parse-str l :separator #\|))

 
;-redone, to give class.txt mkclskm
;(defun 2l2alst (l1 l2) (mapcar #'cons l1 l2)) ;(defun mkhl (h l) (2l2alst h l))
(defun mkhl (h l) 
  "alst of:csv header&list of values for a line"
  (rm-nil (mapcar #'(lambda (a b) (when b (cons a b))) h l)))

(defun mkalst-n (a b n) 
  "mk alst except for nth vals"
  (loop for ia in a 
        for ib in b 
        for count = 0 then (+ count 1)
        unless (= count n) collect (cons ia ib)))


(defun first-nonnil (l) (first (rm-nil l)))
               ;(i (or (first l) (second l)))
(defun assoc2 (i a) 
  "val/2nd of assoc"
  (let ((as (assoc i a :test #'equal)))
    (when as (cdr as)))) ;was second

(defun assoc2nd (i a) 
  "val/2nd of assoc"
  (let ((as (assoc i a :test #'equal)))
    (when as (second as)))) ;was second 
;tried to do both assoc w/assoc-v above ;maybe better than tfec.cl attempt2do it.
(defun assoc_v (k a)  (let ((v (assoc k a :test #'equal))) (when v (first-lv (rest v))))) ;best
(defun assoc_v1 (k a)  (let ((v (assoc k a :test #'equal :key #'first-lv))) (if (consp v) (cdr v) v)))
;-
(defun rm-nohtm (s) (rm-str "><" s))

(defun link-disp-txt (s)
  "part of txt >that if visable in html<"
 (if (not (stringp s)) s
  (let ((ps (positionsl '(#\> #\<) (rm-nohtm s))))
   (if (fulll ps) (subseq s (1+ (second ps)) (third ps))
     s)))) 
;-
;(defgeneric safe_v (s)) ;rest in u2.lisp

;- 
(defun eq-len (a b) ;do for more
  (eq (len a) (len b)))
;- 
;defun subseqs (lst snl &key (start nil) (end nil)) ;orginal in mb_utils
(defun subseq-s (lst snl &key (start nil) (end nil) (add2 t))
  "start# end=t subseq  maps over offset lst of positions"
 (let ((snl1 (if start (cons start snl) snl))
       (snl2 (if end (append snl (list (len lst)))  snl)))
    (when *dbg-ut* (format nil "~&~a ~a" snl1 snl2))
    (mapcar #'(lambda (a b) (subseq lst a (if add2 (1+ b) b)))
        snl1 (rest snl2))))
(defun break-by-pair (s p)
  "seq broken in parts between brakets or whatever"
  (let ((pl (positionsl p s)))
    (subseq-s s pl)))
(defun break-by-brackets (s) ;no only if no embedding  ;finish&get2 json-decode quickly
  (break-by-pair s '(#\[ #\])))
;-have a version that gets from 1st in start to last from end-:ut/ascii.lisp between-str2curlys
(defun between-str2curlys (s) (between-str2by s #\{ #\})) ;better name 
(defun between-str2by2 (str by by2)
  "between (by+by2  by2-on)"
  (list (between-str2by str by by2)
        (last-str2by-end str by2)))
(defun between-str2curlys2 (s) (between-str2by2 s #\{ #\}))
;-
(defun lists2alst (l1 l2) (mapcar #'cons l1 l2)) ;not used yet
(defun l+v2alst (l v) (mapcar #'(lambda (e) (cons e v)) l))
;defun collect-curlys (s )
;-
;http://letoverlambda.com/index.cl/guest/chap5.html http://letoverlambda.com/lol-orig.lisp
(defun predicate-splitter (orderp splitp)
  (lambda (a b)
    (let ((s (funcall splitp a)))
      (if (eq s (funcall splitp b))
        (funcall orderp a b)
        s))))
;(sort '(5 1 2 4 3 8 9 6 7)  (predicate-splitter #'< #'evenp))
;        (2 4 6 8 1 3 5 7 9)

;============from: binary-types-0.90
(defun make-pairs (list)
  "(make-pairs '(1 2 3 4)) => ((1 . 2) (3 . 4))"
  (loop for x on list by #'cddr collect (cons (first x) (second x)))) 

;-
;(defun ls2 (p) (mapcar_ #'(lambda (f) (str-cat p f)) (ls p))) ;next txt-files2
(defun ls2 (&optional (p "")) 
  (if (full p)
    (mapcar_ #'(lambda (f) (str-cat p f)) (ls p))
    (ls)))

(defun explode-by-tab (s) (csv_parse-str s))  ;or csv-parse-str but that gives an array
(defun explode-by-space (s) (collect-if #'full (explode- s))) ;1or more spaces treated as one  
(defun explode-by-ctrl-a (s) (explode- s #\^A)) ;in mh2t will use csv-parse-str to get Tabs
 

(defun txt-p (s) (suffixp ".txt" s))
;defun txt-files2 (&optional (path nil)) 
(defun files-of-p (&optional (path nil) (filetype-p #'txt-p))
  (if path (mapcar_ #'(lambda (f) (str-cat path f)) (collect-if filetype-p (ls path)))
    (collect-if filetype-p (ls))))
(defun txt-files2 (&optional (path nil)) (files-of-p path #'txt-p)) 

(defun clean-htm (s) (replace-substrings s   "\">"   "_")) ;or simple-replace-strings w/alst 2chng;NO
 
;"utr2.lisp" 555 lines --69%- 
(defun tree-map (fn tree)
  "Maps FN over every atom in TREE."
  (cond
   ((null tree) nil)
   ((atom tree) (funcall fn tree))
   (t
    (cons
     (tree-map fn (car tree))
     (tree-map fn (cdr tree)))))) 

(defun split-strs2at2 (strs by)
 (let ((p (position by strs :test #'equal)))
  (when p
      (cons ;(apply #'str-cat 
           (subseq strs 0 p);)
            ;(apply #'str-cat 
               (subseq strs (+ p 1);)
               )))))
(defun rest-from (strs by)
  (rest (split-strs2at2 strs by)))
 
(defun run-ext2 (s)
  "input1str&it breaks4you"
  (apply #'run-ext (tokens s)))
(defun run-ext2ls (l s)
  "start run-ext2 off w/a pre-list"
  (apply #'run-ext (append l (tokens s)))) 

(defun postfix-if-not (post str)
  "make sure it ends w/post"
  (if (suffixp post str) str
    (str-cat str post))) 

(defun replace-all (string part replacement &key (test #'char=))
"Returns a new string in which all the occurences of the part 
is replaced with replacement."
    (with-output-to-string (out)
      (loop with part-length = (length part)
            for old-pos = 0 then (+ pos part-length)
            for pos = (search part string
                              :start2 old-pos
                              :test test)
            do (write-string string out
                             :start old-pos
                             :end (or pos (length string)))
            when pos do (write-string replacement out)
            while pos)))

(defun stream_lines (strm)
  "read stream->1str"
  (loop for line = (read-line strm nil)
        while line
        collect line)) 

(defun file2str (fn) (with-open-file (strm fn :direction :input) (stream_lines strm)))  

(defun warn-nil (nv str1 val1)
  (if nv nv
    (warn str1 val1))) 


(defun genstr (s)
  "unique-str GUID"
  (symbol-name (gensym s))) 

;http://compgroups.net/comp.lang.lisp/increment-hash-value-2/703023
(defun count-items (lst)
  "sum up lst occurances &print"
  (let ((ht (make-hash-table)))
    (loop :for item :in lst :do
       (incf (gethash item ht 0))
       :finally
       (maphash #'(lambda (k v) (format t "~A: ~A~%" k v)) ht))))

(defun count-alst (lst)
  "sum up alst vals &print"
  (let ((ht (make-hash-table)))
    (loop :for item :in lst :do
          ;maximizing 
       (incf (gethash (car item) ht 0) (cdr item))
          ;into mx
       :finally
       (maphash #'(lambda (k v) (format t "~A: ~A~%" k v)) ht)
       ;mx
       ))) ;get a loop max in there too ;have a version that does it
 
(defun assocp (cp)
  (when (consp cp)
    (and (atom (car cp)) (atom (cdr cp)))))

(defun lolp (cp)
  (when (listp cp)
    (and (listp (car cp)) (listp (cdr cp)))))

(defun tree-map2 (fn2 tree &optional (ancestor nil))
  "Maps FN over every atom in TREE." ;might change
  (cond
   ((null tree) nil)
   ((lolp tree) (funcall fn2 tree ancestor))
   ((assocp tree) (funcall fn2 tree ancestor))
   ((atom tree) (funcall fn2 tree ancestor))
   (t
    (cons
     (tree-map2 fn2 (car tree) tree)
     (tree-map2 fn2 (cdr tree) tree))))) 
 
(defun last- (l n)
    (let ((ln (len l)))
          (subseq l (- ln n) (len l)))) 

(defun tree-maps (fn tree &optional (stop nil))
  "Maps FN over every atom or..  in TREE."
  (cond
   ((null tree) nil)
   ((and stop (funcall stop tree)) (funcall fn tree))
   ((atom tree) ;(unless stop (funcall fn tree))
                )
   (t
    (cons
     (tree-maps fn (car tree) stop)
     (tree-maps fn (cdr tree) stop)))))  

;quickly check out a list
(defun plen (l) (if (stringp l) l (len l)))
(defun mplen (l) (mapcar #'plen l))
(defun mplen2 (l) (format t "~%~a" (plen l)) (mplen l)) 
;from: http://common-lisp.net/language.html
;defun explode (string &optional (delimiter #\Space)) ;already in km
(defun explode= (string &optional (delimiter #\Space))
  (let ((pos (position delimiter string)))
    (if (null pos)
        (list string)
        (cons (subseq string 0 pos)
              (explode (subseq string (1+ pos))
                       delimiter)))))
 
