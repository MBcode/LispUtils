;http://en.wikibooks.org/wiki/Common_Lisp/Advanced_topics/CLOS/Example_1
;But these subtitles are no good for you, because your version of the movie for some reason contains a 10.532 second pause in the beginning. It's impossible change all timestamps manually, and let's assume there's no tool that would do it for us. So we have to code a script in Common Lisp (what else). Let's start now!
;The class of objects we will be working with is a set of timestamps. We need to be able to find them in a file, to add them together, and to insert them back.
(defclass srt-time ()
  ((hr :initarg :hr :initform 0 :accessor hr)
   (mi :initarg :mi :initform 0 :accessor mi)
   (se :initarg :se :initform 0 :accessor se)
   (ms :initarg :ms :initform 0 :accessor ms))
  (:documentation "Time format for srt"))

(defgeneric display (what)
  (:documentation "Returns string that represents the object"))

(defgeneric normalise (time)
  (:documentation "Fix overflow of fields"))

(defmethod normalise ((time srt-time))
  (with-slots (hr mi se ms) time 
    (loop until (< ms 1000) do (decf ms 1000) (incf se))
    (loop until (< se 60) do (decf se 60) (incf mi))
    (loop until (< mi 60) do (decf mi 60) (incf hr)))
  time)

(defmethod display ((time srt-time))
  (normalise time)
  (with-slots (hr mi se ms) time 
    (format nil "~2,'0d:~2,'0d:~2,'0d,~3,'0d" hr mi se ms)))

(defun make-srt-time (arglist)
  (destructuring-bind (hr mi se ms) arglist
    (make-instance 'srt-time :hr hr :mi mi :se se :ms ms)))

;display method would return a textual representation of a srt-time object. normalise is a helper function, which fixes all "overflows" of slots (there can't be more than 60 seconds and so on). make-srt-time is a wrapper around make-instance which allows easier creation of srt-time objects.
;Now, we write two methods for adding time.
(defgeneric add (t1 t2))

(defmethod add ((t1 srt-time) (t2 srt-time))
  "Adds two srt-times"
  (normalise 
   (make-srt-time 
    (mapcar #'+ (list (hr t1) (mi t1) (se t1) (ms t1)) 
                (list (hr t2) (mi t2) (se t2) (ms t2))))))

(defmethod add ((t1 srt-time) (t2 integer))
  "Adds some number of seconds"
  (normalise (make-srt-time (list (hr t1) (mi t1) (+ (se t1) t2) (ms t1)))))
;Adding a second way to add times doesn't look like much. But consider that every function that calls add may pass integer for second argument instead of srt-time. As we will see later, this extension of functionality propagates to upper layers of our program, including the function that's intended to be called by the user.
;Now let's think about the second part of our task. Given a text string we must replace all instances of timestamps with modified timestamps. Fortunately, CL-PPCRE can do exactly that thing. We just need to find a fitting regular expression. Regexes aren't a topic of this Wikibook, but if you're not familiar with them, there are many good sites for learning this concept. I'll just write it down: "([0-9]{2,}):([0-9]{2}):([0-9]{2}),([0-9]{3})". At least try to figure how it corresponds to one specific timestamp, say 00:00:44,132. Note that the things that matched a part of regular expression between "(" and ")" are remembered by CL-PPCRE and we will use that fact. For now, let's generate a scanner that corresponds to that regular expression:
(defparameter *find-time* (cl-ppcre:create-scanner 
                            "([0-9]{2,}):([0-9]{2}):([0-9]{2}),([0-9]{3})"
                            ))
;This scanner is actually a compiled function, but it's not necessary to know implementation details, if everything works like intended. The next step is to use this scanner to find and replace some substrings of a string:
(defun modify-times (str fun)
  "Modify all instances of srt-time being hidden in the given string
   using a given function"
  (cl-ppcre:regex-replace-all *find-time* str fun :simple-calls t))
;The function just takes an arbitrary string and arbitrary function and uses this function to transform all timestamps that scanner *find-time* finds in that string. Now we'll write a function that feeds the right function to modify-times.
(defun apply-line-add (str delta)
  (labels ((adder (match hr mi se ms)
             (declare (ignore match)) ;;match is needed for CL-PPCRE
             (display
              (add (make-srt-time (mapcar #'parse-integer (list hr mi se ms)))
                   delta))))
    (modify-times str #'adder)))
;Now wasn't that fun? We construct the needed function at runtime, since we don't know yet how much time the user wants to add! regex-replace-all will call adder with 5 arguments. The first argument, match is for whole match - we don't need it. What we need is the parts (those surrounded by brackets). These correspond to hours, minutes, seconds and milliseconds. We convert them from strings to integers with parse-integer. Then a srt-time object is produced from these numbers, then delta is added to it (note that delta could be either srt-time or integer, we don't know, and we don't care). Then the result is converted back into string using display method. That's what CL-PPCRE wanted from that function, and we can now forget about CL-PPCRE and concentrate on other things.
;The next function, mapline would slice a file into lines, feed these lines to some function and print these lines into output file.
(defun mapline (fun input output)
  "Applies function to lines of input file and outputs the result" 
  (with-open-file (in input)
    (with-open-file (out output :direction :output :if-exists :supersede)
      (loop for str = (read-line in nil nil)
            while str
            do (princ (funcall fun str) out) (terpri out)))))
;Don't you like with-open-file? Concise and clean.
;And now, the final function, that would combine the power of mapline and modify-times:
(defun delay (delay input output)
  "Adjusts all srt-times in file by adding delay to them. Delay can be
  either integer (number of seconds) or srt-time instance."
 ;(mapline (lambda (str) (apply-line-add str delay)) input output)
  (mapline #'(lambda (str) (apply-line-add str delay)) input output)
  )
;Now, why is this example filed under CLOS? Well, it shows why CLOS is good. It makes your programs very scalable. Consider that you need to add the functionality so that delay could be float number of seconds. Just write an appropriate add method. I'm leaving it as an exercise. 
