;including this file w/my very few fncs below, so can compile as one.
;
;https://raw.githubusercontent.com/marijnh/defservice/master/url-encode.lisp
;(defpackage :url-encode
;  (:use :cl)
;  (:export :url-decode :url-encode))
;
;(in-package :url-encode)

(defun char-utf-8-byte-length (char)
  (let ((code (char-code char)))
    (cond ((< code 128) 1)
          ((< code 2048) 2)
          ((< code 65536) 3)
          (t 4))))

(defmacro as-utf-8-bytes (char writer)
  "Given a character, calls the writer function for every byte in the
encoded form of that character."
  (let ((char-code (gensym)))
    `(let ((,char-code (char-code ,char)))
       (declare (type fixnum ,char-code))
       (cond ((< ,char-code 128)
              (,writer ,char-code))
             ((< ,char-code 2048)
              (,writer (logior #b11000000 (ldb (byte 5 6) ,char-code)))
              (,writer (logior #b10000000 (ldb (byte 6 0) ,char-code))))
             ((< ,char-code 65536)
              (,writer (logior #b11100000 (ldb (byte 4 12) ,char-code)))
              (,writer (logior #b10000000 (ldb (byte 6 6) ,char-code)))
              (,writer (logior #b10000000 (ldb (byte 6 0) ,char-code))))
             (t
              (,writer (logior #b11110000 (ldb (byte 3 18) ,char-code)))
              (,writer (logior #b10000000 (ldb (byte 6 12) ,char-code)))
              (,writer (logior #b10000000 (ldb (byte 6 6) ,char-code)))
              (,writer (logior #b10000000 (ldb (byte 6 0) ,char-code))))))))

(defun url-encode (string &optional (to-escape "\"#$%&+,/:;<=>?@"))
  (declare (optimize speed (safety 0)))
  (let ((size (loop :for ch :across string :for code := (char-code ch) :sum
                 (cond ((> code 127) (* (char-utf-8-byte-length ch) 3))
                       ((or (< code 33) (find ch to-escape)) 3)
                       (t 1)))))
    (if (= size (length string))
        string
        (let ((out (make-string size)) (pos 0))
          (macrolet ((wr (ch) `(progn (setf (schar out pos) ,ch) (incf pos))))
            (flet ((code-out (ch)
                     (multiple-value-bind (hi lo) (floor (char-code ch) 16)
                       (wr #\%) (wr (digit-char hi 16)) (wr (digit-char lo 16)))))
              (loop :for ch :across string :for code := (char-code ch) :do
                 (cond ((> code 127) (as-utf-8-bytes ch code-out))
                       ((or (< code 33) (find ch to-escape)) (code-out ch))
                       (t (wr ch))))))
          out))))

(defun utf-8-group-size (byte)
  "Determine the amount of bytes that are part of the character
starting with a given byte."
  (declare (type fixnum byte))
  (cond ((zerop (logand byte #b10000000)) 1)
        ((= (logand byte #b11100000) #b11000000) 2)
        ((= (logand byte #b11110000) #b11100000) 3)
        ((= (logand byte #b11111000) #b11110000) 4)
        (t (error "Invalid UTF-8 byte: 0x~X" byte))))

(defun get-utf-8-character (bytes group-size &aux (start 0))
  "Given an array of bytes and the amount of bytes to use,
extract the character they denote."
  (declare (type (simple-array (unsigned-byte 8) (*)) bytes)
           (type fixnum group-size))
  (macrolet ((next-byte ()
               '(prog1 (elt bytes start)
                 (incf start)))
             (six-bits (byte)
               (let ((b (gensym)))
                 `(let ((,b ,byte))
                    (unless (= (logand ,b #b11000000) #b10000000)
                      (error "Invalid byte 0x~X inside a character." ,b))
                    (ldb (byte 6 0) ,b))))
             (test-overlong (byte min-size)
               (let ((b (gensym)))
                 `(let ((,b ,byte))
                    (unless (> ,b ,min-size)
                      (error "Overlong UTF-8 byte sequence found."))
                    ,b))))
    (ecase group-size
      (1 (next-byte))
      (2 (test-overlong (logior (ash (ldb (byte 5 0) (next-byte)) 6)
                                (six-bits (next-byte))) 128))
      (3 (test-overlong (logior (ash (ldb (byte 4 0) (next-byte)) 12)
                                (ash (six-bits (next-byte)) 6)
                                (six-bits (next-byte))) 2048))
      (4 (test-overlong (logior (ash (ldb (byte 3 0) (next-byte)) 18)
                                (ash (six-bits (next-byte)) 12)
                                (ash (six-bits (next-byte)) 6)
                                (six-bits (next-byte))) 65536)))))

(defun url-decode (string)
  (declare (optimize speed (safety 0)))
  (let ((buf (make-string (length string)))
        (utf-buf (make-array 4 :element-type '(unsigned-byte 8))))
    (with-input-from-string (in string)
      (loop :for pos :from 0 :for ch := (read-char in nil nil) :while ch :do
         (macrolet ((hex ()
                      '(let ((big (digit-char-p (read-char in nil #\x) 16))
                             (small (digit-char-p (read-char in nil #\x) 16)))
                        (unless (and big small) (error "Junk in URL."))
                        (+ small (ash big 4)))))
           (setf (schar buf pos)
                 (case ch
                   (#\+ #\space)
                   (#\% (let* ((code (hex))
                               (group (utf-8-group-size code)))
                          (setf (aref utf-buf 0) code)
                          (loop :for i :from 1 :below group :do
                             (unless (eql (read-char in nil nil) #\%)
                               (error "Nonsense UTF-8 code in URL."))
                             (setf (aref utf-buf i) (hex)))
                          (get-utf-8-character utf-buf group)))
                   (t ch))))
         :finally (return (if (eql pos (length buf)) buf (subseq buf 0 pos)))))))

;;====from ld2.cl &ls,save-lines,read-file-to-string from my mb_utils
(defvar *jldp* "https://json-ld.org/playground/#startTab=tab-visualized&json-ld=")
(defun jld2jps (fnb)
  "encode jsonld for playground-viz"
    (let ((jlds (read-file-to-string (str-cat fnb ".jsonld"))))
      (print (str-cat *jldp* (url-encode jlds)))))

;can (ls) and map over all the .js files
(defun js2jptxt (fn)
  "encode json for playground-viz" ;should pull out jsonld,but has more from extruct
  (let ((tfn (str-cat (rm-ext fn) ".txt"))
        (jlds (read-file-to-string fn)))
    (save-lines (list (str-cat *jldp* (url-encode jlds))) tfn)))

(defun lsjs2jptxt ()
  "convert full js dir" ;there is an ls that would filter to just .js, later
  (mapcar #'js2jptxt (ls)))
