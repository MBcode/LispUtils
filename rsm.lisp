;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          queue.lisp
;;;; Purpose:       Queuing functions.
;;;; Author:        R. Scott McIntire
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id: queue.lisp,v 1.5 2003/10/23 15:43:31 rscottmcintire Exp $
;;;; *************************************************************************

;(in-package rsm.queue)

;(eval-when (:compile-toplevel)
;  (declaim (optimize (speed 3) (debug 0) (safety 1) (space 0))))


;;;; A queue is represented as a cons cell as: 
;;;; queue-list . pointer-to-last-cons-cell-of queue-list.
;;;; The most recent addition is at the end of the queue-list; the oldest
;;;; at the beginning of queue-list.

;(declaim (inline nget-list))
(defun nget-list (queue)
  "Get the internal list of queue, <queue>. The integrity of the queue cannot be
guaranteed if this list is destructively modified."
  (car queue))

;(declaim (inline get-first))
(defun get-first (queue)
  "Get the next element the queue would dequeue. Does not affect the queue."
  (caar queue))

;(declaim (inline get-last))
(defun get-last (queue)  
  "Get the last element the queue would dequeue. Does not affect the queue."
  (cadr queue))

(defun append-queue (&rest queues)
  "Create a new queue which appends the other queues. The original queues are
not changed."
  (let ((que (create)))
    (dolist (queue queues)
      (dolist (el (nget-list queue))
        (enqueue el que)))
    que))


;(declaim (ftype (function () list) create))

(defun create (&optional obj)
  "Create a queue. If <obj> is non nil queue it up. In order to create a queue
with nil as the first element, call queue with no arguments and then call
enqueue with nil as the value to queue."
  (let ((queue (cons nil nil)))
    (if obj
        (enqueue obj queue)
      queue)))

(defun copy-queue (que)
  "Copy a queue."
  (let ((new-que (create))
        (new-list (copy-list (nget-list que))))
    (setf (car new-que) new-list)
    (setf (cdr new-que) (last new-list))
    new-que))


(defun non-empty-queues (queues)
  "Return a list of the non-empty queues."
  (let ((que-que (create)))
    (dolist (queue queues)
      (unless (empty-p queue)
        (enqueue queue que-que)))
    (nget-list que-que)))

(defun nappend-queue (&rest queues)
  "Append (destructively (like nconc) the queues <queues> essentially by
nconsing the internal list of the first nonempty queue with the lists from the
rest of the non empty queues.  
Note: After this operation, do not use the other queues."
  (let ((non-empty-queues (non-empty-queues queues))
        base-que)
    (if (null non-empty-queues)
        (create)
      (let ((rest-ques (cdr non-empty-queues)))
        (setf base-que (car non-empty-queues))
        (dolist (queue rest-ques)
          (setf (cdr (cdr base-que)) (nget-list queue))
          (setf (cdr base-que) (cdr queue)))))
    base-que))


;(declaim (ftype (function (t list) list) enqueue))

(defun enqueue (obj queue)
  "Enqueue an object. Return the queue."
  (if (cdr queue)
      (setf (cdr (cdr queue)) (list obj)
            (cdr queue) (cdr (cdr queue)))
    (setf (cdr queue) (list obj)
          (car queue) (cdr queue)))
  queue)

;(declaim (ftype (function (list) t) dequeue))

(defun dequeue (queue)
  "Dequeue an object. Return the object queued."
  (when (cdr queue)
    (prog1
        (caar queue)
      (if (eq (cdr queue)
              (car queue))
          (setf (car queue) nil
                (cdr queue) nil))
      (setf (car queue) (cdr (car queue))))))


;(declaim (ftype (function (list) t) empty-p))

(defun empty-p (queue)
  "Is the queue empty?"
  (and (not (cdr queue)) t))


;(declaim (ftype (function (list) list) list->queue))

(defun list->queue (list)
  "Return a copy of the list as a queue. The first element of the list
will be the first element queued, and the last element of the list will 
be the last element queued."
  (let ((queue (create)))
    (loop 
      :for elem :in list :do
      (enqueue elem queue))
    queue))


(defun queue->list (que)
  "Return a copy of the queue as a list, from 'first in' to 'last in'."
  (copy-list (car que)))


(defun queue-p (que)
  "Returns true if <que> is a queue."
  (and (consp que)
       (listp (car que))
       (null (cdr (cdr que)))))

(deftype queue ()
  "Type for a queue."
  '(satisfies queue-p))


(defmacro do-queue ((var que &optional result) &body body)
  "Loop construct for queues that sets <var> to the successive values of a copy
of the queue, <que>, (by dequeuing) and then evaluates <body>. If the symbol 
<result> is supplied, its value is returned when the iteration is finished.
Example: (rsm.queue:do-queue (item que) (format t \"queue item = ~s~%\" item)) 
This drains a copy of the queue, <que>, printing each of the elements 
of the queue."
  (let ((copied-queue (gensym))
        (is-empty? (gensym)))
    `(do* ((,copied-queue (copy-queue ,que))
           (,is-empty? (empty-p ,copied-queue)
                       (empty-p ,copied-queue))
           (,var (dequeue ,copied-queue)
                 (dequeue ,copied-queue)))
         ()
       (when ,is-empty?
         (return ,result))
       ,@body
       )))


(defmacro do-nqueue ((var que &optional result) &body body)
  "Loop construct for queues that sets <var> to the successive values of the
queue, <que>, (by dequeuing) and then evaluates <body>. If the symbol <result> 
is supplied, its value is returned when the iteration is finished.
Example: (rsm.queue:do-nqueue (item que) (format t \"queue item = ~s~%\" item)) 
This drains the queue, <que>, printing each of the elements of the queue.  
Note: This is a destructive function. If <body> mutates <que>, this 
construct could go into an infinite loop."
  (let ((is-empty? (gensym)))
    `(do ((,is-empty? (empty-p ,que)
                      (empty-p ,que))
          (,var (dequeue ,que) 
                (dequeue ,que)))
         ()
       (when ,is-empty?
         (return ,result))
       ,@body
       )))


(defun sort-queue (queue sort-func)
  "Sort a copy of queue, <queue>, using sort function <sort-func>."
  (let ((list (sort (copy-list (nget-list queue)) sort-func))
        (new-que (create)))
    (dolist (elem list)
      (enqueue elem new-que))
    new-que))

(defun nsort-queue (queue sort-func)
  "Sort a queue, <queue>, in place using sort function <sort-func>."
  (let ((list (sort (nget-list queue) sort-func)))
    (setf (car queue) list)
    (setf (cdr queue) (last list)))
  queue)

;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          filter.lisp
;;;; Purpose:       Filter lists and trees.
;;;; Author:        R. Scott McIntire
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id: filter.lisp,v 1.4 2003/09/17 01:50:03 rscottmcintire Exp $
;;;; *************************************************************************

;(in-package rsm.filter)

;(eval-when (:compile-toplevel)
;  (declaim (optimize (speed 3) (debug 0) (safety 1) (space 0))))

#+ignore
(declaim (ftype (function (list function) list) filter))
#+ignore
(defun filter (ls pruner)
  "Return a new list formed from selecting only those elements of <list> that do
not satisfy <pruner>.  The order of the elements is preserved.
Example: (rsm.filter:filter '(1 2 3 4 5) #'evenp)
          (1 3 5)"
  (let ((que (create)))
    (dolist (el ls (nget-list que))
      (unless (funcall pruner el)
        (enqueue el que)))))


;(declaim (ftype (function (list predicate) list) prune-tree))

(defun prune-tree (tree pruner)
  "Returns a pruned version of <tree> where pruned elements satisfy the
predicate, <pruner>.
Example: (rsm.filter:prune-tree '(1 2 (3 4 (5) (6 7) 4) 2) #'oddp)
         (2 (4 (6) 4) 2)"
  (labels 
      ((rec (tree que)
         (cond ((null tree) (nget-list que))
               ((consp (car tree))
                (let ((c (rec (car tree) (create))))
                  (when c
                    (enqueue c que))
                  (rec (cdr tree) que)))
               (t
                (let ((c (car tree)))
                  (unless (funcall pruner c)
                    (enqueue c que))
                  (rec (cdr tree) que))))))
    (rec tree (create))))


;(declaim (ftype (function (list) list) tree-sig))

(defun tree-sig (tree)
  "Returns the same tree as <tree> with the value t in every leaf.
Example: (rsm.filter:tree-sig '(1 2 (3 4) 6)
         (t t (t t) t)"
  (labels 
      ((rec (tree que)
         (cond ((null tree) (nget-list que))
               ((consp (car tree))
                (enqueue (rec (car tree) (create)) que)
                (rec (cdr tree) que))
               (t
                (enqueue t que)
                (rec (cdr tree) que)))))
    (rec tree (create))))


;(declaim (ftype (function (predicate function) function) tree-hom))

(defun tree-hom (pruner transformer)
  "Returns a function which takes a tree and returns a pruned, transformed copy.
The tree will be pruned by <pruner> at the leafs and each leaf (that remains)
will be transformed by <transformer>.
Example: (setf *prune* (rsm.filter:tree-hom #'evenp #'(lambda (x) (+ x 10))))
         (funcall *prune* '(1 2 3 (3 4 5 (5 6 7) (7) 8 (9 10))))
   (11 13 (13 15 (15 17) (17) (19)))"
  (labels 
      ((rec (tree que)
         (cond ((null tree) (nget-list que))
               ((consp (car tree))
                (let ((c (rec (car tree) (create))))
                  (when c
                    (enqueue c que))
                  (rec (cdr tree) que)))
               (t
                (let ((c (car tree)))
                  (unless (funcall pruner c)
                    (enqueue (funcall transformer c) que))
                  (rec (cdr tree) que))))))
    #'(lambda (tree)
        (rec tree (create)))))


;(declaim (ftype (function (list function) list) map-tree))

(defun map-tree (tree func)
  "Maps the function <func> over the leaves of tree <tree>.
Example: (rsm.filter:map-tree  '(1 2 (3 4 (5) 6 7) 8) #'1+)
         (2 3 (4 5 (6) 7 8) 9)"
  (labels 
      ((rec (tree que)
         (cond ((null tree) (nget-list que))
               ((consp (car tree))
                (enqueue (rec (car tree) (create)) que)
                (rec (cdr tree) que))
               (t
                (enqueue (funcall func (car tree)) que)
                (rec (cdr tree) que)))))
    (rec tree (create))))

#+ignore
(declaim (ftype (function (list) list) flatten))
#+ignore
(defun flatten (tree)
  "Flattens a tree to a list.
 Example: (rsm.filter:flatten '(1 2 (3 4 (5) 6 7) 8))
          '(1 2 3 4 5 6 7 8)"
  (labels 
      ((rec (tree acc)
         (cond ((null tree) acc)
               ((atom tree) 
                (cons tree acc))
               (t
                (rec (car tree) (rec (cdr tree) acc))))))
    (rec tree nil)))


;(declaim (ftype (function (list &key (:from-end t) (:test function)) list) linearize))

(defun linearize (tree &key (from-end nil) (test #'eql))
  "Linearize a tree, removing duplicates (determined equal by <test>).  If
from-end is non null, then duplicate entries are removed from the end rather
than the beginning of the resulting list.
Example: (rsm.filter:linearize '(a b (c d (e f a) d c w q b)))
          (e f a d c w q b)
Example: (rsm.filter:linearize '(a b (c d (e f a) d c w q b)) :from-end t)
          (a b c d e f w q)"
  (delete-duplicates (flatten tree) :from-end from-end :test test))

