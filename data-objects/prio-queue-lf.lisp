;; prio-queue.lisp -- Lightweight and Fast Shared / UnShared LIFO,
;; FIFO, Prio Queue, and Prio Mailbox
;;
;; DM/RAL 11/17
;; -----------------------------------------------------------------------
#|
The MIT License

Copyright (c) 2017-2018 Refined Audiometrics Laboratory, LLC

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
|#

(in-package #:priq)

;; equiv to #F
(declaim  (OPTIMIZE (SPEED 3) #|(SAFETY 0)|# #+:LISPWORKS (FLOAT 0)))

;; All FIFO Queues provide methods ADDQ, POPQ, and EMPTYQ-P, as well
;; as individual make-xxx constructors for variants.
;;
;; POPQ always returns two values, the value popped and a T flag if
;; the value actually had been popped, and not just a nil value
;; resulting from a POPQ against an empty FIFO queue. It is not an
;; error to POPQ from an empty queue.
;;
;; --------------------------------------------------------------
;;
;; Take advantage of the fact that all our safe data structs are
;; single-slot objects. Hence the code to RMW each of them would be
;; the same. Structure inheritance to the (clean) rescue!

;; --------------------------------------------------------------
;; UNSAFE-LIFO - A LIFO queue - unsafe for sharing

(defstruct (unsafe-lifo
            (:include ref:ref)))

(defmethod addq ((q unsafe-lifo) item &key &allow-other-keys)
  (push item (ref:ref-val q)))

(defmethod popq ((q unsafe-lifo) &key &allow-other-keys)
  (let ((empty-p (null (ref:ref-val q))))
    (values (pop (ref:ref-val q))) (not empty-p)))

(defmethod peekq ((q unsafe-lifo) &key &allow-other-keys)
  (let ((lst (ref:ref-val q)))
    (values (car lst) lst)))

(defmethod emptyq-p ((q unsafe-lifo))
  (null (ref:ref-val q)))

(defmethod contents ((q unsafe-lifo))
  (shiftf (ref:ref-val q) nil))

(defmethod findq ((q unsafe-lifo) val &rest args)
  (apply 'find val (ref:ref-val q) args))

(defmethod lastq ((q unsafe-lifo))
  (car (ref:ref-val q)))

;; --------------------------------------------------------------
;; LIFO - A LIFO queue with safe sharing

(defstruct (lifo
            (:include unsafe-lifo)))

(defmethod addq ((q lifo) item &key &allow-other-keys)
  (sys:atomic-push item (ref:ref-val q)))

(defmethod popq ((q lifo) &key &allow-other-keys)
  (sys:atomic-pop (ref:ref-val q)))

(defmethod contents ((q lifo))
  (sys:atomic-exchange (ref:ref-val q) nil))

;; -------------------------------------------------------------
;; UNSAFE-FIFO - Very Fast FIFO Queue - unsafe for sharing
;;
;; Invariant: A non-empty queue always has a non-nil hd
;; Invariant: hd always points to a cell with null car,
;;   and when (EQ hd tl) the queue is empty

(defstruct (unsafe-fifo
            (:constructor %make-unsafe-fifo)
            (:copier nil))
  hd tl)

(defun make-unsafe-fifo ()
  (let ((empty (list nil)))
    (%make-unsafe-fifo
     :hd empty
     :tl empty)))

(defun copy-unsafe-fifo (f)
  (let ((fcopy (copy-list (unsafe-fifo-hd f))))
    (%make-unsafe-fifo
     :hd fcopy
     :tl (last fcopy))))

(defmethod addq ((q unsafe-fifo) item &key &allow-other-keys)
  (with-slots (tl) q
    (declare (cons tl))
    (setf tl (setf (cdr tl) (list item)))
    ))

(defmethod popq ((q unsafe-fifo) &key &allow-other-keys)
  (with-slots (hd tl) q
    (declare (cons hd tl))
    (if (eq hd tl)
        (values nil nil)
      (values (shiftf (car (setf hd (cdr hd))) nil) ;; keep GC happy
              t))
    ))

(defmethod peekq ((q unsafe-fifo) &key &allow-other-keys)
  (with-slots (hd tl) q
    (declare (cons hd tl))
    (if (eq hd tl)
        (values nil nil)
      (values (cadr hd) t))
    ))

(defmethod emptyq-p ((q unsafe-fifo))
  (with-slots (hd tl) q
    (eq hd tl)))

(defmethod contents ((q unsafe-fifo))
  (with-slots (hd tl) q
    (declare (cons hd tl))
    (shiftf (cdr (setf tl hd)) nil)))

(defmethod set-contents ((q unsafe-fifo) lst)
  (with-slots (hd tl) q
    (declare (cons hd tl))
    (setf (cdr hd) lst
          tl       (last hd)) ))

(defsetf contents set-contents)


(defmethod findq ((q unsafe-fifo) val &rest args)
  (with-slots (hd) q
    (declare (cons hd))
    (apply 'find val (cdr hd) args)))

(defmethod lastq ((q unsafe-fifo))
  (with-slots (tl) q
    (car tl)))

(defmethod countq ((q unsafe-fifo))
  (with-slots (hd) q
    (declare (cons hd))
    (length (cdr hd))))

;; ---------------------------------------------------------------
;; FIFO - Fast FIFO Queue - safe for sharing
;;
;; Invariant: A non-empty queue always has a non-nil hd

(defstruct (fifo
            (:include ref:ref)
            (:constructor %make-fifo)))

(defun make-fifo ()
  (%make-fifo :val (cons nil nil)))

(defun normalize-fifo (hd tl)
  (if hd
      (cons hd tl)
    (cons (reverse tl) nil)))

(defmethod addq ((q fifo) item &key &allow-other-keys)
  (ref:rmw-ref q (lambda* ((hd . tl))
                   (normalize-fifo hd (cons item tl)))
               ))

(defmethod popq ((q fifo) &key &allow-other-keys)
  (let (ans
        found)
    (ref:rmw-ref q (lambda* ((&whole cell hd . tl))
                     (cond (hd
                            (setf ans   (car hd)
                                  found t)
                            (normalize-fifo (cdr hd) tl))
                           
                           (t
                            cell)
                           )))
    (values ans found)))

(defmethod peekq ((q fifo) &key &allow-other-keys)
  (let ((hd (car (ref:rd-ref q))))
    (values (car hd) hd)))

(defmethod emptyq-p ((q fifo))
  (null (car (ref:rd-ref q))))

(defmethod contents ((q fifo))
  (let (pair)
    (ref:rmw-ref q (lambda (qpair)
                     (setf pair qpair)
                     (cons nil nil)))
    (append (car pair) (reverse (cdr pair)))
    ))

(defmethod set-contents ((q fifo) lst)
  (ref:wr-ref q (cons lst nil)))

(defmethod findq ((q fifo) val &rest args)
  (destructuring-bind (hd . tl) (ref:rd-ref q)
    (or (apply 'find val hd args)
        (apply 'find val tl args))))

(defmethod lastq ((q fifo))
  (destructuring-bind (hd . tl) (ref:rd-ref q)
    (if tl
        (car tl)
      (um:last1 hd))
    ))

(defmethod countq ((q fifo))
  (destructuring-bind (hd . tl) (ref:rd-ref q)
    (+ (length hd) (length tl))
    ))

;; ------------------------------------------------------------------
;; UNSAFE-PRIQ - Fast Priority FIFO Queue - unsafe for sharing
;;
;; Invariant: No priority level in the tree has an empty FIFO queue

(defstruct unsafe-priq 
  (tree (maps:empty)))

(defmethod addq ((q unsafe-priq) item &key prio)
  (with-slots (tree) q
    (let ((fq (maps:find tree prio)))
      (unless fq
        (setf fq (make-unsafe-fifo))
        (maps:addf tree prio fq))
      (addq fq item))
    ))

(defmethod popq ((q unsafe-priq) &key prio)
  (with-slots (tree) q
    (labels ((no ()
               (values nil nil))

             (yes (prio fq)
               (let ((ans (popq fq)))
                 (when (emptyq-p fq)
                   (maps:removef tree prio))
                 (values ans t))))
      
      (cond ((maps:is-empty tree) (no))
            
            (prio
             (um:if-let (fq (maps:find tree prio))
                 (yes prio fq)
               (no)))
          
            (t
             (let* ((node (sets:max-elt tree))
                    (prio (maps:map-cell-key node))
                    (fq   (maps:map-cell-val node)))
               (yes prio fq)))
            ))))

(defmethod peekq ((q unsafe-priq) &key prio)
  (with-slots (tree) q
    (labels ((no ()
               (values nil nil)))

      (cond ((maps:is-empty tree) (no))
            
            (prio
             (um:if-let (fq (maps:find tree prio))
                 (peekq fq)
               (no)))
          
            (t
             (let* ((node (sets:max-elt tree))
                    (fq   (maps:map-cell-val node)))
               (peekq fq)))
            ))))

(defmethod emptyq-p ((q unsafe-priq))
  (with-slots (tree) q
    (maps:is-empty tree)))

(defmethod countq ((q unsafe-priq))
  (with-slots (tree) q
    (let ((ct 0))
      (maps:iter tree
                 #'(lambda (k v)
                     (declare (ignore k))
                     (incf ct (countq v))))
      ct)))
          
;; ------------------------------------------------------------------
;; PRIQ - Priority FIFO Queue - safe for sharing
;;
;; Invariant: No priority level in the tree has an empty FIFO queue

(defstruct (priq
            (:include ref:ref)
            (:constructor %make-priq)))

(defun make-priq ()
  (%make-priq
   :val (maps:empty)))

(defmethod addq ((q priq) item &key (prio 0))
  (ref:rmw-ref q (lambda (tree)
                   (let ((fq (maps:find tree prio)))
                     (unless fq
                       (setf fq (make-unsafe-fifo)))
                     (addq fq item)
                     (maps:add tree prio fq))) ;; MAPS:ADD replaces any existing entry
               ))

(defmethod popq ((q priq) &key prio)
  (let (ans
        found)
    (ref:rmw-ref q (lambda (tree)
                     ;;
                     ;; The beautiful thing about this is that while inside
                     ;; this function, the use of purely functional data types
                     ;; ensures that our view of tree won't change, and
                     ;; nothing we do to it (if we play by functional rules!)
                     ;; can be seen by any other processes until we are
                     ;; finished.
                     ;;
                     ;; We might be called to perform this body of code more
                     ;; than once in case someone else changed the underlying
                     ;; data structure before we could finish. But each time
                     ;; through, our view of tree is entirely ours.
                     ;;
                     (labels ((no ()
                                tree)
                              
                              (yes (prio fq)
                                (setf ans   (popq fq)
                                      found t)
                                (if (emptyq-p fq)
                                    (maps:remove tree prio)
                                  (maps:add tree prio fq))))
                       
                       (cond ((maps:is-empty tree) (no))
                             
                             (prio
                              (um:if-let (fq (maps:find tree prio))
                                  (yes prio fq)
                                (no)))
                             
                             (t 
                              (let* ((node (sets:max-elt tree))
                                     (prio (maps:map-cell-key node))
                                     (fq   (maps:map-cell-val node)))
                                (yes prio fq)))
                             )) ))
    (values ans found)))


