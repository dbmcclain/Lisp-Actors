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

(in-package :useful-macros)

;; --------------------------------------------------------------------

(defun make-coll ()
  (let ((coll nil)
        (tail nil))
    (dlambda
      (:add (item)
       (if tail
         (let ((newcons (cons item (cdr tail))))
           (setf (cdr tail) newcons
                 tail       newcons))
         (setf coll (list item)
               tail coll
               (cdr tail) coll))
       (values))
      
      (:push (item)
       (if coll
           (let ((newcons (cons item coll)))
             (setf coll newcons
                   (cdr tail) coll))
         (setf coll (list item)
               tail coll
               (cdr tail) coll))
       (values))
      
      (:discard ()
       (nilf coll tail))
      
      (:empty-p ()
       (null coll))
      
      (:get-items ()
       (let ((ans coll))
         (when coll
           (setf (cdr tail) nil)
           (nilf coll tail))
         ans))
      
      (:pop ()
       (when coll
         (let ((ans (car coll)))
           (if (eq coll tail)
               (nilf coll tail)
             (setf (cdr tail) (cdr coll)
                   coll       (cdr coll)))
           ans)))
      
      (:n-items ()
       (if coll
           (progn
             (setf (cdr tail) nil)
             (prog1
                 (length coll)
               (setf (cdr tail) coll)))
         0))
      )))

;; --------------------------------------------------------------------

(defvar number-of-conses 0)

(declaim (inline counting-cons))

(defun counting-cons (a b)
  (incf number-of-conses)
  (cons a b))

(defmacro! with-conses-counted (&rest body)
  `(let ((,g!orig number-of-conses))
     ,@body
     (- number-of-conses ,g!orig)))

(defmacro counting-push (obj stack)
  `(setq ,stack (counting-cons ,obj ,stack)))

(defmacro with-cons-pool (&rest body)
  `(let ((cons-pool)
         (cons-pool-count 0)
         (cons-pool-limit 100))
     (declare (ignorable cons-pool
                         cons-pool-count
                         cons-pool-limit))
     ,@body))

(defmacro! cons-pool-cons (o!car o!cdr)
  `(if (= cons-pool-count 0)
     (counting-cons ,g!car ,g!cdr)
     (let ((,g!cell cons-pool))
       (decf cons-pool-count)
       (setf cons-pool (cdr cons-pool))
       (setf (car ,g!cell) ,g!car
             (cdr ,g!cell) ,g!cdr)
       ,g!cell)))

(defmacro! cons-pool-free (o!cell)
  `(when (<= cons-pool-count
             (- cons-pool-limit 1))
     (incf cons-pool-count)
     (setf (car ,g!cell) nil)
     (push ,g!cell cons-pool)))

(defmacro make-cons-pool-stack ()
  `(let (stack)
     (dlambda
       (:push (elem)
         (setf stack
               (cons-pool-cons elem stack)))
       (:pop ()
         (if (null stack)
           (error "Tried to pop an empty stack"))
         (let ((cell stack)
               (elem (car stack)))
           (setf stack (cdr stack))
           (cons-pool-free cell)
           elem)))))

(with-cons-pool
  (defun make-shared-cons-pool-stack ()
    (make-cons-pool-stack)))

(defmacro with-dynamic-cons-pools (&rest body)
  `(locally (declare (special cons-pool
                              cons-pool-count
                              cons-pool-limit))
     ,@body))

(defmacro fill-cons-pool ()
  `(let (tp)
     (loop for i from cons-pool-count 
                 to cons-pool-limit
           do (push
                (cons-pool-cons nil nil)
                tp))
     (while tp
       (cons-pool-free (pop tp)))))

;; --------------------------------------------------------------------

(declaim (inline make-tlist tlist-left
                 tlist-right tlist-empty-p))

(defun make-tlist () (cons nil nil))
(defun tlist-left (tl) (caar tl))
(defun tlist-right (tl) (cadr tl))
(defun tlist-empty-p (tl) (null (car tl)))

(declaim (inline tlist-add-left
                 tlist-add-right))

(defun tlist-add-left (tl it)
  (let ((x (cons it (car tl))))
    (if (tlist-empty-p tl)
      (setf (cdr tl) x))
    (setf (car tl) x)))

(defun tlist-add-right (tl it)
  (let ((x (cons it nil)))
    (if (tlist-empty-p tl)
      (setf (car tl) x)
      (setf (cddr tl) x))
    (setf (cdr tl) x)))

(declaim (inline tlist-rem-left))

(defun tlist-rem-left (tl)
  (if (tlist-empty-p tl)
    (error "Remove from empty tlist")
    (let ((x (car tl)))
      (setf (car tl) (cdar tl))
      (if (tlist-empty-p tl)
        (setf (cdr tl) nil)) ;; For gc
      (car x))))

(declaim (inline tlist-update))

(defun tlist-update (tl)
  (setf (cdr tl) (last (car tl))))

;; ----------------------------------------------------------------------


(defmacro! with-fast-stack
           ((sym &key (type 'fixnum) (size 1000)
                      (safe-zone 100))
            &rest body)
  `(let ((,g!index ,safe-zone)
         (,g!mem (make-array ,(+ size (* 2 safe-zone))
                             :element-type ',type)))
     (declare (type (simple-array ,type) ,g!mem)
              (type fixnum ,g!index))
     (macrolet
       ((,(symb 'fast-push- sym) (val)
            `(locally #f
               (setf (aref ,',g!mem ,',g!index) ,val)
               (incf ,',g!index)))
         (,(symb 'fast-pop- sym) ()
            `(locally #f
               (decf ,',g!index)
               (aref ,',g!mem ,',g!index)))
         (,(symb 'check-stack- sym) ()
            `(progn
               (if (<= ,',g!index ,,safe-zone)
                 (error "Stack underflow: ~a"
                        ',',sym))
               (if (<= ,,(- size safe-zone)
                       ,',g!index)
                 (error "Stack overflow: ~a"
                        ',',sym)))))
         ,@body)))

;; ----------------------------------------------------------------------

(defun safe-list-length (lst)
  ;; handle dotted and recursive lists
  (if (endp lst) ;; endp will error on non-lists
      0
    (nlet-tail iter ((nel 1)
                     (lslow (cdr lst))
                     (lfast (and (consp (cdr lst))
                                 (cddr lst))))
      (if (eq lslow lfast)
          nel
        (iter (1+ nel)
              (and (consp lslow)
                   (cdr lslow))
              (and (consp lfast)
                   (consp (cdr lfast))
                   (cddr lfast)))
        ))
    ))

;; -------------------------------------------------------------------
;; macro versions of curry

(defmacro currym (fn &rest args) ;; last one should be lambda list
  (let* ((revargs   (reverse args))
         (pre-names (gensyms (cdr revargs)))
         (args      (mklist (car revargs))))
    `(let ,(mapcar #'list pre-names (cdr revargs))
       (lambda ,args
         (,fn ,@(reverse pre-names) ,@args)))))

(defmacro rcurrym (fn pre-args &rest post-args)
  (let* ((post-names (gensyms post-args))
         (args       (mklist pre-args)))
    `(let ,(mapcar #'list post-names post-args)
       (lambda ,args
         (,fn ,@args ,@post-names))) ))

;; ----------------------------------------
(defun make-unique-object ()
  ;; unique under eq, eql, equal, and equalp
  ;; can be eval either as object or with funcall
  (named-lambda v (&rest args)
    (declare (ignore args))
    #'v))

;; -------------------------------------------------------

(defmacro mv-constantly (&rest args)
  (let ((g!args (gensym)))
    `(lambda (&rest ,g!args)
       (declare (ignore ,g!args))
       (values ,@args))))

;; -------------------------------------------------------

(defun make-list-builder ()
  ;; An O(1) list builder object, in FIFO order. Fast append uses a
  ;; circular list with cell pointing at last cdr, front of list
  ;; always in (cdr cell)
  #F
  (let (cell)
    (labels ((set-cdr (x)
               (when-let (xcell (or cell x))
                 (setf cell (setf (cdr xcell) x))
                 )))
      (declare (inline set-cdr))
      (dlambda
        (:append (x)
         ;; O(1) append item
         (set-cdr (cons x (cdr cell))))
        (:get ()
         ;; break the circle and return the list
         ;; clears the cell and ready to start anew
         (prog1
             (cdr cell)
           (set-cdr nil)))
        (:reset (x)
         ;; reset list to one item
         (setf cell nil)
         (set-cdr (list x)))
        ))))

;; -------------------------------------------------------

