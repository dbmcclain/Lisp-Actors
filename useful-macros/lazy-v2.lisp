
(defpackage :com.ral.useful-macros.lazy-v2
  (:use :common-lisp :com.ral.useful-macros)
  (:export
   #:force
   #:deferred
   #:with-spin
   #:without-spin
   #:lazy
   #:once-only
   #:once-thereafter
   #:future
   #:unsafe-future
   #:pmap
   #:pvmap
   #:npmap
   #:npvmap
   #:par
   ))

(in-package :com.ral.useful-macros.lazy-v2)

(defmacro deferred (&body body)
  `(lambda ()
     ,@body))

(defstruct lazy
  cell)

(defmacro lazy (expr)
  `(make-lazy
    :cell (cons
           (lambda ()
             ,expr)
           :uneval)))

(defgeneric force (obj)
  (:method ((obj lazy))
   (recover-ans-or-exn
    (nlet iter ()
      (let ((cell (lazy-cell obj)))
        (cond
         ((eq (cdr cell) :eval)
          (car cell))
         ((mpcompat:compare-and-swap (cdr cell) :uneval :in-proc)
          (let ((val (call-capturing-ans-or-exn (car cell))))
            (setf (lazy-cell obj) (cons val :eval))
            val))
         (t
          (mpcompat:process-allow-scheduling)
          (go-iter))
         )))))
  (:method ((obj function))
   (funcall obj))
  (:method (obj)
   obj))
  
