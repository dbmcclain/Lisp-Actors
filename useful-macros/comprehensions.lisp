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
;; -----------------------------------------------------------------------
;; List Comprehension = (lc h q1 q2 ... )
;;
;; where h is the head and q_i are qualifiers.
;;
;; HEAD:
;; Head form h can be any lisp form, or one of
;;    :COUNT,
;;    :SOME,
;;    :NOTANY,
;;    :EVERY,
;;    :NOTEVERY,
;;     (:SUM form),
;;     (:MAXIMIZE form),
;;     (:MINIMIZE form),
;;     (:APPEND form),
;;     (:NCONC form),
;;     (:DO form*)
;;
;; QUALIFIERS:
;; A qualifier may be either a generator or a filter.
;;
;;   GENERATORS:
;;   A generator has the form (x <- X)
;;   where x is either an identifier or a list, and X is a Lisp
;;   expression yielding a sequence of type list, string, or vector.
;;
;;   When x is a symbol, it defines a new local variable. When x is a
;;   list, it denotes that the sequence elements need to be
;;   destructured.
;;
;;   Generators use operators <- <-// and <.. A sequence of generator
;;   qualifiers implies a Cartesian product space. Generator <-// is
;;   provided to perform parallel comprehension instead of a Cartesian
;;   product.
;;
;;   (var <- seq)                                         ;; a generator
;;   ((var1 var2 ...) <- seq)                             ;; a destructuring generator
;;   ((var1 var2 ...) <-// seq1 seq2 ...)                 ;; a parallel generator
;;   ((var1 (var2 var3 ...) var4 ...) <-// seq1 seq2 ...) ;; destructuring parallel generator
;;   (var <.. start end &optional incr)                   ;; a numeric generator
;;   (var <-f form)                                       ;; a let-binding generator
;;
;;   FILTERS:
;;   A filter is a Boolean expression typically based on the variables
;;   defined by generators.
;;
;; Using MAP in generators to allow the use of lists and vectors,
;; interchangeably.
;;
;; -----------------------------------------------------------------------

(defpackage #:list-comprehensions
  (:use #:common-lisp)
  (:nicknames #:lc)
  (:export
   #:lc
   ))

(in-package #:list-comprehensions)

;; -----------------------------------------------------------------------

(defun op-eq (sym op-sym)
  (and (symbolp sym)
       (string= (string sym) (string op-sym))))

;;; h is the head and qs the list of qualifiers
(defmacro lc (h &rest qs)
  (cond
    ((eql h :count)
     (let ((count (gensym "count")))
       `(prog ((,count 0))
          (declare (fixnum ,count))
          ,(flcr h qs count)
          (return ,count))
       ))

    ((member h '(:every :notany))
     `(block nil
        ,(flcr h qs nil)
        (return t)))

    ((member h '(:some :notevery))
     `(block nil
        ,(flcr h qs nil)
        (return nil)))

    ((and (consp h)
          (eql (car h) :do))
     `(block nil
        ,(flcr h qs nil)
        (return nil)))

    ((and (consp h)
          (member (car h) '(:maximize :minimize)))
     (let ((val (gensym "val")))
       `(prog (,val)
          ,(flcr h qs val)
          (return ,val))))

    ((and (consp h)
          (eql (car h) :sum))
     (let ((val (gensym "val")))
       `(prog ((,val 0))
          (declare (real ,val))
          ,(flcr h qs val)
          (return ,val))))
    
    (t 
     (let ((tail  (gensym "tail"))
           (rhead (gensym "rhead")))
       `(prog* ((,tail  (list nil))
                (,rhead ,tail))
          (declare (list ,rhead ,tail))
          ,(flcr h qs tail)
          (return (cdr ,rhead))
          )))
    ))

#|
(lc (:sum x) (x <- '(2 4 6)) (evenp x))
(lc (:sum x) (x <.. 0 10) (evenp x))
|#

(defun flcr (h qs tail)
  ;; tail is the identifier to access the end
  ;; of the resulting list.
  (if (null qs)
      (cond
       ((eql h :count)
        `(incf ,tail))

       ((eql h :some)
        `(return t))

       ((member h '(:every :notevery))
        'nil)

       ((eql h :notany)
        `(return nil))

       ((and (consp h)
             (member (car h) '(:minimize :maximize)))
        (let ((gval (gensym))
              (minmax (if (eql (car h) :minimize)
                          'min
                        'max)))
          `(let ((,gval ,(cadr h)))
             (setf ,tail (if ,tail
                             (,minmax ,tail ,gval)
                           ,gval)))))

       ((and (consp h)
             (eql (car h) :sum))
        `(incf ,tail ,(cadr h)))

       ((and (consp h)
             (eql (car h) :do))
        ;; Do something, but don't collect anything
        `(block nil
           ,@(cdr h)))

       ((and (consp h)
             (eql (car h) :append))
        `(setq ,tail (last (rplacd ,tail (copy-list ,(cadr h))))))

       ((and (consp h)
             (eql (car h) :nconc))
        `(setq ,tail (last (rplacd ,tail ,(cadr h)))))

       (t
        `(setq ,tail (cdr (rplacd ,tail (list ,h))))) )
    
    ;; else 
    (let* ((q1 (first qs))
           (qr (rest qs))
           (op (cadr q1)))

      (labels ((destr (dargs gargs)
                 (if (endp dargs)
                     (flcr h qr tail)
                   `(destructuring-bind ,(car dargs) ,(car gargs)
                      ,@(when (find '_ (car dargs) :test 'op-eq)
                          `((declare (ignore ,(intern "_")))))
                      ,(destr (cdr dargs) (cdr gargs)))
                   ))
               (mapper-form (args seqs)
                 (let (dargs
                       gargs)
                   `(map nil (lambda ,(mapcar (lambda (arg)
                                                (if (consp arg)
                                                    (let ((garg (gensym)))
                                                      (push arg  dargs)
                                                      (push garg gargs)
                                                      garg)
                                                  arg))
                                              args)
                               ,(destr (nreverse dargs) (nreverse gargs)))
                         ,@seqs))))
        
        (cond
         ((op-eq op '<-// )  ;; parallel generation
          (destructuring-bind ((&rest args) op &rest seqs) q1
            (declare (ignore op))
            (mapper-form args seqs)))
         
         ((op-eq op '<-) ;; list generator
          (destructuring-bind (arg op seq) q1
            (declare (ignore op))
            (mapper-form (list arg) (list seq))))
         
         ((op-eq op '<-f) ;; function application
          (destructuring-bind (arg op form) q1
            (declare (ignore op))
            `(let ((,arg ,form))
               ,(flcr h qr tail))
            ))
         
         ((op-eq op '<..) ;; numeric generator
          (destructuring-bind (v op start end &optional incr) q1
            (declare (ignore op))
            `(gen-seq ,start ,end ,incr
                      (lambda (,v)
                        ,(flcr h qr tail)))
            ))
         
         (t
          (cond
           ((eql h :every)
            `(if ,q1
                 ,(flcr h qr tail)
               (return nil))) ;; filter
           
           ((eql h :notevery)
            `(if ,q1
                 ,(flcr h qr tail)
               (return t)))
           
           (t
            `(when ,q1 ,(flcr h qr tail))) ;; filter
           ))
         )))))

(defun gen-seq (start end by fn)
  (cond ((>= end start)
         (if by
             (assert (plusp by))
           (setf by 1))
         (do ((ix start (+ ix by)))
             ((>= ix end))
           (funcall fn ix)))
        (t
         (if by
             (assert (minusp by))
           (setf by -1))
         (do ((ix (+ start by) (+ ix by)))
             ((< ix end))
           (funcall fn ix)))
        ))
        
         
;; -----------------------------------------
#|
;; Lapalme's macro from Wadler's rules...
(defmacro comp ((e &rest qs) &optional l2)
  (if (null qs)
      `(cons ,e ,l2) ;; rule A
    (let ((q1  (car qs))
          (q   (cdr qs)))
      (if (not (eq (cadr q1) '<-)) ;; a generator?
          `(if ,q1
               (comp (,e ,@q) ,l2) ;; rule B
             ,l2)
        (let ((v   (car q1))
              (l1  (third q1))
              (h   (gentemp "H-"))
              (us  (gentemp "US-"))
              (us1 (gentemp "US1-")))
          `(labels
               ((,h  (,us)  ; a letrec
                  (if (null ,us)
                      ,l2
                    (let ((,v   (car ,us))
                          (,us1 (cdr ,us)))
                      (comp (,e ,@q) (,h ,us1))))))
             (,h ,l1))
          )))))

#|
(let ((i50000 (lce (x (x <.. 1 50000)))))
  (comp (x (x <- i50000) (y <- '(a b c)) (> 0 x))))
(defun tst ()
  #3F
  (let ((i5000 (lce (x (x <.. 1 5000)))))
    (time
     (lc2 (x (x <- i5000) (y <- i5000) (<= y 10)))
     )))
(let ((i5000 (lce (x (x <.. 1 5000)))))
  (lc2 (x (x <- i5000) (y <- i5000) (<= y 10))))
(lce (x (x <.. 1 5000)))

(loop for x across arr do (diddly x))

;; Pythagorean triples
(lc (list x y z)
    (x <.. 1 100)
    (y <.. (1+ x) 100)
    (z <.. (1+ y) 100)
    (= (* z z) (+ (* x x) (* y y))))

(lc x
    ((x y) <-//
     (vector 1 2 3)
     (list 'a 'b 'c)))

(lc x
    (x <-
       (vector 1 2 3)))

(lc (list a b)
    ((a (b c) (d e)) <-// seq1 seq2 seq3) (tst a d))

(loop for x in lst append x)

(defun tst (n)
  (time 
   (loop repeat n do
         (lc (list x y z)
             (x <.. 1 100)
             (y <.. (1+ x) 100)
             (z <.. (1+ y) 100)
             (= (* z z) (+ (* x x) (* y y)))))))
|#
|#
