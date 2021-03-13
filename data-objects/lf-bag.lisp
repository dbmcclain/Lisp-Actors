;; lf-bag.lisp -- SMP-safe lock-free unordered collections
;;
;; DM/RAL 03/17
;; ---------------------------------------------------------
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

(defpackage #:lf-bag
  (:use #:common-lisp)
  (:export
   #:make-bag
   #:bag-contents
   #:bag-send
   #:bag-read
   #:bag-stuff
   #:bag-trim
   ))

(in-package #:lf-bag)

;; ====================================================================

(declaim (optimize (speed 3) #|(safety 0)|# #+:LISPWORKS (float 0)))

;; -------------------------------------------------------------------

(defstruct bag
  lst)

;; ------------------------------------------------------------------

(defun bag-contents (q)
  (declare (bag q))
  ;; return bag contents as a list, clearing bag.
  (sys:atomic-exchange (bag-lst q) nil))

(defun bag-stuff (q items)
  ;; prepend items on the bag
  (declare (bag  q)
           (list items))
  (when items
    (let (ilast)
      (loop for stuff = (bag-contents q)
            do
            (when stuff
              (setf ilast (last (the cons (or ilast items)))
                    (cdr (the cons ilast)) stuff))
            until (sys:compare-and-swap (bag-lst q) nil items))
      )))

(defun bag-trim (q pred)
  ;; delete items from the bag that satisfy pred
  ;; pred will never be called on more than once
  ;; for each item in the bag
  (declare (bag  q))
  (let (items
        ilast)
    (loop for stuff = (delete-if pred (the list (bag-contents q)))
          do
          (when stuff
            (cond (items
                   (setf ilast (last (the cons (or ilast items)))
                         (cdr (the cons ilast)) stuff))
                  (t
                   (setf items stuff))))
          until (sys:compare-and-swap (bag-lst q) nil items))
    ))

(defun bag-send (q item)
  (declare (bag q))
  (sys:atomic-push item (bag-lst q)))

(defun bag-read (q)
  (declare (bag q))
  (sys:atomic-pop (bag-lst q)))

#|
(tst4) ;; this needed to force BG Exec threads to be created
(progn
  (defvar *q* (make-bag))

  (defun tst ()
    (if (< (random 10) 5)
        (bag-read *q*)
      (bag-send *q* (random 10))))

  (defun tsts (n)
    (loop repeat n do (tst)))

  (defun tst1 (&optional (n 1000000))
    (time (progn
            (tsts n)
            (length (bag-contents *q*)))))

  (defun tst2 (&optional (n 1000000))
    (time
     (progn
       (um:par
         (tsts n)
         (tsts n)
         )
       (length (bag-contents *q*)))))

  (defun tst3 (&optional (n 1000000))
    (time
     (progn
       (um:par
         (tsts n)
         (tsts n)
         (tsts n)
         )
       (length (bag-contents *q*)))))

  (defun tst4 (&optional (n 1000000))
    (time
     (progn
       (um:par
         (tsts n)
         (tsts n)
         (tsts n)
         (tsts n)
         )
       (length (bag-contents *q*)))))

  ) ;; progn
|#

