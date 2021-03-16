;; MCAS-v4.lisp -- Multiple CAS on CAR/CDR of ref-cells.
;;
;; Adapted from "Efficient Multi-word Compare and Swap", by Guerraoui,
;; Kogan, Marathe, and Zablotchi
;;
;; An N-way MCAS, without contention, needs only N+1 CAS instructions.
;;
;; DM/RAL  12/20
;; -------------------------------------------------------------
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

(in-package #:mcas)

(declaim (optimize (speed 3) #|(safety 0)|# #+:LISPWORKS (float 0)))

;; -------------------------------------------------------------------------------------
;; MCAS can only be used on refs that can be sorted into total order.
;; All simple CAS ops must be performed using MCAS,
;; and all REF-VAL calls must be performed using MCAS-READ.

(defvar *mcas-index*  0)

(defstruct (mcas-ref
            (:include ref)
            (:constructor mcas-ref (val)))
  (id (incf *mcas-index*) :read-only t))

(defmethod ord:compare ((a mcas-ref) (b mcas-ref))
  (- (mcas-ref-id a) (mcas-ref-id b)))

;; ------------------
;; MCAS - Multiple CAS
;;
;; NOTE: Any ref that is used in an MCAS operation should really use
;; MCAS and MCAS-READ, even when not part of an ensemble (as in simple
;; CAS), and for querying the value.  These will detect MCAS in
;; progress and help it along for final resolution.

(defstruct (mcas-desc
            (:include ref)
            (:constructor %make-mcas-desc))
  triples)

(declaim (inline undecided-p successful-p))

(defun undecided-p (mdesc)
  (eq :undecided (ref-val mdesc)))

(defun successful-p (mdesc)
  (eq :successful (ref-val mdesc)))

(defstruct word-desc
  parent addr old new)

(defun make-mcas-desc (triples)
  (let ((desc (%make-mcas-desc
               :val :undecided)))
    (setf (mcas-desc-triples desc)
          (mapcar (lambda* ((mref old new))
                    (make-word-desc
                     :parent desc
                     :addr   mref
                     :old    old
                     :new    new))
                  triples))
    desc))

(defun read-helper (mref self)
  ;; mref must be an MCAS-REF
  (prog ()
   again
   (let ((content (ref-val mref)))
     (if (word-desc-p content)
         (let ((parent (word-desc-parent content)))
           (if (and (not (eq parent self))
                    (undecided-p parent))
               (progn
                 (mcas-help parent)
                 (go again))
             ;; else
             (return (values content
                             (if (successful-p parent)
                                 (word-desc-new content)
                               (word-desc-old content))))
             ))
       ;; else - content was not an word descriptor, just a value
       (return (values content content)))
     )))

(defun mcas-read (mref)
  ;; mref must be an MCAS-REF
  (multiple-value-bind (content value)
      (read-helper mref nil)
    (declare (ignore content))
    value))
            
(defun mcas-help (mdesc)
  ;; minimum CAS algorithm, for N locations, needs only N+1 CAS
  (declare (mcas-desc mdesc))
  (cas (ref-val mdesc) :undecided
       (if (every (lambda (wdesc)
                    (declare (word-desc wdesc))
                    (prog ()
                      again
                      (multiple-value-bind (content value)
                          (read-helper (word-desc-addr wdesc) mdesc)
                        (return (or (eq content wdesc)
                                    (and (eq value (word-desc-old wdesc))
                                         (undecided-p mdesc)
                                         (or (cas (ref-val (word-desc-addr wdesc))
                                                  content wdesc)
                                             (go again))
                                         )))
                        )))
                  (mcas-desc-triples mdesc))
           :successful
         :failed))
  (successful-p mdesc))

(defun mcas (&rest triples)
  ;; triples - a sequence of (ref old new) as would be suitable for
  ;; CAS. But each ref must be a total-order MCAS-REF.
  (mcas-help (make-mcas-desc
              (sort (apply 'um:triples triples)
                    '<
                    :key (lambda (tup)
                           (mcas-ref-id (first tup)))
                    ))))

(defmethod ref:val ((m mcas-ref))
  ;; to get the current value of an mcas-ref
  ;; always use either ref:val or mcas-read
  (mcas-read m))

(defmethod um:cas-object ((m mcas-ref) old new)
  (mcas m old new))

;; -------------------------------------------------------------------------------------
#|
(let* ((a  (mcas-ref 15))
       (b  (mcas-ref 16)))
  (mcas  a 15 32
         b 16 33)
  (list (val a) (val b)))
|#

      
