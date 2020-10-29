;; bloom-filter.lisp -- Bloom filters
;;
;; DM/RAL 11/17
;; ----------------------------------------------------------------------
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


(defpackage #:bloom-filter
  (:use #:common-lisp)
  (:import-from #:um
   #:nlet-tail
   #:if-let
   #:when-let
   #:group
   #:dlambda
   #:foreach)
  (:export
   #:bloom-filter
   #:make-bloom-filter
   #:hash32
   #:hash64
   #:add-obj-to-bf
   #:test-obj-hash
   #:test-membership
   ))


;; -----------------------------------------------------------------------

(in-package #:bloom-filter)

;; equiv to #F
(declaim  (OPTIMIZE (SPEED 3) #|(SAFETY 0)|# #+:LISPWORKS (FLOAT 0)))

;; -----------------------------------------------------------------------

(defun hash32 (obj)
  (let ((seq (loenc:encode obj))
        (dig (ironclad:make-digest :sha3/256)))
    (ironclad:update-digest dig seq)
    (ironclad:produce-digest dig)))

(defun hash64 (obj)
  (let ((seq (loenc:encode obj))
        (dig (ironclad:make-digest :sha3)))
    (ironclad:update-digest dig seq)
    (ironclad:produce-digest dig)))

;; --------------------------------------------------------------------------
;; Bloom Filter... Is this item a member of a set? No false negatives,
;; but some false positives.
;;
;; Optimal sizing: For N items, we need M bits per item, and K hashing
;; algorithms, where, for false positive rate p we have:
;;
;;  M = -1.44 Log2 p
;;  K = -Log2 p
;;
;; So, for N = 1000, p < 1%, we have M = 10 N = 10,000 bits, and K = 7
;;
;; We can use successive octets of a SHA256 hash to provide the K hash
;; functions. Since we need 10,000 bits (= M*N) in the table, round
;; that up to 16384 bits = 2^14, so we need 14 bits per hash, make
;; that 2 octets per hash. We need 7 hashes, so that is 14 octets from
;; the SHA256 hash value, which offers 32 octets. That's doable....
;;
;; Unfortunately... this mechanism is only applicable to sets as
;; collections without duplicate items. Linda expressly allows
;; duplicate data in its datastore. So this is only a partial solution
;; to keeping tuple-spaces synchronized.
;;
;; This is not a problem for missing items. If an item is found
;; missing from the other data store, then it and all duplicates will
;; be transfered across. The problem arises only for items already in
;; the other data store. We can't keep items in common up to matching
;; duplication levels using Bloom filters.

(defclass bloom-filter ()
  ((bf-nitems  ;; number of items this filter designed for
    :reader  bf-nitems
    :initarg :nitems)
   (bf-pfalse  ;; probability of false positives 
    :reader  bf-pfalse
    :initarg :pfalse)
   (bf-k       ;; number of hashings needed
    :reader  bf-k
    :initarg :bf-k)
   (bf-m       ;; number of bits in table
    :reader  bf-m
    :initarg :bf-m)
   (bf-mask    ;; bitmask for hash values
    :reader  bf-mask
    :initarg :bf-mask)
   (bf-ix-octets  ;; number of octets needed per hashing key
    :reader  bf-ix-octets
    :initarg :bf-ix-octets)
   (bf-hash
    :reader  bf-hash ;; hash function needed
    :initarg :bf-hash)
   (bf-bits       ;; actual bit table
    :accessor bf-bits
    :initarg  :bf-bits)))

(defun pfalse (n k m)
  (expt (- 1d0 (expt (- 1d0 (/ m)) (* k n))) k))

#|
(plt:fplot 'pfalse '(1 20) (lambda (k)
                             (pfalse #N100 k #N2_000))
           :clear t
           :title "Bloom Filter P_false"
           :xtitle "K"
           :ytitle "P_false"
           :ylog t)
|#

(defun make-bloom-filter (&key (nitems 1000) (pfalse 0.01) hash-fn)
  (let* ((mlg-pfalse   (- (log pfalse 2)))
         (bf-k         (ceiling mlg-pfalse))          ;; nbr hashes
         (bf-m         (um:ceiling-pwr2 (ceiling (* 1.44 nitems mlg-pfalse)))) ;; bits in table
         (bf-mask      (1- bf-m))
         (bf-ix-octets (ceiling (log bf-m 2) 8))              ;; octets of overall hash per indiv hash
         (bf-hash      (or hash-fn
                           (cond ((> (* bf-ix-octets bf-k) 64)
                                  (error "parameters exceed 64-bit hash size"))
                                 ((> (* bf-ix-octets bf-k) 32)  'hash64)
                                 (t  'hash32)
                                 ))))
    (make-instance 'bloom-filter
                   :nitems  nitems
                   :pfalse  pfalse
                   :bf-k    bf-k
                   :bf-m    bf-m
                   :bf-mask bf-mask
                   :bf-ix-octets bf-ix-octets
                   :bf-hash bf-hash
                   :bf-bits (make-array bf-m
                                        :element-type 'bit
                                        :initial-element 0))
    ))

(defun compute-bix (bf hv ix)
  ;; bf points to Bloom Filter
  ;; hv is SHA32 hash octets vector
  ;; ix is starting offset into hash octets vector
  ;; hv is treated as little-endian vector of UB8
  (logand
   (loop repeat (bf-ix-octets bf)
         for jx from ix
         for nsh from 0 by 8
         sum
         (ash (aref hv jx) nsh))
   (bf-mask bf)))
  
(defmethod add-obj-to-bf ((bf bloom-filter) obj)
  (let ((hv  (funcall (bf-hash bf) obj)))
    (loop repeat (bf-k bf)
          for ix from 0 by (bf-ix-octets bf)
          do
          (let ((bix (compute-bix bf hv ix)))
            (setf (aref (bf-bits bf) bix) 1)))
    hv))

(defmethod test-obj-hash ((bf bloom-filter) hash)
  (loop repeat (bf-k bf)
        for ix from 0 by (bf-ix-octets bf)
        do
        (let ((bix (compute-bix bf hash ix)))
          (when (zerop (aref (bf-bits bf) bix))
            (return nil)))
        finally (return :maybe)))

(defmethod test-membership ((bf bloom-filter) obj)
  (test-obj-hash bf (funcall (bf-hash bf) obj)))

#|
(let ((x1  '(1 2 :a 43 (a b c)))
      (x2  '(1 :a 43 3 (d e f)))
      (bf  (make-bloom-filter)))
  (um:lc ((:do
              (add-obj-to-bf bf obj))
          (obj <- x1)))
  (inspect bf)
  (um:lc ((test-membership bf obj)
          (obj <- x2))))
  
 |#
;; ---------------------------------------------------------
#|
(linda:on-rdp ((:full-dir-tree ?t))
  (let* ((all (um:accum acc
                (maps:iter ?t
                           #'(lambda (k v)
                             (acc (cons k v)))
                           )))
         (nel (length all))
         (bf  (make-bloom-filter :nitems nel))
         (hashes (um:accum acc
                   (dolist (file all)
                     (acc (add-obj-to-bf bf file))))))

    (linda:remove-tuples '(:all-files-bloom-filter ?x))
    (linda:remove-tuples '(:all-files ?x))
    (linda:remove-tuples '(:all-files-hashes ?x))
    
    (linda:out `(:all-files-bloom-filter ,bf))
    (linda:out `(:all-files ,all))
    (linda:out `(:all-files-hashes ,hashes))))

;; for use on Dachshund
(progn
  (linda:remove-tuples '(:other-bloom-filter ?x))
  (linda:out `(:other-bloom-filter
               ,(car (linda:remote-srdp '(:all-files-bloom-filter ?bf)
                                        "malachite.local")))))

;; for use on Malachite
(progn
  (linda:remove-tuples '(:other-bloom-filter ?x))
  (linda:out `(:other-bloom-filter
               ,(car (linda:remote-srdp '(:all-files-bloom-filter ?bf)
                                        "dachshund.local")))))

(let* ((other-bf  (car (linda:srdp '(:other-bloom-filter ?bf))))
       (hashes    (car (linda:srdp '(:all-files-hashes ?hashes))))
       (files     (car (linda:srdp '(:all-files ?files))))
       (diffs     (um:accum acc
                    (um:foreach (lambda (hash file)
                                  (unless (test-obj-hash other-bf hash)
                                    (acc (car file))))
                                hashes files))))
  (with-standard-io-syntax
    (pprint diffs))
  diffs)
|#
;; ---------------------------------------------------------
