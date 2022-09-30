;; ctr-hash-drbg.lisp -- Counter Hash DRGB
;; DM/Acudora  06/12,02/17
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

(in-package :prng)

;; ------------------------------------------------------------------------

(def-cached-var ctr-hash-prng
  (ironclad:make-prng :fortuna :seed :urandom))

(defun get-entropy (nb)
  (ironclad:random-data nb (ctr-hash-prng)))

(defun basic-random (n)
  (ironclad:strong-random n (ctr-hash-prng)))

(defun basic-random-between (lo hi)
  ;; random number in interval [lo,hi)
  (+ lo (basic-random (- hi lo))))

;; ------------------------------------------------------------------------

(defstruct ctr-hash-drbg-state
  seed key reseed ctr hash
  get
  (lock (mpcompat:make-lock)))

(defun basic-random-key-256 ()
  (convert-int-to-nbytesv (basic-random (ash 1 256)) 32))

#|
#+:WIN32
(defun get-entropy (nb)
  ;; needs a source of entropy
  (random-key-256))

#+:MAC
(defun get-entropy (nb)
  (let ((ent (make-ub-array nb)))
    (with-open-file (fp "/dev/random"
                        :direction :input
                        :element-type 'ubyte)
      (read-sequence ent fp))
    ent))
|#
;; ----------------------------------

(defmethod reseed-ctr-hash-drbg ((state ctr-hash-drbg-state))
  (with-accessors ((key    ctr-hash-drbg-state-key)
                   (reseed ctr-hash-drbg-state-reseed)) state
    (setf key    (get-entropy 32)
          reseed (ash 1 24)) ))

;; --------------------------------------------------

(defmethod next-ctr-hash-drbg-block ((state ctr-hash-drbg-state))
  (with-accessors ((reseed ctr-hash-drbg-state-reseed)
                   (seed   ctr-hash-drbg-state-seed)
                   (key    ctr-hash-drbg-state-key)
                   (hash   ctr-hash-drbg-state-hash)
                   (ctr    ctr-hash-drbg-state-ctr)
                   (get-ix ctr-hash-drbg-state-get)) state

    (unless (plusp (decf reseed))
      (reseed-ctr-hash-drbg state))
      
    (setf get-ix 0)
    (incf ctr)
    (let ((cvec (convert-int-to-nbytesv ctr 16)))
      (reinitialize-instance hash)
      (ironclad:update-digest hash key)
      (ironclad:update-digest hash seed)
      (ironclad:update-digest hash cvec)
      (setf seed (ironclad:produce-digest hash)))))

(defun make-new-ctr-hash-drbg-state ()
  (let* ((state (make-ctr-hash-drbg-state
                 :seed   (basic-random-key-256)
                 :ctr    (convert-bytes-to-int (make-nonce))
                 :hash   (ironclad:make-digest :sha256) )))
    (reseed-ctr-hash-drbg state)
    (next-ctr-hash-drbg-block state)
    state))

(def-cached-var ctr-hash-drbg-state
  (make-new-ctr-hash-drbg-state))

(defmethod get-ctr-hash-drbg-bits ((state ctr-hash-drbg-state) nb)
  (with-accessors ((lock   ctr-hash-drbg-state-lock)
                   (buf    ctr-hash-drbg-state-seed)
                   (get-ix ctr-hash-drbg-state-get)) state
    
    (mpcompat:with-lock (lock)
      (let* ((buflen (length buf))
             (dst    (make-ub-array nb)))
        
        (um:nlet iter ((nb    nb)
                       (start 0))
          (when (plusp nb)
            (unless (< get-ix buflen)
              (next-ctr-hash-drbg-block state))
            (let* ((nel (min nb
                             (- buflen get-ix))))
              (replace dst buf
                       :start1 start  :end1 (+ start nel)
                       :start2 get-ix :end2 (+ get-ix nel))
              (incf get-ix nel)
              (go-iter (- nb nel) (+ start nel)))
            ))
        dst))))

(defun ctr-hash-drbg (nbits)
  ;; NIST Hash DRBG
  (let ((ans (get-ctr-hash-drbg-bits (ctr-hash-drbg-state) (ceiling nbits 8))))
    (mask-off ans (rem nbits 8)) ))

#|
(defun ctr-hash-drbg-int (nbits)
  (convert-bytes-to-int (ctr-hash-drbg nbits)))
|#

(unless (fboundp 'ctr-drbg)
  (setf (symbol-function 'ctr-drbg)  #'ctr-hash-drbg))

#|
(let* ((pts (loop repeat 10000 collect
                  (list (ctr-hash-drbg-int 16)
                        (ctr-hash-drbg-int 16))))
       (xs (mapcar #'first pts))
       (ys (mapcar #'second pts)))
  (plt:plot 'plt xs ys
            :clear t
            :symbol :dot))
|#

#|
(defun tst ()
  (let ((x (coerce
            (loop for ix from 1 to (ash 1 17) collect
                  (- (ctr-hash-drbg-int 8) 128))
            'vector)))
    (plt:plot 'plt (fft:fwd-magnitude-db x) :clear t)
    (plt:plot 'plt2 (map 'vector #'realpart
                         (fft:inv (map 'vector (lambda (x)
                                                 (* x (conjugate x)))
                                       (fft:fwd x))))
              :clear t
              :xrange '(-40 40)
              :yrange '(-5e7 5e7)
              )
    (subseq x 0 500)
    ))
|#

(defun ctr-drbg-int (nbits)
  (convert-bytes-to-int (ctr-drbg nbits)))

(defun random-between (lo hi)
  ;; random number in interval [lo,hi)
  (let ((rng  (abs (- hi lo)))
        (lmin (min hi lo)))
    (+ lmin (mod (ctr-drbg-int (integer-length rng))
                 rng))))

(defun field-random (base)
  (random-between 1 base))

(defun safe-field-random (base)
  ;; nondeterministic random in the range (Sqrt[base], base-Sqrt[base])
  
  ;; Safe, because if field is large enough all the values will be far
  ;; enough away from field limits (0, base) to discourage brute force
  ;; search against small positive and negative field values.
  (funcall (get-cached-symbol-data
            'safe-field-random 'safe-field-random-fn base
            (lambda ()
              (let* ((lo  (isqrt base))
                     (hi  (- base lo)))
                (lambda ()
                  (random-between lo hi))))
            )))
