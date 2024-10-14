;; siphash.lisp -- DOS secure non-cryptographic hashing
;; DM/RAL  06/15
;; ----------------------------------------------------------------
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

(in-package :ecc-crypto-b571)

;; ---------------------------------------------------------------------------------
;; SipHash-c,d from Aumasson and Bernstein

(defvar *shkey* nil)

(defun ensure-signed-int64 (v)
  (if (>= v #.(ash 1 63))
      (- v #.(ash 1 64))
    v))

(defun back-to-unsigned (x)
  (if (minusp x)
      (+ x #.(ash 1 64))
    x))

(defun prepare-siphash-key (&optional key)
  (let* ((key (if (integerp key)
                  key
                (ctr-drbg-int 128)))
         (k0  (ldb (byte 64  0) key))
         (k1  (ldb (byte 64 64) key)))
    (mapcar (lambda (k v)
              (ensure-signed-int64
               (logxor k v)))
            (list k0 k1 k0 k1)
            '(#x736f6d6570736575 ;; to introduce asymmetry
              #x646f72616e646f6d
              #x6c7967656e657261
              #x7465646279746573)) ))
  
(defun do-with-siphash-key (key fn)
  ;; allow either a pre-prepared key list
  ;; or a 128 bit keying integer
  ;; or anything else which causes a random init
  (let ((*shkey* (if (consp key)
                     key
                   (prepare-siphash-key key))))
    (funcall fn)))

(defmacro with-siphash-key (key &body body)
  `(do-with-siphash-key ,key (lambda () ,@body)))

(defun rekey-siphash ()
  (setf *shkey* (prepare-siphash-key)))

(rekey-siphash)

;; --------------------------------------------------------------

(defun cvt-bytes-to-64w (itemv)
  (if itemv
      (do ((v (let* ((nb (length itemv))
                     (nx (- 8 (mod nb 8)))
                     (xt (nreverse
                          (cons (mod nb 256)
                                (make-list (1- nx)
                                           :initial-element 0)))))
                (um:group (append (coerce itemv 'list) xt) 8))
              (cdr v))
           (ans nil))
          
          ((endp v) (nreverse ans))
        
        (let ((w  (do ((sum 0)
                       (lst (reverse (car v)) (cdr lst)))
                      ((endp lst) sum)
                    (setf sum (logior (car lst) (ash sum 8))))
                  ))
          (push (ensure-signed-int64 w) ans)))
    ;; else
    (list 0)))

;; --------------------------------------------------------------
;; NOTE: the following macros are written with intentional
;; obliviousness to the possibility of multiple evaluation
;; of arguments. These are intended for use against atomic arguments.

(defmacro mreduce (op arg &rest args)
  ;; macro version of reduce
  ;; to avoid function calling and to permit
  ;; compiler to optimize out intermediates in the INT64
  (if args
      `(,op ,arg (mreduce ,op ,@args))
    arg))

(defmacro += (a b)
  `(setf ,a (sys:int64+ ,a ,b)))

(defmacro ^= (a b)
  `(setf ,a (sys:int64-logxor ,a ,b)))

(defmacro cnot (x)
  `(sys:int64-logxor ,x
                     (sys:int64>> ,x 63)))

(defmacro <<<= (a b)
  (let ((bc  (- 64 b)))
    `(setf ,a
           (sys:int64-logxor
            (sys:int64<< (cnot ,a) ,b)
            (sys:int64>> ,a ,bc)))
    ))

#| ;; useful functions...
 
(defun .x (v)
  (write v :base 16))

(defun cnot (x)
  ;; cnot = conditional not
  ;; if x > 0 then x else (not x)
  (sys:int64-to-integer
   (sys:int64-logxor
    (sys:int64>> x 63)
    x)))

(defun rotl (x n)
  ;; rotate x left n bits
  ;; .. when you only have signed right shift
  (sys:int64-to-integer
   (sys:int64-logxor
    (sys:int64<< (cnot x) n)
    (sys:int64>> x (- 64 n)))))

(defun rotr (x n)
  ;; rotate x right by n bits
  ;; ... when you only have signed right shift
  (sys:int64-to-integer
   (sys:int64-logxor
    (sys:int64<< (cnot x) (- 64 n))
    (sys:int64>> x n))))
|#

;; --------------------------------------------------------------

(defun siphash (itemv &key (c 2) (d 4))
  ;; #F
  (declare (optimize #+:LISPWORKS (float 0) (safety 0))
           (type fixnum c d))

  (destructuring-bind (v0 v1 v2 v3) *shkey*

    (declare (type sys:int64 v0 v1 v2 v3)
             (dynamic-extent v0 v1 v2 v3))

    (flet (#|
           (dump-state (mk)
             (princ mk)
             (princ #\space)
             (write (mapcar (lambda (v)
                              (back-to-unsigned
                               (if (integerp v)
                                   v
                                 (sys:int64-to-integer v))))
                            (list v0 v1 v2 v3))
                    :base 16)
             (terpri))
           |#

           (sipround ()
             (+=   v0 v1)
             (+=   v2 v3)
             (<<<= v1 13)
             (<<<= v3 16)
             (^=   v1 v0)
             (^=   v3 v2)
             (<<<= v0 32)
             (+=   v2 v1)
             (+=   v0 v3)
             (<<<= v1 17)
             (<<<= v3 21)
             (^=   v1 v2)
             (^=   v3 v0)
             (<<<= v2 32)
             ))
        
      (let ((ms (cvt-bytes-to-64w (ensure-8bitv itemv))))
        
        (declare (dynamic-extent ms))
        
        (macrolet ((siprounds (n)
                     `(loop repeat ,n do
                            (sipround))))

          #|
          (dump-state :1)
          (write ms :base 16)
          (terpri)
          |#
          ;; -----------------------------------
          ;; Compression rounds on each word of input
          (dolist (mi ms)
            (^= v3 mi)
            ;; (dump-state :2)
            
            (siprounds c)
            ;; (dump-state :3)
            
            (^= v0 mi)
            ;; (dump-state :4)
            )
          ;; -------------------------------------
          ;; Finalization rounds
          (^= v2 #xff)
          ;; (dump-state :5)
          
          (siprounds d)
          ;; (dump-state :6)
          
          (back-to-unsigned
           (sys:int64-to-integer
            (mreduce sys:int64-logxor v0 v1 v2 v3)))
          )))))

#|
(defun tst ()
  (with-siphash-key #x0f0e0d0c0b0a09080706050403020100
    (fast-siphash #(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14))))
;; from Aumasson and Bernstein
;; result should be #xA129CA6149BE45E5
 |#

(fli:define-foreign-function (c_siphash_init #.(c-name "c_siphash_init") :source)
    ((c   :int32)
     (d   :int32)
     (v0  :int64)
     (v1  :int64)
     (v2  :int64)
     (v3  :int64))
  :result-type :void
  :language :ansi-c
  :module :cryptolib)

(fli:define-foreign-function (c_siphash_compression #.(c-name "c_siphash_compression") :source)
    ((w   :uint64))
  :result-type :void
  :language :ansi-c
  :module :cryptolib)

(fli:define-foreign-function (c_siphash_finalization #.(c-name "c_siphash_finalization") :source)
    ()
  :result-type :uint64
  :language :ansi-c
  :module :cryptolib)

;; lock required because C module uses global static vars to hold
;; internal state
(defvar *siphash-lock*
  (mp:make-lock :name "SipHash Lock"))

(defun fast-siphash (itemv &key (c 2) (d 4))
  #F
  (declare (type fixnum c d))
  
  (let ((ms (cvt-bytes-to-64w (ensure-8bitv itemv))))
    (destructuring-bind (v0 v1 v2 v3) *shkey*

      (mp:with-lock (*siphash-lock*)
        (c_siphash_init c d
                        v0 v1 v2 v3)
        (dolist (mi ms)
          (c_siphash_compression mi))
        
        (c_siphash_finalization))
      )))

;; -----------------------------------------------------------------------

(fli:define-c-struct siphash_state
  (k0     :uint64)
  (k1     :uint64)
  (v0     :uint64)
  (v1     :uint64)
  (v2     :uint64)
  (v3     :uint64)
  (c      :int32)
  (d      :int32))

(fli:define-foreign-function (c_siphash #.(c-name "c_siphash") :source)
    ((state (:pointer siphash_state))
     (wds   (:pointer :int64))
     (nwds  :int32))
  :result-type :uint64
  :language :ansi-c
  :module :cryptolib)

(defvar *csiphash-key* (ctr-drbg-int 128))

(defun csiphash(itemv &key (c 2) (d 4))
  (let* ((ms  (cvt-bytes-to-64w (ensure-8bitv itemv)))
         (nms (length ms)))
    (fli:with-dynamic-foreign-objects ((state siphash_state))
      (let* ((wds (fli:allocate-dynamic-foreign-object
                   :type :int64 :nelems nms
                   :initial-contents ms)))
        (setf (fli:foreign-slot-value state 'k0) (ldb (byte 64  0) *csiphash-key*)
              (fli:foreign-slot-value state 'k1) (ldb (byte 64 64) *csiphash-key*)
              (fli:foreign-slot-value state 'c)  c
              (fli:foreign-slot-value state 'd)  d)
        (c_siphash state wds nms))) ))

#|
(defun ctst ()
  (let ((*csiphash-key* #x0f0e0d0c0b0a09080706050403020100))
    (csiphash #(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14))))
;; from Aumasson and Bernstein
;; result should be #xA129CA6149BE45E5
 |#

#| ;; Python randomized hash breaker...
 
(defconstant +pymask+ #.(1- (ash 1 64)))

(defun bytes-hash (p prefix suffix)
  (if (zerop (length p))
      0
    (let ((x (logxor prefix (ash (char-code (char p 0)) 7))))
      (loop for c across p do
            (setf x (logand +pymask+ (logxor (* x 1000003) (char-code c)))))
      (setf x (logxor x (length p) suffix))
      (logand +pymask+
              (if (= x -1)
                  -2
                x)))))

(defvar *pysols*  nil)

(defun solvebit (h1 h2 prefix bits)
  (when (zerop bits)
    (setf *pysols* nil))
  (let* ((f1 1000003)
         (f2 (* f1 f1))
         (target (logxor h1 h2 3)))
    (if (= bits 64)
        (cond ((not (zerop (logand +pymask+
                                   (logxor (* f1 prefix)
                                           (* f2 prefix)
                                           target))))
               *pysols*)

              (t (push (list prefix
                             (logand +pymask+
                                     (logxor h1 1 (* f1 prefix))))
                       *pysols*)))
      ;; else
      (cond ((not (zerop (logand (1- (ash 1 bits))
                                 (logxor (* f1 prefix)
                                         (* f2 prefix)
                                         target))))
             *pysols*)

            (t
             (solvebit h1 h2 prefix (1+ bits))
             (solvebit h1 h2 (+ prefix (ash 1 bits)) (1+ bits)))
            ))))

(let* ((spref  #x0f0e0d0c0b0a0908)
       (ssuf   #x0706050403020100)
       (h1     (bytes-hash (make-string 1) spref ssuf))
       (h2     (bytes-hash (make-string 2) spref ssuf))
       (h3     (bytes-hash "python" spref ssuf)))
  (format t "~%h1: ~16,'0x" h1)
  (format t "~%h2: ~16,'0x" h2)
  (format t "~%h3: ~16,'0x" h3)
  (solvebit h1 h2 0 0)
  (format t "~%~A candidate solutions" (length *pysols*))
  (loop for sol in *pysols* do
        (destructuring-bind (pref suf) sol
          (um:if-let (ok (= (bytes-hash "python" pref suf)
                            (bytes-hash "python" spref ssuf)))
              (progn
                (loop for len from 1 to 10 do
                      (let ((s (make-string len :initial-element (code-char 2))))
                        (if (/= (bytes-hash s pref suf)
                                (bytes-hash s spref ssuf))
                            (setf ok nil))))
                (when ok
                  (format t "~%solution: ~16,'0x ~16,'0x" pref suf)))))
        ))

;; produces...
;; h1: 54B92F92026E6C19
;; h2: B5CAE3FEB4EABC4A
;; h3: EB2CEAC622D51DB0
;; 24 candidate solutions
;; solution: 8F0E0D0C0B0A0908 8706050403020100
;; solution: 0F0E0D0C0B0A0908 0706050403020100

|#

#|
(defun chk-collisions (nbits)
  (let* ((tbl (make-hash-table))
         (tree (car *xptrees*))
         (ans  nil)
         (mask (1- (ash 1 nbits))))
    (flet ((add-str (str)
             (let* ((k (logand mask (csiphash str))))
               (setf (gethash k tbl)
                     (cons str (gethash k tbl))))))
      
    (um:nlet iter ((top (car tree)))
      (when top
        (if (stringp (car top))
            (add-str (car top))
          ;; else
          (iter (car top)))
        (if (stringp (cadr top))
            (add-str (cadr top))
          ;; else
          (iter (cadr top)))))
    (maphash (lambda (k v)
               (when (> (length v) 2)
                 (push (list k v) ans)
                 (print (list k v))))
             tbl)
    (setf ans (sort ans '> :key (lambda (item)
                                  (length (cadr item)))))
    )))
            
|#
