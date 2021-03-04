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

;; ---------------------------------------------------------------

(defvar *ecc-blk-encryption*
  #.(uuid:uuid "{F2BD850E-8F25-11E2-8AEA-C82A14446EA7}"))

(defun ecc-blk-signature ()
  (uuid:uuid-to-byte-array *ecc-blk-encryption*))

;; ---------------------------------------------------------------

(defstruct ecc-enc-state
  sig nonce salt curve pt hmac)

(defun make-ecc-encryptor (key &optional nonce salt)
  (let* ((noncex (or nonce (make-nonce)))
         (saltx  (or salt (ctr-drbg 128)))
         (keyx   (kdf *nbits* key saltx noncex))
         (keyn   (convert-bytes-to-int keyx))
         (s      (gf* keyn (convert-bytes-to-int (concatenate 'vector noncex saltx))))
         (mu     (ash 1 (mod (ldb (byte 10 24) s) 571)))
         (pt     (ecc-mul *ecc-gen* keyn)))
    (if (or (zerop keyn)
            (zerop s)
            (ecc-infinite-p pt))
        (if nonce
            (error "invalid nonce")
          ;; else
          (progn
            (sleep 1)
            (make-ecc-encryptor key nil salt)))
      ;; else
      (with-accessors ((x ecc-pt-x)
                       (y ecc-pt-y)) pt
        (let* ((a (gf^ *ecc-a* mu))
               (b (gf^ *ecc-b* mu))
               (x (gf^ x mu))
               (y (gf^ y mu))
               (curve (make-ecc-curve
                       :a   (gf+ a
                                 (gf* s (gf+ s 1)))
                       :b   b
                       :gen (make-ecc-pt
                             :x x
                             :y (gf+ y (gf* s x)))))
               (sig   (ecc-blk-signature))
               (hkey  (subseq keyx 16 48))
               (hmac  (ironclad:make-hmac hkey :sha256)))
          (dolist (item (list sig saltx noncex))
            (ironclad:update-hmac hmac item))
          (with-ecc-curve curve
            (make-ecc-enc-state
             :sig   sig
             :nonce noncex
             :salt  saltx
             :curve curve
             :pt    (ecc-mul *ecc-gen* s)
             :hmac  hmac) ))))))

(defun ecc-accum-hmac (blk state)
  (with-accessors ((hmac ecc-enc-state-hmac)) state
    (ironclad:update-hmac hmac (ensure-8bitv (subseq blk 0 (length blk))))
    blk))

(defun ecc-encrypt-block (blk state)
  (with-accessors ((curve  ecc-enc-state-curve)
                   (hmac   ecc-enc-state-hmac)
                   (pt     ecc-enc-state-pt)) state
    (with-accessors ((y ecc-pt-y)) pt
      ;; Compute next point with sufficient quality.
      ;; Quality means that point is not infinity
      ;; and number of 1 bits in y must be >= 200,
      ;; and there must not be more than two consecutive
      ;; zero bytes in y.
      (with-ecc-curve curve
        (um:nlet iter ()
          (setf pt (ecc-add pt *ecc-gen*))
          (let ((yv (subseq (convert-int-to-nbytesv y 72) 4 68)))
            (if (or (ecc-infinite-p pt)
                    (< (logcount y) 200)
                    (search #(0 0 0) yv))
                (go-iter)
              (map-into blk #'logxor blk yv))) )) )))

(defun do-chunks (fn buf nchunk)
  (let ((len (length buf)))
    (loop for off from 0 below len by nchunk do
          (let* ((nel  (min nchunk (- len off)))
                 (arr  (make-array nel
                                   :element-type (array-element-type buf)
                                   :displaced-to buf
                                   :displaced-index-offset off)))
            (funcall fn arr))))
  buf)

(defun ecc-encrypt-sequence (seq key)
  (let* ((state (make-ecc-encryptor key)))
    (with-accessors ((sig   ecc-enc-state-sig)
                     (nonce ecc-enc-state-nonce)
                     (salt  ecc-enc-state-salt)
                     (hmac  ecc-enc-state-hmac)) state
      (concatenate 'vector
                   sig nonce salt
                   (do-chunks (um:compose (um:rcurry #'ecc-accum-hmac state)
                                          (um:rcurry #'ecc-encrypt-block state))
                              (ensure-8bitv seq) 64)
                   (ironclad:hmac-digest hmac)))))

(defun ecc-decrypt-sequence (seq key)
  (unless (equalp (ecc-blk-signature)
                  (subseq seq 0 16))
    (error "signature mismatch"))
  (let* ((state (make-ecc-encryptor key
                                    (ensure-8bitv (subseq seq 16 32))
                                    (ensure-8bitv (subseq seq 32 48))))
         (tlpos (- (length seq) 32))
         (dec   (do-chunks (um:compose
                            (um:rcurry #'ecc-encrypt-block state)
                            (um:rcurry #'ecc-accum-hmac state))
                           (subseq seq 48 tlpos) 64))
         (tl    (subseq seq tlpos))
         (dig   (ironclad:hmac-digest (ecc-enc-state-hmac state))))
    (unless (equalp dig tl)
      (error "hmac error"))
    dec))

#|
(defun ecc-encrypt-block (blk state)
  (with-accessors ((curve  ecc-enc-state-curve)
                   (seed   ecc-enc-state-seed)) state
    (with-ecc-curve curve
      (with-accessors ((x  ecc-pt-x)
                       (y  ecc-pt-y)) *ecc-gen*
        (um:nlet iter ((seedx (gf* 2 seed)))
          (let ((s (gf+ x seedx)))
            (if (zerop s)
                (go-iter (gf* 2 seedx))
              ;; else
              (let* ((curvex (make-ecc-curve
                              :a   (gf+ *ecc-a*
                                        (gf* s (gf+ s 1)))
                              :gen (make-ecc-pt
                                    :x x
                                    :y (gf+ y (gf* s x))) )))
                (with-ecc-curve curvex
                  (let ((ptx (ecc-mul *ecc-gen* s)))
                    (setf seed  seedx
                          curve curvex)
                    (convert-int-to-nbytes (gf+ (convert-bytes-to-int blk)
                                                (ash (ecc-pt-x ptx) -24))
                                           (length blk))
                    )))))))) ))
|#

#|
(let* ((str "
(in-package :ecc-crypto-b571)

(defstruct ecc-enc-state
  seed curve)

(defun ecc-encrypt-block (blk state)
  (with-accessors ((curve  ecc-enc-state-curve)
                   (seed   ecc-enc-state-seed)) state
    (with-ecc-curve curve
      (with-accessors ((x  ecc-pt-x)
                       (y  ecc-pt-y)) *ecc-gen*
        (um:nlet iter ((seedx  (gf* 2 seed)))
          (let ((s (gf+ x seedx)))
            (if (zerop s)
                (go-iter (gf* 2 seedx))
              ;; else
              (let* ((curvex (make-ecc-curve
                              :a   (gf+ *ecc-a*
                                        (gf* s (gf+ s 1)))
                              :gen (make-ecc-pt
                                    :x x
                                    :y (gf+ y (gf* s x))) )))
                (with-ecc-curve curvex
                  (let ((ptx (ecc-mul *ecc-gen* s)))
                    (setf seed  seedx
                          curve curvex)
                    (convert-int-to-nbytes (gf+ (convert-bytes-to-int blk)
                                                (ash (ecc-pt-x ptx) -24))
                                           (length blk))
                    )))))))) ))

(defun do-chunks (fn buf nchunk)
  (um:nlet iter ((buf buf)
                      (ans nil))
    (let ((len (length buf)))
      (if (plusp len)
          (let ((nel (min len nchunk)))
            (go-iter (um:drop nel buf)
                  (cons (funcall fn (um:take nel buf))
                        ans)))
        ;; else
        (nreverse ans)))))
")
       (key  "this is a test")
       (enc  (ecc-encrypt-sequence str key))
       (dec  (ecc-decrypt-sequence enc key)))
  (print (encode-bytes-to-base64 enc))
  (print (encode-bytes-to-base64 (ensure-8bitv dec)))
  (print (convert-bytes-to-string dec))
  (values))

(defun seq-entropy (seq)
  (let* ((arr    (make-array 256
                             :initial-element 0))
         (nb     (length seq))
         (buf    seq))
    (loop for ix from 0 below nb do
          (let ((jx (aref buf ix)))
            (incf (aref arr jx)) ))
    
    (labels ((compute (arr &rest plt-args)
               (map-into arr (um:rcurry #'/ (reduce #'+ arr)) arr)
               (map-into arr (lambda (p)
                               (if (plusp p)
                                   (- (* p (log p 2)))
                                 0))
                         arr)
               (apply #'plt:plot 'plt arr :linestyle :steps plt-args)
               (/ (reduce #'+ arr) 8)))
      (compute arr :clear t))))

(let* ((n 16)
       (pt1 (ecc-add *ecc-gen* *ecc-gen*))
       (pt1x (make-ecc-pt
              :x (gf^ (ecc-pt-x pt1) n)
              :y (gf^ (ecc-pt-y pt1) n)))
       (pt2 (with-ecc-curve (make-ecc-curve
                             :a  (gf^ *ecc-a* n)
                             :b  (gf^ *ecc-b* n)
                             :gen (make-ecc-pt
                                   :x (gf^ (ecc-pt-x *ecc-gen*) n)
                                   :y (gf^ (ecc-pt-y *ecc-gen*) n)))
              (ecc-add *ecc-gen* *ecc-gen*))))
  (ecc-pt= pt1x pt2))

|#
                          