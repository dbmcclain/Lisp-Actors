;; ctr-ec-drbg.lisp -- dual ECC deterministic random number generator
;; Fix the problem of limit cycles in Dual EC-DRBG by incorporating a counter
;; just like in all the other DRBG models.
;;
;; -------------------------------------------------------------

(in-package :ecc-crypto-b571)

;; ------------------------------------------------------------------------

(defstruct ctr-ec-drbg-state
  seed reseed curve-p curve-q ctr)

#+:WIN32
(defun get-entropy571 ()
  ;; needs a source of entropy
  (ecc-random-key))

#+:MAC
(defun get-entropy571 ()
  (let* ((nel (ceiling *nbits* 8))
         (ent (make-ub-array nel)))
    (with-open-file (fp "/dev/random"
                        :direction :input
                        :element-type 'ubyte)
      (loop for byte from 0 below nel by 32 do
            (let ((nb (min 32 (- nel byte))))
              (read-sequence ent fp :start byte :end (+ byte nb)) )))
    (convert-bytes-to-int (mask-off ent (rem *nbits* 8))) ))

(defmethod reseed-drbg ((state ctr-ec-drbg-state))
  (with-accessors ((seed    ctr-ec-drbg-state-seed)
                   (reseed  ctr-ec-drbg-state-reseed)
                   (curve-p ctr-ec-drbg-state-curve-p)
                   (curve-q ctr-ec-drbg-state-curve-q)) state
    (um:nlet-tail iter ()
      (let* ((ent   (get-entropy571))
             (ran   (ecc-random-key))
             (ns    (logxor ent ran)))
      (if (zerop ns)
          (iter)
        ;; choose a random isomorphic curve
        ;; for second curve, based off a random point on C(a,b)
        ;; NOTE: second curve will have same Trace as parent curve.
        (setf seed    ns
              reseed  (ash 1 30)
              curve-p (choose-random-ecc-curve)
              curve-q (choose-random-ecc-curve)) ))) ))

(defun make-new-ctr-ec-drbg-state ()
  (let ((state (make-ctr-ec-drbg-state
                :ctr  (convert-bytes-to-int (make-nonce)))))
    (reseed-drbg state)
    state))

(def-cached-var ctr-ec-drbg-state
  (make-new-ctr-ec-drbg-state))

;; --------------------------------------------------

(defmethod next-block ((state ctr-ec-drbg-state))
  (with-accessors ((reseed  ctr-ec-drbg-state-reseed)
                   (seed    ctr-ec-drbg-state-seed)
                   (ctr     ctr-ec-drbg-state-ctr)
                   (curve-p ctr-ec-drbg-state-curve-p)
                   (curve-q ctr-ec-drbg-state-curve-q)) state
    (incf ctr)
    (let* ((ns   (ecc-pt-y (with-ecc-curve curve-p
                             (ecc-mul *ecc-gen* (+ seed ctr)))))
           (r    (ecc-pt-y (with-ecc-curve curve-q
                             (ecc-mul *ecc-gen* ns)))))
      (setf seed ns)
      (decf reseed)
      (when (zerop reseed)
        (reseed-drbg state))
      #|
      (let ((nb (truncate *nbits* 16))) ;; half the bits measured in bytes
        (basic-hash-with-protocol :sha256
                                  reseed
                                  (convert-int-to-bytes
                                   (ldb (byte 285 1) r) ;; remove LSB (always a Trace bit)
                                   ) ))
      |#
      ;; (basic-hash-with-protocol :sha256 r)
      (convert-int-to-nbytes (ldb (byte 312 8) r) 32)
      )))

(defstruct ctr-ec-drbg-buf
  (buf  (make-ub-array 512))
  (nb   0)
  (put  0)
  (get  0)
  (lock (mp:make-lock)))

(def-cached-var ctr-ec-drbg-buf
                (make-ctr-ec-drbg-buf))

(defmethod put-buf ((db ctr-ec-drbg-buf) src)
  (with-accessors  ((buf    ctr-ec-drbg-buf-buf)
                    (put-ix ctr-ec-drbg-buf-put)
                    (navail ctr-ec-drbg-buf-nb)
                    (lock   ctr-ec-drbg-buf-lock)) db
    (let ((buflen (length buf)))
      (um:nlet-tail iter ((nb    (length src))
                          (start 0))
        (when (plusp nb)
          (if (< navail buflen)
              (let* ((nel (min nb
                               (- buflen navail)
                               (- buflen put-ix))))
                (replace buf src
                         :start1 put-ix :end1 (+ put-ix nel)
                         :start2 start  :end2 (+ start nel))
                (mp:with-lock (lock)
                  (incf navail nel)
                  (setf put-ix (mod (+ put-ix nel) buflen)))
                (iter (- nb nel) (+ start nel)))
            ;; else
            (progn
              (mp:process-wait "Waiting to replace entropy"
                               (lambda ()
                                 (< navail buflen)))
              (iter nb start)) ))) )))

(defun make-drbg-thread ()
  (mp:process-run-function "CTR-EC-DRBG Thread" nil
                           (lambda ()
                             (loop do
                                   (put-buf (ctr-ec-drbg-buf)
                                            (next-block (ctr-ec-drbg-state))) ))))

(def-cached-var ensure-drbg-thread (make-drbg-thread))

(defmethod get-buf ((db ctr-ec-drbg-buf) nb)
  (ensure-drbg-thread)
  (with-accessors ((buf    ctr-ec-drbg-buf-buf)
                   (navail ctr-ec-drbg-buf-nb)
                   (get-ix ctr-ec-drbg-buf-get)
                   (lock   ctr-ec-drbg-buf-lock)) db
    (let* ((buflen (length buf))
           (dst    (make-ub-array nb)))
      
      (um:nlet-tail iter ((nb    nb)
                          (start 0))
        (if (plusp nb)
            (if (plusp navail)
                  (let* ((nel (min nb navail
                                   (- buflen get-ix))))
                    (replace dst buf
                             :start1 start :end1 (+ start nel)
                             :start2 get-ix :end2 (+ get-ix nel))
                    (mp:with-lock (lock)
                      (setf get-ix (mod (+ get-ix nel) buflen))
                      (decf navail nel))
                    (iter (- nb nel) (+ start nel)))
                ;; else
                (progn
                  (mp:process-wait "Waiting for more entropy"
                                   (lambda ()
                                     (plusp navail)))
                  (iter nb start)) )))
      dst)))

(defun ctr-ec-drbg (nbits)
  ;; NIST Dual EC DRBG
  (let ((ans (get-buf (ctr-ec-drbg-buf) (ceiling nbits 8))))
    (mask-off ans (rem nbits 8)) ))

#|
(defun ctr-ec-drbg-int (nbits)
  (convert-bytes-to-int (ctr-ec-drbg nbits)))
|#

(unless (fboundp 'ctr-drbg)
  (setf (symbol-function 'ctr-drbg)  #'ctr-ec-drbg))


#|
  ;; show that special bindings are thread-specific
(defun tst ()  
  (let* ((strm *standard-output*)
         (thrd (mp:process-run-function "Tester" ()
                                        (lambda ()
                                          (loop do
                                                (sleep 1)
                                                (format strm "~&A = ~A" *ecc-a*))))))
    (sleep 2)
    (with-ecc-curve 15
      (format strm "~&Setting A = ~A" *ecc-a*)
      (sleep 5))
    (format strm "~&Unsetting A")
    (sleep 2)
    (mp:process-kill thrd)))
|#

#|
(defun tst ()
  (let ((x (coerce
            (loop for ix from 1 to 16384 collect
                  (- (ctr-ec-drbg-int 8) 128))
            'vector)))
    (plt:plot 'plt (fft:fwd-magnitude-db x) :clear t)
    (plt:plot 'plt2 (map 'vector #'realpart
                         (fft:inv (map 'vector (lambda (x)
                                                 (* x (conjugate x)))
                                       (fft:fwd x))))
              :clear t
              ;; :xrange '(0 400)
              ;; :yrange '(-5e6 5e6)
              )
    (subseq x 0 500)
    ))
|#