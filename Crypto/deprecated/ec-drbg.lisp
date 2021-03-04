;; ec-drbg.lisp -- dual ECC deterministic random number generator
;;
;; -------------------------------------------------------------

(in-package :ecc-crypto-b571)

;; ------------------------------------------------------------------------

(defstruct ec-drbg-state
  seed reseed curve-p curve-q pt)

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
      (let ((tmp (make-ub-array 64)))
        (loop for byte from 0 below nel by 32 do
              (let ((nb (min 32 (- nel byte))))
                (read-sequence tmp fp)
                (replace ent tmp
                         :start1 byte
                         :end1 (+ byte nb)) ))))
    (mod (convert-bytes-to-int (mask-off ent (rem *nbits* 8))) *ecc-r*)))

(defmethod reseed-drbg ((state ec-drbg-state))
  (with-accessors ((seed    ec-drbg-state-seed)
                   (reseed  ec-drbg-state-reseed)
                   (curve-p ec-drbg-state-curve-p)
                   (pt      ec-drbg-state-pt)
                   (curve-q ec-drbg-state-curve-q)) state
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
              pt      (with-ecc-curve curve-p
                       (ecc-mul *ecc-gen* seed))
              curve-q (choose-random-ecc-curve)) ))) ))

(defun make-new-ec-drbg-state ()
  (let ((state (make-ec-drbg-state)))
    (reseed-drbg state)
    state))

(def-cached-var ec-drbg-state
  (make-new-ec-drbg-state))

;; --------------------------------------------------

(defmethod next-block ((state ec-drbg-state))
  (with-accessors ((reseed  ec-drbg-state-reseed)
                   (curve-p ec-drbg-state-curve-p)
                   (pt      ec-drbg-state-pt)
                   (curve-q ec-drbg-state-curve-q)) state
    (let* ((spt  (with-ecc-curve curve-p
                   (ecc-add *ecc-gen* pt)))
           (r    (ecc-pt-x (with-ecc-curve curve-q
                             (ecc-mul *ecc-gen* (ecc-pt-x pt))))))
      (setf pt spt)
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

(defstruct ec-drbg-buf
  (buf  (make-ub-array 512))
  (nb   0)
  (put  0)
  (get  0)
  (lock (mp:make-lock)))

(def-cached-var ec-drbg-buf
                (make-ec-drbg-buf))

(defmethod put-buf ((db ec-drbg-buf) src)
  (with-accessors  ((buf    ec-drbg-buf-buf)
                    (put-ix ec-drbg-buf-put)
                    (navail ec-drbg-buf-nb)
                    (lock   ec-drbg-buf-lock)) db
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
  (mp:process-run-function "EC-DRBG Thread" nil
                           (lambda ()
                             (loop do
                                   (put-buf (ec-drbg-buf)
                                            (next-block (ec-drbg-state))) ))))

(def-cached-var ensure-drbg-thread (make-drbg-thread))

(defmethod get-buf ((db ec-drbg-buf) nb)
  (ensure-drbg-thread)
  (with-accessors ((buf    ec-drbg-buf-buf)
                   (navail ec-drbg-buf-nb)
                   (get-ix ec-drbg-buf-get)
                   (lock   ec-drbg-buf-lock)) db
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

(defun ec-drbg (nbits)
  ;; NIST Dual EC DRBG
  (let ((ans (get-buf (ec-drbg-buf) (ceiling nbits 8))))
    (mask-off ans (rem nbits 8)) ))

(defun ec-drbg-int (nbits)
  (convert-bytes-to-int (ec-drbg nbits)))

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
                  (- (ctr-drbg-int 8) 128))
            'vector)))
    (plt:plot 'plt (fft:fwd-magnitude-db x) :clear t)
    (plt:plot 'plt2 (map 'vector #'realpart
                         (fft:inv (map 'vector (lambda (x)
                                                 (* x (conjugate x)))
                                       (fft:fwd x))))
              :clear t
              ;; :xrange '(0 400)
              :yrange '(-5e6 5e6)
              )
    (subseq x 0 500)
    ))
|#