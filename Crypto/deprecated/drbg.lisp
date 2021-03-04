;; drbg.lisp -- dual ECC deterministic random number generator
;;
;; -------------------------------------------------------------

(in-package :ecc-crypto-b571)

;; ------------------------------------------------------------------------

(defstruct drbg-state
  seed reseed pa pt-p qa pt-q)

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
                (replace ent (kdf (* 8 nb) tmp)
                         :start1 byte :end1 (+ byte nb)) ))))
    (mod (convert-bytes-to-int (mask-off ent (rem *nbits* 8))) *ecc-r*)))

(defmethod reseed-drbg ((state drbg-state))
  (with-accessors ((seed   drbg-state-seed)
                   (reseed drbg-state-reseed)
                   (pa     drbg-state-pa)
                   (pt-p   drbg-state-pt-p)
                   (qa     drbg-state-qa)
                   (pt-q   drbg-state-pt-q)) state
    (um:nlet-tail iter ()
      (let* ((ent   (get-entropy571))
             (ran   (ecc-random-key))
             (ns    (logxor ent ran)))
      (if (zerop ns)
          (iter)
        ;; choose a random isomorphic curve
        ;; for second curve, based off a random point on C(a,b)
        ;; NOTE: second curve will have same Trace as parent curve.
        (with-ecc-curve pa
          (let* ((y  (ecc-random-key))
                 ;; choose random point for its base x coord
                 (pt (ecc-mul pt-p
                              (ecc-random-key))))
            (destructuring-bind (pt a)
                (pt-and-curve-for-y y pt)
              (setf seed   ns
                    reseed (ash 1 30)
                    qa     a
                    pt-q   pt))) ))))))

(defun make-new-drbg-state ()
  (let ((state (make-drbg-state
                :pa   *ecc-a*
                :pt-a *ecc-gen*)))
    (reseed-drbg state)
    state))

(def-cached-var ec-drbg-state
                (make-new-drbg-state))

;; --------------------------------------------------

(defmethod next-block ((state drbg-state))
  (with-accessors ((seed   drbg-state-seed)
                   (reseed drbg-state-reseed)
                   (pa     drbg-state-pa)
                   (pt-p   drbg-state-pt-p)
                   (qa     drbg-state-qa)
                   (pt-q   drbg-state-pt-q)) state
    (let* ((s  (ecc-pt-x (with-ecc-curve pa
                           (ecc-mul
                            pt-p seed))))
           (r  (ecc-pt-x (with-ecc-curve qa
                           (ecc-mul
                            pt-q s))))
           (ns (ecc-pt-x (with-ecc-curve pa
                           (ecc-mul
                            pt-p s)))))
      (setf seed ns)
      (decf reseed)
      (when (zerop reseed)
        (reseed-drbg state))
      (let ((nb (truncate *nbits* 16))) ;; half the bits measured in bytes
        (basic-hash-with-protocol :sha256
                                  (convert-int-to-nbytes
                                   (ldb (byte (* 8 nb) 1) r) ;; remove LSB (always a Trace bit)
                                   nb) )))))

(defstruct drbg-buf
  (buf  (make-ub-array 512))
  (nb   0)
  (put  0)
  (get  0)
  (lock (mp:make-lock)))

(def-cached-var drbg-buf
                (make-drbg-buf))

(defmethod put-buf ((db drbg-buf) src)
  (with-accessors  ((buf    drbg-buf-buf)
                    (put-ix drbg-buf-put)
                    (navail drbg-buf-nb)
                    (lock   drbg-buf-lock)) db
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
  (mp:process-run-function "ECDRBG Thread" nil
                           (lambda ()
                             (loop do
                                   (put-buf (drbg-buf)
                                            (next-block (ec-drbg-state))) ))))

(def-cached-var ensure-drbg-thread (make-drbg-thread))

(defmethod get-buf ((db drbg-buf) nb)
  (ensure-drbg-thread)
  (with-accessors ((buf    drbg-buf-buf)
                   (navail drbg-buf-nb)
                   (get-ix drbg-buf-get)
                   (lock   drbg-buf-lock)) db
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
  (let ((ans (get-buf (drbg-buf) (ceiling nbits 8))))
    (mask-off ans (rem nbits 8)) ))

(defun ec-drbg-int (nbits)
  (convert-bytes-to-int (ec-drbg nbits)))

