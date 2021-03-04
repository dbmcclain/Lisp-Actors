;; ctr-drbg.lisp -- Counter Block Encryption DRGB
;; DM/Acudora  06/12
;; -------------------------------------------------------------

(in-package :ecc-crypto-b571)

;; ------------------------------------------------------------------------

(defstruct ctr-drbg-state
  seed reseed key ctr cipher)

(defun random-key-256 ()
  (convert-int-to-nbytesv (random-between 0 (ash 1 256)) 32))

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

(defmethod reseed-ctr-drbg ((state ctr-drbg-state))
  (with-accessors ((seed   ctr-drbg-state-seed)
                   (reseed ctr-drbg-state-reseed)
                   (cipher ctr-drbg-state-cipher)
                   (ctr    ctr-drbg-state-ctr)
                   (key    ctr-drbg-state-key)) state
    (let* ((ent   (get-entropy 32))
           (ran   (random-key-256))
           (ns    (map 'vector #'logxor ent ran)))
      (setf seed   ns
            key    (basic-hash-with-protocol :sha256
                                             key seed ctr)
            reseed (ash 1 16))
      (reinitialize-instance cipher :key key)) ))

(defun make-new-ctr-drbg-state ()
  (let* ((key   (make-nonce))
         (state (make-ctr-drbg-state
                 :ctr    (convert-bytes-to-int key)
                 :cipher (ironclad:make-cipher :aesx
                                               :key  (basic-hash-with-protocol :sha256 key)
                                               :mode :ecb))))
    (reseed-ctr-drbg state)
    state))

(def-cached-var ctr-drbg-state
  (make-new-ctr-drbg-state))

;; --------------------------------------------------

(defmethod next-ctr-drbg-block ((state ctr-drbg-state))
  (with-accessors ((reseed ctr-drbg-state-reseed)
                   (cipher ctr-drbg-state-cipher)
                   (key    ctr-drbg-state-key)
                   (ctr    ctr-drbg-state-ctr)) state
    (labels ((generate-block ()
               (incf ctr)
               (let ((cvec (convert-int-to-nbytesv ctr 16)))
                 (ironclad:encrypt-in-place cipher cvec)
                 (basic-hash-with-protocol :sha256 cvec key))))
      
      (decf reseed)
      (when (zerop reseed)
        (replace key (generate-block))
        (reseed-ctr-drbg state))
      
      (generate-block))))

(defstruct ctr-drbg-buf
  (buf  (make-ub-array 512))
  (nb   0)
  (put  0)
  (get  0)
  (lock (mp:make-lock)))

(def-cached-var ctr-drbg-buf
  (make-ctr-drbg-buf))

(defmethod put-ctr-drbg-buf ((db ctr-drbg-buf) src)
  (with-accessors  ((buf    ctr-drbg-buf-buf)
                    (put-ix ctr-drbg-buf-put)
                    (navail ctr-drbg-buf-nb)
                    (lock   ctr-drbg-buf-lock)) db
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

(defun make-ctr-drbg-thread ()
  (mp:process-run-function "CTR-DRBG Thread" nil
                           (lambda ()
                             (loop do
                                   (put-ctr-drbg-buf (ctr-drbg-buf)
                                            (next-ctr-drbg-block (ctr-drbg-state))) ))))

(def-cached-var ensure-ctr-drbg-thread
  (make-ctr-drbg-thread))

(defmethod get-ctr-drbg-buf ((db ctr-drbg-buf) nb)
  (ensure-ctr-drbg-thread)
  (with-accessors ((buf    ctr-drbg-buf-buf)
                   (navail ctr-drbg-buf-nb)
                   (get-ix ctr-drbg-buf-get)
                   (lock   ctr-drbg-buf-lock)) db
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

(defun ctr-drbg (nbits)
  ;; NIST Dual EC DRBG
  (let ((ans (get-ctr-drbg-buf (ctr-drbg-buf) (ceiling nbits 8))))
    (mask-off ans (rem nbits 8)) ))

#|
 ;; moved to utilities.lisp
(defun ctr-drbg-int (nbits)
  (convert-bytes-to-int (ctr-drbg nbits)))
|#

#|
(let* ((pts (loop repeat 10000 collect
                  (list (ctr-drbg-int 16)
                        (ctr-drbg-int 16))))
       (xs (mapcar #'first pts))
       (ys (mapcar #'second pts)))
  (plt:plot 'plt xs ys
            :clear t
            :symbol :dot))
|#