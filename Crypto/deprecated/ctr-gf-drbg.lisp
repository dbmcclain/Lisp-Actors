;; ctr-gf-drbg.lisp -- Counter Galois Field DRGB
;; DM/Acudora  06/12
;; -------------------------------------------------------------

(in-package :ecc-crypto-b571)

;; ------------------------------------------------------------------------

(defstruct ctr-gf-drbg-state
  seed ctr reseed)

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

(defmethod reseed-ctr-gf-drbg ((state ctr-gf-drbg-state))
  (with-accessors ((seed   ctr-gf-drbg-state-seed)
                   (reseed ctr-gf-drbg-state-reseed)) state
    (setf seed   (convert-bytes-to-int (get-entropy 571))
          reseed (ash 1 24)) ))

(defun make-new-ctr-gf-drbg-state ()
  (let* ((state (make-ctr-gf-drbg-state
                 :seed   (ecc-random-key)
                 :ctr    (convert-bytes-to-int (make-nonce)) )))
    (reseed-ctr-gf-drbg state)
    state))

(def-cached-var ctr-gf-drbg-state
  (make-new-ctr-gf-drbg-state))

;; --------------------------------------------------

(defmethod next-ctr-gf-drbg-block ((state ctr-gf-drbg-state))
  (with-accessors ((reseed ctr-gf-drbg-state-reseed)
                   (seed   ctr-gf-drbg-state-seed)
                   (ctr    ctr-gf-drbg-state-ctr)) state
    (labels ((generate-block ()
               (setf ctr  (gf* 2 ctr)
                     seed (gf+ ctr (gf* seed seed)))))
      
      (decf reseed)
      (when (zerop reseed)
        (reseed-ctr-gf-drbg state))
      
      (convert-int-to-bytes (ldb (byte 440 64) (generate-block))) )))

(defstruct ctr-gf-drbg-buf
  (buf  (make-ub-array 512))
  (nb   0)
  (put  0)
  (get  0)
  (lock (mp:make-lock)))

(def-cached-var ctr-gf-drbg-buf
  (make-ctr-gf-drbg-buf))

(defmethod put-ctr-gf-drbg-buf ((db ctr-gf-drbg-buf) src)
  (with-accessors  ((buf    ctr-gf-drbg-buf-buf)
                    (put-ix ctr-gf-drbg-buf-put)
                    (navail ctr-gf-drbg-buf-nb)
                    (lock   ctr-gf-drbg-buf-lock)) db
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

(defun make-ctr-gf-drbg-thread ()
  (mp:process-run-function "CTR-GF-DRBG Thread" nil
                           (lambda ()
                             (loop do
                                   (put-ctr-gf-drbg-buf (ctr-gf-drbg-buf)
                                            (next-ctr-gf-drbg-block (ctr-gf-drbg-state))) ))))

(def-cached-var ensure-ctr-gf-drbg-thread
  (make-ctr-gf-drbg-thread))

(defmethod get-ctr-gf-drbg-buf ((db ctr-gf-drbg-buf) nb)
  (ensure-ctr-gf-drbg-thread)
  (with-accessors ((buf    ctr-gf-drbg-buf-buf)
                   (navail ctr-gf-drbg-buf-nb)
                   (get-ix ctr-gf-drbg-buf-get)
                   (lock   ctr-gf-drbg-buf-lock)) db
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

(defun ctr-gf-drbg (nbits)
  ;; NIST Dual EC DRBG
  (let ((ans (get-ctr-gf-drbg-buf (ctr-gf-drbg-buf) (ceiling nbits 8))))
    (mask-off ans (rem nbits 8)) ))

#|
(defun ctr-gf-drbg-int (nbits)
  (convert-bytes-to-int (ctr-gf-drbg nbits)))
|#

(unless (fboundp 'ctr-drbg)
  (setf (symbol-function 'ctr-drbg) #'ctr-gf-drbg))

#|
(let* ((pts (loop repeat 10000 collect
                  (list (ctr-gf-drbg-int 16)
                        (ctr-gf-drbg-int 16))))
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
                  (- (ctr-gf-drbg-int 8) 128))
            'vector)))
    (plt:plot 'plt (fft:fwd-magnitude-db x) :clear t)
    (plt:plot 'plt2 (map 'vector #'realpart
                         (fft:inv (map 'vector (lambda (x)
                                                 (* x (conjugate x)))
                                       (fft:fwd x))))
              :clear t
              ;; :xrange '(0 400)
              ;; :yrange '(-5e7 5e7)
              )
    (subseq x 0 500)
    ))
|#

#|
;; Show that the output is entirely predictable given any two consecutive samples.
;;
;; Here, although the generator uses:
;;
;;    C(n+1) = 2 * C(n)
;;    S(n+1) = C(n+1) + S(n)^2
;;
;; We can compare any two consecutive outputs to reveail that:
;;
;;   S(n+1) = S(n)^2 + 2*[S(n) + S(n-1)^2]
;;
(defun tst ()
  (with-gf2^7
    (let* ((seed 15)
           (ctr  63)
           (seq  (coerce
                  (loop for ix from 0 below 1000 collect
                        (list ix
                              (setf ctr  (gf* 2 ctr)
                                    seed (gf+ ctr (gf* seed seed)))))
                  'vector))
           (seq2 (let ((sm1 (cadr (aref seq 0)))
                       (sm  (cadr (aref seq 1))))
                   (coerce
                    (loop repeat 1000 collect
                          (shiftf sm1 sm (gf+ (gf^2 sm)
                                              (gf* 2
                                                   (gf+ sm (gf^2 sm1)))) ))
                    'vector))))
      (list seq seq2))))
|#

#|
(defun tst ()
  (with-gf2^7
    (let* ((seed 15)
           (ctr  63)
           (seq  (coerce
                  (loop for ix from 0 below 1000 collect
                        (setf ctr  (gf* 2 ctr)
                              seed (gf* ctr (gf+ 1 seed (gf^2 seed)))) )
                  'vector)))
    (plt:plot 'plt (fft:fwd-magnitude-db seq) :clear t)
    (plt:plot 'plt2 (map 'vector #'realpart
                         (fft:inv (map 'vector (lambda (x)
                                                 (* x (conjugate x)))
                                       (fft:fwd seq))))
              :clear t
              ;; :xrange '(0 400)
              ;; :yrange '(-5e7 5e7)
              )
      seq)))
|#

