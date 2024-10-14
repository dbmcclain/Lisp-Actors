
;; === DM/RAL 09/23 ========================================================================
(in-package #:com.ral.crypto.lattice-key-exchange)

#|
(send lattice-skey println)
(β (skey)
    (send lattice-skey β)
  (β (sys)
      (send lattice-system β)
    (let* ((pkey     (lattice::lat2-gen-pkey skey sys))
           (pkey-id  (um:kwsymb :pkey- (hex-str (hash/256 pkey)))))
      (send kvdb:kvdb println :add :my-pkeyid pkey-id)
      (send kvdb:kvdb println :add pkey-id pkey)
      )))

(let ((pkey-id (with-open-file (f "~/.my-pkeyid")
                 (read f)))
      (pkey    (with-open-file (f "~/.my-pkey")
                 (read f))))
  (send kvdb:kvdb println :add :my-pkeyid pkey-id)
  (send kvdb:kvdb println :add pkey-id pkey))

(β (pkey-id)
    (send kvdb:kvdb β :find :my-pkeyid)
  (β (pkey)
      (send kvdb:kvdb β :find pkey-id)
    (with-standard-io-syntax
      (with-open-file (f "~/.my-pkeyid"
                         :direction :output
                         :if-exists :supersede)
        (write pkey-id :stream f))
      (with-open-file (f "~/.my-pkey"
                         :direction :output
                         :if-exists :supersede)
        (write pkey :stream f))
      )))

|#

#|
(deflex lattice-skey
  (create
   (lambda (cust)
     (send kvdb:kvdb cust :find :my-syzygy))
   ))
|#

#|
(defun node-to-kw (node-name &key (prefix "") (suffix ""))
  (intern (string-upcase (concatenate 'string prefix node-name suffix))
          (find-package :keyword)))

(deflex lattice-pkey
  (create
   (lambda (cust node)
     (β (pkey)
         (send kvdb:kvdb β :find (node-to-kw node :prefix "lat2-pkey-"))
       (unless pkey
         (error "No PKEY for node: ~A" node))
       (send cust pkey)))
   ))
|#

#|
(β (pkey)
    (send srv-pkey β)
  (let ((id "{d73be812-5309-11ee-9c10-f643f5d48a65}"))
    (send kvdb:kvdb println :add id pkey)
    (send kvdb:kvdb println :add :my-pkeyid id)))
|#
#| -------------------------------------------------------------------
(with-open-file (f "~/.my-pkeyid"
                   :direction :input)
  (read f))
                   
(multiple-value-bind (skey pkey)
    (lattice::lat2-gen-keys (lattice::get-lattice-system))
  (let ((pkey-id (um:kwsymb "Pkey-" (hex-str (hash/256 pkey)))))
    (with-standard-io-syntax
      (with-open-file (f "~/.my-pkeyid"
                         :direction :output
                         :if-exists :supersede)
        (write pkey-id :stream f))
      (with-open-file (f "~/.my-pkey"
                         :direction :output
                         :if-exists :supersede)
        (write pkey :stream f))
      (with-open-file (f "~/.my-syzygy"
                         :direction :output
                         :if-exists :supersede)
        (write skey :stream f))
      )))

(let ((pkey-id  (with-open-file (f "~/.my-pkeyid")
                  (read f))))
  (send kvdb:kvdb println :add :my-pkeyid pkey-id)
  (let ((pkey     (with-open-file (f "~/.my-pkey")
                    (read f))))
    (send kvdb:kvdb println :add pkey-id pkey))
  (let ((skey     (with-open-file (f "~/.my-syzygy")
                    (read f))))
    (send kvdb:kvdb println :add :my-syzygy skey)))
 
(defun lat2-gen-all (&optional (sys (lattice::get-lattice-system)))
  (with-open-file (f "~/.syzygy"
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
    (loop for node in '("fornax.local"
                        "arroyo.local"
                        "zircon.local"
                        "rincon.local"
                        "umbra.local"
                        "david-pc.local")
        collect
          (let ((sym  (node-to-kw node :prefix "lat2-pkey-")))
            (multiple-value-bind (skey pkey)
                (lattice::lat2-gen-keys sys)
              (with-standard-io-syntax
                (print (list node skey pkey) f))
              (list sym skey pkey)))
          )))

(setf *all-keys* (lat2-gen-all))
(setf *my-lat2-skey* (second (fifth *all-keys*)))

(dolist (pars *all-keys*)
  (send kvdb:kvdb println :add (car pars) (third pars)))

(send kvdb:kvdb println :add :lat2-syzygy *my-lat2-skey*)

(let ((sys (ask kvdb:kvdb :find :lat2-system)))
  (with-open-file (f "~/.lat2-system"
                     :if-does-not-exist :create
                     :if-exists :supersede
                     :direction :output)
    (with-standard-io-syntax
      (print sys f))))

(with-open-file (f "~/.lat2-system"
                   :direction :input)
  (let ((sys (read f)))
    (send kvdb:kvdb println :add :lat2-system sys)))

(defun inhale ()
  (with-open-file (f "~/.syzygy"
                     :direction :input)
    (let* ((machid  (machine-instance))
           (machlen (length machid)))
      (um:nlet iter ()
        (let ((rec (read f t f)))
          (unless (eql rec f)
            (destructuring-bind (name skey pkey) rec
              (send kvdb:kvdb println :add (node-to-kw name :prefix "lat2-pkey-") pkey)
              (let ((len (length name)))
                (when (and (>= len machlen)
                           (string-equal (subseq name 0 machlen) machid))
                  (send kvdb:kvdb println :add :lat2-syzygy skey))
                (go-iter)))
            ))))))
(inhale)
|#
;; === DM/RAL 09/23 ========================================================================

#|
(defun tst ()
  (let* (;; (m     (1+ (* 3 4096)))
         (m     (- (ash 1 30) 35))
         (m/2   (ash m -1))
         (ncols 32)
         (nrows 32))
    (with-mod m
      (let* ((tt (loop for ix from 1 below ncols collect
                         (lmod (edec::random-between 0 m))))
             (s  (coerce (cons 1 tt) 'vector))
             (ttv (coerce tt 'vector))
             (amat   (coerce
                      (loop for ix from 1 to nrows collect
                              (coerce
                               (loop for jx from 1 below ncols collect
                                       (lmod (edec::random-between 0 m)))
                               'vector))
                      'vector))
             (noise (map 'vector #'round (vops:vscale (* m/2 1/4 (/ (sqrt nrows))) (vm:gnoise nrows))))
             (b  (vec+ (mat*v amat ttv) noise))
             (ptrn (coerce
                    (cons b
                          (loop for ix from 0 below (1- ncols) collect
                                  (coerce
                                   (loop for jx from 0 below nrows collect
                                           (lmneg (aref (aref amat jx) ix)))
                                   'vector)))
                    'vector))
             (p  (coerce
                  (loop for ix from 0 below nrows collect
                          (coerce
                           (let ((arow (aref amat ix)))
                             (cons (aref b ix)
                                   (loop for jx from 0 below (1- ncols) collect
                                           (lmneg (aref arow jx)))))
                           'vector))
                  'vector))
             (r  (coerce
                  (loop for ix from 1 to nrows collect
                        (random 2))
                  'vector))
             (msg (coerce
                   (cons (* 1 m/2) ;; msg bit
                         (make-list (1- ncols) :initial-element 0))
                   'vector))
             (c  (vec+
                  (mat*v ptrn r)
                  msg))
             (cdots  (vdot c s))
             (cdotsm (mod (round cdots m/2) 2))
             )
        (assert (equalp p (trn ptrn)))
        (list s p ptrn noise c cdots cdotsm m/2)
        ))))

(tst)

(defun maxabs (x y)
  (max (abs x) (abs y)))

;; Largest seen variate is around 5.6 sigma So, there is a finite
;; probability of a mis-decoding. Hence, need to use ECC encoding of
;; message.
(let ((v (vm:gnoise 1000000)))
  (reduce #'max (map 'vector #'abs v)))

;; ---------------------------------
;; Hamming(7,4) encoding
;; Detect and correct any 1-bit error in 4-bit groups using 7-bit encoding

(defun btst (n m)
  (if (zerop (logand n m)) 0 1))

(defun bin4 (n)
  (list (btst n 8)
        (btst n 4)
        (btst n 2)
        (btst n 1)))

(defun bin7 (n)
  (list
   (btst n 64)
   (btst n 32)
   (btst n 16)
   (btst n 8)
   (btst n 4)
   (btst n 2)
   (btst n 1)))

(defun dec3 (lst)
  (+ (elt lst 0)
     (* 2 (elt lst 1))
     (* 4 (elt lst 2))))

(defun dec4 (lst)
  (+ (* 8 (elt lst 0))
     (* 4 (elt lst 1))
     (* 2 (elt lst 2))
     (* 1 (elt lst 3))))

(defun dec7 (lst)
  (+ (* 64 (elt lst 0))
     (* 32 (elt lst 1))
     (* 16 (elt lst 2))
     (* 8  (elt lst 3))
     (* 4  (elt lst 4))
     (* 2 (elt lst 5))
     (* 1 (elt lst 6))))

(defun trnx (m)
  (when (car m)
    (cons (mapcar #'car m)
          (trnx (mapcar #'cdr m)))))

(let* ((gt '((1 1 0 1)
             (1 0 1 1)
             (1 0 0 0)
             (0 1 1 1)
             (0 1 0 0)
             (0 0 1 0)
             (0 0 0 1)))
       (h  '((1 0 1 0 1 0 1)
             (0 1 1 0 0 1 1)
             (0 0 0 1 1 1 1)))
       (r  '((0 0 1 0 0 0 0)
             (0 0 0 0 1 0 0)
             (0 0 0 0 0 1 0)
             (0 0 0 0 0 0 1))))
  (labels ((enc/dec (n m)
             (mapcar (lambda (v)
                       (reduce #'logxor (mapcar #'* n v)))
                     m))
           (enc (n)
             (enc/dec n gt))
           (dec (n)
             (enc/dec n h))
           (cor (n)
             (let ((d (1- (dec3 (dec n)))))
               (if (minusp d)
                   n
                 (let ((nn (copy-seq n)))
                   (setf (elt nn d) (logxor 1 (elt nn d)))
                   nn))
               )))
    (loop for ix from 0 below 16 collect
          (let* ((b (bin4 ix))
                 (e (enc b))
                 (d (dec e))
                 (r (enc/dec e r)))
            (list ix b e d r)))
    #||#
    ;; try single-bit errors
    (loop for enc in '((0 0 0 0 0 0 0)
                       (1 0 0 0 0 0 0)
                       (0 1 0 0 0 0 0)
                       (0 0 1 0 0 0 0)
                       (0 0 0 1 0 0 0)
                       (0 0 0 0 1 0 0)
                       (0 0 0 0 0 1 0)
                       (0 0 0 0 0 0 1))
            collect
            (list enc (dec enc) (enc/dec (cor enc) r)))
    #||#
    #|
    (loop for enc in '((1 1 0 1 0 0 1 0)
                       (0 1 0 1 0 0 1 0)
                       (1 0 0 1 0 0 1 0)
                       (1 1 1 1 0 0 1 0)
                       (1 1 0 0 0 0 1 0)
                       (1 1 0 1 1 0 1 0)
                       (1 1 0 1 0 1 1 0)
                       (1 1 0 1 0 0 0 0)
                       (1 1 0 1 0 0 1 1))
            collect
            (list enc (dec enc)))
          |#
    (loop for ix from 0 below 128 collect
          (list ix (dec4 (enc/dec (cor (bin7 ix)) r))))
    (with-standard-io-syntax
      (print
       (list
        (coerce
         (loop for ix from 0 below 16 collect
                 (dec7 (enc (bin4 ix))))
         'vector)
        (coerce
         (loop for ix from 0 below 128 collect
                 (dec4 (enc/dec (cor (bin7 ix)) r)))
         'vector)
        )))
    ))

(setf *print-base* 16.)
(setf *print-base* 10.)

|#
