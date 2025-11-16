;; aont-distr.lisp - Secure Distributed File Sharing
;;
;; DM/RAL  2024/10/13 09:41:02 UTC
;; ----------------------------------

(defpackage #:aont-distr
  (:use #:common-lisp #:vec-repr #:hash #:finite-field #:edec #:ac))

(in-package #:aont-distr)

;; ----------------------------------
;; Because we use Schnorr signatures on the distributed portions,
;; there is no need for VSS since the shared secret exists only for
;; our benefit.
;;
;; Even the Schnorr signatures do not need to be verifiable by anyone
;; else. So we elide the Public key part of the Schnorr signature,
;; making it impossible to forge a valid signature.  We retain the
;; public key for our verifcation purposes.
;;
;; If any custodian of our data tries to cheat, it will
;; be detected with non-valid Schnorr signatures in the returned
;; portions.
;;
;; We can use the Hash of the non-disclosed PKey as our half of the
;; XOR decryption mask, the Hash of the shared secret being the other
;; half.

(defun schnorr-signature (skey data)
  ;; Schnorr signatures are versatile, but must be cautiosly designed.
  ;;
  ;; In the verification equation:
  ;;
  ;;      u*G = K + H(K,P,data)*P
  ;;
  ;; which derives from:
  ;;
  ;;      u = krand + H(K,P,Data)*s
  ;;
  ;; If krand were constant per (s,P) keying, even if randomly
  ;; dependent on keying, then two successive data sets with different
  ;; hash values for H(K,P,data) could be used to discover secret key
  ;; s:
  ;;
  ;;     u1 = krand(s,P) + H(K,P,data1)*s
  ;;     u2 = krand(s,P) + H(K,P,data2)*s
  ;; hence
  ;;     s = (u1-u2)/(H(K,P,data1)-H(K,P,data2))
  ;;
  ;; So it is very important that krand be dependent also on data
  ;; krand(s,P,data), or else thoroughly randomly chosen each time.
  ;;
  (with-curve-field
   (let* ((pkey  (ed-nth-pt skey))
          (hash  (hash/256 pkey data)) ;; <-- Warning!
          (krand (int hash))           ;; krand must be random per PKey and Data
          (kpt   (ed-nth-pt krand))
          (h     (int (hash/256 kpt pkey data)))
          (u     (int (ff+ krand (ff* h skey)))))
     (list (ed-compress-pt pkey)
           (ed-compress-pt kpt)
           u)
     )))

(defun validate-schnorr-signature (data sig)
  (with-curve-field
   (destructuring-bind (pkey kpt u) sig
     (let* ((hash  (hash/256 kpt pkey data)))
       (ed-pt= (ed-nth-pt u)
                    (ed-add kpt
                                 (ed-mul pkey (int hash))))
       ))))

#|
(with-curve-field
  (let* ((skey (edec-ff::rand))
         (pkey (ed-nth-pt skey))
         (data :testing))
    (inspect
     (list (schnorr-signature skey data)
           (schnorr-signature skey data)))
    ))
|#

;; --------------------------------------------

(defstruct aont-local
  index pkey)

(defstruct aont-distr
  index chan share data sig)

(defun gen-aont-shares (secret nneeded &optional (nshares nneeded))
  (with-curve-field
    (let* ((ks    (um:range 1 (1+ nshares)))
           (coffs (mapcar (lambda (j)
                            (declare (ignore j))
                            (edec-ff::rand))
                          (um:range 1 nneeded)))
           (fpoly (lambda (x)
                    (um:nlet iter ((x^n   x)
                                   (coffs coffs)
                                   (acc   secret))
                      (if (endp coffs)
                          acc
                        (go-iter (ff* x x^n)
                                 (cdr coffs)
                                 (ff+ acc (ff* x^n (car coffs))))
                        ))))
           (fks   (mapcar fpoly ks))
           (cjs   (mapcar #'ed-nth-pt (cons secret coffs))))
      (values
       (vec (hash/256 cjs)) ;; the sharing problem index
       ;; hash is sensitive to both value and order of
       ;; coeffients
       (mapcar (lambda (k fk)
                 (cons k (int fk)))
               ks fks))
      )))
  
(defun aont-distr-encode (obj)
  (with-curve-field
   (let* ((bytes         (ecc:aont-encode obj))
          (skey          (edec-ff::rand))
          (pkey          (ed-compress-pt (ed-nth-pt skey)))
          (dist-seed     (edec-ff::rand))
          (my-xor-mask   (vec (hash/256 pkey)))
          (dist-xor-mask (vec (hash/256 dist-seed))))
     (map-into bytes #'logxor bytes my-xor-mask dist-xor-mask)
     (let* ((rsenc (ecc:rs-encode-bytes bytes)))
       (multiple-value-bind (index shares)
           (gen-aont-shares dist-seed 6 8)
         (let* ((share-index-str (vec-repr:hex-str index))
                (my-entry        (make-aont-local
                                  :index share-index-str
                                  :pkey  (vec-repr:hex-str pkey)))
                (distr-shares    (map 'list
                                      (lambda (chan share data)
                                        (make-aont-distr
                                         :index share-index-str
                                         :chan  chan
                                         :share share
                                         :data  data
                                         :sig   (cdr ;; elide Pkey
                                                 (schnorr-signature
                                                  skey
                                                  (list share-index-str chan share data)))
                                         ))
                                      (um:range 0 8)
                                      shares
                                      rsenc)))
           (values my-entry
                   distr-shares)
           )))
     )))

(defun lagrange (shares)
  ;; Shares is a list of dotted-pairs (abscissa . ordinate).
  ;; Denominators are constant with respect to x.
  ;; Pre-compute the inverse denominators.
  (let ((1/dens (mapcar (lambda (share)
                          (let ((k (car share)))
                            (cons k
                                  (ff/ (reduce (lambda (acc share)
                                                 (let ((m (car share)))
                                                   (if (= m k)
                                                       acc
                                                     (ff* acc (ff- k m)))
                                                   ))
                                               shares
                                               :initial-value 1)))
                            ))
                        shares)))
    (lambda (x)
      (reduce (lambda (acc share)
                (let* ((k  (car share))
                       (fk (cdr share))
                       (num   (reduce (lambda (acc share)
                                        (let ((m (car share)))
                                          (if (= m k)
                                              acc
                                            (ff* acc (ff- x m)))
                                          ))
                                      shares
                                      :initial-value 1))
                       (1/den (cdr (assoc k 1/dens))))
                  (ff+ acc (ff* fk (ff* num 1/den)))
                  ))
              shares
              :initial-value 0)
      )))

#|
(defun #1=aont-distr-decode (local slices)
  (let* ((rsvec      (make-array 8
                                 :initial-element nil))
         (sh-index   (getf local :distr-data-root))
         (local-seed (getf local :seed))
         (shares      nil))
    (dolist (slice slices)
      (let* ((chan  (getf slice :chan))
             (data  (getf slice :data))
             (share (getf slice :share))
             (index (getf slice :distr-data)))
        (unless (equalp index sh-index)
          (return-from #1#))
        (unless (aref rsvec chan)
          (setf (aref rsvec chan) data)
          (push share shares))))
    (let* ((index      (vec (hex sh-index)))
           (distr-seed (ignore-errors
                         (provable-sharing::combine-shares index shares))))
      (when distr-seed
        (let ((bytes  (ecc:rs-decode-bytes rsvec)))
          (when bytes
            (let* ((distr-mask (vec (hash/256 distr-seed)))
                   (local-mask (vec (hash/256 local-seed)))
                   (xor-mask   (map 'vector #'logxor distr-mask local-mask)))
              (map-into bytes #'logxor bytes xor-mask)
              (ecc:aont-decode bytes)
              )))))
    ))
|#
;; --------------------------------------------

(defstruct entry
  dest pkey shares rsvec)

(defun share-combiner-beh (&optional (tree (maps:empty)))
  (alambda
   ;; --------------------------------------------
   
   ((cust :start local-info dest)
    (send cust :ok)
    (with-slots (index pkey) local-info
      (become (share-combiner-beh
               (maps:add tree index (make-entry
                                     :dest   dest
                                     :pkey   (ed-decompress-pt
                                              (vec-repr:hex pkey))
                                     :shares nil
                                     :rsvec  (make-array 8
                                                         :initial-element nil))
                         )))))
   
   ;; --------------------------------------------

   ((cust :add-slice slice)
    (send cust :ok)
    (with-slots (index
                 share
                 chan
                 data
                 sig) slice
      (let ((entry  nil))
        (when (and
               (setf entry (maps:find tree index))
               (entry-dest entry)
               (validate-schnorr-signature (list index chan share data)
                                           (cons (entry-pkey entry) sig))
               (not (find share (entry-shares entry)
                          :key #'car))
               (null (aref (entry-rsvec entry) chan)))
          (let ((new-shares (cons share (entry-shares entry)))
                (new-rsvec  (copy-seq (entry-rsvec entry)))
                (new-entry  (copy-entry entry)))
            (setf (aref new-rsvec chan) data
                  (entry-shares new-entry) new-shares
                  (entry-rsvec  new-entry) new-rsvec)
            (cond ((< (count-if #'identity new-rsvec) 6)
                   (become (share-combiner-beh
                            (maps:add tree index new-entry))
                           ))
                  (t
                   (let ((dist-seed (with-curve-field
                                      (funcall (lagrange new-shares) 0)))
                         (bytes     (ecc:rs-decode-bytes new-rsvec)))
                     (cond (bytes
                            (become (share-combiner-beh
                                     (maps:add tree index (make-entry))))
                             (let ((xor-mask      (vec (hash/256 (entry-pkey entry))))
                                  (dist-xor-mask  (vec (hash/256 dist-seed))))
                              (map-into bytes #'logxor bytes xor-mask dist-xor-mask)
                              (send (entry-dest entry) (ecc:aont-decode bytes))
                              ))
                           (t
                            (become (share-combiner-beh
                                     (maps:add tree index new-entry))
                                    ))
                           )))
                  )))
        )))
   ))

(deflex share-combiner
  (create (share-combiner-beh)))
    
;; --------------------------------------------
#|

(multiple-value-bind (local slices)
    (aont-distr-encode :testing)
  (inspect (list local slices))
  (with-slots ((id index)
               pkey) local
    (β _
        (send share-combiner β :start local println)
      (send (create
             (lambda (slices)
               (when slices
                 (let ((me self))
                   (β _
                       (send share-combiner β :add-slice (car slices))
                     (send me (cdr slices))
                     )))
               ))
            slices)
      )))

(multiple-value-bind (local slices)
    (aont-distr-encode :testing)
  (inspect (list local slices))
  (with-slots ((id index)
               pkey) local
    (β _
        (send share-combiner β :start local println)
      (dolist (slice slices)
        (send share-combiner sink :add-slice slice))
      )))

(multiple-value-bind (local shares)
    (aont-distr-encode :testing)
  (aont-distr-decode local (cddr shares)))
  
(let* ((vec (coerce
             (loop for ix from 1 to 24 nconc
                   (make-list 16 :initial-element ix))
             'vector)))
  (aont-distr-encode vec))

(let* ((enc (rs-encode-bytes vec))
       (dec (rs-decode-bytes enc)))
  (assert (equalp dec vec))
  (with-standard-io-syntax
    (write
     `(:original ,vec
       :enc      ,enc))))

|#
