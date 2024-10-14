;; aont-distr.lisp
;;
;; DM/RAL  2024/10/13 09:41:02 UTC
;; ----------------------------------

(defpackage #:aont-distr
  (:use #:common-lisp #:vec-repr #:hash #:finite-field #:ac))

(in-package #:aont-distr)

;; ----------------------------------

(defun schnorr-signature (skey data)
  (edec-ff::with-curve-field
   (let* ((pkey  (edec:ed-nth-pt skey))
          (hash  (hash/256 pkey data))
          (krand (int hash))
          (kpt   (edec:ed-nth-pt krand))
          (h     (int (hash/256 kpt pkey data)))
          (u     (int (ff+ krand (ff* h skey)))))
     (list kpt u))))

(defun validate-schnorr-signature (pkey data sig)
  (edec-ff::with-curve-field
   (destructuring-bind (kpt u) sig
     (let* ((hash  (hash/256 kpt pkey data)))
       (edec:ed-pt= (edec:ed-nth-pt u)
                    (edec:ed-add kpt
                                 (edec:ed-mul pkey (int hash))))
       ))))

#|
(edec-ff::with-curve-field
  (let* ((skey (edec-ff::rand))
         (pkey (edec:ed-nth-pt skey))
         (data :testing))
    (inspect
     (list (schnorr-signature skey data)
           (schnorr-signature skey data)))
    ))
|#

;; --------------------------------------------

(defstruct aont-local
  index pkey seed)

(defstruct aont-distr
  index chan share data sig)

(defun aont-distr-encode (obj &optional my-seed)
  (edec-ff::with-curve-field
   (let* ((bytes         (ecc:aont-encode obj))
          (my-seed       (or my-seed (edec-ff::rand)))
          (dist-seed     (edec-ff::rand))
          (my-xor-mask   (vec (hash/256 my-seed)))
          (dist-xor-mask (vec (hash/256 dist-seed))))
     (map-into bytes #'logxor bytes my-xor-mask dist-xor-mask)
     (let* ((rsenc (ecc:rs-encode-bytes bytes)))
       (multiple-value-bind (index shares)
           (provable-sharing::gen-shares dist-seed 6 8)
         (let* ((share-index-str (vec-repr:hex-str index))
                (skey            (edec-ff::rand))
                (my-entry        (make-aont-local
                                  :index share-index-str
                                  :pkey  (edec:ed-nth-pt skey)
                                  :seed  my-seed))
                (distr-shares    (map 'list
                                      (lambda (chan share data)
                                        (make-aont-distr
                                         :index share-index-str
                                         :chan  chan
                                         :share share
                                         :data  data
                                         :sig   (schnorr-signature
                                                 skey
                                                 (list share-index-str chan share data))
                                         ))
                                      (um:range 0 8)
                                      shares
                                      rsenc)))
           (values my-entry
                   distr-shares)
           )))
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
  dest pkey seed shares rsvec)

(defun share-combiner-beh (&optional (tree (maps:empty)))
  (alambda
   ;; --------------------------------------------
   
   ((cust :start local-info dest)
    (send cust :ok)
    (with-slots (index pkey seed) local-info
      (become (share-combiner-beh
               (maps:add tree index (make-entry
                                     :dest   dest
                                     :pkey   pkey
                                     :seed   seed
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
      (let ((prob-index (vec (hex index)))
            (entry      nil))
        (when (and
               (setf entry (maps:find tree index))
               (entry-dest entry)
               (validate-schnorr-signature (entry-pkey entry)
                                           (list index chan share data)
                                           sig)
               (not (find share (entry-shares entry)
                          :key #'caar))
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
                   (let ((dist-seed (ignore-errors
                                      (provable-sharing::combine-shares
                                       prob-index new-shares)))
                         (bytes     (ecc:rs-decode-bytes new-rsvec)))
                     (cond ((and dist-seed
                                 bytes)
                            (become (share-combiner-beh
                                     (maps:add tree index (make-entry))))
                            (let ((xor-mask       (vec (hash/256 (entry-seed entry))))
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
               seed) local
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
  ;; (inspect (list local slices))
  (with-slots ((id index)
               seed) local
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
  (aont-distr-encode vec)
       (enc (rs-encode-bytes vec))
       (dec (rs-decode-bytes enc)))
  (assert (equalp dec vec))
  (with-standard-io-syntax
    (write
     `(:original ,vec
       :enc      ,enc))))                      
|#