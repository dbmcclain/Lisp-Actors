
(in-package :ac)

;; ----------------------------------------------------
;; Useful primitives...

(defun plist-keys (plist)
  (declare (list plist))
  (do ((lst  plist (cddr lst))
       (keys nil))
      ((endp lst) keys)
    (declare (list lst keys))
    (push (car lst) keys)))

(defun reapply (fn reqd restargs &rest parms)
  ;; Like APPLY, but used to substitute new keyword args for old,
  ;; removing all the old kw args to prevent accumulation of old stuff
  ;; and prevent its GC.
  (declare (list reqd restargs parms))
  (multiple-value-call fn (values-list reqd) (values-list parms)
    (values-list (apply #'alexandria:remove-from-plist restargs
                        (plist-keys parms)))
    ))

(defun encrypt (ekey seq msg)
  (let* ((enc  (loenc:encode msg))
         (mask (vec-repr:vec (hash:get-hash-nbytes (length enc) ekey seq))))
    (map 'vector #'logxor enc mask)))

(defun decrypt (ekey seq emsg)
  (let ((mask  (vec-repr:vec (hash:get-hash-nbytes (length emsg) ekey seq))))
    (loenc:decode (map 'vector #'logxor emsg mask))))

(defun pt->int (ecc-pt)
  (vec-repr:int (edec:ed-compress-pt ecc-pt)))

(defun int->pt (int)
  (edec:ed-decompress-pt int))

(defun make-signature (seq emsg skey)
  (let* ((pkey  (edec:ed-mul edec:*ed-gen* skey))
         (krand (vec-repr:int (hash:hash/256 seq emsg skey pkey)))
         (kpt   (edec:ed-mul edec:*ed-gen* krand))
         (h     (vec-repr:int (hash:hash/256 seq emsg kpt pkey)))
         (u     (modmath:with-mod edec:*ed-r*
                  (modmath:m+ krand (modmath:m* h skey))))
         (upt   (edec:ed-mul edec:*ed-gen* u)))
    (list (pt->int upt) krand)
    ))

(defun check-signature (seq emsg auth pkey)
  (destructuring-bind (upt krand) auth
    (let* ((kpt  (edec:ed-mul edec:*ed-gen* krand))
           (h    (vec-repr:int (hash:hash/256 seq emsg kpt pkey))))
      (edec:ed-pt= (int->pt upt) (edec:ed-add kpt (edec:ed-mul pkey h)))
      )))

;; --------------------------------------------------

(defun marshal-encoding ()
  (actor (cust &rest msg)
    (send cust (loenc:encode msg))))

(defun marshal-decoding ()
  (actor (cust msg)
    (send* cust (loenc:decode msg))))

(defun marshal-compress ()
  (actor (cust &rest msg)
    (send cust (lzw:zl-compress msg))))

(defun marshal-decompress ()
  (actor (cust msg)
    (send* cust (lzw:decompress msg))))

(defun encryptor (ekey)
  (labels ((encryptor-beh (ekey seq)
             (lambda (cust &rest msg)
               (let ((emsg (encrypt ekey seq msg)))
                 (send cust seq emsg)
                 (become (encryptor-beh ekey (1+ seq)))))))
    (make-actor (encryptor-beh ekey 0))
    ))

(defun signing (skey)
  (actor (cust seq emsg)
    (let ((sig (make-signature seq emsg skey)))
      (send cust seq emsg sig))))

(defun signature-validation (pkey)
  (actor (cust seq emsg sig)
    (when (check-signature seq emsg sig pkey)
      (send cust seq emsg))))

(defun decryptor (ekey)
  (actor (cust seq emsg)
    (let ((ans (decrypt ekey seq emsg)))
      (send* cust ans))))

(defun self-sync-encoding ()
  (actor (cust bytevec)
    (send cust (self-sync:encode bytevec))))

(defun self-sync-decoding ()
  (actor (cust bytevec)
    (send cust (self-sync:decode bytevec))))

(defun chunker (&key (max-size 65536))
  (actor (cust byte-vec)
    (let* ((size    (length byte-vec))
           (nchunks (ceiling size max-size))
           (id      (uuid:make-v1-uuid)))
      (send cust :init id nchunks size)
      (do ((offs  0  (+ offs max-size)))
          ((>= offs size))
        (send cust :chunk id offs
              (subseq byte-vec offs (min size (+ offs max-size)))))
      )))

;; ------------------------------------

(defun null-ordered-delivery-beh ()
  (alambda
   ((cust :prune)
    (send cust :pruned self-beh))

   ((_ :init . _)
    (let ((next (make-actor self-beh)))
      (become (ordered-init-delivery-beh *whole-message* next)) ))

   ((_ :chunk . _)
    (let ((next (make-actor self-beh)))
      (become (ordered-chunk-delivery-beh *whole-message* next)) ))
   ))

(defun ordered-init-delivery-beh (pend next)
  (alambda
   ((cust :prune)
    (send cust :pruned self-beh))

   ((cust :init?)
    (send* cust pend)
    (prune-self next))

   (msg
    (declare (ignore msg))
    (repeat-send next))
   ))

(defun ordered-chunk-delivery-beh (pend next)
  (alambda
   ((cust :prune)
    (send cust :pruned self-beh))

   ((cust :chunk? id) when (uuid:uuid= id (third pend))
    (send* cust pend)
    (prune-self next))

   (msg
    (declare (ignore msg))
    (repeat-send next))
   ))

(defun dechunker ()
  ;; No assumptions about chunk or init delivery order.
  (labels ((initial-dechunker-beh (delivery)
             (alambda
              ((_ :init id nchunks size)
               (become (dechunker-beh
                        :vec      (make-array size
                                              :element-type '(unsigned-byte 8))
                        :id       id
                        :nchunks  nchunks
                        :delivery delivery))
               (send delivery self :chunk? id))

              (msg
               (send* delivery msg))
              ))

           (dechunker-beh (&rest args &key vec id nchunks (ctr 0) delivery)
             (alambda
              ((cust :chunk an-id offs chunk-vec) when (uuid:uuid= an-id id)
               (replace vec chunk-vec :start1 offs)
               (cond ((>= (1+ ctr) nchunks)
                      (send cust vec)
                      (become (initial-dechunker-beh delivery))
                      (send delivery self :init?))

                     (t
                      (become (reapply #'dechunker-beh nil args
                                       :ctr (1+ ctr)))
                      (send delivery self :chunk? id))
                     ))
              (msg
               (send* delivery self msg))
              )))
    (let ((delivery (make-actor (null-ordered-delivery-beh))))
      (make-actor (initial-dechunker-beh delivery))
      )))

;; ------------------------------------------------------------

(defun chain (&rest elts)
  (labels ((chain-beh (a b)
             (lambda (cust &rest msg)
               (beta ans
                   (send* a beta msg)
                 (send* b cust ans)))
             ))
    (cond ((cdr elts)
           (make-actor (chain-beh (car elts)
                                  (apply #'chain (cdr elts)))))
          (elts  (car elts))
          (t     sink)
          )))

;; -----------------------------------------------------------

(defun printer ()
  (actor (cust &rest msg)
    (send* println msg)
    (send* cust msg)))

(defun writer ()
  (actor (cust &rest msg)
    (send* writeln msg)
    (send* cust msg)))

(defun netw-encoder (ekey skey &key (max-chunk 65536))
  (chain (marshal-encoding)   ;; to get arb msg objects into bytevecc form
         (chunker :max-size max-chunk) ;; we want to limit network message sizes
         ;; --- then, for each chunk... ---
         (marshal-compress)   ;; generates a compressed data struct
         (encryptor ekey)     ;; generates seq, enctext
         (signing skey)       ;; generates seq, enctext, sig
         (marshal-encoding))) ;; to turn seq, etext, sig into byte vector

(defun netw-decoder (ekey pkey)
  (chain (marshal-decoding)    ;; decodes byte vector into seq, enc text, sig
         (signature-validation pkey) ;; pass along seq, enc text
         (decryptor ekey)      ;; generates a compressed data struct
         (marshal-decompress)  ;; generates a byte vector
         (dechunker)           ;; de-chunking back into original byte vector
         (marshal-decoding)))  ;; decode byte vector into message objects

(defun disk-encoder (&key (max-chunk 65536))
  (chain (marshal-encoding) ;; to get arb msg into bytevec form
         (chunker :max-size max-chunk)
         (marshal-compress)
         (marshal-encoding) ;; to get lzw data struct into bytevec form
         (self-sync-encoding)))
                     
(defun disk-decoder ()
  (chain (self-sync-decoding)
         (marshal-decoding)
         (marshal-decompress)
         (dechunker)
         (marshal-decoding)))

(defun encr-disk-encoder (ekey skey &key (max-chunk 65536))
  (chain (netw-encoder ekey skey :max-chunk max-chunk)
         (self-sync-encoding)))

(defun encr-disk-decoder (ekey pkey)
  (chain (self-sync-decoding)
         (netw-decoder ekey pkey)))

;; Reed-Solomon? anyone?... TBD

;; -------------------------------------------------------------

#|
(multiple-value-bind (skey pkey)
    (edec:make-deterministic-keys :test)
  (let ((ekey (hash:hash/256 skey pkey)))
    (send (chain (encr-disk-encoder ekey skey :max-chunk 16)
                 (writer)
                 (encr-disk-decoder ekey pkey)
                 (writer))
          sink "This is a test")))

(let ((junk (make-array 1022
                        :element-type '(unsigned-byte 8))))
  (beta (ans)
      (send (chain (chunker :max-size 16) (dechunker)) beta junk)
    (send println (if (equalp ans junk) :yes :no))))
|#

