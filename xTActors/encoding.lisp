;; encoding.lisp -- extensions for primitive Actors
;;
;; DM/RAL 11/21
;; -------------------------------------------------

(in-package :actors/base)

;; ----------------------------------------------------
;; Useful primitives...

(defun encrypt/decrypt (ekey seq bytevec)
  ;; takes a bytevec and produces an encrypted/decrypted bytevec
  ;;
  ;; One-time-pad encryption via XOR with random mask. Take care to
  ;; never re-use the same mask for encryption, which is the hash of
  ;; the ekey concat with seq.
  (let ((mask (vec-repr:vec (hash:get-hash-nbytes (length bytevec) ekey seq))))
    (map 'vector #'logxor bytevec mask)))

(defun pt->int (ecc-pt)
  (vec-repr:int (edec:ed-compress-pt ecc-pt)))

(defun int->pt (int)
  (edec:ed-decompress-pt int))

(defun make-signature (seq emsg skey)
  ;; Generate and append a Schnorr signature - signature includes seq
  ;; and emsg.
  ;;
  ;; We take care to use deterministic hashing so that the same
  ;; message and seq always produces the same signature, for the given
  ;; secret key, skey. This is doubly cautious here, since seq is only
  ;; ever supposed to be used just once.
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
  ;; takes seq, emsg, and sig (a Schnorr signature on seq+emsg), and
  ;; produce t/f on signature as having come from pkey.
  (destructuring-bind (upt krand) auth
    (let* ((kpt  (edec:ed-mul edec:*ed-gen* krand))
           (h    (vec-repr:int (hash:hash/256 seq emsg kpt pkey))))
      (edec:ed-pt= (int->pt upt) (edec:ed-add kpt (edec:ed-mul pkey h)))
      )))

;; --------------------------------------------------
;; The term "Arbitrary Objects" here refers to serializable objects -
;; just about anything, except compiled closures.

(defun marshal-encoder ()
  (actor (cust &rest msg)
    ;; takes arbitrary objects and producdes an encoded bytevec
    (send cust (loenc:encode msg))))

(defun marshal-decoder ()
  ;; takes an encoded bytevec and produces arbitrary objects
  (actor (cust msg)
    (send* cust (loenc:decode msg))))

(defun marshal-compressor ()
  ;; takes bytevec and produces bytevec
  (actor (cust bytevec)
    (send cust (xzlib:compress bytevec :fixed))))

(defun marshal-decompressor ()
  ;; takes a bytevec and produces a bytevec
  (actor (cust cmprvec)
    (send cust (xzlib:uncompress cmprvec))))

;; ---------------------------------------------------------------

(defun noncer ()
  (let ((fname "~/.actors-nonce"))
    (labels ((wr-nonce (nonce)
               (with-open-file (f fname
                                  :direction :output
                                  :if-does-not-exist :create
                                  :if-exists :supersede)
                 (with-standard-io-syntax
                   (write nonce :stream f))))
             (rd-nonce ()
               (handler-case
                   (with-open-file (f fname
                                      :direction :input)
                     (with-standard-io-syntax
                       (read f)))
                 (error ()
                   (let ((seq (vec-repr:int (hash:hash/256 (uuid:make-v1-uuid)))))
                     (wr-nonce seq)
                     seq))
                 ))
             (noncer-beh (nonce tag)
               (alambda
                ((cust :get-nonce)
                 (send cust nonce)
                 ;; update nonce to increment of current one.
                 ;;
                 ;; Nonces start out as the numeric value of the
                 ;; hash/256 of the UUID of the host machine, at the
                 ;; start time.
                 ;;
                 ;; We increment by 2^256, thereby assuring that we
                 ;; never coincide with nonces generated previously on
                 ;; any machine.
                 ;;
                 ;; We use the hash/256 of the UUID to preserve
                 ;; anonymity in the nonces.
                 ;;
                 (let ((new-nonce (+ #.(ash 1 256) nonce))
                       (tag       (tag self)))
                   ;; sync to disk 10s after most recent get-nonce. If
                   ;; another happens during that time window, the
                   ;; sync is rescheduled.
                   (send (scheduled-message (io tag) 10 :write-nonce))
                   (become (noncer-beh new-nonce tag))))
                
                ((cust :write-nonce) when (eq cust tag)
                 (wr-nonce nonce))
                )))
      (make-actor (noncer-beh (rd-nonce) #() ))
      )))

(defvar *noncer* (noncer))

;; ---------------------------------------------------------------------

(defun encryptor (ekey)
  ;; Takes a bytevec and produces an encrypted bytevec.
  ;;
  ;; Since we are encrypting via XOR with a random mask, we must
  ;; ensure that no two messages are ever encrypted with the same
  ;; keying. We do that by ensuring that every encryption is by way of
  ;; a new mask chosen from a PRF. This is a one-time-pad encryption.
  ;;
  ;; BTW... precisely the same is true for AES/CTR encryption. For
  ;; that you need a nonce to start the CTR of the mode. A nonce is
  ;; the analog of our seq.
  ;;
  ;; We use SHA3/256 as that PRF, and compute the next seq as the hash
  ;; of the current one.  The likelihood of seeing a same key arise is
  ;; the same as the likelihood of finding a SHA3/256 hash collision.
  ;; Not very likely...
  ;;
  ;; The initial seq is chosen as the SHA3/256 hash of a unique
  ;; 128-bit UUID, which includes the time of creation to 100ns
  ;; precision, concatenated with 256 bits of randomness generated by
  ;; NIST Hash DRBG.
  ;;
  ;; The master key never changes, but the keying used for any message
  ;; is the hash of the master key concatenated with the seq. The seq
  ;; is sent along as part of the message so that someone sharing the
  ;; same master key will be able to decrypt the message.
  ;;
  ;; Secrecy is protected because of the one-wayness of the hash
  ;; function. It is infeasible to determine a hash preimage from a
  ;; hash value.
  ;;
  ;; Attackers can discern the XOR mask for one message by encrypting
  ;; a known plaintext, then XOR the encryption result with that
  ;; plaintext. But seeing one, or even multiple, encryption XOR masks
  ;; is not helpful in any way to them.
  ;;
  (actor (cust bytevec)
    (beta (seq)
        (send *noncer* beta :get-nonce)
      (send cust seq (encrypt/decrypt ekey seq bytevec)))))

(defun decryptor (ekey)
  ;; Takes an encrypted bytevec and produces a bytevec
  (actor (cust seq emsg)
    (send cust (encrypt/decrypt ekey seq emsg))))

(defun signing (skey)
  (actor (cust seq emsg)
    (let ((sig (make-signature seq emsg skey)))
      (send cust seq emsg sig))))

(defun signature-validation (pkey)
  (actor (cust seq emsg sig)
    (when (check-signature seq emsg sig pkey)
      (send cust seq emsg))))

(defun self-sync-encoder ()
  ;; takes a bytevec and produces a self-sync bytevec
  (actor (cust bytevec)
    (send cust (self-sync:encode bytevec))))

(defun self-sync-decoder ()
  ;; takes a self-sync bytevec and produces a bytevec
  (actor (cust bytevec)
    (send cust (self-sync:decode bytevec))))

(defun chunker (&key (max-size 65536))
  ;; takes a bytevec and produces a sequence of chunk encodings
  (actor (cust byte-vec)
    (let* ((size    (length byte-vec))
           (nchunks (ceiling size max-size))
           (id      (uuid:make-v1-uuid)))
      (send cust :init id nchunks size)
      (do ((offs  0  (+ offs max-size))
           (ix    0  (1+ ix)))
          ((>= offs size))
        (send cust :chunk id ix offs
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

   ((_ :init id . _) when (uuid:uuid= id (third pend))
    ;; duplicate - just ignore
    )

   ( _
    (repeat-send next))
   ))

(defun ordered-chunk-delivery-beh (pend next)
  (alambda
   ((cust :prune)
    (send cust :pruned self-beh))

   ((cust :chunk? id ix) when (and (uuid:uuid= id (third pend))
                                   (= ix (fourth pend)))
    (send* cust pend)
    (prune-self next))

   ((_ :chunk id ix . _) when (and (uuid:uuid= id (third pend))
                                   (= ix (fourth pend)))
    ;; duplicate - just ignore
    )

   ( _
    (repeat-send next))
   ))

(defun dechunker ()
  ;; No assumptions about chunk or init delivery order.
  ;; Takes a sequence of chunk encodings and produces a bytevec
  (labels ((initial-dechunker-beh (delivery)
             (alambda
              ((_ :init id nchunks size)
               (become (dechunker-beh
                        :vec      (make-array size
                                              :element-type '(unsigned-byte 8))
                        :id       id
                        :nchunks  nchunks
                        :delivery delivery))
               (send delivery self :chunk? id 0))

              ( _ 
               (repeat-send delivery))
              ))

           (dechunker-beh (&rest args &key vec id nchunks (ctr 0) delivery)
             (alambda
              ((cust :chunk an-id ix offs chunk-vec) when (and (uuid:uuid= an-id id)
                                                               (= ix ctr))
               (replace vec chunk-vec :start1 offs)
               (let ((next-ctr (1+ ctr)))
                 (cond ((>= next-ctr nchunks)
                        (send cust vec)
                        (become (initial-dechunker-beh delivery))
                        (send delivery self :init?))

                       (t
                        (become (um:reapply #'dechunker-beh nil args
                                            :ctr next-ctr))
                        (send delivery self :chunk? id next-ctr))
                       )))
              ( _
                (repeat-send delivery))
              )))
    (let ((delivery (make-actor (null-ordered-delivery-beh))))
      (make-actor (initial-dechunker-beh delivery))
      )))

;; -----------------------------------------------------------

(defun printer ()
  ;; prints the message and forwards to cust
  (actor (cust &rest msg)
    (send* println msg)
    (send* cust msg)))

(defun writer ()
  ;; prints the message and forwards to cust
  (actor (cust &rest msg)
    (send* writeln msg)
    (send* cust msg)))

(defun logger ()
  ;; provides a log output as the message is passed along
  (actor (cust &rest msg)
    (send* logger cust msg)
    (send* cust msg)))

(defun marker (&rest txt)
  (actor (cust &rest msg)
    (send* println txt)
    (send* cust msg)))

(defun netw-encoder (ekey skey &key (max-chunk 65536))
  ;; takes arbitrary objects and produces a bytevec
  (pipe (marshal-encoder)       ;; to get arb msg objects into bytevecc form
        (chunker :max-size max-chunk) ;; we want to limit network message sizes
        ;; --- then, for each chunk... ---
        (marshal-encoder)       ;; generates bytevec from chunker encoding
        (marshal-compressor)    ;; generates a compressed data vec
        (encryptor ekey)        ;; generates seq, enctext
        (signing skey)          ;; generates seq, enctext, sig
        (marshal-encoder)))     ;; turn seq, etext, sig into byte vector

(defun netw-decoder (ekey pkey)
  ;; takes a bytevec and produces arbitrary objects
  (pipe (marshal-decoder)       ;; decodes byte vector into seq, enc text, sig
        (signature-validation pkey) ;; pass along seq, enc text
        (decryptor ekey)        ;; generates a bytevec
        (marshal-decompressor)  ;; generates a byte vector
        (marshal-decoder)       ;; generates chunker encoding
        (dechunker)             ;; de-chunking back into original byte vector
        (marshal-decoder)))     ;; decode byte vector into message objects

(defun disk-encoder (&key (max-chunk 65536))
  ;; takes arbitrary objects and produces a bytevec
  (pipe (marshal-encoder)       ;; to get arb msg into bytevec form
        (chunker :max-size max-chunk)
        (marshal-encoder)
        (marshal-compressor)
        (self-sync-encoder)))

(defun disk-decoder ()
  ;; takes a bytevec and produces arbitrary objects
  (pipe (self-sync-decoder)
        (marshal-decompressor)
        (marshal-decoder)
        (dechunker)
        (marshal-decoder)))

(defun encr-disk-encoder (ekey skey &key (max-chunk 65536))
  ;; takes arbitrary objects and produces a bytevec
  (pipe (netw-encoder ekey skey :max-chunk max-chunk)
        (self-sync-encoder)))

(defun encr-disk-decoder (ekey pkey)
  ;; takes a bytevec and produces arbitrary objects
  (pipe (self-sync-decoder)
        (netw-decoder ekey pkey)))

;; Reed-Solomon? anyone?... TBD

;; -------------------------------------------------------------

#|
(multiple-value-bind (skey pkey)
    (edec:make-deterministic-keys :test)
  (let ((ekey (hash:hash/256 skey pkey)))
    (send (pipe (encr-disk-encoder ekey skey :max-chunk 16)
                (writer)
                (encr-disk-decoder ekey pkey)
                (writer))
          sink "This is a test")))

(let ((junk (make-array 1022
                        :element-type '(unsigned-byte 8))))
  (beta (ans)
      (send (pipe (chunker :max-size 16) (dechunker)) beta junk)
    (send println (if (equalp ans junk) :yes :no))))

(multiple-value-bind (skey pkey)
    (edec:make-deterministic-keys :test)
  (let ((ekey (hash:hash/256 skey pkey)))
    (send (pipe (marshal-encoder) (encryptor ekey)) println "This is a test")))
|#

