;; encoding.lisp -- extensions for primitive Actors
;;
;; DM/RAL 11/21
;; -------------------------------------------------

(in-package :com.ral.actors.encoding)

(um:eval-always
  (import '(vec-repr:vec
            vec-repr:int
            vec-repr:hex
            vec-repr:ub8
            vec-repr:ub8-vector
            vec-repr:make-ub8-vector
            hash:hash/256
            hash:hash=
            hash:get-hash-nbytes
            hash:in-place-otp
            edec:ed-mul
            edec:ed-add
            edec:ed-compress-pt
            edec:ed-decompress-pt
            edec:make-deterministic-keys
            edec:ed-nth-pt
            edec:*ed-r*
            edec:ed-pt=
            modmath:with-mod
            modmath:m+
            modmath:m*
            )))

(um:eval-always
  (hcl:add-package-local-nickname :self-sync :com.ral.actors.encoding.self-sync))

;; ----------------------------------------------------
;; Useful primitives...

(defun encrypt/decrypt (ekey seq bytevec)
  ;; takes a bytevec and produces an encrypted/decrypted bytevec
  ;;
  ;; One-time-pad encryption via XOR with random mask. Take care to
  ;; never re-use the same mask for encryption, which is the hash of
  ;; the ekey concat with seq.
  (in-place-otp bytevec :ENC ekey seq))

;; ------------------------------------------

(defun make-auth-key (ekey seq)
  (vec (hash/256 :AUTH ekey seq)))

(defun make-auth (ekey seq emsg)
  (let ((auth-key (make-auth-key ekey seq)))
    (vec (hash/256 auth-key seq emsg))))

(defun check-auth (ekey seq emsg auth)
  (ignore-errors
    (equalp auth (make-auth ekey seq emsg))))

;; --------------------------------------------
;; Schnorr Signatures - Non-Repudiable
(defun make-signature (seq emsg skey)
  ;; Generate and append a Schnorr signature - signature includes seq
  ;; and emsg.
  ;;
  ;; We take care to use deterministic hashing so that the same
  ;; message and seq always produces the same signature, for the given
  ;; secret key, skey. This is doubly cautious here, since seq is only
  ;; ever supposed to be used just once.
  (let* ((pkey  (ed-nth-pt skey))
         (krand (int (hash/256 seq emsg skey pkey)))
         (kpt   (ed-nth-pt krand))
         (h     (int (hash/256 seq emsg kpt pkey)))
         (u     (with-mod *ed-r*
                  (m+ krand (m* h skey))))
         (upt   (ed-nth-pt u)))
    (list (int upt) krand)
    ))

(defun check-signature (seq emsg sig pkey)
  ;; takes seq, emsg, and sig (a Schnorr signature on seq+emsg),
  ;; and produce t/f on signature as having come from pkey.
  ;;
  ;; Note: we are already wise to small group attacks. Anytime we are
  ;; handed an ECC compressed point as an argument we multiply by the
  ;; cofactor to get the actual point that will be used. If the point
  ;; were bogus, this would produce the neutral point.
  ;;
  ;; Similarly, when we compress a point to hand off to someone, we
  ;; divide by the cofactor before compression.
  (ignore-errors
    (destructuring-bind (upt krand) sig
      (let* ((kpt  (ed-nth-pt krand))
             (h    (int (hash/256 seq emsg kpt pkey))))
        (ed-pt= (ed-decompress-pt upt) (ed-add kpt (ed-mul pkey h)))
        ))))

;; --------------------------------------------------
;; The term "Arbitrary Objects" here refers to serializable objects -
;; just about anything, except compiled closures.

;; ---------------------------------------------------------------------
;; Useful Actors

(defun list-imploder ()
  ;; take a sequence of args and send to cust as one list
  (actor (cust &rest msg)
    (send cust msg)))

(defun list-exploder ()
  ;; take one list and send to cust as a sequence of args
  (actor (cust msg-list)
    (send* cust msg-list)))

(defun printer ()
  ;; prints the message and forwards to cust
  (tee println))

(defun writer ()
  ;; prints the message and forwards to cust
  (tee writeln))

(defun marker (&rest txt)
  (actor (cust &rest msg)
    (send* println txt)
    (send* cust msg)))

(defun marshal-encoder ()
  (actor (cust &rest args)
    ;; (send fmt-println "Marshal Encoder")
    (send cust (loenc:encode (coerce args 'vector)))))

(defun marshal-decoder ()
  (actor (cust vec)
    ;; (send fmt-println "Marshal Decoder")
    (send* cust (coerce (loenc:decode vec) 'list))))

(defun fail-silent-marshal-decoder ()
  (αα
   ((cust vec) / (typep vec 'ub8-vector)
    ;; (send fmt-println "Fail-Silent Marshal Decoder")
    ;;
    ;; The premise here is that, when using malleable encrption and
    ;; refutable signatures, an attacker can form a validly signed but
    ;; mutated encryption in a replay attack. That will likely produce
    ;; gibberish as a marshal encoding, and we need to intercept these
    ;; kinds of attacks and not let them pass, nor cause trouble for
    ;; us.
    (let ((dec (ignore-errors
                 (loenc:decode vec))))
      (when (and dec
                 (vectorp dec))
        (send* cust (coerce dec 'list)))
      ))
   ))

#|
(defun marshal-cmpr-encoder ()
  (actor (cust &rest msg)
    ;; takes arbitrary objects and producdes an encoded bytevec
    (send (marshal-compressor) cust (loenc:encode msg))))

(defun uncompressed? (vec)
  (and (>= (length vec) 4)
       (equalp #(82. 65. 76. 69.) (subseq vec 0 4)))) ;; "RALE"

(defun marshal-decmpr-decoder ()
  ;; takes an encoded bytevec and produces arbitrary objects
  (actor (cust msg)
    (beta (enc)
        (send (marshal-decompressor) beta msg)
      (send* cust (loenc:decode enc)))))

(defun marshal-compressor ()
  ;; takes bytevec and produces bytevec
  (actor (cust bytevec)
    (send cust (if (uncompressed? bytevec)
                   (handler-case
                       ;; sometimes zlib fails...
                       (let ((cmpr (subseq (zlib:compress bytevec :fixed) 0) ))
                         ;; we'd like to know if compression ever mimics uncompressed...
                         (assert (not (uncompressed? cmpr)))
                         cmpr)
                     (error (c)
                       (declare (ignore c))
                       (warn "~%ZLIB: compression failure")
                       bytevec))
                 ;; else - already compressed
                 bytevec))))

(defun marshal-decompressor ()
  ;; takes a bytevec and produces a bytevec
  (actor (cust cmprvec)
    (let ((vec (if (uncompressed? cmprvec)
                   cmprvec
                 (handler-case
                     (zlib:uncompress cmprvec) ;; Windows version has been producing some errors...
                   (error (c)
                     (declare (ignore c))
                     (warn "~%ZLIB: decompression failure")
                     ;; (error c)
                     nil))
                 )))
      (when vec
        (send cust vec))
      )))
|#

(defun simple-compress (vec)
  ;; vec is UB8
  (multiple-value-bind (outvec nb)
      (snappy:compress vec 0 (length vec))
    (adjust-array outvec nb)))

(defun simple-uncompress (vec)
  ;; vec is UB8
  (snappy:uncompress vec 0 (length vec)))

#||#
;; try using Google's SNAPPY
(defun marshal-compressor ()
  (actor (cust vec)
    (send cust (simple-compress vec))))

(defun marshal-decompressor ()
  (actor (cust vec)
    (send cust (simple-uncompress vec))))

(defun fail-silent-marshal-decompressor ()
  (αα
   ((cust vec)
    (let ((ans (ignore-errors
                 (simple-uncompress vec))))
      (when ans
        (send cust ans))
      ))
   ))

#||#
#|
(defun marshal-compressor ()
  (actor (cust vec)
    ;; (send fmt-println "Marshal Compressor")
    (send cust vec)))

(defun marshal-decompressor ()
  (actor (cust vec)
    ;; (send fmt-println "Marshal Decompressor")
    (send cust vec)))
|#
;; ---------------------------------------------------------------
#|
(defun make-noncer ()
  ;; The initial seq nonce is chosen as the SHA3/256 hash of a unique
  ;; 128-bit UUID, which includes the MAC Address of the machine, and
  ;; the time of creation to 100ns precision.
  ;;
  ;; The nonce is incremented after every use, in a manner to avoid
  ;; ever coinciding with a nonce generated previously on any machine,
  ;; assuming they all use the same nonce maintenance mechanism.
  ;;
  ;; Nonces are maintained on disk for continued use across multiple
  ;; Lisp sessions.
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
                   (let ((seq (vec-repr:int (hash/256 (uuid:make-v1-uuid)))))
                     (wr-nonce seq)
                     seq))
                 ))
             (noncer-beh (nonce tag nonce-writer)
               (alambda
                ((cust :get-nonce)
                 ;; Send nonce to customer, then immediately increment
                 ;; nonce to prevent re-use. The best we can do is
                 ;; furnish a fresh nonce when asked. It is still up
                 ;; to the customer to be sure it won't be reused.
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
                 (send cust nonce)
                 (let ((new-tag   (tag self))
                       (new-nonce (+ nonce #.(ash 1 256))))
                   (become (noncer-beh new-nonce new-tag nonce-writer))
                   ;; sync to disk 10s after most recent get-nonce. If
                   ;; another happens during that time window, the
                   ;; sync is rescheduled.
                   (send-after 10 new-tag :write-nonce)))
                
                ((cust :write-nonce) when (eq cust tag)
                 (send nonce-writer nonce))
                )))
      ;; all of the following will likely happen before MP has started...
      (create (noncer-beh (rd-nonce) #()
                          (create #'wr-nonce) ))
      )))
|#
(def-beh noncer-beh (nonce)
  ((cust :get-nonce)
   ;; The initial seq nonce is chosen as the SHA3/256 hash of a unique
   ;; 128-bit UUID, which includes the MAC Address of the machine, and
   ;; the time of creation to 100ns precision.
   ;;
   ;; The nonce is incremented before every use, in a manner to avoid
   ;; ever coinciding with a nonce generated previously on any machine,
   ;; assuming they all use the same nonce maintenance mechanism.
   ;;
   ;; Increment nonce and send to customer. The best we can do is
   ;; furnish a fresh nonce when asked. It is still up to the
   ;; customer to be sure it won't be reused.
   ;;
   ;; Nonces start out as the numeric value of the hash/256 of the
   ;; UUID of the host machine, at the start time.
   ;;
   ;; We increment by 2^256, thereby assuring that we never coincide
   ;; with nonces generated previously on any machine.
   ;;
   ;; We use the hash/256 of the UUID to preserve anonymity in the
   ;; nonces.
   ;;
   (let ((new-nonce (+ nonce #.(ash 1 256))))
     (send cust new-nonce)
     (become (noncer-beh new-nonce)))))

(deflex noncer (create
                (noncer-beh
                 (vec-repr:int (hash/256 (uuid:make-v1-uuid)))
                 )))

;; -------------------------------------------------------------------

(defun encryptor (ekey)
  ;; Takes a bytevec and produces an encrypted bytevec.
  ;;
  ;; Since we are encrypting via XOR with a random mask, we must
  ;; ensure that no two messages are ever encrypted with the same XOR
  ;; mask. We do that by ensuring that every encryption is by way of a
  ;; new mask chosen from a PRF. This is one-time-pad encryption.
  ;;
  ;; BTW... precisely the same is true for AES/CTR encryption. For
  ;; that you need a nonce to start the CTR of the mode. A nonce is
  ;; the analog of our seq.
  ;;
  ;; We use SHA3/256 as that PRF, taking the hash of the master
  ;; encryption key concatenated with a nonce. The nonce is sent along
  ;; with the encrypted data so that downstream decoders using the
  ;; same master encryption key can successfully decrypt the
  ;; cyphertext.
  ;;
  ;; Our nonces are guaranteed unique to each machine and to never
  ;; coincide with a nonce used previously on any machine. To ensure
  ;; use-once, every get-nonce request returns the current nonce
  ;; value, and immediately increments it before the next request.
  ;;
  ;; So even if the same master encryption key is reused, every
  ;; encryption will be using its own unique keying to generate the
  ;; XOR mask.
  ;;
  ;; -----------------------
  ;;
  ;; But even with perfectly random sampling of a finite number field,
  ;; hash collisions are a remote possibility, and could occur after
  ;; roughly 2^128 (~3e38) hashes, even as they all use unique keying.
  ;; We are mapping, via hashing, some 512 bits of key information,
  ;; down to 256 bits of hash mask. So there must be an enormous
  ;; number of hash collision preimages.
  ;;
  ;; On average, every hash mask corresponds to at least 2^256
  ;; different possible key patterns. But from an initial field of 512
  ;; bits, those 2^256 synonyms would be rare to find.
  ;;
  ;; To see a hash collision in the XOR masking, with its 256 bits,
  ;; with only a 1:1,000,000 chance of happening, would require
  ;; encrypting messages every microsecond, 24/7, for the next 2.5
  ;; billion years.
  ;;
  ;; -----------------------
  ;;
  ;; Secrecy is protected because of the one-wayness of the Sha3/256
  ;; hash function. It is infeasible to determine a hash preimage from
  ;; a hash value.
  ;;
  ;; Attackers can discern the XOR mask for one message by encrypting
  ;; a known plaintext, then XOR the encryption result with that
  ;; plaintext. But seeing one, or even multiple, encryption XOR masks
  ;; is not helpful in any way to them. It tells them nothing about
  ;; the next encryption, even with foreknowledge about what nonce
  ;; will be used, unless they also have the master encryption key.
  ;;
  ;; Knowing what nonce will be used, if attackers also knew that
  ;; master encryption keys were short enough, they could mount a
  ;; brute force attack over the field of possible master encryption
  ;; keys. So it is incumbant on us to maintain sufficiently large
  ;; master encryption keys. For that purpose we also use randomly
  ;; generated Sha3/256 hash values. With 256 bits of master keying,
  ;; it becomes infeasible to mount a brute-force attack.
  ;;
  ;; By default, we use Diffie-Hellman secret key agreement between
  ;; client and server, based on Elliptic Curve Ed1174, which has a
  ;; field size of 2^249 bits. A random point, of size 2^251 bits,
  ;; from Curve Ed1174, is fed to SHA3/256 to derive the master
  ;; encryption key for the session. Those fields are still large
  ;; enough to prevent brute-force attacks.
  ;;
  (actor (cust bytevec)
    ;; (send fmt-println "Encryptor")
    (beta (seq)
        (send noncer beta :get-nonce)
      (send cust seq (encrypt/decrypt ekey seq bytevec))
      )))

(defun decryptor (ekey)
  ;; Takes an encrypted bytevec and produces a bytevec
  (αα
   ((cust seq emsg)
    ;; (send fmt-println "Decryptor")
    (let ((bytvec (ignore-errors
                    (encrypt/decrypt ekey seq emsg))))
      (when bytvec
        (send cust bytvec)
        )))
   ))

;; --------------------------------------

(defun authentication (ekey)
  (α (cust seq emsg)
    (let ((auth (make-auth ekey seq emsg)))
      (send cust seq emsg auth))
    ))

(defun check-authentication (ekey)
  (labels ((auth-beh (seqs)
             (alambda
              ((cust seq emsg auth) / (and (integerp seq)
                                           (not (sets:mem seqs seq))
                                           (check-auth ekey seq emsg auth))
               ;; seq is integer (bignum)
               ;;
               ;; With our 3-way authentication scheme, spoofing attacks from 3rd
               ;; parties becomes infeasible. But we still need to avoid replay
               ;; attacks, in case the service were non-idempotent.
               ;;
               ;; The shared private ekey is per connection and session.
               ;; Encryption and authentication keying are derived from the
               ;; shared private ekey and a random roving seq. So we need not
               ;; share this seq history with any other connection nor future
               ;; sessions. We use an FPL Red-Black Tree for the history to give
               ;; us an O(Log(N)) lookup.
               ;;
               ;; 3-way authentication keying allows for complete repudiation
               ;; since it only requires knowledge of another's public key to
               ;; make up a valid-appearing but fictitious session log.
               ;;
               ;; Yet we have complete forward security since every session uses
               ;; a different random iniitial ekey, and every transmission uses a
               ;; new random roving seq. Hence, changing encryption and
               ;; authentication keying - which can only be known to the pair of
               ;; participants in the session.
               ;;
               ;; Once a connection ends, all keying is forgotten by both sides.
               ;; There is no way to predict the next connection keying, nor be
               ;; able to read a historical record of encryptions.
               ;;
               (send cust seq emsg)
               (become (auth-beh (sets:add seqs seq))))
              )))
    (create (auth-beh (sets:empty)))
   ))

;; --------------------------------------

(defun signing (skey)
  (actor (cust seq emsg)
    ;; (send fmt-println "Signing")
    (let ((sig (make-signature seq emsg skey)))
      (send cust seq emsg sig))))

(defun signature-validation (pkey)
  (αα
   ((cust seq emsg sig) / (check-signature seq emsg sig pkey)
    (send cust seq emsg))
   ))

;; --------------------------------------

(defun self-sync-encoder ()
  ;; takes a bytevec and produces a self-sync bytevec
  (actor (cust bytevec)
    (send cust (self-sync:encode bytevec))))

;; --------------------------------------

(defun checksum ()
  ;; produce a prefix checksum on the message
  (actor (cust &rest msg)
    (send* cust (vec (hash/256 msg)) msg)
    ))

(defun verify-checksum ()
  ;; if a replay attack with mutated encryption manages to become
  ;; unmarshalled, then we need to stop it here by checking the
  ;; checksum.
  (actor (cust check &rest msg)
    (when (equalp check (vec (hash/256 msg)))
      (send* cust msg))
    ))

;; --------------------------------------------------------------
;; CHUNKER - splitting large vectors into chunks for transmission.
;;
;; In our Actors system, CHUNKER can lead to a flurry of parallel
;; concurrent activity in the customer - most often the tail of a
;; systolic processing pipeline. So even if you SERIALIZER on input to
;; the chunker, you may (and probably will) need to re-SERIALIZER
;; somewhere downstream.
;;
;; Very fast! Imagine all the cores of the CPU further encoding each
;; chunk in parallel.

(defun chunker (&key (max-size 65536))
  ;; takes a bytevec and produces a sequence of chunk encodings
  (actor (cust byte-vec)
    ;; (send fmt-println "Chunker")
    (let ((size (length byte-vec)))
      (cond ((<= size max-size)
             ;; (send fmt-println "1 chunk, ~D bytes" size)
             (send cust :pass byte-vec))
            (t
             (let* ((nchunks (ceiling size max-size))
                    (id      (int (hash/256 (uuid:make-v1-uuid)))))
               ;; (send fmt-println "~D chunks, ~D bytes" nchunks size)
               (send cust :init id nchunks size)
               (do ((offs  0  (+ offs max-size)))
                   ((>= offs size))
                 (let ((frag (subseq byte-vec offs (min size (+ offs max-size)) )))
                   (send cust :chunk id offs frag)))
               ))
            ))))

;; ------------------------------------
;; Dechunker - With message delivery not guaranteed in any order,
;; multiple concurrent dechunkings can be happening, interleaved, some
;; with data preceding their init records, and possible duplicate
;; deliveries. We need to be robust against every possibility.

#||#
(def-beh dechunk-assembler-beh (cust nchunks chunks-seen out-vec)
  ;; Assemblers are constructed as soon as we have the init record
  ((offs byte-vec) / (and (integerp offs)
                          (typep byte-vec 'ub8-vector))
   ;; (send fmt-println "Dechunk Assembler: offs ~D, size ~D" offs (length byte-vec))
   (unless (find offs chunks-seen) ;; toss duplicates
     (let ((replacer    (α (cust)
                          ;; avoids repeated copying on BECOME retry
                          (replace out-vec byte-vec :start1 offs)
                          (send cust out-vec)))
           (new-seen    (cons offs chunks-seen))
           (new-nchunks (1- nchunks)))
       (cond ((zerop new-nchunks)
              (send replacer cust)
              (become (sink-beh)))
             (t
              (send replacer nil)
              (become (dechunk-assembler-beh cust new-nchunks new-seen out-vec)))
             )))
   ))
#||#
#|
(defun dechunk-assembler-beh (cust nchunks chunks-seen out-vec)
  ;; Assemblers are constructed as soon as we have the init record
  (lambda (offs byte-vec)
    (unless (find offs chunks-seen) ;; toss duplicates
      (replace out-vec byte-vec :start1 offs)
      (let ((new-seen    (cons offs chunks-seen))
            (new-nchunks (1- nchunks)))
        (cond ((zerop new-nchunks)
               (send cust out-vec)
               (become (sink-beh)))
              (t
               (become (dechunk-assembler-beh cust new-nchunks new-seen out-vec)))
              )))))
|#

(defun make-dechunk-assembler (cust nchunks size)
  (create (dechunk-assembler-beh cust nchunks nil (make-ub8-vector size) )))

(def-beh dechunk-interceptor-beh (id assembler next)
  ;; A node that intercepts incoming chunks for a given id, once the
  ;; init record has been received
  ((_ :chunk an-id offs byte-vec) when (and (eql an-id id)
                                            (integerp offs)
                                            (typep byte-vec 'ub8-vector))
   ;; (send fmt-println "Intercept Dechunker: CHUNK id ~A offs ~D, ~D bytes" an-id offs (length byte-vec))
   (send assembler offs byte-vec))
  
  ((_ :init an-id . _) when (eql an-id id)
   ;; (send fmt-println "Intercept Dechunker: Spurious INIT id ~A" an-id)
   ;; toss duplicates
   )
  
  (_
   (repeat-send next)))

(def-beh dechunk-pending-beh (id pend next)
  ;; A node that enqueues data chunks for a given id, while we await
  ;; the arrival of the init record.
  ((cust :init an-id nchunks size) when (and (eql an-id id)
                                             (integerp nchunks)
                                             (integerp size))
   ;; (send fmt-println "Pending Dechunker: INIT id ~A ~D chunks, ~D bytes" an-id nchunks size)
   (let ((assembler (make-dechunk-assembler cust nchunks size)))
     (become (dechunk-interceptor-beh id assembler next))
     (dolist (args pend)
       (send* assembler args))
     ))
  
  ((_ :chunk an-id offs byte-vec) when (and (eql an-id id)
                                            (integerp offs)
                                            (typep byte-vec 'ub8-vector))
   ;; (send fmt-println "Pending Dechunker: CHUNK id ~A, offs ~D, len ~D" id offs (length byte-vec))
   (become (dechunk-pending-beh id
                                (cons (list offs byte-vec) pend)
                                next)))
  
  (_
   (repeat-send next)))

(def-beh null-dechunk-beh ()
  ((cust :pass bytevec) / (typep bytevec 'ub8-vector)
   ;; (send fmt-println "Null Dechunker: pass")
   (send cust bytevec))
  
  ((cust :init id nchunks size) / (and (integerp id)
                                       (integerp nchunks)
                                       (integerp size))
   ;; (send fmt-println "Null Dechunker: INIT id ~A ~D chunks, ~D bytes" id nchunks size)
   (let ((next      (create self-beh))
         (assembler (make-dechunk-assembler cust nchunks size)))
     (become (dechunk-interceptor-beh id assembler next))
     ))
  
  ((_ :chunk id offs byte-vec) / (and (integerp id)
                                      (integerp offs)
                                      (typep byte-vec 'ub8-vector))
   ;; (send fmt-println "Null Dechunker: CHUNK id ~A, offs ~D, len ~D" id offs (length byte-vec))
   (let ((next (create self-beh)))
     (become (dechunk-pending-beh id
                                  (list
                                   (list offs byte-vec))
                                  next))
     )))

(defun dechunker ()
  ;; No assumptions about chunk or init delivery order.
  ;; Takes a sequence of chunk encodings and produces a bytevec
  (create (null-dechunk-beh)))

;; -------------------------------------------------------------------------
;; Chunk Monitor - solving the problem of determining when a chunked
;; write group has finished physically writing to I/O.
;;
;; Construct a Chunk Monitor pointed toward original customer, and
;; surround the portion of the systolic encoding pipeline, from just
;; past the CHUNKER, to the end of the pipeline. The original customer
;; is notified only after all chunks of data have finished writing to
;; I/O.

(defun chunk-monitor-beh (mycust)
  (lambda (&rest msg)
    (send* msg)
    (match msg
      ((_ :pass . _)
       (become (pending-monitor-beh mycust 1)))

      ((_ :init _ nchunks _)
       (become (pending-monitor-beh mycust (1+ nchunks))))
      
      ((_ :chunk . _)
       (become (worried-chunk-monitor-beh mycust 0)))
      )))

(defun pending-monitor-beh (mycust remct)
  (lambda (&rest msg)
    (match msg
      ((_ :chunk . _)
       (let ((newct (1- remct)))
         (cond ((plusp newct)
                (become (pending-monitor-beh mycust (1- remct)))
                (send* msg))
               (t
                (error "Illogical pipeline behavior"))
               )))
                

      ((:ok)
       (let ((new-remct (1- remct)))
         (cond ((zerop new-remct)
                (send mycust :ok)
                (become (chunk-monitor-beh mycust)))
               (t
                (become (pending-monitor-beh mycust new-remct)))
               )))
      )))

(defun worried-chunk-monitor-beh (mycust okcnt)
  (lambda (&rest msg)
    (match msg
      ((_ :init _ nchunks _)
       (let ((new-remct (- (1+ nchunks) okcnt)))
         (become (pending-monitor-beh mycust new-remct))
         (send* msg)))

      ((_ :chunk . _)
       (send* msg))
      
      ((:ok)
       (become (worried-chunk-monitor-beh mycust (1+ okcnt))))
      )))

(defun chunk-monitor (cust)
  (create (chunk-monitor-beh cust)))
    
;; -----------------------------------------------------------

(defun netw-encoder (ekey skey dest &key (max-chunk 65536))
  ;; takes arbitrary objects and sends one or more chunks to dest
  ;; dest must reply with :OK after writing to I/O
  (serializer
   (α (cust &rest msg)
     (with-basic-error-response cust
       (let ((monitor (chunk-monitor cust)))
         (send* (sink-pipe (marshal-encoder)       ;; to get arb msg objects into bytevecc form
                           (marshal-compressor)
                           (chunker :max-size max-chunk) ;; we want to limit network message sizes
                           ;; --- then, for each chunk... ---
                           monitor
                           (marshal-encoder)       ;; generates bytevec from chunker encoding
                           (encryptor ekey)        ;; generates seq, enctext
                           (signing skey)          ;; generates seq, enctext, sig
                           (marshal-encoder)       ;; turn seq, etext, sig into byte vector
                           (self-sync-encoder)
                           (serializer dest)
                           monitor)
                msg)
         ))
     )))

(defun netw-decoder (ekey pkey cust)
  ;; takes a bytevec and produces arbitrary objects
  (self-sync:stream-decoder
   (sink-pipe (fail-silent-marshal-decoder)       ;; decodes byte vector into seq, enc text, sig
              (signature-validation pkey) ;; pass along seq, enc text
              (decryptor ekey)        ;; generates a bytevec
              (fail-silent-marshal-decoder)       ;; generates chunker encoding
              (dechunker)             ;; de-chunking back into original byte vector
              (fail-silent-marshal-decompressor)
              (fail-silent-marshal-decoder)       ;; decode byte vector into message objects
              cust)))

(defun disk-encoder (dest &key (max-chunk 65536.))
  ;; takes arbitrary objects and sends one or more bytevecs to dest
  ;; dest must reply with :OK after writing to disk.
  (serializer
   (α (cust &rest msg)
     (with-basic-error-response cust
       (let ((monitor (chunk-monitor cust)))
         (send* (sink-pipe (marshal-encoder)       ;; to get arb msg into bytevec form
                         (marshal-compressor)
                         (chunker :max-size max-chunk)
                         monitor
                         (marshal-encoder)
                         (self-sync-encoder)
                         (serializer dest)
                         monitor)
                msg)
         ))
     )))

(defun disk-decoder (cust)
  ;; takes chunks of self-sync data and produces arbitrary objects
  (self-sync:stream-decoder
   (sink-pipe (marshal-decoder)
              (dechunker)
              (marshal-decompressor)
              (marshal-decoder)
              cust)))
  
(defun self-sync-stream-writer (stream &optional (max-chunk 65536.))
  (let ((stream-writer (α (cust vec)
                         ;; blocking I/O so response happens only
                         ;; after the write has occurred
                         (write-sequence vec stream)
                         (send cust :ok))))
    (disk-encoder stream-writer :max-chunk max-chunk)))

(defun read-self-sync-stream (cust stream &key (start 0) end)
  (let* ((nbuf 4096)
         (buf  (make-array nbuf
                           :element-type '(unsigned-byte 8)))
         (decoder (disk-decoder cust)))
    (file-position stream start)
    (um:nlet iter ((pos   start)
                   (bufix 1))
      (when (or (null end)
                (< pos end))
        (let ((nel (read-sequence buf stream)))
          (when (plusp nel)
            (send decoder :deliver bufix (subseq buf 0 nel))
            (go-iter (+ pos nel) (1+ bufix)))
          )))
    ))

#|
(setf s (hcl:file-string "taxes.lisp"))

(let* ((stream (ubyte-streams:make-ubyte-output-stream))
       (closer (α _
                 (let ((stream (ubyte-streams:make-ubyte-input-stream (ubyte-streams:stream-bytes stream))))
                   (read-self-sync-stream writeln stream))) ))
  (send (self-sync-stream-writer stream 256) closer :write s))
|#

;; Reed-Solomon? anyone?... TBD

;; -------------------------------------------------------------
;; Tests...
#|
(multiple-value-bind (skey pkey)
    (make-deterministic-keys :test)
  (let* ((ekey   (hash/256 skey pkey))
         (reader (encr-disk-decoder ekey pkey writeln))
         (ct     0)
         (sender (α (chunk)
                   (send reader :deliver (incf ct) chunk))))
    (send (sink-pipe (encr-disk-encoder ekey skey :max-chunk 16)
                     (writer)
                     sender)
          "This is a test")))

(let ((junk (make-ub8-vector 1022)))
  (beta (ans)
      (send (pipe (chunker :max-size 16) (dechunker)) beta junk)
    (send println (if (equalp ans junk) :yes :no))))

(multiple-value-bind (skey pkey)
    (make-deterministic-keys :test)
  (let ((ekey (hash/256 skey pkey)))
    (send (pipe (marshal-encoder) (encryptor ekey)) println "This is a test")))

(let ((x (hcl:file-string "./xTActors/encoding.lisp"))
      ;; (x "test string")
      )
  (multiple-value-bind (skey pkey)
      (make-deterministic-keys :test)
    (let ((ekey (hash/256 skey pkey))
          (inp  nil))
      (beta (ans)
          (send (encr-disk-encoder ekey skey) beta x)
          ;; (send (disk-encoder) beta x)
          ;; (send (netw-encoder ekey skey) beta x)
          #|
          (send (pipe (marshal-encoder)
                      (chunker)
                      (marshal-encoder)
                      (writer)
                      (actor (cust bytvec)
                        (setf inp bytvec)
                        (send cust bytvec))
                      (encryptor ekey)
                      (signing skey)
                      (writer)
                      (signature-validation pkey)
                      (decryptor ekey)
                      (writer)
                      (actor (cust bytvec)
                        (let ((diff (map 'vector #'logxor bytvec inp)))
                          (send println diff)
                          (send println (position-if-not #'zerop diff))
                          (send cust bytvec)))
                      (marshal-decoder)
                      (dechunker)
                      (marshal-decoder))
                beta x)
          |#
        (send (encr-disk-decoder ekey pkey) println ans)
        ;; (send (disk-decoder) println ans)
        ;; (send (netw-decoder ekey pkey) println ans)
        ;; (send println ans)
        
        (send println (format nil "x-size: ~A" (length x)))
        (send println (format nil "enc-size: ~A" (length ans)))
        ))))

(let* ((x   (hcl:file-string "./xTActors/encoding.lisp"))
       (xe  (loenc:encode x))
       (xc  (subseq (xzlib:compress xe :fixed) 0))
       (xx  (xzlib:uncompress xc))
       (xd  (loenc:decode xx)))
  (print xd))

|#
#|
;; -------------------------------------------------------------------
;; Encrypted Disk Files - AONT Encoding
;;

    +------------------------------------+
    | File Type UUID                     | = AONT type {b532fc4e-bf2b-123a-9307-24f67702cdaa}
    +------------------------------------+
    | (LIST pkey-vec encr-data aont-vec) |
    +------------------------------------+

  The encr-data in the :TEXT section is a marshal encoding of a list of 3 items:
   (LIST seq cipher-text sig)
  where,
    seq = the subkey of this encryption. Actual encryption key is H(master-key | seq).
    cipher-text = encrypted, compressed, marshaled list of Lisp data items.
    sig = signature on (seq cipher-text) and validated against pkey.

  The sig is an authentication of the written data.
  Encryption is by way of XOR with one-time pad.
  
  There is no relation between pkey and the master encryption key. The
  skey and pkey can be chosen independently from the master encryption
  key. skey and pkey of creator are related through EdEC Curve1174.

  This is *Confidential* encoding, not *SECRET*. Anyone can read this
  data, provided they read all of it first, and can then derive the
  master encryption key. This takes special effort, and so the data is
  not visible to the casual reader.
|#

(defconstant +AONT-FILE-TYPE-ID+ (vec #/uuid/{b532fc4e-bf2b-123a-9307-24f67702cdaa}))

(defun aont-encoder (skey ekey)
  (actor (cust &rest msg)
    ;; takes arbitrary Lisp data items and sends as an encrypted
    ;; single list to cust, along with AONT parameters
    (let ((pkey-vec (vec (ed-nth-pt skey))))
      (beta (data-packet)
          (send* (pipe (marshal-encoder)
                       (marshal-compressor)
                       (encryptor ekey)
                       (signing skey)
                       (marshal-encoder))
                 beta msg)
          (let* ((aont-vec (vec (hash/256 pkey-vec data-packet))))
            (map-into aont-vec #'logxor aont-vec ekey)
            (send cust pkey-vec data-packet aont-vec)
            ))
      )))

(deflex aont-decoder
  ;; sends a sequence of original Lisp data items to cust
  (actor (cust pkey-vec data-packet aont-vec)
    (let ((pkey  (ed-decompress-pt pkey-vec))
          (ekey  (vec (hash/256 pkey-vec data-packet))))
      (map-into ekey #'logxor ekey aont-vec)
      (send (pipe (marshal-decoder)
                  (signature-validation pkey)
                  (decryptor ekey)
                  (marshal-decompressor)
                  (marshal-decoder))
            cust data-packet)
      )))

;; ------------------------------------------------------

(defun aont-file-writer (fname skey ekey)
  ;; writes a single AONT record to a file
  (actor (cust &rest msg)
    (beta vecs
        (send* (aont-encoder skey ekey) beta msg)
      (with-open-file (fd fname
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create
                          :element-type '(unsigned-byte 8))
        (write-sequence +AONT-FILE-TYPE-ID+ fd)
        (loenc:serialize vecs fd)
        (send cust :ok)
        ))))

(defun aont-file-reader (fname)
  ;; Reads a single AONT record from a file, sending the original data
  ;; items as a sequence of args to cust.
  (actor (cust)
    (with-open-file (fd fname
                        :direction :input
                        :element-type '(unsigned-byte 8))
      (let ((file-type (make-ub8-vector 16)))
        (read-sequence file-type fd)
        (if (equalp +AONT-FILE-TYPE-ID+ file-type)
            (send* aont-decoder cust (loenc:deserialize fd))
          (error "~A: Not an AONT encoded file" fname))
        ))))

;; -----------------------------------------------------------------
;; Tests...
#|
(defun tst ()
  (let ((msg (hcl:file-string "./xTActors/encoding.lisp")))
    (multiple-value-bind (skey pkey)
        (make-deterministic-keys :test)
      (let ((ekey (vec (hash/256 skey pkey))))
        (beta (pkey-vec data-packet aont-vec)
            (send (aont-encoder skey ekey) beta msg)
          (send writeln (list pkey-vec data-packet aont-vec))
          (beta (dmsg)
              (send aont-decoder beta pkey-vec data-packet aont-vec)
            (send writeln dmsg)
            (assert (equalp msg dmsg))
            )))
      )))
(tst)


(let ((msg (hcl:file-string "./xTActors/encoding.lisp"))
      (fout "./xTActors/aont-test"))
  (multiple-value-bind (skey pkey)
      (make-deterministic-keys :test)
    (let ((ekey (vec (hash/256 skey pkey))))
      (send (aont-file-writer fout skey ekey) println msg)
      )))

(defun tst ()
  (let ((msg (hcl:file-string "./xTActors/encoding.lisp"))
        (finp "./xTActors/aont-test")
        (fout "./xTActors/aont-test-result.txt"))
    (beta (dmsg)
        (send (aont-file-reader finp) beta)
      (with-open-file (fd fout
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
        (write-string dmsg fd))
      
      (assert (string= dmsg msg))
      (send writeln dmsg)
      )))
(tst)

(defun tst ()
  ;; Look at the frequency of occurrence of duplicate bytes in 32-byte
  ;; hashes...  ... about 86% of them have at least one duplicate
  ;; byte. 99.8% of them have 5 or fewer duplicates. None were
  ;; detected, in 10,000 trials, having more than 8 duplicates.
  ;;
  ;; 13.2% have no duplicates (about 1 in 7)
  ;; 29.4% of them have just one duplicate (almost 1 in 3)
  ;; 29.0% have 2 duplicates (about 1 in 3)
  ;; 17.9% have 3 duplicates (about 1 in 5)
  ;;  7.3% have 4 duplicates (about 1 in 14)
  ;;  2.4% have 5 duplicates (about 1 in 40)
  ;;  0.64% have 6 duplicates (about 1 in 160)
  ;;  0.06% have 7 duplicates (about 1 in 1700)
  ;;  - four were found to have 8 duplicates out of 10,000 trials
  ;;  - none were found with more than 8 duplicates
  ;;
  ;; Birthday Paradox would have predicted that among 32 candidates
  ;; from an alphabet of 256, we should see duplicates around 86.5% of
  ;; the time. Pdup(r; d) = (1 - Exp(-r^2/(2*d)))
  ;;
  ;; So to this extent it appears that SHA3/256 rehashings constitute
  ;; a reasonable PRF, doing about what you would have expected of
  ;; purely random sampling.
  ;;
  (let ((arr (make-array 33
                         :initial-element 0))
        (syms (make-array 256
                          :initial-element 0))
        (h (hash/256 :test)))
    (loop repeat 10000 do
          (let ((ix (- 32 (length (remove-duplicates (vec h))))))
            (incf (aref arr ix) 0.0001)
            (loop for sym across (vec h) do
                  (incf (aref syms sym) (/ 256 320_000)))
            (setf h (hash/256 h))
            ))
    (plt:plot 'syms syms
              :clear t
              :title "Distribution of Symbols in Hash (Uniform=1)"
              :xtitle "Symbol value"
              :ytitle "Fraction of cases"
              :thick 2
              :line-type :stepped
              :yrange '(0 3)
              )
    (plt:plot 'hist arr
              :clear t
              :title "Probability of duplicate bytes in SHA3/256 hash"
              :xtitle "Number of duplicates"
              :ytitle "Fraction of cases"
              :thick 2
              :line-type :stepped)
    (let ((carr (copy-seq arr)))
      (loop for ix from 1 to 32 do
            (setf (aref carr ix) (+ (aref carr ix)
                                    (aref carr (1- ix)))))
      (plt:plot 'cum carr
                :clear t
                :title "Cumulative Probability of duplicate bytes in SHA3/256 hash"
                :xtitle "Number of duplicates"
                :ytitle "Fraction of cases"
                :thick 2
                :line-type :stepped))
    (list arr
          (vm:mean syms)
          (vm:stdev syms))
    ))

(tst)

(defun tst ()
  ;; Testing QOE - Quality of Encryption - looking for pattern
  ;; irregularities in the encrypted output. There are none to be
  ;; found. See QOE.pdf.
  (let* ((img (make-array '(512 512)
                          :element-type '(unsigned-byte 8)))
         (h   (hash/256 :test)))
    (loop for ix from 0 below (array-total-size img) by 32 do
          (let ((v  (vec h)))
            (loop for jx from ix
                  for kx from 0 below 32
                  do
                  (setf (row-major-aref img jx) (aref v kx))
                  ))
          (setf h (hash/256 h)))
    (loop for ix from 250 below 350 do
          (loop for iy from 200 below 300 do
                (setf (aref img ix iy) 0)))
    (plt:window 'img :width 512 :height 512)
    (plt:tvscl 'img img)
    (let ((ekey (hash/256 :again)))
      (beta (seq enc)
          (send (pipe (marshal-encoder)
                      (encryptor ekey))
                beta img)
        (declare (ignore seq))
        (let ((dimg (make-array (array-dimensions img)
                                :element-type (array-element-type img)
                                :displaced-to enc
                                :displaced-index-offset 0)))
          (plt:window 'dimg :width 512 :height 512)
          (plt:tvscl 'dimg dimg)
          )))
    ))

(tst)

(defun tst ()
  ;; Testing QOE - Quality of Encryption - XOR of two encryptions to
  ;; show that no hint of the original documents leaks through. See
  ;; QOE.pdf.
  (let* ((img1 (make-array '(512 512)
                           :element-type '(unsigned-byte 8)))
         (img2 (make-array '(512 512)
                           :element-type '(unsigned-byte 8)))
         (h   (hash/256 :test)))
    (loop for ix from 0 below (array-total-size img1) by 32 do
          (let ((v  (vec h)))
            (loop for jx from ix
                  for kx from 0 below 32
                  do
                  (setf (row-major-aref img2 jx)
                        (setf (row-major-aref img1 jx) (aref v kx)))
                  ))
          (setf h (hash/256 h)))
    (loop for ix from 250 below 350 do
          (loop for iy from 200 below 300 do
                (setf (aref img1 ix iy) 0)))
    (loop for ix from 125 below 175 do
          (loop for iy from 125 below 175 do
                (setf (aref img2 ix iy) 0)))
    (plt:window 'img1 :width 512 :height 512)
    (plt:tvscl 'img1 img1)
    (plt:window 'img2 :width 512 :height 512)
    (plt:tvscl 'img2 img2)
    (let ((ekey (hash/256 :again)))
      (beta (seq enc1)
          (send (pipe (marshal-encoder)
                      (encryptor ekey))
                beta img1)
        (declare (ignore seq))
        (beta (seq enc2)
            (send (pipe (marshal-encoder)
                        (encryptor ekey))
                  beta img2)
          (declare (ignore seq))
        (let ((dimg1 (make-array (array-dimensions img1)
                                 :element-type (array-element-type img1)
                                 :displaced-to enc1
                                 :displaced-index-offset 0)))
          (map-into enc1 #'logxor enc1 enc2)
          (plt:window 'xor :width 512 :height 512)
          (plt:tvscl 'xor dimg1)
          )))
      )))
(tst)

;; -------------------------------------------------------------

(setf s (hcl:file-string "./xTActors/encoding.lisp"))

(let ((pipe (sink-pipe (marshal-encoder)
                       (chunker :max-size 16000)
                       (marshal-encoder)
                       ;; channel out
                       ;; channel in
                       (marshal-decoder)
                       (dechunker)
                       (marshal-decoder)
                       println)))
  (send (α _
          (send pipe s))))
  |#
