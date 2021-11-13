;; encoding.lisp -- extensions for primitive Actors
;;
;; DM/RAL 11/21
;; -------------------------------------------------

(in-package :actors/base)

(um:eval-always
  (import '(vec-repr:vec
            vec-repr:int
            vec-repr:hex
            hash:hash/256
            hash:get-hash-nbytes
            edec:ed-mul
            edec:ed-add
            edec:ed-compress-pt
            edec:ed-decompress-pt
            edec:make-deterministic-keys
            edec:*ed-gen*
            edec:*ed-r*
            edec:ed-pt=
            modmath:with-mod
            modmath:m+
            modmath:m*
            )))

;; ----------------------------------------------------
;; Useful primitives...

(defun encrypt/decrypt (ekey seq bytevec)
  ;; takes a bytevec and produces an encrypted/decrypted bytevec
  ;;
  ;; One-time-pad encryption via XOR with random mask. Take care to
  ;; never re-use the same mask for encryption, which is the hash of
  ;; the ekey concat with seq.
  (let* ((nel  (length bytevec))
         (mask (vec (get-hash-nbytes nel (vec ekey) seq))))
    (map-into mask #'logxor bytevec mask)))

(defun make-signature (seq emsg chk skey)
  ;; Generate and append a Schnorr signature - signature includes seq
  ;; and emsg.
  ;;
  ;; We take care to use deterministic hashing so that the same
  ;; message and seq always produces the same signature, for the given
  ;; secret key, skey. This is doubly cautious here, since seq is only
  ;; ever supposed to be used just once.
  (let* ((pkey  (ed-mul *ed-gen* skey))
         (krand (int (hash/256 seq emsg chk skey pkey)))
         (kpt   (ed-mul *ed-gen* krand))
         (h     (int (hash/256 seq emsg chk kpt pkey)))
         (u     (with-mod *ed-r*
                  (m+ krand (m* h skey))))
         (upt   (ed-mul *ed-gen* u)))
    (list (int upt) krand)
    ))

(defun check-signature (seq emsg chk sig pkey)
  ;; takes seq, emsg, chk, and sig (a Schnorr signature on seq+emsg+chk),
  ;; and produce t/f on signature as having come from pkey.
  (destructuring-bind (upt krand) sig
    (let* ((kpt  (ed-mul *ed-gen* krand))
           (h    (int (hash/256 seq emsg chk kpt pkey))))
      (ed-pt= (ed-decompress-pt upt) (ed-add kpt (ed-mul pkey h)))
      )))

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
    (send cust (subseq (xzlib:compress bytevec :fixed) 0))))

(defun marshal-decompressor ()
  ;; takes a bytevec and produces a bytevec
  (actor (cust cmprvec)
    (send cust (xzlib:uncompress cmprvec))))

;; ---------------------------------------------------------------

(defun noncer ()
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
    (beta (seq)
        (send *noncer* beta :get-nonce)
      (let ((chk (vec (hash/256 bytevec))))
        (send cust seq (encrypt/decrypt ekey seq bytevec) chk)
        ))))

(defun decryptor (ekey)
  ;; Takes an encrypted bytevec and produces a bytevec
  (actor (cust seq emsg chk)
    (let* ((bytvec (encrypt/decrypt ekey seq emsg))
           (dchk   (vec (hash/256 bytvec))))
      (if (equalp dchk chk)
          (send cust bytvec)
        (error "decryptor: failure")
        ))))

(defun signing (skey)
  (actor (cust seq emsg chk)
    (let ((sig (make-signature seq emsg chk skey)))
      (send cust seq emsg chk sig))))

(defun signature-validation (pkey)
  (actor (cust seq emsg chk sig)
    (if (check-signature seq emsg chk sig pkey)
        (send cust seq emsg chk)
      (error "signature-validation: failure"))))

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
    (let ((size (length byte-vec)))
      (cond ((<= size max-size)
             (send cust :pass byte-vec))
            (t
             (let* ((nchunks (ceiling size max-size))
                    (id      (int (hash/256 (uuid:make-v1-uuid)))))
               (send cust :init id nchunks size)
               (do ((offs  0  (+ offs max-size))
                    (ix    0  (1+ ix)))
                   ((>= offs size))
                 (send cust :chunk id ix offs
                       (subseq byte-vec offs (min size (+ offs max-size)))))
               ))
            ))))

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

(defun make-ubv (nb)
  (make-array nb
              :element-type '(unsigned-byte 8)))

(defun dechunker ()
  ;; No assumptions about chunk or init delivery order.
  ;; Takes a sequence of chunk encodings and produces a bytevec
  (labels ((initial-dechunker-beh (delivery)
             (alambda
              ((_ :init id nchunks size)
               (become (dechunker-beh
                        :vec      (make-ubv size)
                        :id       id
                        :nchunks  nchunks
                        :delivery delivery))
               (send delivery self :chunk? id 0))

              ((cust :pass bytevec)
               (send cust bytevec))

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

              ((cust :pass bytevec)
               (send cust bytevec))
              
              ( _
                (repeat-send delivery))
              )))
    (let ((delivery (make-actor (null-ordered-delivery-beh))))
      (make-actor (initial-dechunker-beh delivery))
      )))

;; -----------------------------------------------------------

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
    (make-deterministic-keys :test)
  (let ((ekey (hash/256 skey pkey)))
    (send (pipe (encr-disk-encoder ekey skey :max-chunk 16)
                (writer)
                (encr-disk-decoder ekey pkey)
                (writer))
          sink "This is a test")))

(let ((junk (make-ubv 1022)))
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
                      (marshal-compressor)
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
                      (marshal-decompressor)
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
;; -----------------------------------------------------------------------
#|
;; -------------------------------------------------------------------
;; Encrypted Disk Files - AONT Encoding
;;
;; File is saved in self-sync encoding. That doesn't help much if the
;; encrypted portions of the file get clobbered, but could help in the
;; recovery of remaining items. File uses flexible length encodings
;; rather than fixed allocations.

    +------------------------------------+
    | File Type UUID                     | = AONT type {b532fc4e-bf2b-123a-9307-24f67702cdaa}
    +------------------------------------+
    | (LIST pkey-vec encr-data aont-vec) |
    +------------------------------------+

  The encr-data in the :TEXT section is a marshal encoding of a list of 4 items:
   (LIST seq cipher-text chk sig)
  where,
    seq = the subkey of this encryption. Actual encryption key is H(master-key | seq).
    cipher-text = encrypted, compressed, marshaled list of Lisp data items.
    chk = SHA3/256 hash of pre-encryption compressed, marshaled, data.
    sig = signature on (seq cipher-text chk) and validated against pkey.

  The chk serves to indicate whether or not decryption has succeeded.
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
    ;; takes arbitrary Lisp data items and encodes as a list
    (let ((pkey-vec (vec (ed-mul *ed-gen* skey))))
      (beta (data-packet)
          (send (pipe (marshal-encoder)
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

(defun aont-decoder ()
  ;; returns a list of original Lisp data items
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
  (ioreq
   (io
    (actor (cust &rest msg)
      (beta (packet)
          (send* (pipe (aont-encoder skey ekey)
                       (marshal-encoder)
                       (self-sync-encoder))
                 beta msg)
        (with-open-file (fd fname
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create
                            :element-type '(unsigned-byte 8))
          (write-sequence +AONT-FILE-TYPE-ID+ fd)
          (write-sequence packet fd)
          (send cust :ok)
          )))
    )))

(defun aont-file-reader (fname)
  ;; Reads a single AONT record from a file
  (ioreq
   (io
    (actor (cust)
      (with-open-file (fd fname
                          :direction :input
                          :element-type '(unsigned-byte 8))
        (let ((file-type (make-ubv 16)))
          (read-sequence file-type fd)
          (if (equalp +AONT-FILE-TYPE-ID+ file-type)
              (let ((reader (self-sync:make-reader fd)))
                (send (pipe (marshal-decoder)
                            (aont-decoder))
                      cust (funcall reader)))
            (error "~A: Not an AONT encoded file" fname))
          )))
    )))

;; -----------------------------------------------------------------
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
              (send (aont-decoder) beta pkey-vec data-packet aont-vec)
            (send writeln dmsg)
            (assert (equalp msg (car dmsg)))
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
        (write-string (car dmsg) fd))
      
      (assert (string= (car dmsg) msg))
      (send writeln dmsg)
      )))
(tst)
  |#
