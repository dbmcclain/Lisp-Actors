;; double-ratchet.lisp
;;
;; DM/RAL  2024/01/29 05:07:52 UTC
;; ----------------------------------

(in-package :com.ral.actors.encoding)

;; -----------------------------------------------------------
;; Double-Ratchet done properly...

(defun ratchet-manager-beh (role state &optional tag)
  (labels ((iter-hash (key pt ct)
             (if (zerop ct)
                 key
               (iter-hash (hash/256 :ekey key pt) pt (1- ct))
               )))
    (with-state-vals ((root-key :root-key)    ;; current root for ratcheting, hash - evolving
                      (tx-nbr   :tx-nbr)      ;; sequence number of last Tx cryptotext
                      (skey     :skey)        ;; my secret key
                      (pkey     :pkey)        ;; other party's public key
                      (dh-pt    :dh-pt)       ;; current DH shared point, integer - evolving
                      (dh-key   :dh-key)      ;; current DH random point, integer - evolving
                      (ack-key  :ack-key)     ;; prior DH random point, integer - from recipient
                      (dict     :dict)) state ;; limited dictionary of stale encryption keying
      (alambda
       ;; ------------------------------------------------------
       ;; Get encryption keying and preamble
       
       ((cust :tx-key)
        (with-ed-curve +ECC-CURVE+
          ;; Refreshing the Elligator encodings provides for a slighly
          ;; randomized presentation when the unerlying Elligators are
          ;; repeated in another preamble.
          (let* ((next-tx-nbr  (1+ tx-nbr))
                 (dh-tau       (refresh-elligator dh-key))
                 (ack-tau      (refresh-elligator ack-key))
                 (tx-key       (iter-hash root-key dh-pt next-tx-nbr)))
            (become (ratchet-manager-beh
                     role
                     (state-with state
                                 :tx-nbr  next-tx-nbr)
                     tag))
            ;; Encryption key, tk-key, is: K(n)= H(root, dh-pt)^n,
            ;; for tx-nbr = n, root-key = root, dh-pt = random point, R.
            (send cust next-tx-nbr dh-tau ack-tau tx-key) )))
       ;;                  ^         ^       ^      ^
       ;;                  |         |       |      |
       ;;                  |         |       |      +-- encryption key to use
       ;;                  |         |       +-- Old keying acknowledgement
       ;;                  |         +-- New keying in use
       ;;                  +-- Tx in series for DH-TAU keying: 1,2,3...
       ;;
       ;; DH-TAU, ACK-TAU are Elligator encodings of random EC points (integer).
       ;; NEXT-TX-NBR is a simple integer. Every message numbered sequentially.
       ;; TX-KEY is a 256-bit hash, used as an AES-256 key.
       ;;
       ;; (Hint: DH = Diffie-Hellman). I send you a random point, R = r*G.
       ;; Our shared key is
       ;;
       ;;           K = s*R = r*P = r*s*G
       ;;
       ;;  where s is recipient's secret key, and P is recipient's
       ;;  public key. I know the random value, r, used, and
       ;;  recipient's public key, P.
       ;;
       ;;  But we actually send across an Elligator encoding of random
       ;;  point, R.)
       ;;
       ;; --------------------------------------------------------
       ;; Get decryption keying

       ((cust :rx-key seq tau ack iv ctxt auth)
        ;; It is the nature of the beast, that Elligator makes a curve
        ;; point appear as a random string, and does so in a random
        ;; manner.
        ;;
        ;; Every new Elligator encoding of a point will look
        ;; different. So you can't use an Elligator encoding to
        ;; discern differences. You have to decode to a fixed
        ;; representation first.
        ;;
        (with-ed-curve +ECC-CURVE+
          (let ((inp-ack-key (ignore-errors
                               (elligator-body ack)))
                (tau-pt      (ignore-errors
                               (elli2-decode tau))))
            (cond
             ((or (null tau-pt)
                  (not (integerp seq)))
              ;; Intentional injection attack. Extremely rare bit
              ;; pattern to be one of the very few invalid encodings.
              ;; Else, just wasn't an integer. Just drop and ignore
              ;; the input message.
              )
             ;; ----------------------------------
             
             ((eql inp-ack-key dh-key) ;; does message ack our proposed keying?
              ;; Time to ratchet forward...
              ;; Choose new root-key and DH points
              (let+ ((new-ack-pt  (int (ed-mul tau-pt skey))) ;; other party's random DH point
                     (:mvb (new-root rx-key new-dict)
                      (case role
                        (:CLIENT ;; clients always initiate a conversation
                         (let+ ((new-root   (hash/256 :ratchet-1 root-key dh-pt))
                                (rx-key     (iter-hash new-root new-ack-pt seq))
                                (new-dict   (maps:add dict
                                                      inp-ack-key
                                                      (actor-state
                                                       :bday  (get-universal-time) ;; birthday of entry
                                                       :root  new-root             ;; root at time of ratchet
                                                       :dh-pt new-ack-pt           ;; new DH random pt
                                                       :seqs  (list seq))
                                                      )))
                           (values new-root rx-key new-dict)))
                        (:SERVER ;; servers simply respond at first
                         (let+ ((new-dict   (maps:add dict
                                                      inp-ack-key
                                                      (actor-state
                                                       :bday  (get-universal-time)
                                                       :root  root-key
                                                       :dh-pt new-ack-pt
                                                       :seqs  (list seq))
                                                      ))
                                (rx-key     (iter-hash root-key new-ack-pt seq))
                                (new-root   (hash/256 :ratchet-1 root-key new-ack-pt)))
                           (values new-root rx-key new-dict)))
                        ))
                     ;; ------------------------------------------
                     ;; At this point we have a decryption key. Let's be
                     ;; sure we aren't being spoofed before making any
                     ;; updates to our state.
                     (:mvb (is-ok auth-key)
                      (ignore-errors
                        (check-auth rx-key iv ctxt auth))))
                (when is-ok
                  (let+ ((:mvb (new-dh-rand new-dh-tau) ;; new random Elligator
                          (compute-deterministic-elligator-skey
                           :ratchet-2 new-root dh-pt))
                         (new-dh-key (elligator-body new-dh-tau))     ;; our new DH random point
                         (new-dh-pt  (int (ed-mul pkey new-dh-rand))) ;; our new DH shared point
                         (tag        (tag self)))
                    (become (ratchet-manager-beh
                             role
                             (state-with state
                                         :root-key new-root
                                         :tx-nbr   0            ;; start counting anew
                                         :ack-key  (elligator-body tau) ;; sender's last DH Elligator
                                         :dh-key   new-dh-key   ;; our new DH Elligator
                                         :dh-pt    new-dh-pt    ;; our DH shared point
                                         :dict     new-dict)
                             tag))
                    (send cust rx-key auth-key)
                    (send-after 10 tag :clean) ;; clear out stale encryption info in 10 sec
                    ))
                ))
             
             ;; ----------------------------------------------
             ;; Stale keying in message - lookup how to decrypt
             (t
              (um:when-let (entry (maps:find dict inp-ack-key))
                (with-state-vals ((root  :root)  ;; root key at time we saw inp-ack to ratchet forward
                                  (dh-pt :dh-pt) ;; DH shared pt
                                  (seqs  :seqs)) entry
                  ;; avoid replay attacks
                  (unless (member seq seqs)
                    (let+ ((rx-key  (iter-hash root dh-pt seq))
                           ;; ------------------------------------------
                           ;; At this point we have a decryption key. Let's be
                           ;; sure we aren't being spoofed before making any
                           ;; updates to our state.
                           (:mvb (is-ok auth-key)
                            (check-auth rx-key iv ctxt auth)))
                      (when is-ok
                        (let+ ((new-entry (state-with entry
                                                      :bday (get-universal-time) ;; new birthday for entry
                                                      :seqs (cons seq seqs)))
                               (new-dict  (maps:add dict ack-key new-entry))
                               (tag       (tag self)))
                          (become (ratchet-manager-beh
                                   role
                                   (state-with state
                                               :dict new-dict)
                                   tag))
                          (send cust rx-key auth-key)
                          (send-after 10 tag :clean)
                          ))))
                  )))
             ))))
       ;; ---------------------------------------------------
       ;; Clean out old stale-keying entries
       
       ((atag :clean) / (eq atag tag)
        (let+ ((now      (get-universal-time))
               (new-dict (maps:fold dict
                                    (lambda (k v acc)
                                      (with-state-vals ((bday :bday)) v
                                        (if (> (- now bday) 10)
                                            (maps:remove acc k)
                                          acc)))
                                dict)))
          (unless (eq dict new-dict)
            (let ((tag (unless (maps:is-empty new-dict)
                         (tag self))))
              (become (ratchet-manager-beh
                       role
                       (state-with state
                                   :dict new-dict)
                       tag))
              (when tag
                (send-after 10 tag :clean))
              ))
          ))
       ))))

(defun dummy-ratchet-keying (ekey)
  (let+ ((ack-pt  (int (ed-pt-from-seed
                        :initial-ack-point ekey)))
         (:mvb (_ ack-tau)
          (compute-deterministic-elligator-skey
           :ratchet-2 ekey ack-pt))
         (ack-key (elligator-body ack-tau)))
    (values ack-pt ack-key)))
  
(defun client-ratchet-manager (ekey skey pkey)
  (with-ed-curve +ECC-CURVE+
    (let+ ((:mvb (ack-pt ack-key)
            (dummy-ratchet-keying ekey))
           (:mvb (dh-rand dh-tau)
            (compute-deterministic-elligator-skey
             :ratchet-2 ekey ack-pt pkey))
           (dh-key (elligator-body dh-tau))
           (dh-pt  (int (ed-mul pkey dh-rand))))
      (create
       (ratchet-manager-beh
        :CLIENT
        (actor-state
         :root-key  ekey
         :pkey      pkey
         :skey      skey
         :tx-nbr    0
         :dh-pt     dh-pt
         :dh-key    dh-key
         :ack-key   ack-key
         :dict      (maps:empty)
         )))
      )))

(defun server-ratchet-manager (ekey skey pkey)
  (with-ed-curve +ECC-CURVE+
    (let+ ((:mvb (ack-pt ack-key)
            (dummy-ratchet-keying ekey)))
      (create
       (ratchet-manager-beh
        :SERVER
        (actor-state
         :root-key  ekey
         :pkey      pkey
         :skey      skey
         :dh-pt     ack-pt
         :dh-key    ack-key
         :dict      (maps:empty)
         ))
       ))))

(defun ratchet-encryptor (ratchet-manager)
  (create
   (lambda (cust bytevec)
     (let+ ((:β (seq tau ack ekey) (racurry ratchet-manager :tx-key))
            (nonce (encr/decr ekey nil bytevec))
            (auth  (make-auth ekey nonce bytevec)))
       (>> cust seq tau ack nonce bytevec auth)
       ))
   ))

(defun ratchet-decryptor (socket ratchet-manager)
  (create
   (alambda
    ((cust seq tau ack nonce bytevec auth)
     (let+ ((:β (ekey auth-key)
                (racurry ratchet-manager
                         :rx-key seq tau ack nonce bytevec auth)))
       ;; Our committment to refutability - publish the last auth-key
       (>> socket :auth-key (vec auth-key))
       (encr/decr ekey nonce bytevec)
       (send cust bytevec))
     ))
   ))

;; -----------------------------------------------------------
