;; tests.lisp
;;
;; DM/RAL  2024/01/31 02:20:44 UTC
;; ----------------------------------

(defpackage #:tests
  (:use #:common-lisp #:ac #:edec #:modmath #:hash #:vec-repr))

(in-package #:tests)

;; ----------------------------------

(defun mdiv-test (cust curve niter)
  (create
   (lambda ()
     (with-ed-curve curve
       (with-mod *ed-r*
         (loop repeat niter do
               (m/ (random *ed-r*)))))
     (send cust :mdiv-test))))

(defun mexp-test (cust curve niter)
  (create
   (lambda ()
     (with-ed-curve curve
       (with-mod *ed-r*
         (loop repeat niter do
               (m^ (random *ed-r*) (- *ed-r* 2)))
         ))
     (send cust :mexp-test))))

#|
(let* ((niter 100_000)
       (curve :curve1174)
       (gate (once writeln))
       (mdiv (mdiv-test gate curve niter))
       (mexp (mexp-test gate curve niter)))
  (send mdiv)
  (send mexp))
|#
;; --------------------------------------------

(defun ratchet-manager-beh (role state)
  (labels ((iter-key (key pt ct)
             (if (zerop ct)
                 key
               (iter-key (hash/256 :ekey key pt) pt (1- ct))
               )))
    (with-state-vals ((root-key :root-key)
                      (tx-nbr   :tx-nbr)
                      (skey     :skey)
                      (pkey     :pkey)
                      (dh-pt    :dh-pt)
                      (dh-tau   :dh-tau)
                      (dh-key   :dh-key)
                      (ack-tau  :ack-tau)
                      (dict     :dict)) state
      (alambda
       ;; ------------------------------------------------------
       ;; Get encryption keying
       
       ((cust :tx-key)
        (let* ((next-tx-nbr  (1+ tx-nbr))
               (tx-key       (iter-key root-key dh-pt next-tx-nbr)))
          (become (ratchet-manager-beh
                   role
                   (state-with state
                               :tx-nbr  next-tx-nbr)))
          (send cust next-tx-nbr dh-tau ack-tau tx-key)))

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
        (let ((ack-key (int (elli2-decode ack))))
          (cond
           ((eql ack-key dh-key) ;; does message ack our proposed keying?
            ;; Time to ratchet forward...
            (let+ ((new-ack-pt (ed-mul (elli2-decode tau) skey))
                   (:mvb (new-root rx-key new-dict)
                    (case role
                      (:CLIENT ;; clients always initiate a conversation
                       (let+ ((new-root   (hash/256 :ratchet-1 root-key dh-pt))
                              (rx-key     (iter-key new-root new-ack-pt seq))
                              (new-dict   (maps:add dict
                                                    ack-key (actor-state
                                                             :root  new-root
                                                             :dh-pt new-ack-pt
                                                             :seqs  (list seq))
                                                    )))
                         (values new-root rx-key new-dict)))
                      (:SERVER ;; servers simply respond at first
                       (let+ ((new-dict   (maps:add dict
                                                    ack-key (actor-state
                                                             :root  root-key
                                                             :dh-pt new-ack-pt
                                                             :seqs  (list seq))
                                                    ))
                              (rx-key     (iter-key root-key new-ack-pt seq))
                              (new-root   (hash/256 :ratchet-1 root-key new-ack-pt)))
                         (values new-root rx-key new-dict)))
                      ))
                   ;; ------------------------------------------
                   ;; At this point we have a decryption key. Let's be
                   ;; sure we aren't being spoofed before making any
                   ;; updates to our state.
                   (:mvb (is-ok auth-key)
                    #+nil (check-auth rx-key iv ctxt auth)
                    #-nil (values t 15))) ;; for testing...
              (when is-ok
                (let+ ((:mvb (new-dh-rand new-dh-tau)
                        (compute-deterministic-elligator-skey
                         :ratchet2 new-root dh-pt))
                       (new-dh-key (int (ed-nth-pt new-dh-rand)))
                       (new-dh-pt  (ed-mul pkey new-dh-rand)))
                  (become (ratchet-manager-beh
                           role
                           (state-with state
                                       :root-key new-root
                                       :tx-nbr   0
                                       :ack-tau  tau
                                       :dh-tau   new-dh-tau
                                       :dh-pt    new-dh-pt
                                       :dh-key   new-dh-key
                                       :dict     new-dict)
                           ))
                  (send cust rx-key auth-key)
                  ))
              ))
           
           ;; ----------------------------------------------
           ;; Stale keying in message - lookup how to decrypt
           (t
            (um:when-let (entry (maps:find dict ack-key))
              (with-state-vals ((root  :root)
                                (dh-pt :dh-pt)
                                (seqs  :seqs)) entry
                ;; avoid replay attacks
                (unless (member seq seqs)
                  (let+ ((rx-key  (iter-key root dh-pt seq))
                         ;; ------------------------------------------
                         ;; At this point we have a decryption key. Let's be
                         ;; sure we aren't being spoofed before making any
                         ;; updates to our state.
                         (:mvb (is-ok auth-key)
                          #+nil (check-auth rx-key iv ctxt auth)
                          #-nil (values t 15))) ;; for testing...
                    (when is-ok
                      (let+ ((new-entry (state-with entry
                                                    :seqs (cons seq seqs)))
                             (new-dict  (maps:add dict ack-key new-entry)))
                        (become (ratchet-manager-beh
                                 role
                                 (state-with state
                                             :dict new-dict)))
                        (send cust rx-key auth-key)
                        ))))
                )))
           ;; ---------------------------------------------------
           )))
       ))))
#|
(defun client-ratchet-manager-beh (state)
  (labels ((iter-key (key pt ct)
             (if (zerop ct)
                 key
               (iter-key (hash/256 :ekey key pt) pt (1- ct))
               )))
    (with-state-vals ((root-key :root-key)
                      (tx-nbr   :tx-nbr)
                      (skey     :skey)
                      (pkey     :pkey)
                      (dh-pt    :dh-pt)
                      (dh-tau   :dh-tau)
                      (dh-key   :dh-key)
                      (ack-tau  :ack-tau)
                      (dict     :dict)) state
      (alambda
       ((cust :tx-key)
        (let* ((next-tx-nbr  (1+ tx-nbr))
               (tx-key       (iter-key root-key dh-pt next-tx-nbr)))
          (become (client-ratchet-manager-beh (state-with state
                                                   :tx-nbr  next-tx-nbr)))
          (send cust next-tx-nbr dh-tau ack-tau tx-key)))
      
       ((cust :rx-key seq tau ack)
        (let ((ack-key (int (elli2-decode ack))))
          (cond ((eql ack-key dh-key)
                 (let+ ((new-ack-pt (ed-mul (elli2-decode tau) skey))
                        ;; -----------------------------------------------
                        (new-root   (hash/256 :ratchet-1 root-key dh-pt))
                        (rx-key     (iter-key new-root new-ack-pt seq))
                        ;; -----------------------------------------------
                        (new-dict   (maps:add dict
                                              ack-key (actor-state
                                                       :root  new-root
                                                       :dh-pt new-ack-pt
                                                       :seqs  (list seq))
                                              ))
                        ;; -----------------------------------------------
                        (:mvb (new-dh-rand new-dh-tau)
                         (compute-deterministic-elligator-skey
                          :ratchet2 new-root dh-pt))
                        (new-dh-key (int (ed-nth-pt new-dh-rand)))
                        (new-dh-pt  (ed-mul pkey new-dh-rand)))
                   (become (client-ratchet-manager-beh
                            (state-with state
                                        :root-key new-root
                                        :tx-nbr   0
                                        :ack-tau  tau
                                        :dh-tau   new-dh-tau
                                        :dh-pt    new-dh-pt
                                        :dh-key   new-dh-key
                                        :dict     new-dict)
                            ))
                   (send cust rx-key)
                   ))

                ;; ----------------------------------------------
                ;; Stale keying in message - lookup how to decrypt
                (t
                 (um:when-let (entry (maps:find dict ack-key))
                   (with-state-vals ((root  :root)
                                     (dh-pt :dh-pt)
                                     (seqs  :seqs)) entry
                     ;; avoid replay attacks
                     (unless (member seq seqs)
                       (let+ ((rx-key    (iter-key root dh-pt seq))
                              (new-entry (state-with entry
                                                     :seqs (cons seq seqs)))
                              (new-dict  (maps:add dict ack-key new-entry)))
                         (become (client-ratchet-manager-beh
                                  (state-with state
                                              :dict new-dict)))
                         (send cust rx-key)
                         ))
                     )))
                )))
       ))))

(defun server-ratchet-manager-beh (state)
  (labels ((iter-key (key pt ct)
             (if (zerop ct)
                 key
               (iter-key (hash/256 :ekey key pt) pt (1- ct))
               )))
    (with-state-vals ((root-key :root-key)
                      (tx-nbr   :tx-nbr)
                      (skey     :skey)
                      (pkey     :pkey)
                      (dh-pt    :dh-pt)
                      (dh-tau   :dh-tau)
                      (dh-key   :dh-key)
                      (ack-tau  :ack-tau)
                      (dict     :dict)) state
      (alambda
       ((cust :tx-key)
        (let* ((next-tx-nbr  (1+ tx-nbr))
               (tx-key       (iter-key root-key dh-pt next-tx-nbr)))
          (become (server-ratchet-manager-beh (state-with state
                                                   :tx-nbr  next-tx-nbr)))
          (send cust next-tx-nbr dh-tau ack-tau tx-key)))
      
       ((cust :rx-key seq tau ack)
        (let ((ack-key  (int (elli2-decode ack))))
          (cond ((eql ack-key dh-key)
                 (let+ ((new-ack-pt (ed-mul (elli2-decode tau) skey))
                        ;; -----------------------------------------------
                        (new-dict   (maps:add dict
                                              ack-key (actor-state
                                                       :root  root-key
                                                       :dh-pt new-ack-pt
                                                       :seqs  (list seq))
                                              ))
                        ;; -----------------------------------------------
                        (rx-key     (iter-key root-key new-ack-pt seq))
                        (new-root   (hash/256 :ratchet-1 root-key new-ack-pt))
                        ;; -----------------------------------------------
                        (:mvb (new-dh-rand new-dh-tau)
                         (compute-deterministic-elligator-skey
                          :ratchet2 new-root dh-pt))
                        (new-dh-key (int (ed-nth-pt new-dh-rand)))
                        (new-dh-pt  (ed-mul pkey new-dh-rand)))
                   (become (server-ratchet-manager-beh
                            (state-with state
                                        :root-key new-root
                                        :tx-nbr   0
                                        :ack-tau  tau
                                        :dh-tau   new-dh-tau
                                        :dh-pt    new-dh-pt
                                        :dh-key   new-dh-key
                                        :dict     new-dict)
                            ))
                   (send cust rx-key)
                   ))

                ;; ----------------------------------------------
                ;; Stale keying in message - lookup how to decrypt
                (t
                 (um:when-let (entry (maps:find dict ack-key))
                   (with-state-vals ((root  :root)
                                     (dh-pt :dh-pt)
                                     (seqs  :seqs)) entry
                     ;; avoid replay attacks
                     (unless (member seq seqs)
                       (let+ ((rx-key    (iter-key root dh-pt seq))
                              (new-entry (state-with entry
                                                     :seqs (cons seq seqs)))
                              (new-dict  (maps:add dict ack-key new-entry)))
                         (become (server-ratchet-manager-beh
                                  (state-with state
                                              :dict new-dict)))
                         (send cust rx-key)
                         ))
                     )))
                )))
       ))))
|#

(defun dummy-ratchet-keying (ekey)
  (let+ ((ack-pt  (ed-nth-pt 99))
         (:mvb (ack-rand ack-tau)
          (compute-deterministic-elligator-skey
           :ratchet-2 ekey ack-pt))
         (ack-key (int (ed-nth-pt ack-rand))))
    (values ack-tau ack-pt ack-key)))
  
(defun client-ratchet-manager (ekey skey pkey)
  (let+ ((:mvb (ack-tau ack-pt)
          (dummy-ratchet-keying ekey))
         (:mvb (dh-rand dh-tau)
          (compute-deterministic-elligator-skey
           :ratchet-2 ekey ack-pt pkey))
         (dh-key (int (ed-nth-pt dh-rand)))
         (dh-pt  (ed-mul pkey dh-rand)))
    (create
     (ratchet-manager-beh
      :CLIENT
      (actor-state
       :root-key  ekey
       :pkey      pkey
       :skey      skey
       :tx-nbr    0
       :dh-tau    dh-tau
       :dh-pt     dh-pt
       :dh-key    dh-key
       :ack-tau   ack-tau
       :dict      (maps:empty)
       )))
   ))

(defun server-ratchet-manager (ekey skey pkey)
  (let+ ((:mvb (ack-tau ack-pt ack-key)
          (dummy-ratchet-keying ekey)))
    (create
     (ratchet-manager-beh
      :SERVER
      (actor-state
       :root-key  ekey
       :pkey      pkey
       :skey      skey
       :dh-tau    ack-tau
       :dh-pt     ack-pt
       :dh-key    ack-key
       :dict      (maps:empty)
       ))
     )))

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
       (>> socket :auth-key seq tau ack nonce (vec auth-key))
       (encr/decr ekey nonce bytevec)
       (send cust bytevec))
     ))
   ))

;; ------------------------------------------------------------------

(defun ratchet-encryptorx (ratchet-manager)
  (create
   (lambda (cust bytevec)
     (let+ ((:β (seq tau ack ekey) (racurry ratchet-manager :tx-key))
            (nonce :nonce)
            (auth  :auth))
       (send cust seq tau ack nonce ekey auth)
       ))
   ))

(defun ratchet-decryptorx (socket ratchet-manager)
  (create
   (alambda
    ((cust seq tau ack nonce bytevec auth)
     (let+ ((:β (ekey auth-key)
                (racurry ratchet-manager
                         :rx-key seq tau ack nonce bytevec auth)))
       (send writeln (list ekey bytevec))
       (>> socket :auth-key (vec auth-key))
       (send cust bytevec))
       ))
   ))

#|
(let+ ((:mvb (skc pkc) (ed-random-pair))
       (:mvb (sks pks) (ed-random-pair))
       (ekey (hash/256 :test))
       (rmc  (client-ratchet-manager ekey skc pks))
       (rms  (server-ratchet-manager ekey sks pkc))
       (sockc (create
               (lambda* msg
                 (send fmt-println "Client -> ~S" msg))))
       (socks (create
               (lambda* msg
                 (send fmt-println "Server -> ~S" msg))))
       (rec  (ratchet-encryptorx rmc))
       (rdc  (ratchet-decryptorx sockc rmc))
       (res  (ratchet-encryptorx rms))
       (rds  (ratchet-decryptorx socks rms))
       (double-up (create (lambda (cust &rest msg)
                            (send* cust msg)
                            (send* cust msg))))
       (you-made-it (create (lambda (cust &rest msg)
                              (send println "You made it!")
                              (send* cust msg))))
       (top  (sink-pipe
              rec
              rds
              res
              rdc
              double-up
              rec
              rds
              res
              rdc
              you-made-it
              writeln)))
  (print "=======================================")
  (send top "Hello")
  )
|#

             
