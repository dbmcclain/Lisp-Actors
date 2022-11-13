;; multi-committer.lisp
;;
;; DM/RAL  2022/11/08 06:34:36
;; ----------------------------------

(defpackage #:com.ral.actors.multi-committer
  (:use #:common-lisp #:ac #:kvdb)
  (:export
   #:multi-committer
   ))

(in-package #:com.ral.actors.multi-committer)

;; ----------------------------------
;; In situations where more than one database is involved in a
;; coordinated mutation it becomes necessary to invoke a kind of
;; SERIALIZER protocol.
;;
;; We need an ordered acqisition of such SERIALERs to avoid livelock.
;; To that end, we ensure that all databases have a persistent,
;; orderable, ID. Then we serialize in ordered sequence by those ID's.

(defun plist-items (plist)
  (um:nlet iter ((lst plist)
                 (acc nil))
    (if (endp lst)
        (nreverse acc)
      (go-iter (cddr lst) (cons (cadr lst) acc)))
    ))

(defun sort-kvdbs (cust kvdbs)
  ;; We expect a list of alternating db-id and kvdb Actor, where the
  ;; Actors are associated with the user specified db-id's (keyword
  ;; symbols?).
  ;;
  ;; Once sorted, the user will need those tags in order to locate the
  ;; kvdb Actor in the sorted access list.
  (let* ((grps (um:group kvdbs 2))
         (svcs (mapcar (lambda (pair)
                         (service (cadr pair) :find-or-add 'kvdb::kvdb-sequence (uuid:make-v1-uuid)))
                       grps)))
    (β ids
        (send (apply #'fork svcs) β)
      (send cust (mapcan #'identity
                         (mapcar #'cdr
                                 (sort (mapcar #'cons ids grps) #'uuid:uuid<
                                       :key #'car))))
      )))

;; -------------------------------------------------------------------------
;; The various kvdb's do not use a SERIALIZER gate, in order to permit
;; parallel queries with other ongong updates. But if you :REQ-EXCL,
;; that grants exclusive ownership for the purposes of :COMMIT. Once
;; you get ownership, you *must* either :COMMIT (which always
;; succeeds), or else :ABORT, to release ownership and allow others a
;; chance to proceed.
;;
;; Exclusive commit sessions are identified by a unique token. The
;; excl proxy handles this for you.  Committing or aborting must both
;; state the same token as was used when requesting exclusive commit
;; ownership.
;;
;; During exclusive commit ownership, any other requests for exclusive
;; commit ownership, or attempts by others to commit, become staged
;; for later execution. All other activities are permitted to run in
;; parallel with you.
;;
;; For the purposes of commit and requesting exclusive ownership, the
;; effect is the same as going through a SERIALIZER gate. Ordered
;; access to kvdb's is required in order to prevent livelock.
;; ------------------------------------------------------------------------

(defun excl-commits (cust kvdbs-plist &optional (owner (create)) acc)
  ;; Input a plist of id, kvdb -> plist of id, excl
  ;; Must be performed sequentially to avoid livelock.
  (if (endp kvdbs-plist)
      (send cust acc)
    (β (proxy)
        (send (cadr kvdbs-plist) β :req-proxy)
      (β (excl)
          (send proxy β :req-excl owner)
        (excl-commits cust (cddr kvdbs-plist) owner
                      (list* (car kvdbs-plist) excl acc))
        ))))

(defun terminate-session (cust oper open-kvdbs-plist)
  (let ((svcs (mapcar (lambda (open-kvdb)
                        (service open-kvdb oper))
                      (plist-items open-kvdbs-plist))))
    (become-sink)
    (β _
        (send (apply #'fork svcs) β)
      (send cust :ok))
    ))

(defun finish-multi-commit (cust open-dbs)
  (create
   (alambda
    ((:commit)
     (terminate-session cust :commit open-dbs))
    
    ((:abort)
     (terminate-session cust :abort open-dbs))
    )))

(defun multi-commit (cust action kvdbs-plist)
  (β (ordered-kvdbs-plist)
      (sort-kvdbs β kvdbs-plist)
    (β (open-kvdbs-plist)
        (excl-commits β ordered-kvdbs-plist)
      (let ((handler (finish-multi-commit cust open-kvdbs-plist)))
        (send action handler open-kvdbs-plist))
      )))

;; ---------------------------------------------------------------    
;; Protocol for use:
;;
;; Call function MULTI-COMMIT:
;;
;;      (MULTI-COMMIT cust action-Actor pliist-of-kvdbs)
;;
;; MULTI-COMMIT then opens all the kvdbs in consistent order, using
;; :REQ-EXCL, then sends a message to your action-Actor:
;;
;;      (SEND action-Actor handler plist-of-open-dbs)
;;
;; Action-Actor should do whatever processing it needs to do against
;; the open kvdb's and then send a response back to handler containing
;; either an :ABORT, or a :COMMIT.
;;
;;    (SEND handler :ABORT)
;;
;;     - or -
;;
;;    (SEND handler :COMMIT)
;;
;; At the end, whether by :ABORT or :COMMIT, an :OK message is sent to
;; your original cust argument.
;; -----------------------------------------------------------------

#|
(defun init-bank-bal ()
  (let-β ((kvdb1  (service (const kvdb)))
          (kvdb2  (service kvdb-maker :make-kvdb "diddly.db")))
    (let-β ((_  (service kvdb1 :add :bank-bal 10))
            (_  (service kvdb2 :add :bank-bal 0)))
      (let-β ((bal1  (service kvdb1 :find :bank-bal))
              (bal2  (service kvdb2 :find :bank-bal)))
        (send fmt-println "Bal1 = ~A" bal1)
        (send fmt-println "Bal2 = ~A" bal2))
      )))
(init-bank-bal)

(defun tst ()
  (let-β ((kvdb1  (service (const kvdb)))
          (kvdb2  (service kvdb-maker :make-kvdb "diddly.db")))
    (let-β ((bal1  (service kvdb1 :find-or-add :bank-bal 10))
            (bal2  (service kvdb2 :find-or-add :bank-bal 0)))
      (send fmt-println "Bal1 = ~A" bal1)
      (send fmt-println "Bal2 = ~A" bal2)
      (let ((action
             (create
              (lambda (handler plist)
                (let ((kvdb1 (getf plist :kvdb1))
                      (kvdb2 (getf plist :kvdb2))
                      (dd    7))
                  (let-β ((bal1  (service kvdb1 :find :bank-bal))
                          (bal2  (service kvdb2 :find :bank-bal)))
                    (let-β ((_ (service kvdb1 :add :bank-bal (- bal1 dd)))
                            (_ (service kvdb2 :add :bank-bal (+ bal2 dd))))
                      (send handler :commit)))
                  )))))
        (β _
            (multi-commit β action
                          `(:kvdb1 ,kvdb1
                            :kvdb2 ,kvdb2))
          (let-β ((bal1  (service kvdb1 :find :bank-bal))
                  (bal2  (service kvdb2 :find :bank-bal)))
            (send fmt-println "New Bal1 = ~A" bal1)
            (send fmt-println "New Bal2 = ~A" bal2)
            (send fmt-println "Sum = ~A" (+ bal1 bal2))))
        ))))
(tst)
      
 |#

