;; multi-committer.lisp
;;
;; DM/RAL  2022/11/08 06:34:36
;; ----------------------------------

(defpackage #:com.ral.actors.multi-committer
  (:use #:common-lisp #:ac #:kvdb)
  (:export
   #:multi-comitter
   ))

(in-package #:com.ral.actors.multi-committer)

;; ----------------------------------
;; In situations where more than one database is involved in a
;; coordinated mutation it becomes necessary to invoke a SERIALIZER
;; protocol. We need an ordered acqisition of SERIALERs to avoid
;; livelock.
;;
;; To that end, we ensure that all databases have a persistent,
;; orderable, ID. Then we serialize in sequence by those ID's.

(defvar *ordering-id-key*  #/uuid/{5caa85a4-5f7b-11ed-86c1-787b8acbe32e})

(defun ensure-orderable (cust db)
  (β  (id)
      (send db β :lookup *ordering-id-key*)
    (if id
        (send cust id)
      (let ((id  (uuid:make-v1-uuid)))
        (β _
            (send db β :add *ordering-id-key* id)
          (send cust id)))
      )))

(defun sort-kvdbs (cust &rest dbs)
  ;; we expect a list of alternating db-id and kvdb Actor, where the
  ;; Actors are associated with the user specified db-id's (keyword
  ;; symbols?).
  ;;
  ;; Once sorted, the user will need those tags in order to locate the
  ;; kvdb Actor in the sorted access list.
  (let ((sorter (create
                 (lambda (dbs &optional acc)
                   (if (endp dbs)
                       ;; return a PLIST of sorted kvdbs
                       (send cust (mapcan #'list
                                          (mapcar #'cdr
                                                  (sort acc #'uuid:uuid<
                                                        :key #'car))))
                     (let ((me  self)
                           (db-id  (car dbs))
                           (db     (cadr dbs)))
                       (β (id)
                           (ensure-orderable β db)
                         (send me (cdr dbs) (acons id (cons db-id db) acc))
                         ))
                     ))
                 )))
    (send sorter dbs)))

;; The various kvdb's do not use a SERIALIZER gate, in order to permit
;; parallel queries with other ongong updates. But if you :REQ-EXCL,
;; that grants exclusive ownership for the purposes of :COMMIT. Once
;; you get ownership, you *must* either :COMMIT successfully, or else
;; :ABORT, to release ownership and allow others a chance to
;; proceed.
;;
;; Committing and giving-up, must both state the same customer as was
;; used when requesting exclusive commit ownership. Otherwise you will
;; be staged and wind up livelocking the database.
;;
;; During exclusive commit ownership, any other requests for exclusive
;; commit ownership, or attempts by others to commit, become staged
;; for later execution. All other activities are permitted to run in
;; parallel with you.
;;
;; For the purposes of commit and requesting exclusive ownership, the
;; effect is the same as going through a SERIALIZER gate. Ordered
;; access to kvdb's is required in order to prevent livelock.

(defun switchable-target-tag (target tag)
  (create
   (alambda
    ((cust atag :deliver-to new-target) / (eql atag tag)
     (become (switchable-target-tag new-target tag))
     (send cust :ok))
    (msg
     (send* target self msg))
    )))

(defun multi-commit-beh (st-tag ctrl-tag &optional cust action open-dbs)
  (alambda
   ((cust :process actionActor kvdbs-plist)
    (become (multi-commit-beh st-tag ctrl-tag cust actionActor nil))
    (let ((me  self))
      (β (ordered-kvdbs-plist)
          (sort-kvdbs β kvdbs-plist)
        (send me st-tag :open-dbs ordered-kvdbs-plist)
        )))

   ((atag :open-dbs ordered-kvdbs-plist) / (eql atag st-tag)
    (cond ((endp ordered-kvdbs-plist)
           (let ((dbs (mapcan #'list
                              (mapcar (lambda (triple)
                                        (list (car triple) (third triple)))
                                      (reverse open-dbs)))
                      ))
             (send action self dbs) ;; send act a plist of open dbs
             ))
          (t
           (let* ((me     self)
                  (db-id  (car ordered-kvdbs-plist))
                  (kvdb   (cadr ordered-kvdbs-plist))
                  (waiter (create (lambda (atag db)
                                    (declare (ignore atag))
                                    (β _
                                        (send me β st-tag :add-db db-id kvdb db)
                                      (send me st-tag :open-dbs (cddr ordered-kvdbs-plist))))
                                  )))
             (β _
                 (send ctrl-tag β :deliver-to waiter)
               (send kvdb st-tag :req-excl))
             ))
          ))

   ((acust atag :add-db db-id kvdb db) / (eql atag st-tag)
    (become (multi-commit-beh st-tag ctrl-tag cust action
                              (cons (list db-id kvdb db) open-dbs)))
    (send acust :ok))

   ((acust :commit upd-dbs-plist)
    (labels ((commit-beh (lst)
               (lambda (atag reply)
                 (declare (ignore atag reply))
                 (cond ((endp open-dbs)
                        (send cust  :ok)
                        (send acust :ok))
                       (t
                        (let* ((grp    (car lst))
                               (id     (car grp))
                               (kvdb   (cadr grp))
                               (old-db (third grp))
                               (new-db (getf upd-dbs-plist id old-db)))
                          (send kvdb st-tag :commit old-db new-db)
                          (become (commit-beh (cdr lst)))
                          ))
                       ))))
      (let ((committer (create (commit-beh open-dbs))))
        (β _
            (send ctrl-tag β :deliver-to committer)
          (send committer nil nil))
        )))

   ((acust :abort)
    (labels ((release-beh (lst)
               (lambda (atag reply)
                 (declare (ignore atag reply))
                 (cond ((endp lst)
                        (send cust :ok)
                        (send acust :ok))
                       (t
                        (let* ((grp  (car lst))
                               (kvdb (cadr grp)))
                          (send kvdb st-tag :abort)
                          (become (release-beh (cdr lst)))
                          ))
                       ))))
      (let ((releaser (create (release-beh open-dbs))))
        (β _
            (send ctrl-tag β :deliver-to releaser)
          (send releaser nil nil))
        )))
   ))

      
(defun multi-commit-orchestrator-beh (open-dbs pend)
  ;; serves as a SERIALIZER gate for requests that overlap kvdb usage
  ;; with extant multi-committers. Otherwise, for mutually exclusive
  ;; use groups, they are launched into a fresh muti-committer in
  ;; parallel.
  (lambda (&rest msg)
    (match msg
      ((cust :process actionActor kvdbs-plist)
       (let ((req-kvdbs (mapcar #'cadr (um:group kvdbs-plist 2))))
         (cond ((some (lambda (grp)
                        (intersection req-kvdbs (third grp)))
                      open-dbs)
                ;; we have overlapping use, so enqueue this req for later
                (become (multi-commit-orchestrator-beh open-dbs (addq pend msg))))
               (t
                ;; no overlap - so launch him
                (actors ((tag-to-me (tag self))
                         (st-tag    (switchable-target-tag sink ctrl-tag))
                         (ctrl-tag  (tag st-tag))
                         (handler   (create (multi-commit-beh st-tag ctrl-tag))))
                  (become (multi-commit-orchestrator-beh
                           (cons (list tag-to-me cust req-kvdbs)
                                 open-dbs)
                           pend))
                  (send handler tag-to-me :process actionActor kvdbs-plist)
                  ))
               )))

      ((atag . msg)
       (let* ((grp  (find atag open-dbs :key #'car))
              (cust (cadr grp)))
         (when cust
           (send* cust msg)
           (become (multi-commit-orchestrator-beh (remove grp open-dbs) +emptyq+))
           (do-queue (msg pend)
             (send* self msg))
           )))
      )))
      
(deflex multi-committer
  (create (multi-commit-orchestrator-beh nil +emptyq+)))

;; ---------------------------------------------------------------    
;; Protocol for use:
;;
;; Send a message to MULTI-COMMITTER:
;;
;;      (SEND MULTI-COMMITTER cust :PROCESS action-Actor pliist-of-kvdbs)
;;
;; MULTI-COMMITTER then opens all the kvdbs in consistent order, using
;; :REQ-EXCL, then sends a message to your action-Actor:
;;
;;      (SEND action-Actor cust plist-of-open-dbs)
;;
;; Action-Actor should do whatever processing it needs to do against
;; the open kvdb's and then send a response back to cust containing
;; either an :ABORT, or a :COMMIT with a plist of updated dbs. Any dbs
;; that have not been changed can be omitted from the plist in the
;; response.
;;
;;    (SEND cust :ABORT)
;;
;;     - or -
;;
;;    (SEND cust :COMMIT plist-of-updated-dbs)
;;
;; At the end, whether by :ABORT or :COMMIT, a message is sent to your
;; original cust argument from the :PROCESS message, as well as to the
;; cust specified in the :ABORT or :COMMIT message.
