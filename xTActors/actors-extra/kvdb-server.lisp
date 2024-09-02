(in-package :kvdb)

;; -----------------------------------------------------
;; Local Proxy KVDB's for remote access - avoid shuttling entire map
;; objects across the network.

(defun local-proxy-for-remote-db-access-beh (kvdb)
  (alambda
   ((cust :add key val)
    (let ((me self))
      (β _
          (send kvdb β :add key val)
        (send cust me))
      ))
   
   ((cust :remove key)
    (let ((me self))
      (β _
          (send kvdb β :remove key)
        (send cust me))
      ))
      
   ((_ :find . _)
    (repeat-send kvdb))

   ((_ :find-or-add . _)
    (repeat-send kvdb))

   ((_ :really-let-me-see-map)
    ;; Okay... you asked for it...
    (repeat-send kvdb))

   ((cust :req)
    (let ((me  self)
          (tag (tag self)))
      (become (future-become-beh tag))
      (β (db)
          (send kvdb β :req)
        (send tag (local-updateable-proxy-for-remote-db-access-beh
                   (actor-state
                    :kvdb     kvdb
                    :orig-map db
                    :upd-map  db)))
        (send cust me))
      ))
   
   ((cust :req-proxy)
    ;; Really no reason to be sending this message, we are already
    ;; a proxy Actor.
    (send cust self))

   ((cust :req-excl owner timeout) / (and (realp timeout)
                                          (plusp timeout))
    (let ((me  self)
          (tag (tag self)))
      (become (future-become-beh tag))
      (β (db)
          (send kvdb β :req-excl owner timeout)
        (cond
         ((eq :fail db)
          (send tag (local-proxy-for-remote-db-access-beh kvdb)) ;; revert beh
          (send cust :fail))
         (t
          (send-after timeout tag 'forced-abort)
          (send tag (local-excl-proxy-for-remote-db-access-beh
                     (actor-state
                      :kvdb      kvdb
                      :orig-map  db
                      :upd-map   db
                      :owner     owner
                      :abort-tag tag)))
          (send cust me))
         ))
      ))

   (((cust . _) :commit)
    ;; Nothing to do here, already fully committed. User should have
    ;; performed a :REQ first.
    (send cust self))

   ((cust :abort)
    ;; Nothing to do here, already fully committed. User should have
    ;; performed a :REQ first.
    (send cust self))

   ((_ :db-path)
    (repeat-send kvdb))
   ))

(defun local-updateable-proxy-for-remote-db-access-beh (state)
  (with-state-vals ((kvdb     :kvdb)
                    (orig-map :orig-map)
                    (upd-map  :upd-map)) state
    (alambda
     ((cust :add key val)
      (become (local-updateable-proxy-for-remote-db-access-beh
               (with state
                 :upd-map (db-add upd-map key val))))
      (send cust self))
     
     ((cust :remove key)
      (become (local-updateable-proxy-for-remote-db-access-beh
               (with state
                 :upd-map (db-remove upd-map key))))
      (send cust self))
     
     ((cust :find key . default)
      (send cust (db-find upd-map key (car default))))
     
     ((cust :find-or-add key def-val)
      (multiple-value-bind (val new-db)
          (db-find-or-add upd-map key def-val)
        (become (local-updateable-proxy-for-remote-db-access-beh
                 (with state
                   :upd-map new-db)))
        (send cust val)))
     
     ((cust :really-let-me-see-map)
      ;; Okay... you asked for it...
      (send cust upd-map))
     
     ((cust :req)
      (send cust self))
     
     ((cust :req-proxy)
      (send cust self))
     
     ((cust :req-excl owner timeout) / (and (realp timeout)
                                            (plusp timeout))
      ;; This might have to abandon existing updates and start fresh.
      ;; You should have requested excl from outset.
      (let ((me  self))
        (become (local-proxy-for-remote-db-access-beh kvdb))
        (β _
            (send kvdb `(,β . ,β) :commit orig-map upd-map) ;; might fail
          (send me cust :req-excl owner timeout)
          )))
     
     ((cust :abort)
      (send cust :ok)
      (become (local-proxy-for-remote-db-access-beh kvdb)))
     
     (((cust . retry) :commit)
      (let* ((me  self)
             (committed (create (behav (ans)
                                  (declare (ignore ans))
                                  (send cust me))))
             (lcl-retry (create (behav (ans)
                                  (declare (ignore ans))
                                  (send retry me)))))
        (become (local-proxy-for-remote-db-access-beh kvdb))
        (send kvdb `(,committed . ,lcl-retry) :commit orig-map upd-map)))
     
     ((_ :db-path)
      (repeat-send kvdb))
     )))
  
(defun local-excl-proxy-for-remote-db-access-beh (state)
  (with-state-vals ((kvdb      :kvdb)
                    (orig-map  :orig-map)
                    (upd-map   :upd-map)
                    (owner     :owner)
                    (abort-tag :abort-tag)) state
    ;;
    ;; Inside here we have the kvdb under commit lock, so keep it short,
    ;; Jack...
    ;;
    ;; Remote user must issue one or more of these provided message
    ;; types and finish with either :ABORT or :COMMIT. This proxy avoids
    ;; shuttling entire map objects across the network.
    ;;
    (alambda
     ((cust :add key val)
      (become (local-excl-proxy-for-remote-db-access-beh
               (with state
                 :upd-map (db-add upd-map key val))))
      (send cust self))
     
     ((cust :remove key)
      (become (local-excl-proxy-for-remote-db-access-beh
               (with state
                 :upd-map (db-remove upd-map key))))
      (send cust self))
     
     ((cust :find key . default)
      (send cust (db-find upd-map key (car default))))
     
     ((cust :find-or-add key def-val)
      (multiple-value-bind (val new-db)
          (db-find-or-add upd-map key def-val)
        (become (local-excl-proxy-for-remote-db-access-beh
                 (with state
                   :upd-map new-db)))
        (send cust val)))
     
     ((cust :really-let-me-see-map)
      ;; Okay... you asked for it...
      (send cust upd-map))
     
     ((cust :req)
      (send cust self))
     
     ((cust :req-proxy)
      (send cust self))
     
     ((cust :req-excl . _)
      ;; Really no reason to be sending this message, we are already
      ;; under excl commit.
      (send cust self))
     
     ((cust :abort)
      (let ((me self))
        (become (local-proxy-for-remote-db-access-beh kvdb))
        (β _
            (send kvdb β :abort owner)
          (send cust me)
          )))

     ((atag 'forced-abort) / (eq atag abort-tag)
      (become (local-proxy-for-remote-db-access-beh kvdb))
      (send kvdb sink :abort owner))
     
     ((cust :commit)
      (let ((me self))
        (become (local-proxy-for-remote-db-access-beh kvdb))
        (β _
            (send kvdb `(,β . ,owner) :commit orig-map upd-map)
          (send cust me)
          )))
     
     ((_ :db-path)
      (repeat-send kvdb))
     )))

