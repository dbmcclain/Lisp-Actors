
(in-package :ac)

(defun restorer-beh (sav)
  (λ (cust)
    (dolist (pair sav)
      (destructuring-bind (ac beh) pair
        (setf (actor-beh ac) beh)))
    (send cust :ok)))

(defun restorer (&rest acs)
  (create
   (restorer-beh (mapcar (lambda (ac)
                           (cons ac (actor-beh ac)))
                         acs))))


;; ----------------------------------------------------
;; Safe Serializer - serializer with unblocking channel and timeout

(defun new-pend-serializer-beh (svc ret timeout cust waitq msg)
  (actors ((gate  (once-beh ret))
           (tmout (tag-beh gate))
           (reply (tag-beh gate)))
    (send-after timeout tmout)
    (become (pend-serializer-beh svc ret timeout cust tmout reply waitq))
    (send* svc reply msg)))

(defun no-pend-serializer-beh (svc ret timeout)
  (alambda
   ((tag . _) / (eql tag ret))

   ((cust . msg)
    (new-pend-serializer-beh svc ret timeout cust +emptyq+ msg))
   ))

(defun pend-serializer-beh (svc ret timeout cur-cust tmout reply waitq)
  (alambda
   ((tag type-tag . ans) / (eql tag ret)
    (when (or (and (eql type-tag reply)
                   (or (send* cur-cust ans)
                       t))
              (eql type-tag tmout)
              (eql type-tag self))
      (if (emptyq? waitq)
          (become (no-pend-serializer-beh svc ret timeout))
        (multiple-value-bind (top newq) (popq waitq)
          (destructuring-bind (new-cust . new-msg) top
            (new-pend-serializer-beh svc ret timeout new-cust newq new-msg)
            ))
        )))

   (msg
    (become (pend-serializer-beh svc ret timeout cur-cust tmout reply (addq waitq msg))))
   ))

(defun serializer (svc &optional (timeout 10))
  (actors ((ret  (tag-beh gate))
           (gate (no-pend-serializer-beh svc ret timeout)))
    (values gate ret)))

(defun unblock-serializer (ser ret)
  ;; emergency use back door
  (send ret ser))

;; --------------------------------------

(let ((ser (serializer (α (cust . msg)
                         (send* writeln msg)
                         ;; (send cust :ok)
                         )
                       )))
  (send ser sink :hello)
  (send ser sink :there))