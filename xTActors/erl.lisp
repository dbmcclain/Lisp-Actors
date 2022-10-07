;; erl.lisp -- Experimental Erlang-like behaviors
;;
;; Looking into how to handle errors in distributed systems. We have
;; no dynamic execution chain like one has in CALL/RETURN. So instead
;; we need some other way of notifying supervisors of error
;; conditions. This might be one potential systematic approach.
;;
;; DM/RAL 11/22
;; ----------------------------------------------------
;;
;; Declare associated Actor behaviors with DEF-ERL-BEH using
;; ALAMBDA-style handler clauses. The clauses will be guarded by an
;; error handler which will send :ERROR-FROM messages to all linked
;; Actors.
;;
;; The first argument to DEF-ERL-BEH must be a link or list of links.
;; Behaviors can use LINK, UNLINK, LINK-BETWEEN, UNLINK-BETWEEN, or
;; just set up links in advance when associated Actor's behaviors are
;; being defined. Advance setup avoids potential race conditions.
;;
;; If an error occurs in a guarded clause, an :ERROR-FROM message is
;; sent to all linked Actors, informing them of the errant Actor and
;; its error condition. Then the behavior exits as though no other
;; SENDs nor BECOMEs happened, as usual.
;;
;; Erl behaviors can choose to handle :ERROR-FROM messages or not. If
;; they do not, then the message is propagated upward through its
;; links. Links remain intact across all associated Actors.
;;
;; Unlike Erlang, our Actors do not denote threads, nor do they have
;; PID's.  They merely have their "SELF" Actor designation. And there
;; is no "EXIT" from an Actor with Erlang process connotation.
;;
;; Nothing can die, because nothing lives. Actors are passive message
;; handlers that get invoked when messages are sent their way. An
;; Actor cannot die, just like a function cannot die. If you want to
;; "cause the death of an Actor", then just stop sending it messages.
;; Or else have it become SINK.
;;
;; (I don't think Erlang has anything equivalent to our BECOME...)

(defpackage #:com.ral.actors.erl
  (:use #:common-lisp #:com.ral.actors)
  (:export
   #:def-erl-beh
   #:link
   #:unlink
   #:link-between
   #:unlink-between
   ))

(in-package :com.ral.actors.erl)

;; ---------------------------------------------------------

(defun link (pid-to)
  (send pid-to :link-to self))

(defun unlink (pid-to)
  (send pid-to :unlink-from self))

(defun link-between (pid-from pid-to)
  (send pid-to :link-to pid-from)
  (send pid-from :link-to pid-to))

(defun unlink-between (pid-from pid-to)
  (send pid-to :unlink-from pid-from)
  (send pid-from :unlink-from pid-to))

(defun strip-&args (args)
  (mapcan (lambda (arg)
            (cond ((and (symbolp arg)
                        (member arg '(&optional &rest &key)))
                   nil)
                  ((atom arg)
                   (list arg))
                  (t
                   (list (car arg)))
                  ))
          args))

(defmacro def-erl-beh (name args &rest clauses)
  ;; first arg must be a link or a list of links
  (lw:with-unique-names (msg)
    (let ((arg-names (strip-&args args)))
      `(defun ,name ,args
         (lambda (&rest ,msg)
           (with-error-response (,(car args))
             (match ,msg

               ((:link-to pid-from)
                (become (,name (adjoin pid-from (um:mklist ,(car args))) ,@(cdr arg-names))))
               
               ((:unlink-from pid-from)
                (become (,name (remove pid-from (um:mklist ,(car args))) ,@(cdr arg-names))))
               
               ,(let ((err-clause (find :error-from clauses :key #'caar)))
                  (cond (err-clause
                         (setf clauses (remove err-clause clauses))
                         err-clause)
                        (t
                         `((:error-from from err)
                           (apply #'send-to-all (um:mklist ,(car args)) *current-message*)))
                        ))
               ,@clauses)
             ))
         ))))

#+:LISPWORKS
(editor:setup-indent "def-erl-beh" 2)

#|
(def-erl-beh doit-beh (links arg1 arg2)
  ((:diddly msg)
   (do-something msg))
  ((:error-from pid err)
   (report-error pid err)))

(def-erl-beh doit-beh (links arg1 arg2)
  ((:diddly msg)
   (do-something msg)))
 |#

