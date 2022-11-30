;; filer.lisp - Responsible File Use with Actors
;;
;; DM/RAL  2022/11/30 06:59:42
;; ------------------------------------------------------
(in-package :com.ral.actors.base)

;; ------------------------------------------
;; FILER -- Open a file and return an access channel Actor. The file
;; will be automatically closed after more than timeout of no
;; activity. Channel users should send messages to it:
;;
;;   (cust :CLOSE) - close the file and shut down the channel. The
;;   channel will reply :CLOSED to all subsequent messages.
;;
;;   (cust :OPER op-actor) - send op-actor a message (cust fd) where
;;   that actor should reply to its cust (will be a different cust)
;;   after it is finished, and fd is the open-file-descriptor. The
;;   same reply will be sent back to the original cust.
;;
;; There are simply too many different kinds of things one might want
;; to do with an open file descriptor. So we let you handle it with
;; the :OPER message. You can even close it if you like. But at some
;; point, if you don't issue a :CLOSE message, the timeout will take
;; care of it for you.
;;
;; In an Actor world, there is no concept of "Scope" and so it is
;; counterproductive, and impossible, to use WITH-OPEN-FILE around
;; your file access procedures, unless you take care of everything
;; within just one Actor. But when you can't do that, the next best
;; thing is MAKE-FILER.

(def-ser-beh open-filer-beh (fd timeout)
  ((cust :close)
   (close fd)
   (become (const-beh :closed))
   (send cust :ok))
  
  ((cust :oper op)
   (let ((gate (timed-gate cust timeout)))
     (send op gate fd))) )

(defun retrig-filer-gate-beh (chan tag &key (timeout 10))
  ;; Gateway to Open Filer. Every new request resets the timeout
  ;; timer. On timeout, a close is issued.  Once the file has been
  ;; closed, we reply :CLOSED to any new requests.
  (alambda
   ((cust :timeout)
    (when (eql cust tag)
      (send chan sink :close)
      (become (const-beh :closed))
      ))

   ((cust :close)
    (repeat-send chan)
    (become (const-beh :closed)))
   
   (_
    (let ((new-tag (timed-tag self timeout)))
      (become (retrig-filer-gate-beh chan new-tag :timeout timeout))
      (repeat-send chan)))
   ))

(def-ser-beh filer-beh ()
  ((cust :open fname . args)
   (let ((op-timeout  (getf args :op-timeout 10))    ;; limit on file ops
         (close-after (getf args :close-after 10)))  ;; idle timeout
     (remf args :op-timeout)
     (remf args :close-after)
     (let* ((fd   (apply #'open fname args))
            (chan (serializer (create (open-filer-beh fd op-timeout))))
            (gate (create))
            (tag  (timed-tag gate close-after)))
       (set-beh gate (retrig-filer-gate-beh chan tag :timeout close-after))
       (send cust gate))
     )))

(deflex filer
  (serializer
   ;; because we are performing non-idempotent file open/create
   (create (filer-beh))))
    
#|
(defun tst (&rest args &key (timeout 10) &allow-other-keys)
  (remf args :timeout)
  (print args)
  (print timeout))
(tst :a 1 :b 2 :c 3 :timeout 5)

(defun tst ()
  (β (fp)
      (send filer β :open "junk.dat"
            :close-after       3
            :direction         :output
            :element-type      '(unsigned-byte 8)
            :if-does-not-exist :create
            :if-exists         :append) ;; :rename
    ;; (sleep 10)
    (sleep 1)
    (β ans
        (send fp β :oper
              (α (cust fd)
                (write-sequence #(1 2 3) fd)
                (send cust :ok)))
      (send writeln ans)
      )))
(tst)

|#
