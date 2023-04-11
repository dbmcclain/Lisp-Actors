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
;; within just one Actor behavior function. But when you can't do
;; that, the next best thing is FILER.
#|

  Sending :OPEN to FILER produces an Open File Channel Actor. That
  Channel consists of a retriggerable gate feeding into a Serializer
  and then to the Open Filer Actor. The Open Filer Actor responds to
  messages :OPER and :CLOSE.

  The Retriggerable Gate serves to issue a :CLOSE if no activity has
  been seen for more than :CLOSE-AFTER seconds.

  The Open Filer is protected during :OPER operations with a timeout
  given by :OP-TIMEOUT sec.

	       +-------------+   +------------+	  +------------+     	       
   User Msg -->| Retrig Gate |-->| Serializer |-->| Open Filer |
	       +-------------+   +------------+	  +------------+     	       
 |#

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
   ((cust nil :timeout)
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

(defconstant +DEFAULT-OP-TIMEOUT+    10)
(defconstant +DEFAULT-CLOSE-TIMEOUT+ 10)

(deflex filer
  (alambda
   ((cust :open fname . args)
    (let* ((op-timeout  (getf args :op-timeout  +DEFAULT-OP-TIMEOUT+))
           (close-after (getf args :close-after +DEFAULT-CLOSE-TIMEOUT+))
           (open-args   (getf args :open-args))
           (fd   (apply #'open fname open-args)))
      (actors ((chan (serializer-beh (create (open-filer-beh fd op-timeout))))
               (gate (retrig-filter-gate-beh chan tag :timeout close-after))
               (tag  (timed-tag-beh gate close-after)))
        (send cust gate))
      ))))

;; --------------------------------------------------
#|
(defun tst (&rest args &key (timeout 10) &allow-other-keys)
  (remf args :timeout)
  (print args)
  (print timeout))
(tst :a 1 :b 2 :c 3 :timeout 5)

(defun tst ()
  (β (fp)
      (send filer β :open "junk.dat"
            :close-after 3
            :open-args   (list 
                          :direction         :output
                          :element-type      '(unsigned-byte 8)
                          :if-does-not-exist :create
                          :if-exists         :append)) ;; :rename
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
