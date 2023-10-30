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

(def-ser-beh open-filer-beh (fd)
  ((cust :close . abort)
   (apply #'close fd abort)
   (multiple-value-bind (x err)
       ;; force an error to get a good descriptive error from the
       ;; system.
       (ignore-errors (read fd))
     (declare (ignore x))
     (become (const-beh err))
     (send cust :ok)))
  
  ((cust :oper op)
   (send op cust fd)))


(defun retrig-filer-gate-beh (chan reply-tag timeout-tag &key (timeout 10))
  ;; Gateway to Open Filer. Every new request resets the timeout
  ;; timer. On timeout, a close is issued.
  ;;
  ;; Once the file has been closed, the channel replies with a
  ;; closed-file error for any new requests.
  ;;
  ;; Error replies from channel cause us to send an abort close
  ;; request to the channel.
  ;;
  (alambda
   ((cust _ ) / (eql cust timeout-tag)
    (send chan sink :close)
    (become (fwd-beh chan)))

   ((atag cust err) / (and (eql atag reply-tag)
                           (typep err 'error))
    (send chan sink :close t)
    (become (fwd-beh chan))
    (send cust err))
   
   ((atag cust . reply) / (eql atag reply-tag)
    (send* cust reply))
   
   ((cust :close . abort)
    (repeat-send chan)
    (become (fwd-beh chan)))

   ((cust . args)
    (let ((new-tag    (timed-once-tag self timeout))
          (reply-cust (label reply-tag cust)))
      (become (retrig-filer-gate-beh chan reply-tag new-tag :timeout timeout))
      (send* chan reply-cust args)))
   ))

(defconstant +DEFAULT-OP-TIMEOUT+    10)
(defconstant +DEFAULT-CLOSE-TIMEOUT+ 10)

(deflex filer
  (alambda
   ((cust :open fname . args)
    (apply #'(lambda (&key
                      (op-timeout  +DEFAULT-OP-TIMEOUT+)
                      (close-after +DEFAULT-CLOSE-TIMEOUT+)
                      open-args)
               (let ((fd  (apply #'open fname open-args)))
                 (um:letrec ((chan        (serializer
                                           (create
                                            (open-filer-beh fd))))
                             (reply-tag   (tag gate))
                             (timeout-tag (tag gate))
                             (gate        (create
                                           (retrig-filer-gate-beh chan reply-tag timeout-tag
                                                                  :timeout close-after))))
                   (send-after close-after timeout-tag +timed-out+)
                   (send cust gate))
                 ))
           args))
   ))

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


(defun tst ()
  (let* ((fname "junk.jnk")
         (ac1 (create (lambda (cust)
                        (with-open-file (f fname)
                          (sleep 1)
                          (send cust :ok)))))
         (ac2 (create (lambda (cust)
                        (with-open-file (f fname)
                          (sleep 1)
                          (send cust :ok))))))
    (with-open-file (f fname
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
      (print :hello f)
      (send ac1 sink)
      (ask ac2))
    ))

(tst)

|#
