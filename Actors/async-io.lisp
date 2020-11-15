
(defpackage :async-io
  (:use :common-lisp :actors)
  (:export
   :invalid-io-command
   :stream-closed
   :open-stream
   :exec-io
   :perform
   ))

(in-package :async-io)

(defclass async-io (actor)
  ((stream-table  :reader stream-table :initform (make-hash-table))))

(defvar *async-io*  (make-instance 'async-io))
(register-actor :async-io *async-io*)

(defun do-async-open (reply-to filename &rest args)
  (send reply-to
        (um:capture-ans-or-exn
          (apply #'open filename args))))

(defun report-invalid-io-command (condition stream)
  (format stream "Invalid I/O Command: ~S" (error-cmd condition)))

(define-condition invalid-io-command (error)
  ((cmd :accessor error-cmd :initarg :cmd))
  (:report report-invalid-io-command))

(defun do-async-io (reply-to stream cmds)
  (send reply-to
        (um:capture-ans-or-exn
          (um:accum acc
            (dolist (cmd cmds)
              (acc (list cmd
                         (um:dcase* cmd
                           (:close ()
                            (close stream))
                           (:file-position (pos)
                            (file-position stream pos))
                           (:read-byte (&rest args)
                            (apply #'read-byte stream args))
                           (:read-char (&rest args)
                            (apply #'read-char stream args))
                           (:read-line (&rest args)
                            (apply #'read-line stream args))
                           (:read (&rest args)
                            (apply #'read stream args))
                           (:read-sequence (seq &rest args)
                            (apply #'read-sequence seq stream args))
                           (:print (x)
                            (print x stream))
                           (:princ (x)
                            (princ x stream))
                           (:prin1 (x)
                            (prin1 x stream))
                           (:terpri ()
                            (terpri stream))
                           (:finish-output ()
                            (finish-output stream))
                           (:force-output ()
                            (force-output stream))
                           (:print-object (x)
                            (print-object x stream))
                           (:format (fmt &rest args)
                            (apply #'format stream fmt args))
                           (:write-byte (b)
                            (write-byte b stream))
                           (:write-char (c)
                            (write-char c stream))
                           (:write-string (str &rest args)
                            (apply #'write-string str stream args))
                           (:write-line (str &rest args)
                            (apply #'write-line str stream args))
                           (:write (x &rest args)
                            (apply #'write x :stream stream args))
                           (:write-sequence (seq &rest args)
                            (apply #'write-sequence seq stream args))
                           (t (&rest cmd)
                              (error 'invalid-io-command :cmd cmd))
                           ))))
            ))))

(defun open-stream (reply-to filename &rest args)
  (apply #'spawn-worker #'do-async-open reply-to filename args))

(define-condition stream-closed (error)
  ())

(defun exec-io (reply-to stream cmds)
  (with-slots (stream-table) *async-io*
    (perform-in-actor *async-io*
      (multiple-value-bind (queue foundp)
          (gethash stream stream-table)
        (unless foundp
          (setf queue (hcl:make-unlocked-queue)
                (gethash stream stream-table) queue))
        (let ((busy (hcl:unlocked-queue-ready queue))
              (pair (cons reply-to cmds)))
          (hcl:unlocked-queue-send queue pair)
          (unless busy
            (um:nlet iter ((pair pair))
              (when pair
                (destructuring-bind (reply-to . cmds) pair
                  (=bind (ans)
                      (spawn-worker #'do-async-io =bind-cont stream cmds)
                    (hcl:unlocked-queue-read queue)
                    (send reply-to ans)
                    (if (find :close cmds :key #'car)
                        (um:nlet-tail dump ()
                          (um:if-let (pair (hcl:unlocked-queue-read queue))
                              (progn
                                (send (car pair) (um:capture-ans-or-exn
                                                   (error 'stream-closed)))
                                (dump))
                            ;; else
                            (remhash stream stream-table)))
                      ;; else
                      (iter (hcl:unlocked-queue-peek queue)))
                  ))))
            ))))))

(=defun perform (fn)
  (spawn-worker (=lambda ()
                  (=values (um:call-capturing-ans-or-exn fn)))
                =bind-cont))