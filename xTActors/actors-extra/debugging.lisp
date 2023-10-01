;; debugging.lisp -- Support for denbugging of Actors
;;
;; DM/RAL 12/21
;; ----------------------------------------------------------

(in-package com.ral.actors.base)

;; ------------------------------------------

(defun format-usec (usec)
  (multiple-value-bind (utc frac)
      (truncate usec 1000000)
    (multiple-value-bind (ss mm hh d m y dow)
        (decode-universal-time utc 0)
      (declare (ignore d m y dow))
      (format nil "~{~2,'0D~^\:~}.~6,'0D" (list hh mm ss) frac))))
  
(defun logger-timestamp ()
  (format-usec (usec:get-universal-time-usec)))

(deflex logger
  (α msg
    (send* println
           (mapcar (lambda (args)
                     (apply #'format nil args))
                   `(("----- Logger at ~A -----" ,(logger-timestamp))
                     ("  To: ~A" ,(car msg))
                     ("  With: ~S" ,(cdr msg))
                     ))
           )))

(defun logger ()
  ;; provides a log output as the message is passed along
  (actor (cust &rest msg)
    (send* logger cust msg)
    (send* cust msg)))

(defun logged-beh (actor)
  (lambda (&rest msg)
    (send* logger actor msg)
    (send* actor msg)))

(defun logged (actor)
  (create (logged-beh actor)))
  
;; ---------------------------------------------------------
;; For use in debugging - ATRACE is not initially installed because it
;; adds some performance drag on the SEND/dispatch timing. In order to
;; use ATRACE, first run INSTALL-ATRACE.

(defvar *atrace*  nil)

(defun install-atrace ()
  (setf *atrace* t)
  (lw:defadvice (send send-tracer :before)
      (&rest msg)
    (when *atrace*
      (with-printer (s *standard-output*)
	(format s "~&~{~A~%~^~}"
		(mapcar (lambda (args)
			  (apply #'format nil args))
			`(("----- Send at ~A -----" ,(logger-timestamp))
			  ("  From: ~A" ,self)
			  ("  To: ~A" ,(car msg))
			  ("  With: ~S" ,(cdr msg))
			  )))
	))))

(defun uninstall-atrace ()
  (setf *atrace* nil)
  (hcl:delete-advice send send-tracer))

(defun atrace (&optional (do-tracing t))
  (setf *atrace* do-tracing))

#|
(setf *print-length* 10)
(install-atrace)
(atrace)
(atrace nil)
(uninstall-atrace)
|#

(defun time-tag-beh (actor)
  (lambda* msg
    (send* actor (usec:get-universal-time-usec) msg)))

(defun time-tag (actor)
  (create (time-tag-beh actor)))

;; ====================================================

(defun tracing-send (target &rest msg)
  (let ((*current-message-frame* t))
    (send* target msg)))

(defun untraced-send (target &rest msg)
  (let ((*current-message-frame* nil))
    (send* target msg)))

(defun tracer-beh ()
  (alambda
   ((cust :trace from)
    (um:nlet iter ((msg   from)
                   (trail nil))
      (cond
       ((msg-p msg)
        (unless (msg-id msg)
          ;; global mutation, needs to be behind a Serializer
          (setf (msg-id msg) (uuid:make-v1-uuid)))
        (go-iter (msg-parent msg) (cons (list (msg-id msg)
                                              (msg-actor msg)
                                              (msg-args  msg))
                                        trail)))
       (t
        (send cust (cons "=== Traceback ===" (nreverse trail))))
       )))
   ))

(deflex tracer
  (serializer (create (tracer-beh))
              :timeout 5))

(defun trace-me ()
  (send tracer writeln :trace *current-message-frame*))

(defun dbg-trace ()
  ;; for use in a debugger REPL
  (send-to-pool tracer writeln :trace *current-message-frame*))

(defun dbg-trace-inspect ()
  ;; for use in a debugger REPL
  (send-to-pool tracer (create #'inspect) :trace *current-message-frame*))

;; ---------------------------------------------------
;; Memory Stressor... When message tracing is on, this chews up
;; unbounded memory

(defun stressor-beh (&optional (ctr 0) tag)
  (alambda
   ((cust :??)
    (send cust ctr))
   ((cust :start)
    (let ((tag (tag self)))
      (send cust :ok)
      (become (stressor-beh 0 tag))
      (send tag)))
   ((cust :stop)
    (become (stressor-beh ctr (tag self)))
    (send cust :ok))
   ((atag . _) / (eq atag tag)
    (become (stressor-beh (1+ ctr) tag))
    (send tag))
   ))

(deflex stressor 
  (create (stressor-beh)))