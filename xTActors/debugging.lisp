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

(defun decode-sponsor (spon)
  (let ((pair (rassoc spon *all-sponsors*)))
    (if pair
        (car pair)
      (and spon
           (format nil "~X" (sys:object-address spon)))
      )))

(deflex logger
  (actor msg
    (send* println
           (mapcar (lambda (args)
                     (apply #'format nil args))
                   `(("----- Logger at ~A -----" ,(logger-timestamp))
                     ("  To: ~A" ,(car msg))
                     ("  With: ~S" ,(cdr msg))
                     ("  In Sponsor: ~S" ,(decode-sponsor self-sponsor))
                     ))
           )))

(defun logged-beh (actor)
  (lambda (&rest msg)
    (send* logger actor msg)
    (send* actor msg)))

(defun logged (actor)
  (make-actor (logged-beh actor)))
  
;; ---------------------------------------------------------
;; For use in debugging - ATRACE is not initially installed because it
;; adds some performance drag on the SEND/dispatch timing. In order to
;; use ATRACE, first run INSTALL-ATRACE.

(defvar *atrace*  nil)

(defun install-atrace ()
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
                          ("  In Sponsor: ~S" ,(decode-sponsor self-sponsor))
                          )))
        ))))

(defun uninstall-atrace ()
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
  (make-actor (time-tag-beh actor)))
