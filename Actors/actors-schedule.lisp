;; Actors-schedule.lisp -- Scheduled actions for Actors
;;
;; DM/RAL  06/18
;; -----------------------------------------------------------
#|
The MIT License

Copyright (c) 2017-2018 Refined Audiometrics Laboratory, LLC

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
|#

(in-package #:actors.base)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(cps:=cont

            mp:make-timer
            )))

;; -------------------------------------------------------
;; SCHEDULE-AFTER -- a macro to hide the gory details...

(defun do-schedule-after (timeout fn)
  (let ((timer (make-timer
                'mp:funcall-async (=cont fn))))
    (mp:schedule-timer-relative timer timeout)
    timer))

(defmacro schedule-after (timeout &body body)
  `(do-schedule-after ,timeout (lambda ()
                                 ,@body)))

;; ---------------------------------------------------------------
;; DO-RECV - for DLAMBDA-style and MATCH-style handlers
;;
;; RECV simply swizzles the user-function behavior of the Actor. It
;; does not block waiting for a qualifying message or timeout event.
;;
;; Once a qualified message arrives, or the timeout occurs, the
;; user-function of the Actor gets restored, and all non-qualifying
;; messages that arrived during the active RECV get replayed for
;; delivery, ahead of new messages in the Actor mailbox.
;;
(defun do-recv (conds-fn timeout-fn timeout)
  (let ((dyn-env (um:capture-dynamic-environment))
        user-fn msg-queue timer)
    (labels
        ((restore-actor ()
           (when timer
             (mp:unschedule-timer timer))
           (become user-fn)
           (let ((mbox (actor-mailbox (current-actor))))
             (setf (actor-message-replay mbox)
                   (nconc (nreverse msg-queue)
                          (actor-message-replay mbox)))
             ))

         (process-message (fn)
           (restore-actor)
           (um:call-with-dynamic-environment dyn-env fn))
         
         (filter-message (&rest msg)
           (um:if-let (fn (and (not (in-ask-p))
                               ;; message arrived not by way of ASK
                               (funcall conds-fn msg)))
               (process-message fn)
             ;; else
             (progn
               (push (whole-message) msg-queue)
               (signal 'no-immedate-answer)
               )))
         
         (handle-timeout ()
           ;; always executed in Actor context
           (setf timer nil)
           (process-message (lambda ()
                              (if timeout-fn
                                  (funcall timeout-fn)
                                (error 'timeout)))
                            )))

      (when timeout
        (setf timer (do-schedule-after timeout #'handle-timeout)))
      (setf user-fn (become #'filter-message)) ;; save prior handler
      (signal 'no-immediate-answer)
      )))

;; ---------------------------------------------------------------
;; RECV - for DLAMBDA style handlers

(defmacro recv ((&key timeout) &rest clauses)
  (let ((g!handler    (gensym (string 'handler)))
        (g!tester     (gensym (string 'tester)))
        (g!on-timeout (gensym (string 'on-timeout)))
        (selectors    (mapcar 'first clauses)))
    `(let ((,g!handler  nil))
       (labels
           ((,g!tester (msg)
              (and ,(if (member t selectors)
                        t
                      `(member (car msg) ',selectors))
                   (lambda ()
                     (apply ,g!handler msg))))
            (,g!on-timeout ()
              ;; on timeout, handler will be handed a :ON-TIMEOUT message
              (funcall ,g!handler :on-timeout))
            (retry-recv ()
              (do-recv #',g!tester #',g!on-timeout ,timeout)))
         (setf ,g!handler (dlambda*
                            ,@clauses))
         (retry-recv)
         ))
    ))


#|
(defun tst-recv ()
  (let ((actor (spawn (dlambda*
                        (:start ()
                         (recv (:timeout 5)
                           (:diddly (x)
                            (pr x))
                           (:on-timeout ()
                            (pr :timed-out))))
                        (:diddlyx (arg)
                         (pr arg)
                         arg))
                      :start)))
    (ask actor :diddly :dodad)))

 |#
