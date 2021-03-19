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

(in-package #:actors/base)

(um:eval-always
  (import '(mp:make-timer
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
                               (apply conds-fn msg)))
               (process-message fn)
             ;; else
             (progn
               (push (whole-message) msg-queue)
               (signal 'no-immediate-answer)
               )))
         
         (handle-timeout ()
           ;; always executed in Actor context
           (setf timer nil)
           (process-message timeout-fn)))
      
      (setf user-fn (become #'filter-message)) ;; save prior handler
      (when timeout
        (setf timer (do-schedule-after timeout #'handle-timeout)))
      (signal 'no-immediate-answer)
      )))

;; ---------------------------------------------------------------
;; RECV - for DLAMBDA style handlers

(defun default-timeout ()
  (error 'timeout))

(defmacro tlambda (&rest clauses)
  ;; a variant on DLAMBDA - instead of executing a matching clause, it
  ;; returns a closure that can do so later, or NIL of no clauses
  ;; match.
  (lw:with-unique-names (args)
    `(labels*
         ;; using LABELS allows any clause to invoke another by name
         ,clauses
       (lambda (&rest ,args)
         (case (car ,args)
           ,@(mapcar (lambda (clause)
                       (let ((sel (car clause)))
                         `(,(if (eq t sel)
                                t
                              `(,sel))
                           (lambda ()
                             (apply #',sel ,(if (eq t sel)
                                                args
                                              `(cdr ,args)))))
                         ))
                     clauses)
           )))
    ))

(defmacro recv (&whole recv-form (&key timeout) &rest clauses)
  (let* ((on-timeout nil)
         (testers    (um:nlet clean ((mix clauses)
                                     (ans nil))
                       ;; Unlike a normal DLAMBA function, we convert
                       ;; the clauses into simple selector pattern
                       ;; matchers which return a functional closure
                       ;; on a match. DO-RECV will execute the closure
                       ;; if found.
                       (if mix
                           (destructuring-bind (hd . tl) mix
                             (cond
                              ((consp hd)
                               (go-clean tl (cons hd ans)))
                              ((eql hd :on-timeout)
                               (unless on-timeout  ;; first one takes it
                                 (setf on-timeout (car tl)))
                               (go-clean (cdr tl) ans))
                              ((eql hd :timeout) ;; allow old syntax too
                               (unless timeout   ;; first one takes it
                                 (setf timeout (car tl)))
                               (go-clean (cdr tl) ans))
                              (t
                               (error "Syntax error in ~S" recv-form))
                              ))
                         ;; else - finished
                         (nreverse ans)))
                     ))
    (lw:with-unique-names (handler timeout-fn new-timeout)
      `(let (,handler
             (,timeout-fn  ,(if on-timeout
                                `(lambda ()
                                   ,on-timeout)
                              `'default-timeout)))
         (flet ((retry-recv (&optional (,new-timeout ,timeout))
                  (do-recv ,handler ,timeout-fn ,new-timeout)))
           (setf ,handler (tlambda ,@testers))
           (retry-recv)))
      )))

#+:LISPWORKS
(editor:setup-indent "recv" 1)

#|
(defun tst-recv ()
  (let ((actor (spawn (dlambda*
                        (:start ()
                         (recv (:timeout 5)
                           (:diddly (x)
                            (pr x))
                           (t (a b)
                              (+ a b))
                           #||#
                           :on-timeout
                           (pr :timed-out)
                           #||#
                           ))
                        (:diddlyx (arg)
                         (pr arg)
                         arg))
                      :start)))
    (ask actor :diddly :dodad)))
(tst-recv)
 |#
