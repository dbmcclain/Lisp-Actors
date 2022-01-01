;; Actors.lisp -- An implementation of Actors
;;
;; Single thread semantics across multithreaded and SMP systems
;;
;; DM/RAL  12/17
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


(in-package #:com.ral.actors.base)

;; equiv to #F
(declaim  (OPTIMIZE (SPEED 3) (SAFETY 3) (debug 2) #+:LISPWORKS (FLOAT 0)))

;; --------------------------------------------------------------------

;; -----------------------------------------------------
;; Actors are simply indirect refs to a beh closure (= function + state).
;;
;; Actor behavior/state can change without affecting the identity of
;; the Actor.
;;               +------+-----+
;;  Actor Ref -->| Type | Beh |
;;               +------+-----+
;;                  |      |
;;                  |      v      Closure
;;                  |    +----+-------+
;;                  v    | Fn | State |
;;             T(Actor)  +----+-------+     Bindings
;;                         |      |      +------+-----+-----+---
;;                         |      +----->| Data | ... | ... |
;;                         |             +------+-----+-----|---
;;                         |    +------+-----+-----+---
;;                         +--->| Code | ... | ... |
;;                              +------+-----+-----+---
;; ------------------------------------------------------------------

(defstruct (actor
               (:constructor make-actor (&optional (beh #'lw:do-nothing))))
  (beh #'lw:do-nothing :type function))

;; --------------------------------------------------------
;; Core RUN for Actors

;; Per-Thread for Activated Actor
(defvar *whole-message*    nil) ;; Current Event Message
(defvar *current-actor*    nil) ;; Current Actor
(defvar *current-behavior* nil) ;; Current Actor's behavior
(defvar *current-sponsor*  nil) ;; Current Sponsor active during Actor exec
(defvar *new-beh*          nil) ;; result of BECOME

(define-symbol-macro self         *current-actor*)
(define-symbol-macro self-beh     *current-behavior*)
(define-symbol-macro self-sponsor *current-sponsor*)
(define-symbol-macro self-msg     *whole-message*)

;; -------------------------------------------------
;; Message Frames - submitted to the event queue. These carry their
;; own link pointer to obviate consing on the event queue.
;;
;; Minimal garbage generation since most Actors send at least one
;; message. We re-use the last message frame received. If no messages
;; are sent by the Actor, then the message frame becomes garbage.

(defstruct (msg
            (:constructor msg (actor args)))
  link
  (actor (make-actor) :type actor)
  args)

(defvar *current-evt*  nil) ;; message frame just processed

(defun new-msg (actor args)
  ;; try to re-use the last message frame
  #F
  (let ((msg *current-evt*))
    (cond (msg
           (setf *current-evt* nil
                 (msg-actor (the msg msg)) actor
                 (msg-args  (the msg msg)) args)
           msg)

          (t
           (msg actor args))
          )))

;; -----------------------------------------------------------------
;; Generic RUN for all threads, across all Sponsors
;;
;; SENDs and BECOME are optimistically committed.  In more uncommon
;; case of error, the tail of the event queue is rolled back, and the
;; Actor behavior of the current Actor is restored.
;;
;; Actors are now completely thread-safe - only one sponsor at a time
;; can be running any given Actor. But that also means there is no
;; longer any parallel execution of Actors, even when a non-mutating
;; behavior would be safe to run in parallel.
;;
;; We are also now open to potential spin-lock loops if an Actor is
;; popular among multiple sponsors and takes too long to perform. In
;; that case, it would be better to always perform on a stated
;; sponsor.

(defvar *send*
  (lambda (actor &rest msg)
    (apply (actor-beh base-sponsor) actor msg)
    (values)))
    
(defun run-actors (*current-sponsor* mbox)
  #F
  (let ((qhd  nil)
        (qtl  nil))
    (labels ((add-evq (msg)
               (declare (msg msg))
               (setf (msg-link msg) nil
                     qtl            (if qhd
                                        (setf (msg-link (the msg qtl)) msg)
                                      (setf qhd msg))
                     ))
             (pop-evq ()
               (let ((msg qhd))
                 (when msg
                   (setf qhd (msg-link (the msg msg)))
                   msg)))
             
             (send (actor &rest msg)
               (and actor
                    (add-evq (new-msg actor msg)))))
      
      (declare (dynamic-extent #'add-evq #'pop-evq #'send))
      
      ;; -------------------------------------------------------
      ;; Think of these global vars as dedicated registers of a
      ;; special architecture CPU which uses a FIFO queue for its
      ;; instruction stream, instead of linear memory, and which
      ;; executes breadth-first instead of depth-first. This maximizes
      ;; concurrency.
      (let ((qsave              nil) ;; rollback copy
            (*current-evt*      (msg (make-actor) nil))
            (*current-actor*    (make-actor))
            (*current-behavior* #'lw:do-nothing)
            (*whole-message*    nil)
            (*new-beh*          nil)
            (*send*             #'send))
        
        (declare (msg      *current-evt*)
                 (function *current-behavior*)
                 (list     *whole-message*))

        (loop
           (with-simple-restart (abort "Handle next event")
             ;; unroll the committed SENDS and BECOME
             (loop
                ;; Get a Foreign SEND event if any
                (when (mp:mailbox-not-empty-p mbox)
                  (add-evq (mp:mailbox-read mbox)))
                
                ;; Fetch next event from event queue - ideally, this
                ;; would be just a handful of simple register/memory
                ;; moves and direct jump. No call/return needed, and
                ;; stack useful only for a microcoding assist. Our
                ;; depth is never more than one Actor at a time,
                ;; before trampolining back here.
                (setf *current-evt* (or (pop-evq)
                                        (mp:mailbox-read mbox))
                      ;; Setup Actor context
                      self     (msg-actor *current-evt*)
                      self-beh (sys:atomic-exchange (actor-beh self) nil))
                (cond (self-beh
                       ;; ---------------------------------
                       ;; Dispatch to Actor behavior with message args
                       (setf qsave           (and qhd qtl)  ;; queue state for possible rollback
                             *whole-message* (msg-args  *current-evt*)
                             *new-beh*       self-beh)
                       (apply self-beh *whole-message*)
                       (setf  (actor-beh self) *new-beh*))
                      
                      (t
                       (add-evq *current-evt*))
                      )))
           ;; ------------------------------------
           ;; we come here on Abort - back out optimistic commits of SEND/BECOME
           (setf (actor-beh self) self-beh)
           (if (setf qtl qsave)
               (setf (msg-link (the msg qsave)) nil)
             (setf qhd nil))
           ))
      )))

;; ----------------------------------------------------------
;; SPONSORS -- offer an event queue and have an associated runtime
;; thread performing RUN dispatching of Actor events.
;;

(defvar *all-sponsors* nil)

(defun add-to-sponsors (name spon)
  (let ((pair (assoc name *all-sponsors*)))
    (if pair
        (setf (cdr pair) spon)
      (setf *all-sponsors* (acons name spon *all-sponsors*)))))

(defmacro def-sponsor (name)
  `(progn
     (defvar ,name (make-actor))
     (add-to-sponsors ',name ,name)))

(def-sponsor base-sponsor)
(def-sponsor slow-sponsor)

(defun is-pure-sink? (actor)
  (or (null actor)
      (eq (actor-beh actor) #'lw:do-nothing)))

(defun sponsor-beh (mbox thread)
  ;; this one is just slightly special
  (alambda
   ((:shutdown)
    (become #'lw:do-nothing)
    (mp:process-terminate thread))
   
   ((actor . msg)
    (unless (is-pure-sink? actor)
      (mp:mailbox-send mbox (new-msg actor msg))
      ))
   ))

(defun make-sponsor (title)
  (let ((spon (make-actor)))
    (restart-sponsor spon title)))

(defun kill-sponsor (sponsor)
  (send sponsor :shutdown))

(defun restart-sponsor (sponsor title)
  (check-type sponsor actor)
  (let* ((mbox   (mp:make-mailbox))
         (thread (mp:process-run-function title () #'run-actors sponsor mbox)))
    (setf (actor-beh sponsor) (sponsor-beh mbox thread))
    sponsor))

;; -----------------------------------------------
;; SEND/BECOME
;;
;; SEND can only be called on an Actor. BECOME can only be called from
;; within an Actor.
;;
;; SEND and BECOME are transactionally staged, and will commit *ONLY*
;; upon error free completion of the Actor body code.
;;
;; So if you need them to take effect, even as you call potentially
;; unsafe functions, then surround your function calls with
;; HANDLER-CASE, HANDLER-BIND, or IGNORE-ERRORS. Otherwise, an error
;; will make it seem that the message causing the error was never
;; delivered.

(defun send (actor &rest msg)
  (apply *send* actor msg))

(defmacro send* (actor &rest msg)
  `(apply #'send ,actor ,@msg))

(defun repeat-send (actor)
  (send* actor self-msg))

(defun send-combined-msg (cust msg1 msg2)
  (multiple-value-call #'send cust (values-list msg1) (values-list msg2)))

(defun become (new-beh)
  (setf *new-beh* new-beh))

#|
(defun do-with-sponsor (where fn)
  (let ((spon (or where base-sponsor)))
    (if (eq spon self-sponsor)
        (funcall fn)
      (send* spon self self-msg))))

(defmacro with-sponsor (where &body body)
  ;; Properly belongs just after message detection which might trigger
  ;; BECOME. Should be used ahead of any side-effecting code in the
  ;; handler clause.
  `(do-with-sponsor ,where
                    (lambda ()
                      ,@body)))

#+:LISPWORKS
(editor:setup-indent "with-sponsor" 1)

(defmacro with-mutable-beh ((&optional preferred-sponsor) beh)
  ;; This version does not force execution onto a sponsor unless it
  ;; was specified as preferred-sponsor. Possibly takes better
  ;; advantage of multiple threads?
  (lw:with-unique-names (msg)
    (flet ((gen-body ()
             `(apply (macrolet ((become (new-beh)
                                  `(setf *new-beh* ,new-beh)))
                       ,beh)
                     ,msg)
             ))
      `(lambda* ,msg
         ,(if preferred-sponsor
              `(with-sponsor ,preferred-sponsor ,(gen-body))
            (gen-body)))
      )))

#+:LISPWORKS
(editor:setup-indent "with-mutable-beh" 1)
|#

;; ----------------------------------------------------------------
;; Start with two Sponsors: there is no difference between them. But
;; we envision that the SLOW-SPONSOR will be used to run Actors with
;; blocking actions, e.g., I/O.

(defun restart-actors-system ()
  (restart-sponsor base-sponsor "Actor Thread")
  (restart-sponsor slow-sponsor "Actor I/O Thread"))

(defun kill-actors-system ()
  (kill-sponsor base-sponsor)
  (kill-sponsor slow-sponsor))

#|
(kill-actors-system)
(restart-actors-system)
 |#

;; -------------------------------------------------------
;; Cross-sponsor sends

(defun in-sponsor-beh (sponsor actor)
  (lambda* msg
    (if (eq sponsor self-sponsor)
        (send* actor msg)
      (send* sponsor actor msg))))

(defun in-sponsor (sponsor actor)
  (make-actor (in-sponsor-beh sponsor actor)))

;; -------------
#|
(defun par-safe-beh (actor)
  (in-sponsor-beh base-sponsor actor))

(defun par-safe (actor)
  (make-actor (par-safe-beh actor)))
|#
;; -------------

(defun io-beh (actor)
  (in-sponsor-beh slow-sponsor actor))

(defun io (actor)
  (make-actor (io-beh actor)))

;; ------------

(defun in-this-sponsor (actor)
  (in-sponsor self-sponsor actor))

(defun ioreq (actor)
  ;; send to actor, return its reply to cust in sender's original sponsor.
  ;; typically, actor will be (IO actor)
  (actor (cust &rest msg)
    (send* actor (in-this-sponsor cust) msg)))

;; --------------------------------------

(defun sink-beh ()
  #'lw:do-nothing)

(deflex sink
  (make-actor (sink-beh)))

;; --------------------------------------

;; alas, with MPX we still needs locks sometimes
(defvar *printer-lock* (mp:make-lock))

(defmacro with-printer (&body body)
  `(mp:with-lock (*printer-lock*)
     ,@body))

(deflex println
  (io
    ;; because we are managing an output stream
    (actor msg
      (with-printer
       (format t "~&~{~A~%~^~}" msg)))
     ))

(deflex writeln
  (io
    ;; because we are managing an output stream
    (actor msg
      (with-printer
       (format t "~&~{~S~%~^~}" msg)))
     ))

;; ------------------------------------------------
;; The bridge between imperative code and the Actors world

(defun mbox-sender-beh (mbox)
  (check-type mbox mp:mailbox)
  (lambda (&rest ans)
    (mp:mailbox-send mbox ans)))

(defun mbox-sender (mbox)
  (make-actor (mbox-sender-beh mbox)))

(defun ask (actor &rest msg)
  ;; Actor should expect a cust arg in first position. Here, the
  ;; mailbox.
  (if self
      ;; Counterproductive when called from an Actor, except for
      ;; possible side effects. Should use BETA forms if you want the
      ;; answer.
      (send* actor sink msg)
    (let ((mbox (mp:make-mailbox)))
      (send* actor (mbox-sender mbox) msg)
      (values-list (mp:mailbox-read mbox)))
    ))

(defun maybe-safe-ask (actor &rest msg)
  ;; For Actors calling upon other Actors in a functional manner,
  ;; instead of using continuation Actors. This is to be seriously
  ;; discouraged...
  ;;
  ;; Maybe it will work, or maybe not... As long as the target Actor
  ;; executes entirely in one sponsor, then this will work. Otherwise,
  ;; maybe...  Still, this is blocking wait, and poor form for Actors
  ;; code. If the target Actor winds up trying to run in our sponsor,
  ;; then we become deadlocked.
  (if self
      (let ((spon  (if (eq self-sponsor base-sponsor)
                       ;; choose not our sponsor
                       slow-sponsor
                     base-sponsor))
            (mbox (mp:make-mailbox)))
        (apply (actor-beh spon) actor (mbox-sender mbox) msg)
        (values-list (mp:mailbox-read mbox)))
    ;; else
    (apply #'ask actor msg)))

;; -----------------------------------------------------
;; FN-EVAL - eval function and send results to customer

(deflex fn-eval (make-actor
                 (lambda (cust fn &rest args)
                   (send* cust (multiple-value-list (apply fn args))))))

;; ----------------------------------------
;; We must defer startup until the MP system has been instantiated.

(defun lw-start-actors (&rest _)
  (declare (ignore _))
  (restart-actors-system)
  (princ "Actors are alive!"))

(defun lw-kill-actors (&rest _)
  (declare (ignore _))
  (kill-actors-system)
  (print "Actors have been shut down."))

(let ((lw:*handle-existing-action-in-action-list* '(:silent :skip)))

  (lw:define-action "Initialize LispWorks Tools"
                    "Start up Actor System"
                    'lw-start-actors
                    :after "Run the environment start up functions"
                    :once)

  (lw:define-action "Save Session Before"
                    "Stop Actor System"
                    'lw-kill-actors)

  (lw:define-action "Save Session After"
                    "Restart Actor System"
                    'lw-start-actors)
  )

#| ;; for manual loading mode...
(when (eq (actor-beh base-sponsor) #'lw:do-nothing)
  (if (mp:get-current-process)
      (lw-start-actors)
    ;; else
    (pushnew '("Start Actors" () lw-start-actors) mp:*initial-processes*
             :key #'third)))

|#