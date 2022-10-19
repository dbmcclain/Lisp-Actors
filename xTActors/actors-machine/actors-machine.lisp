;; actors-machine.lisp -- Emulator for an Actors Machine
;;
;; The rationale for this is here:
;; http://www.dalnefre.com/wp/2022/08/memory-safety-simplifies-microprocessor-design/
;;
;; A pure Actors Machine is simpler, more secure, and uses less power
;; to accomplish feats.
;;
;; Here I am experimenting with a twist on the architecture mentioned
;; in the paper. I am allowing multiple invocation of the same Actor
;; so we have true parallelism, at the granularity of an Actor
;; A-Machine instruction, all within one running machine thread.
;;
;; The paper takes a more restrictive stance in that any one Actor can
;; only be running one invocation at a time. That makes it safe to
;; peform mutation on shared data. But since the Commit/Fail protocol
;; requires all visible changes to be rolled back on failure, it is
;; simpler to just avoid mutation of shared data, using FPL code, in
;; which case there is no reason to avoid parallel invocations.
;;
;; We accomplish this multiple-invocation by having each running
;; instance carry its own context.  The context is what identifies any
;; particular invocation. That context is sent back and forth between
;; the Actor and the A-Machine. Only the A-Machine peers inside, and
;; makes mutable changes to it.
;;
;; The Actor never mutates anything visible outside itself. It uses
;; pure FPL code. Only the A-Machine mutates anything, and it is very
;; restricted. Since there is only one running thread in the
;; A-Machine, there is no danger here.
;;
;; On an A-Machine, you have instruction level concurrency. We have
;; only 8 instructions in the emulated A-Machine:
;;
;;   -----------------------
;;   :SEND -- (ctxt cont :SEND target . msg)
;;   Enqueues a message send. Will be sent at final commit. Args ctxt
;;   and cont are the context object for the Actor invocation, and the
;;   continuation Actor for resumption (typically a β Actor).
;;
;;   Unlike Call/Return CPS, we are using Actors and SEND to effect
;;   continuation resumption.
;;
;;   -----------------------
;;   :BECOME -- (ctxt cont :BECOME new-beh)
;;    Allows an Actor to mutate its behavior and/or state upon final
;;    commit.
;;
;;   -----------------------
;;   :CREATE -- (ctxt cont :CREATE beh)
;;   Creates a new A-Machine AM-ACTOR with the indicated behavior. On
;;   the continuation resumption, that new Actor will be handed back
;;   to the sending Actor as an argument for its own use.
;;
;;   -----------------------
;;   :OPER -- (ctxt cont :OPER fn . args)
;;   A catchall opcode, allowing the Actor to perform any other Lisp
;;   operation, but only one, before being rescheduled at the back of
;;   the instruction queue, to give other Actors a chance to run.
;;
;;   -----------------------
;;   :CONT -- (ctxt cont :CONT . args)
;;   Issued only by the A-Machine CPU to schedule a continuation for
;;   an Actor.
;;
;;   -----------------------
;;   :RX -- (:RX target . msg)
;;   An event from a SEND has arrived. Causes the A-Machine CPU to
;;   construct an initial context, and enqueues a continuation for the
;;   target Actor at its behavior entry point.
;;
;;   -----------------------
;;   :COMMIT -- (ctxt :COMMIT)
;;   Commits all pending SEND and BECOME, and terminates the running
;;   of the Actor. No continuation after this instruction.
;;
;;   The context argument contains all the necessary information. If
;;   the parallel operation of the Actor is the first to BECOME and
;;   reach commit, then it takes the day. Otherwise, the Actor gets
;;   retried from its beginning.
;; 
;;   If an Actor never issues BECOME, then it simply sends all
;;   committed message SENDS.
;;
;;   A commit is the final insruction in an Actor. Any errors along
;;   the way will prevent reaching this final instruction.
;;
;;   -----------------------
;;   :EXIT -- not really an instruction op-code. But gives us a way to
;;   have the A-Machine terminate is activity.
;;
;; Our A-Machine Actors are a subtype of ACTOR. Normal message SEND
;; works on them, just as with any Actor. But they can be
;; distinguished using AM-ACTOR-P, and they are expected to behave
;; differently, issuing instruction-level messages to the A-Machine,
;; and they always need a context argument in first place.
;;
;; Any messages sent to their outer wrapper Actor from the outside
;; Actors system will result in an :RX instruction being sent to the
;; A-Machine for the corresponding embedded A-Machine Actor.
;;
;; Similarly, the A-Machine Actors are permitted to send to other
;; A-Machine Actors and also to outside Actors. But they should issue
;; a :SEND instruction, and not use a normal SEND. A normal SEND will
;; be immediate, not staged for later commit. These AM-ACTORS are not
;; operating under the transactioning protocol of normal Actors. They
;; have their own transaction protocol, enforced by the A-Machine CPU.
;;
;; Due to the design of this emulator, it calls for CPS-like code
;; inside the A-Machine Actors, using contination Actors (e.g., β
;; forms). I try to ease that by using alternate syntax: SENDING,
;; instead of SEND, BECOMING instead of BECOME, CREATING instead of
;; CREATE, and OPERATING.
;;
;; I also implemented a dynamic binding for the running context, which
;; must always be passed back and forth in first position. The
;; Customer argumemnt, which normally occupies first position, is now
;; in second position. But thanks to Lisp dynamic binding we can hide
;; that fact, and keep things looking like always with customer in
;; first position in the code we write.
;;
;; What we really need here is a CPS Conversion at the Actor level,
;; which requires a code walker and a lot more effort. For now we just
;; do the conversion by hand.
;;
;; DM/RAL  2022/10/18 00:00:22
;; ------------------------------------------------------------------------

(in-package "actors-machine")

(defvar *mcpu-istream*)
(defvar *self-ctxt*)
(defvar *ac-machine* nil)

(defstruct (am-actor
            (:include actor)))

(defstruct ctxt
  actor
  pend-send
  pend-become
  original-msg
  original-beh)

(defun startup-send-message (target &rest msg)
  (when (sys:compare-and-swap *ac-machine* nil t)
    ;; Start the A-Machine, if not already running, or about to run.
    (setf *mcpu-istream*  (mp:make-mailbox)
          *ac-machine*    (mp:process-run-function "Actor Machine" () #'am-cpu)
          *send-message*  #'running-send-message))
  (apply #'send-message target msg))

(defun running-send-message (target &rest msg)
  (cond ((am-actor-p target)
         (mp:mailbox-send *mcpu-istream* (list* :rx target msg)))

        ((actor-p target)
         (send* target msg))
        ))

(defvar *send-message* #'startup-send-message)

(defun send-message (target &rest msg)
  (when (actor-p target)
    (apply *send-message* target msg)))

(defmacro send-message* (target &rest msg)
  `(apply #'send-message ,target ,@msg))
  
(defun send-messages (msgs)
  (dolist (msg msgs)
    (apply #'send-message msg)))

(defun instr (ctxt cont op &rest args)
  (mp:mailbox-send *mcpu-istream* (list* ctxt cont op args)))

(defmacro instr* (&rest op)
  `(apply #'instr ,@op))

(defun contin (ctxt cont &rest args)
  (instr* ctxt cont :cont args))

(defmacro contin* (ctxt cont &rest args)
  `(apply #'contin ,ctxt ,cont ,@args))

(defun #1=am-cpu ()
  (loop
     (with-simple-restart (abort "Do next A-Machine instruction")
       (loop
          (match (mp:mailbox-read *mcpu-istream*)
            
            ((ctxt cont :send target . msg)
             (push (cons target msg) (ctxt-pend-send ctxt))
             (contin ctxt cont))
            
            ((ctxt cont :become new-beh-fn)
             (let ((new-actor (funcall new-beh-fn sink :get-am-actor)))
               (setf (ctxt-pend-become ctxt) (am-actor-beh new-actor))
               (contin ctxt cont)))
            
            ((ctxt cont :create new-beh-fn)
             (let ((new-actor (funcall new-beh-fn sink :get-am-actor)))
               (contin ctxt cont new-actor)))
            
            ((ctxt cont :oper fn . args)
             (contin* ctxt cont (multiple-value-list (apply fn args))))
            
            ((ctxt cont :cont . msg)
             ;; interesting - anti-blocking tactic
             ;; (this is an A-Machine emulator, not a speed contest.)
             (send* cont ctxt msg))
            
            ((ctxt :commit)
             (let ((actor     (ctxt-actor ctxt))
                   (old-beh   (ctxt-original-beh ctxt))
                   (new-beh   (ctxt-pend-become ctxt)))
               
               (cond ((or (null new-beh)
                          (sys:compare-and-swap (am-actor-beh actor) old-beh new-beh))
                        ;; no BECOME performed, or else we got here first
                        (send-messages (ctxt-pend-send ctxt)))
                     
                     (t ;; must retry
                        (let ((cur-beh  (am-actor-beh actor)))
                          (setf (ctxt-original-beh ctxt) cur-beh
                                (ctxt-pend-become  ctxt) nil
                                (ctxt-pend-send    ctxt) nil)
                          (let ((msg (ctxt-original-msg ctxt)))
                            (contin* ctxt cur-beh msg)))
                        ))))
            
            ((:rx target . msg)
             (let* ((beh   (am-actor-beh target))
                    (ctxt  (make-ctxt
                            :actor         target
                            :original-beh  beh
                            :original-msg  msg)))
               (contin* ctxt target msg)))

            ((:exit)
             (setf *send-message* #'startup-send-message
                   *mcpu-istream* nil
                   *ac-machine*   nil)
             (sys:ensure-memory-after-store)
             (return-from #1#))            
            ))
       )))

(defun kill-am ()
  (mp:mailbox-send *mcpu-istream* '(:exit)))
#|
(kill-am)
|#
;; -----------------------------------------------
;; Writing Actor Machine Behaviors

(um:eval-always
  (defun chk-commit (body)
    (if (endp body)
        `((commit))
      body))

  (defun make-am-beh (clause)
    (destructuring-bind (pat . body) clause
      `(,pat
        ,@(when (and (consp body)
                     (eql 'when (car body))
                     (cadr body))
            (prog1
                `(when ,(cadr body))
              (setf body (cddr body))))
        ,@body))))

(defmacro sending ((&rest msg) &body body)
  `(β  (*self-ctxt*)
       (instr *self-ctxt* β :send ,@msg)
     ,@(chk-commit body)))

(defmacro sending* ((&rest msg) &body body)
  `(β  (*self-ctxt*)
       (instr* *self-ctxt* β :send ,@msg)
     ,@(chk-commit body)))

(defmacro becoming (beh-fn-form &body body)
  `(β (*self-ctxt*)
       (instr *self-ctxt* β :become ,beh-fn-form)
     ,@(chk-commit body)))

(defmacro creating ((ans beh-fn-form) &body body)
  `(β (*self-ctxt* ,ans)
       (instr *self-ctxt* β :create ,beh-fn-form)
     ,@(chk-commit body)))

(defmacro operating ((ans (fn &rest args)) &body body)
  `(β (*self-ctxt* ,ans)
       (instr *self-ctxt* β :oper #',fn ,@args)
     ,@(chk-commit body)))

(defun commit ()
  (mp:mailbox-send *mcpu-istream* (list *self-ctxt* :commit)))

;; ----------------------------------
;; Defining new A-Machine Behaviors

(defmacro am-lambda (args &body body)
  `(lambda* ,(cons '*self-ctxt* args)
     (symbol-macrolet ((self  (ctxt-actor *self-ctxt*)))
       ,@body)))



(defmacro def-am-beh (name args &body clauses)
  (lw:with-unique-names (am-actor msg)
    `(defun ,name ,args
       ;; We contain an embedded A-Machine Actor, carrying the real
       ;; behavior code.
       (let ((,am-actor (make-am-actor
                         :beh  (am-lambda (&rest ,msg)
                                 (match ,msg
                                   ,@(mapcar #'make-am-beh clauses)))
                         )))
         ;; ...and present a normal Actor behavior skin to the outside
         ;; world. Messages sent here from outside the A-Machine get
         ;; redirected to the embedded A-Machine Actor.
         (alambda
          ((cust :get-am-actor)
           (send cust ,am-actor)
           ,am-actor)
          
          (,msg
           (send-message* ,am-actor ,msg))
          )))))

;; ------------------------------------------------------------
;; A-Machine versions of some primitive Actors
;;
;; You can use then as normal Actors too. Messages sent to them, from
;; outer Actors-land, will be redirected to the internal A-Machine
;; Actor embedded within.
;;
;; Messages sent by A-Machine Actors to each other, go directly toward
;; that embedded A-Machine Actor.

(def-am-beh am-sink-beh ()
  (_))

(deflex am-sink
  (create (am-sink-beh)))


(def-am-beh am-const-beh (&rest val)
  ((cust)
   (sending* (cust val))))

(defun am-const (&rest val)
  (create (apply #'am-const-beh val)))


(def-am-beh am-tag-beh (target)
  ;; prepend SELF to the message and forwward to target.
  (msg
   (sending* (target self msg)
     )) )
            
(defun am-tag (target)
  (create (am-tag-beh target)))


(def-am-beh am-label-beh (target label)
  (msg
   (sending* (target label msg))))

(defun am-label (target label)
  (create (am-label-beh target label)))

;; ----------------------------------------------------
#|
(def-am-beh count-to-beh (from to)
  ((:next)
   (sending (println from)
     (operating (ans (1+ from))
       (if (< ans to)
           (becoming (count-to-beh ans to)
             (sending (self :next)))
         (sending (println :done)
           (becoming (am-sink-beh)))
         )))))

(def-am-beh count-start-beh ()
  ((from to)
   (creating (am-a (count-to-beh from to))
     (sending (am-a :next)))))

(let ((supv (create (count-start-beh))))
  (dotimes (ix 100)
    (send supv ix (+ ix 100))))

(let ((supv (create (count-start-beh))))
  (send supv 15 20)
  (send supv 100 105)
  (send supv 1500 1508))

(progn
  (send (create (count-to-beh 15 20)) :next)
  (send (create (count-to-beh 100 105)) :next)
  (send (create (count-to-beh 1500 1508)) :next))

(def-am-beh am-error-beh ()
  (_
   (operating (ans (/ 0)))
   ))

(let ((am (create (am-error-beh))))
  (send am))
|#

           
