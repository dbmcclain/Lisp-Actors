
(in-package #:actors-base)

#| ====================================================================
                  --- Structure of Actors --- 

     Messages
        |
        v                
  +--------------+    == Actor Behavior ==
  | Actor Object |         (Closure)
  +--------------+      +--------------+                              
     |   ^              | Local State  |         (Closure)			     
     |   |              +--------------+     +-----------------+
     |   |       +----->|     THIS     |---->| Message Handler |
     |   |       |      +--------------+     +-----------------+
     |   +-------|------|     SELF     |   
     |           |      +--------------+   
     +-----------|----->|   Pandoric   |
                 +------|    Wrapper   |  (Calls through THIS)                              
                        +--------------+                              

   PANDORIC-HOTPATCH -- affects only the THIS pointer
   
   PANDORIC-RECODE   -- affects both the THIS pointer and the pointer to
                        a pandoric wrapper in the Actor Object, if pandoric
                        bindings are expanded with BECOME.

   THIS -- points to the message handler currently in force.
   
   SELF -- points to the Actor Object

   When using Hoyte's pandoric alone, the SELF pointer points directly
   at the pandoric wrapper. Under use by Actors, we go indirect via the
   Actor object. An Actor is a funcallable object through its
   behaviour pointer which points to the same location as a pandoric
   SELF pointer. We simply repurpose the SELF to always point at the
   Actor obect.

   Basing the Actor behaviors on pandoric closures allows for a degree
   of introspection that we wouldn't have otherwise. (Closures are
   generally closed)                                                               
   
 ====================================================================== |#
#|
  ;;
  ;; MAKE-ACTOR is to the Actor system what LAMBDA is to Lisp. It
  ;; constructs an anonymous embodiment of behaior which responds to
  ;; some messages. Only one instance with the same local state held
  ;; in lexical bindings is permitted to run at any time.
  ;;
  ;; If you want to pandorically get/set lexical bindings in the
  ;; enclosing context, then copy them into declared state bindings.
  ;;
  ;; All state bindings are accessible by get-pandoric, along with ME
  ;; = original behavior function, PENV = pandoric environment, and
  ;; THIS = current behavior function..
  ;;
  ;; ME contains a copy of the original behavior in order to support
  ;; the REVERT function. The outer behavior is held in the Actor
  ;; instance object BEHAVIOR slot, as well as PENV, and always
  ;; points to the pandoric wrapper inside the Actor. The inner
  ;; behavior is held inside the Actor's THIS binding, and points to
  ;; the actual Actor message handling code.
  ;;
  ;; NOTE: LET* is used for the initial state bindings, so that later
  ;; bindings can refer to earlier ones. And SELF, ME, PENV, and THIS
  ;; all appear earlier than any of the user state bindings. And so
  ;; any of these can be referred to in user state bindings that refer
  ;; to them in a closure.
  ;;
  ;; *BUT*... none of SELF, ME, PENV, nor THIS are bound until after
  ;; the user state bindings. So attempting to use one of SELF, ME,
  ;; PENV, or THIS in a funciton call will not produce what you want.
  ;; For such state bindings, an explicit SETF will be required later
  ;; in the body code.
  ;;
|#

(defmacro! make-actor (lexref state &body body &environment env)
  (um:ensure-thread-eval
   `(let ((,g!actor (make-instance 'Actor)))
      (pandoriclet ,lexref ,state
        ;; <-- SELF & THIS are not bound until here...
        (setf (actor-save ,g!actor) (list ,a!self this)
              (actor-behavior ,g!actor) ,a!self
              ,a!self ,g!actor)
              ,@body))
   env))

(editor:setup-indent "make-actor" 2)

#|
  ;;
  ;; change the behavior of the Actor for future messages while
  ;; keeping local bindings intact. Does not add additional local
  ;; state, but surrounding context might.
  ;;
  ;; bargs - the names of local state bindings for which you want to
  ;; keep (pandoric) access.
  ;;
  ;; pargs - the names of vars in the current lexical environment that
  ;; you want to make available to the outside world.
  ;;
  ;; Can only be used inside of a scope with a visible PENV binding,
  ;; i.e., either inside of the original Actor body, or a DEFPAN used
  ;; on it with a PENV argument passed to it.
  ;;
|#

(defmacro! become (lexref state-ref new-behavior)
  ;;
  ;; change the behavior of the Actor for future messages while
  ;; keeping local bindings intact. Does not add additional local
  ;; state, but surrounding context might.
  ;;
  ;; bargs - the names of local state bindings for which you want to
  ;; keep (pandoric) access.
  ;;
  ;; pargs - the names of vars in the current lexical environment that
  ;; you want to make available to the outside world.
  ;;
  ;; Can only be used inside of a scope with a visible PENV binding,
  ;; i.e., either inside of the original Actor body, or a DEFPAN used
  ;; on it with a PENV argument passed to it.
  ;;
  (if lexref
      ;; pargs refer to lexical bindings in outer scope
      ;; Create a new PENV with its own local THIS binding.
      `(let ((,g!actor ,a!self))
         (with-pandoric ,state-ref (actor-behavior ,a!self)
           (pandoriclet (,@state-ref ,@lexref) ()
             (setf (actor-behavior ,g!actor) ,a!self
                   ,a!self ,g!actor)
             ,new-behavior)))
    
    ;; else - we continue using the same PENV and just change
    ;; the THIS bindings
    `(pandoric-recode ,state-ref ,a!self ,new-behavior)
    ))

(editor:setup-indent "become" 2)


(defpan revert () ()
  ;;
  ;; Reset our SELF behavior to its original
  ;;
  ;; This resets all the pandoric access, undoes all BECOME's, but
  ;; keeps the original local state intact.
  ;;
  (let ((me (actor-save self)))
    (setf (actor-behavior self) (car me))
    (pandoric-hotpatch (car me) (cadr me))))
  

#| ==================================================================
 What about BECOME-LIKE, where you could construct a new Actor with
MAKE-ACTOR, and then switch identities with it? Neat idea! But the
problem with it is that unless you promise to immediately discard the
newly created Actor, it would be possible to have two running
instances of the same Actor, with the same local state. Even worse if
you model after an extant Actor.

That violates the invariant that only one copy of local state can be
running at one time. And, if permitted, you would have to ensure that
local state were carefully coordinated for SMP sharing.

One of the prime motivations for Actor-based programming was to move
away from the incessant headaches of SMP sharing, keeping access to
local state unshared.
 ==================================================================== |#

#|
  ;;
  ;; state is like a LET binding
  ;; args is a list of args expected by the outer behavior.
  ;;
  ;; This macro builds a function that can be called to make multiple
  ;; instances of the same behavior (same kind of Actor), each with
  ;; their own private copy of internal state.
  ;;
  ;; within body you can refer to symbols ME = original body code
  ;; function, THIS = current body code function, SELF = current
  ;; actor, and any of the symbols named in the binding forms of the
  ;; initial state. State is panorically available, along with ME and
  ;; THIS.
  ;;
|#

(defmacro def-factory (name args state &body body &environment env)
  (um:ensure-thread-eval-def name
    `(defun ,name (&key ,@args)
       (make-actor ,args ,state ,@body))
    env))

(defun spawn (behavior &rest args)
  ;; SPAWN - MAKE-ACTOR combined with immediate SEND
  (let ((actor  (make-actor () ()
                  behavior)))
    (apply #'send actor args)
    actor))


(defmethod lispworks:get-inspector-values ((actor actor) (mode (eql 'Pandoric-Slots)))
  (declare (ignore mode))
  (loop with plst = (ask actor :pandoric-vals)
        for (key val) in (um:group plst 2)
        collect key into keys
        collect val into vals
        finally (return (values `(:pandoric-syms ,@keys)
                                `((,@keys) ,@vals)))
        ))

#|
(defpan ext2 () (a)
  (let ((c 3))
    (become (c) (a)
      (lambda (&rest msg)
        (pr :from-ext2 msg :a a :c c)
        (revert self)))))

(defpan ext1 () (a)
  (let ((b 2))
    (become (b) (a)
      (lambda (&rest msg)
        (pr :from-ext1 msg :a a :b b)
        (ext2 self)))))


(setf x (make-actor () ((a 1))
          (lambda (&rest msg)
            (pr :from-original msg :a a)
            (ext1 self))))
(ask x :pandoric-vals)
(send x :hello)
 |#
