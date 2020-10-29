# Lisp-Actors
Thread-agnostic Actors in Common Lisp

This repo contains an ongoing investigation into the use of the Actor model in Common Lisp. It was begun nearly 3 years ago and has had the benefit of real-world application. That experience helped to refine what you now see. 

When Actors began, I used the Comer DLAMBDA style for Actor body code. That still works. But along the way, I asked myself about the possibility of making Actor code extensible, and about using CLOS to provide more accessible infrastructure. From that question arose the larger proliferation of my Actors as CLOS Classes and new Actor behaviors that can be constructed with CLOS Methods. The PERFORM-IN-ACTOR macro allows any method or function to wrap a body of code and ship it off to an Actor for execution. 

DLAMBDA style Actor bodies invite the use of private state in LET bindings. By contrast, Actors as Classes invite the use of Instance Slots for state information. Slots are not hidden from view, but an informal agreement should be that only the Actor itself can mutate that state. As with much of Common Lisp, I think this exposure of information can be beneficial. But with freedom comes responsibility...

Actors are Class Instances with a mailbox for sent messages and a USER-FUNCTION slot. That function describes its behavior to incoming messages. An Actor is not a thread. Rather, Actors with waiting messages are enqueued in a global mailbox to which a pool of Executive Threads are waiting for work items. When a message arrives to an Actor's mailbox, if that Actor is not currently executing, it becomes enqueued in the Ready Queue and will be taken at the next opportunity by one of the Executive Threads for execution of its message handling code.

The main premise is that Actors are guaranteed to run in a single-thread context. All Actor code should abide by this premise.

In real life situations, programming inevitably involves calling on blocking I/O. We have a choice to make here. Either allow the Actor code to call the blocking I/O, or else spawn off a WORKER onto another Executive thread for blocking execution and leave the Actor ready to handle more messages. But that implies that we have callback closures into the Actor body. And to preserve single-thread semantics, those callbacks must be performed by the Actor itself. So when work is spawned off, the continuation closure handed to it will send a private CONTINUATION message back to the Actor along with the actual closure code to execute.

There are only two private messages for Actors. One is the CONTINUATION message. The other is an ASK message. Every message handling function among the Actor behaviors is a normal Lisp function and can return a value. But that value is just dropped, unless it is executed in the context of an ASK. When that happens its result is bundled up (including any exceptions that may have happened) and sent back to the waiting asker.

By default, the USER-FUNCTION for a new Actor is #'FUNCALL. This allows any thunk or function call message to be performed in the Actor context. And since Actors can only be executed on one thread at any time, this serializes the function calls. SEND always enqueues new messages to the Actor's mailbox, for delivery handling in the main Actor loop. But an Actor can perform one of its own behaviors immediately by calling SELF-CALL. (SEND to Actors is always asynchronous, non-blocking.)

By default, an Actor can be used as a Hoare-Monitor. It can also be used to serialize access to a shared resource - like *STANDARD-OUTPUT*.

Using CONTINUATIONS invites problems with HANDLER-CASE, HANDLER-BIND, CATCH, etc. Anything that sets up a dynamic context will see that context exited after an Actor spawns off a WORKER to perform some action. On re-entry the CONTINUATION will start anew without the benefit of those handlers or that original dynamic state. In recoginition of this fact, the CPS code has extensions =HANDLER-CASE, =HANDLER-BIND, =CATCH, etc., that allows a CONTINUATION to re-establish handlers and the dynamic context that existed at the time of its closure construction.

So far so good. But Dynamic Bindings that were in effect cannot be restored, and UNWIND-PROTECT presents some interesting puzzles. If you really need some dynamic bindings restored, then use the CPS =LET and =LET* forms. But UNWIND-PROTECT remains a puzzle. In particular we have difficulties restarting irreversible actions...

One interesting outcome of Actor style is that the particular Thread on which an Actor is executing is almost irrelevant. CURRENT-PROCESS is irrelevant. What is more important is CURRENT-ACTOR, which is used when forming continuation closures and elsewhere. It doesn't matter which thread an Actor is running on. But what does matter is whether or not some Actor is the CURRENT-ACTOR, for that warns us off from making slot mutations and instead asking the other Actor to perform those mutations for itself.

The difference between an Actor and a Worker? Actors contain a mailbox for messages, Workers do not. Both run on Executive Threads. A Worker therefore only executes one action. An Actor can communicate readily (2-way interactions) with other Actors and the outside world, and provides for open-ended possibilities of future actions.

Actors is the further outgrowth of my Buttefly system (c.a. 2008), which attempted to mimic Erlang-style interactions between threads and machines. Actors have shown themselves even more capable in their own right, and significantly smaller and lighter weight, and so my entire Butterfly system has been scrapped in favor of Actors. 

When this Actors system starts up it initializes an Actor Server on TCP Port 65001. There is an EVAL Actor that can respond to external requests. So asking an Actor on another machine to send back the result of some action is as simple as:

    (ask "eval@rincon.local" '(list (machine-instance) (get-universal-time)))
    
EVAL has been registered as a known Actor, along with ECHO, SYSTEM-LOG, and any other services that are invented and registered. Actors can be reached through the registry by name (string or symbol).

The Actor service is by way of self-encryped SSL channels on TCP/IP. A good real-world example of the use of Actors is the Asynchronous Socket Handling in "network-connection.lisp", a part of this Actor system. In there are examples of async event handling, continuations, and much more. 

The self-encryption with authentication, atop the SSL security, makes use of a modified SRP-6 handshake protocol to establish keying, independent for each direction. Internally the data are encrypted with AES-128. Large packets are fragmented and reassembled. The largest valid transmission packet is 64k, ameliorating DOS attacks carrying large payloads.

This work is performed mainly on Lispworks systems. Some effort has gone into making a portable source. But recent work has ignored that.

- DM

