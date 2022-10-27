-- 27 October 2022 -- Where do Actors Make Sense?
---
I have now completed several large projects using Actors, as well as exploring what would happen if we had an Actors-machine? Several conclusions follow.

Okay, you have finally completed your high-performance math subroutine library. It makes maximum use of every trick Lisp has to offer, and then some. Lots of procedural, serially performed actions in that code. It handles error conditions well.

On a CALL/RETURN architecture, it makes sense to leave that in Lisp, or whatever imperative language you prefer. It is fast, fits naturally with the machine architecture, and does exactly what you want. An Actors-machine might also perform really well, but we'll probably never know for sure, because it is such a vast departure from existing computer architectures, and for now, there is only a splinter group to which it appeals. So the next best thing for ardent adventurers is an Actor-machine Emulator. And that is cute, but slow as molasses compared to native CALL/RETURN. So live with the CALL/RETURN.

But then, the next step is to incorporate your fancy library into a real application. And that app gets messy real fast in CALL/RETURN style. Too many possible sources of information competing for access to your library. The control flow around servicing becomes hairy. That's where you really benefit from an Actors environment. All the executive control, handling unexpected asynchronous events, and so forth. I wouldn't want to be without Actors for that stuff. 

I just finished a hybrid conversion of an app that scans all my music files, performing EBU-R128 Loudness Analysis on every track, and building a database of loudness history in the 3 different analysis windows - Momentary, Short Term, and Integrated Loudness. As well as histograms on the peak-to-loudness ratios, true peak level, etc.

A lot of programs like this exist. Some can only do the analysis in real-time playback situations. Others try to be faster than realtime. And the app has a simple GUI, but complicated enough that it gets messy in CALL/RETURN architecture. So my conversion took the hairy control flow parts and changed it into an Actor network of interconnected Hewitt blocks, and left the core analysis guts (lots of math) in conventional imperative CALL/RETURN style.

GUI programming becomes a breeze - just have each GUI control button send a message to an Actor. That processing will occur in its own Actors machine threads, not bothering the GUI rendering thread. In the GUI thread, every action is short and simple. Just send a message. Let the Actors deal with the data complexity and the interconnections between GUI widgets. Let the GUI do what it does best - drawing and generating events.

The result was much simpler at the app level, and blew away everything on performance. Because now, with Actors in control at the top level, they can spin off entire analysis chains for every track you throw at it - all running in full multi-core parallel fashion. I can analyze 11 hours of playback in 2.5 minutes. That's 200x faster than realtime playback speed. A clear win for Actors and parallel code.

And using Actors means you never even have to think about machine threads, synchronization between threads, locking shared vars, etc. Actors let you focus on the problem as though you are the sole user of the machine, and Actors automatically manage all the threading issues, sight unseen. At the Actor level there are no threads, there are no locks. 

Oh sure, you sometimes need to serialize access to certain portions of the code - like the file handling. And serializers are the Actors equivalent of Locks. And yes you can find yourself in logical deadlocks in some portions of the Actors network if you don't use ordered access through serializers, just like what happens with Locks. But on the whole, much much easier to program than with conventional imperative languages. It becomes so easy to accommodate yet another source of asynchronous requests. No code rewrite at all.

But instead of machine threads, you now have logical threads - or chains of activity that may interact with other chains through messaging, or not. Every Actor can be programmed as though only one activity at a time is using the code. And every Actor in the chains may be running on different machine threads, or even different CPU cores, and possibly even in full parallel fashion with you. That doesn't matter to you. Just write the code as though you are the sole user of it - but use the discipline of Functionally Pure code.

I am a true believer now, in Transactional Hewitt Actors. But I cannot fathom being a purist and doing absolutely everything with them. Leave that kind of thinking to the Smalltalk world. Live in hybrid mode and get the best of each style.

- DM



-- 31 August 2022 -- Secure Connections Without the Need for Passwords
---
Er, what am I missing? What's with all the noise and effort with Passwords? PAKE? SRP? 1Password? 2FA?

We use secure network connections between Actors nodes. Each participant has a list of Public Keys of other participants that it will recognize. Every connection is established by a client sending across an ephemeral reply-to UUID, a random point on an Elliptic Curve, and their Public Key. 

That Public Key is cross checked at the remote server site against the list of recognized participants. If it is on the list, then we go ahead and make the connection, sending back the server site's fresh randomly generated connection UUID, another random point on the curve, and its own Public Key. Back at the client side the remote server's Public Key is likewise checked against a list of participants.

Assume client private key `c`, and public key `C = c*G`. Server private key `s` and public key `S = s*G`. Scalar numbers are lower case, ECC points are upper case. G = ECC generator point. We happen to use a very fast implementation for Curve1174, but can easily be changed.

Generate random `a` at client and start a connection by sending `(A = a*G, C)`. Server generates random `b` and sends back `(B = b*G, S)`. Now shared private key becomes `EKey = H(a*B | c*B | a*S)` at the client side, and `EKey = H(A*b | C*b | A*s)` at the server side. These two keys are the same. And only those with knowledge of their own random value and private key can produce the shared secret key.

Assuming the two public keys are on the lists, the shared private session key is used to generate roving encryption and authentication keying for every message following the initial connection message. 

Communications are completely refutable and private to the two parties. No signatures are required, yet attributable authentication is assured for both parties if successful communications occur - they both privately know the other side controls the random value corresponding to their advertised random point, and the private key corresponding to their advertised public key. 

All shared keying is forgotten after the connection is closed. Any participant can act as both client and server. All of the connection keying ballet, self-sync coding/decoding, message marshaling, compression/decompression, chunking and reassembly, and encryption/decryption, happens behind the scenes. The user only needs to know the IP Address of the server (and perhaps IP Port, default = 65001), and the name of the service to be contacted. Connections are transparently established on demand, and remain alive for some duration after the last exchange (currently 20s). 

We usually define a local proxy Actor for a remote service using `(REMOTE-SERVICE name host-ip-addr)`. The proxy handles the connection on demand as needed. And so sending messages to a remote Actor appears no different than sending to a local Actor. 

There are some restrictions on what can be sent in a message to a remote Actor - you can send Actors, and any Lisp objects, including self-referential objects, except for compiled closures or objects containing such. This is almost the same restriction you face when serializing messages to persistent storage. (You can't serialize Actors to persistent store.)

The remote proxy Actor translates all embedded Actor args in a message into ephemeral receiver Actors identified to the server by UUIDs. The ephemeral receiver Actors decrypt and forward any messages received from the server to their local customer Actors. Complementary proxy Actors are automatically produced on the server to represent the client's UUID targets for use by the server's local Actors.

Ephemeral Actors are discarded either when a message arrives or after some Time-to-Live (TTL) duration has expired. The TTL can be specified at creation time, but defaults to 10s.

For every message between client and server, shared secret key EKey:
```
      H(x) = SHA3/256(x)
      E(k,x) = SHAKE/256(k) XOR x
      
      Seq = Nonce
      Ke = H(:ENC | EKey | Seq)
      Ka = H(:AUTH | EKey | Seq)
      CipherText = E(Ke, msg)
      Auth = H(Ka | Seq | CipherText)
      transmit (Seq, CipherText, Auth)
```
      
Messages are transmitted using self-sync encoding. We are protected against DOS attacks because of the self-sync encoding, and we accept only those messages that respect the protocol format and which pass authentication. We are protected against replay attacks because we reject duplicate Seq messages. And we are protected against malicious messages that can't be correctly reassembled (dechunked), decompressed, and unmarshalled. Messages destined for unknown services, or to non-existent Actors, are silently dropped.

If anyone tries to spoof the system by using one of the Public Keys in the list of participants, they'll get back a connection, but they won't be able to communicate across it unless they also control the corresponding Private Key, to derive shared keying. 

And even if they hacked some poor user and stole his private key, they can't eavesdrop because they won't have access to the private random value and still can't derive shared keying. But they would be able to impersonate the poor user in a fresh connection. 

So protect your private keys. But you never need to rely on someone else to protect your private information - like passwords.

Communications are completely refutable because anyone can fake a transcript with a participant. Just make up two random values `a` and `b`, and use the participant's public key. Then go ahead and generate a transcript of encrypted messages, pretending to be both sides of the conversation. You don't even have to contact the participant to do so. So since anyone can do this, there is no way to prove that a specific participant engaged in the transcript.

The risk to anyone is nil if the list of participants becomes known. That list contains only public keys. Nothing can be gained from this knowledge. Store it in the clear, that's okay.

So where is the need for any passwords?

(The clever bit of shared keying and repudiable messaging was derived from comments by Trevor Perrin and Moxie Marlinspike of Signal Foundation. https://signal.org/blog/simplifying-otr-deniability/ )


-- 26 August 2022 -- Self Synchronizing TCP Framing
---
Many TCP protocols use either length prefixing or embedded delimiters for indicating message boundaries. Today we implemented a different, and possibly better, method - self-synchronizing encoding. TCP is a stream protocol, not a message protocol. So if the stream contains embedded messages, it is up to us to find them.

The problem with length prefixing is that a corrupted message could indicate an incorrect message size. As a result, the entire future stream of data will have lost sync lock. This is an easy target for DOS attacks. Sanity limits on message sizes across TCP should always be employed, but this doesn't help with loss of message boundary sync.

Self-sync encoding has some similarities with embedded delimiters. But rather than singling out a single byte value as the delimiter, ours borrows from ideas presented by Paul Khuong ( https://pvk.ca/Blog/2021/01/11/stuff-your-logs/ ), and uses the sync symbol pair #xFE #xFD as the delimiter. His is an efficient system using these delimiters and run-length encoding, and uses an encoder to avoid the appearance of any embedded start sequences in the body of the message. The protocol always begins new messages with the delimiter start sequence, and uses embedded length encoding with a CRC cross check. 

I wanted to see if this encoding would be suitable for TCP message handling. It turns out to be very successful, vastly reduces the complexity of the TCP message reader, and uses a simple state machine for encoding and decoding of messages.

In fact, once implemented on the reader side, the entirety of the TCP socket reader code could be eliminated. All we do is send incoming async message fragments to the decoder state machine and let it decide when it has seen a complete message. We don't have to worry about length sanity checking on the incoming fragments - they will have already been limited by the underlying TCP machinery. If a message becomes damaged enroute, we simply lose that particular message, and perhaps the following one, before we reestablish message boundary sync.

State machines can be readily managed with Actors code. But there are requirements of operation sequencing that must be overtly performed. Users of Call/Return programming systems already have this kind of operation sequencing since the next operation can only proceed after the current one returns. But Actors in a fully parallel concurrent system have to be forced to operate sequentially. It does not happen naturally. Actors can, and usually do, operate in a randomized order since we have multiple threads operating on the event message queue.

So while we can write an Actors based state machine, it is actually simpler (and operates faster) to just write it with Call/Return semantics. We need to operate in both worlds, since the natural way to write the TCP receive interaction is via message passing. But inside the message reciever we resort to Call/Return for the FSM to give us the natural sequencing of operations needed by the FSM.

(In a variation of conventional state machine programming, we want to recognize valid whole messages embedded in a stream. We must not look ahead by one symbol, or to an EOF indicator, to discern the end of a message. We need to know and act when a message is complete, without waiting to see what might come next. This is a stream decoder, where the stream has an unknown and indefinite future, and where a next symbol might take an arbitrarily long time to arrive, or perhaps never.)

But there is still one twist that must be managed - while the TCP machine always presents incoming packet fragments in chronological order, once those fragments are sent in messages to the Actors system, the order in which the fragments are handled can become arbitrary. In a self-synchronizing encoding the arrival order has importance. 

(Actors always respect the arrival order of messages in the event queue, but in a fully parallel concurrent Actors system, some later Actors may operate on new messages before earlier ones have finished. This can scramble the arrival order of fragments to the TCP reader queue - which is distinct from the event queue.

Here is the crux of the problem - in a lock-free parallel concurrent system, how do you move messages from one shared queue to another shared queue all while preserving the original order?)

So we call upon Actors to implement a proper order control on incoming packet fragments. As they are received by the underlying async TCP reader, each fragment is assigned a sequential serial number before being sent to the Actors system. On the Actors side the fragments are retrieved from an arrival queue in sequential order, or else the Actor pauses until it receives the next sequential packet fragment. This kind of sequencing logic is far easier to implement with Actors than with Call/Return programming.

(Call/Return would need something like CoRoutines to pull this off. An Actors machine is a natural coroutine machine, since the event handler is a kind of natural trampoline, even as we are operating in a Call/Return architecture. Trampolining is one method for performing coroutines and continuations.)

With Actors on a Call/Return architecture we actually have the best of both worlds in some sense. TCP self-sync encoding is a successful example of this.



-- 30 July 2022 -- Notes
---

The current framework has been updated and speeded up. SEND/Dispatch speed was measured to be 46ns on Intel i9 iMac (21.7M dispatches/s).

There is only one global event queue (a Mailbox) with a default of 8 threads each running the dispatch loop awaiting events. This number can be changed using global var NBR-POOL. It becomes too confusing to have multiple cooperating Sponsors. So now we use just one Sponsor = collection of Dispatch threads + Event Queue. External (non-Actor) threads send message events directly to the event queue. If you have many different I/O activities that could block waiting, then simply increase the size of the dispatch pool.

The current philosophy is to have each Actor behavior be an FPL pure function without side effects, so that multiple threads can be executing in parallel and concurrently within the same Actor. You maximize concurrency by having lots of little Actors sending messages to other little Actors. FPL purity is a responsibility of the Lisp programmer. Lisp allows you to abuse things to your heart's content, but lock-free parallel concurrency can only be safe with FPL pure code. We never need to use LOCK/UNLOCK in an Actors system, which also means we don't have to worry about deadlock.

All BECOME and SEND operations in an executing Actor behavior function are transactionally staged, queued up locally in the running dispatch thread, and committed, in total, at behavior exit. So all SENDS occur logically simultaneously. There is no meaning to their individual ordering and you should not assume any particular ordering, except to say that all messages sent from an Actor will be enqueued later (FIFO ordering) than all messages sent by the same Actor from an earlier invocation.

However, when an Actor performs BECOME, at exit only one thread running in parallel in the same Actor, will be able to commit its staged SENDs and the BECOME. That will mutate the behavior cell of the Actor and spill its sent messages into the global event queue. Any other parallel executions will fail their commits and be retried. (Hence the need for idempotent, FPL pure, behaviors) 

The committed BECOME is the only visible mutation in the system. All other changes to behavior parameters should happen atomically via fresh values supplied to the BECOME behavior generating function. You should never directly mutate any behavior parameter. 

E.g., you can pass a REMOVEd list argument to a behavior generating function, but you should never perform DELETE on that list, if that list is visible to any other parallel threads - i.e., a behavior parameter.

Sent messages are accumulated locally into a chain of message events, linked by next pointers. When a block of messages is committed into the event queue, the entire chain is placed on the event queue. The next available thread will dequeue that chain, peel off the first message for itself, and spill the remaining messages back into the event queue. As a speedup optimization, when an Actor has generated new messages, its current dispatch thread will peel off the first for itself and submit the remainder (if any) as a block to the event queue.

You cannot reliably predict which thread will dispatch any particular message sent to an Actor.

If an Actor exits abnormally then all of its staged BECOME and SENDs are discarded, and it is as though the errant message were never delivered. 

If an Actor cannot be safely executed in parallel, then you should serialize all accesses to that Actor using a SERIALIZER Actor acting as a gateway. But that also requires that the guarded Actor send a response message at some point to a customer Actor, where that customer was provided in the initial access message to the guarded Actor. The SERIALIZER gateway interposes itself as the customer during guarded Actor execution, and then forwards the response message back to the original customer Actor. A SERIALIZER also enqueues any additional accesses to the guarded Actor while that Actor is busy responding to a message. On reply, the SERIALIZER allows the next request to proceed.

Quite unlike the CALL / RETURN semantics of Lisp, an Actor system has no concept of dynamic scope. There is only ever one Actor deep, performing its behavior function against a message. There can be no UNWIND-PROTECT in a distributed Actor concurrent system. If you need alternate behavior depending on some outcome, then supply a success/failure pair of customers to the Actor. Or have the customer respond to a success/failure message.

There are two broad categories of messages sent to Actors. Many (most?) have a customer Actor argument to which additional messages or responses should be sent. By convention a customer is always the first argument of those messages. The other kind of message has no customer. It is simply a command of some type, and no response is expected. But unlike CALL/RETURN semantics, any responses are sent to a customer, and not back to the sender of the message.

To help out, we have the BETA macro which has a syntax much like MULTIPLE-VALUE-BIND. The BETA macro creates an anonymous continuation Actor (just like LAMBDA produces an anonymous function), and the name BETA can be used as a noun, in that form, referring to that anonymous Actor to designate a customer argument in a sent message.

```
(beta (arg1 arg2 ...)
      (send some-actor BETA more-args ...)
   (body-of-anonymous-actor-acting-on-message-args arg1 arg2 ...)
   ...)
```

This is the same as:

```
  (let ((anon (create (lambda (arg1 arg2 ...)
                         (body-of-anonymous-actor-acting-on-message-args arg1 arg2 ...)
                         ...))))
    (send some-actor ANON more-args ...))
```
Be cognizant that any and all bindings surrounding the use of the BETA macro act as behavior parameters, and should never be mutated. They become closed over in the functional closure produced by the LAMBDA form. This is as true for the original behavior parameters of the enclosing Actor as it is for any locally generated LET bindings, if those LET bindings can be viewed by more than one parallel concurrent thread.



-- Feb 8 2022 -- Reppy Channels in Actors!
--
File: xTActors/sync-msg.lisp

I find this totally amazing! I just did the essentials of Reppy Channels - composable synchronous rendezvous events -- in about a page of Actors code. Compare that with the original Lisp implememntation I did about 10 years ago - a mass of horribly complicated SMP code. Using Actors, which are completely FPL, parallel, lock-free, and asynchronous, and yet we can have composable synchronous events.

Imagine waiting for some I/O on one of several I/O ports. You don't know which one will trigger, but one of them will. And you need to cancel the wait on the remaining ports when that happens. Oh! And you would also like to limit your waits with a timeout. Easy peasy, with Reppy Channels:

```
(let ((ch1 (chan))
      (ch2 (chan))
      (ch3 (chan)))
  (sync (timeout-evt 2
                     (choose-evt (recv-evt ch1)
                                 (recv-evt ch2)
                                 (recv-evt ch3)))
        println)
  (sleep 1)
  (sync (timeout-evt 2
                     (choose-evt (send-evt ch3 3)
                                 (send-evt ch2 2)
                                 (send-evt ch1 1)))
         println))
```


--- Feb 2022 - Now for something new... Radul & Sussman Propagator Networks
---
Folder: xTActors/Propagators

Actors are perfect as a substrate for implementing R&S Propagator Networks. And this is an exciting new territory for programming. No longer bound by strict logic, you can write programs that provide justification for their decisions, even in the face of contradictory information - just like the real world. 

**(As a quick aside, work your way through Section 4 of the R&S Propagators paper, and see how trivial it is to perform multi-directional computations, as you find in Prolog and Erlang. No need to specify separate routines showing arity and direction of function arguments. Simply state Propagator legs in the various directions, changing which CELLS are inputs vs output, and when it becomes possible to compute something new, it just happens.)**

The real magic begins to happen in Section 6 of the paper, where it is shown a simple implementation for dealing with mutually inconsistent information.

The code in Folder xTActors/Propagators is my quick translation of Sussman's Scheme code into Lisp Actors code. It just works, and it is so simple to do. 

**[The latest incarnation of code has been vastly simplified using Lisp and Actors capabilities, and departs further from the R&S Scheme given in their paper. But the behavior is the same.]**

"Supported" values might offer yet another possible way to debug the complexity of large asynchronous Actor systems, by having data carry along its tag information, and showing the provenance of resulting computations.



--- Debugging Actors Code
---
While Actors radically simplify many things, debugging is a whole new universe, compared to what we are accustomed to with function call/return-style debugging. Totally!

I spent the better part of one full day trying to track down why my secure network connections were acting flaky - sometimes working, sometimes working several times in a row, till they stopped working. It all comes down to concurrency races.

We all have a pretty good idea of what a data race is. But this is another level beyond data races. In FPL pure code, there can be no data races, and I was careful the ensure that I wrote FPL pure code for my Actors bodies. But there is one overt, intentional data race possible in fully parallel code - the action of committing a BECOME on exit from the Actor body. But we already have that covered with CAS/retry.

But at a higher level, despite these precautions, and even when running in just one machine thread, it is still possible to have concurrency races, where the actions of several coordinated messages effect a mutation of state in the system. And unless you limit these actions to sections where concurrency is restricted, by using classical SERIALIZERs, you still run the risk of concurrency state races. And figuring out just where to restrict concurrency can be a challenge.

So when do you need a SERIALIZER? Certainly when you are addressing a shared resource, like a disk file. Only one logical thread of execution can be permitted to write to the file at any one time. A logical thread of execution is a serial chain of SEND's that will have an effect of mutating the state of the shared resource. That one is easy. Same too for shared I/O ports, as in the connections to my radios in the lab here.

In the past we often dedicated an entire system thread to a shared resource, just to be sure to restrict concurrency against that resource. With Actors code this is wasteful and unnecessary. But you still need to restrict concurrency when mucking with the shared state of the device or resource. And we do that by ensuring that all logical threads of execution reach that resource through a SERIALIZER. 

(One system thread can support an unlimited number of logical Actor threads. But Actor code doesn't describe itself in terms of logical threads. These logical threads just grow organically as a result of greater-than-one fanout with SENDs.)

But here is the tricky one... We have self-maintaining Actor-lists, a kind of list formed by a chain of Actors, each referencing the next Actor via their local state, typically called NEXT. A self-maintaining Actor-list can grow new nodes, usually done at the tail of the list, and they can also remove interior nodes through cooperation with the NEXT node, using PRUNE-SELF. 

Actor-lists are usually constructed using PRUNABLE-ALAMBDA for their Actor bodies. PRUNABLE-ALAMBDA is a convenience macro that defines PRUNE-SELF, provides a messaage handler to respond to the PRUNE message, and then inserts your other message pattern handlers. Each node in an Actor-list can have a different personality (behavior), which makes these Actor-lists really useful.

But the act of mutating an Actor-list, using PRUNE-SELF, is actually a short chain of message SENDs that effects the pruning, culminating with a final BECOME, which causes the pruned node to assume the behavior and state of the NEXT node, which excises the NEXT node as garbage to be GC'd. The fact that this pruning requires multiple SEND operations is the clue. 

**Any time a state mutation takes more than one coordinated message dispatch, that sequence of messages needs to be protected by a SERIALIZER to avoid concurrency state races.** 

It all seems so clear in hindsight, but believe me, it took a while for the reality to become so clear.

A SERIALIZER is an Actor which maintains a FIFO queue of messages sent to it. It takes the first message and forwards it to the Actor under its supervision. That Actor can do whatever it needs to do, send any number of messages, and no additional external messages will be sent to it from the SERIALIZER until that Actor replies back to the SERIALIZER through a provided TAG in customer argument position. 

So every pathway in the supervised Actor must culminate with a SEND back to that TAG, to enable the next message in the FIFO queue to proceed. Messages sent to a SERIALIZER should include a customer argument. The reply from the supervised Actor back to the SERIALIZER gets forwarded to the original customer by the SERIALIZER.

Debugging Actors code is challenging because for every delivered message to an Actor, there is no clue about who sent it. There is no call/return in Actor code. There is a kind of call/forward at play here. You tell the Actor where to send its results.

Everything in an Actor body execution can be considered Atomic from the viewpoint of the Actors system. An instruction cycle consists of a message dispatch, then everything performed in the Actor body, and culminating in committing of SENDS and BECOME. It's when you have a chain of coordinated messages that you break atomicity in the Actor system. And that's precisely when you need to control concurrency. 

Concurrency can still be running at full speed elsewhere in the system, but along the logical thread under supervision by a SERIALIZER, there is only one concurrent thread of activity. Of course, the supervised Actor can spawn a whole slew of concurrent actions on its own. But no outside messages will be permitted until the Actor chain is ready, as signaled by replying to the SERIALIZER TAG.

For debugging, you can insert printout messages in various places. But those messages pop out in peculiar order. There seems no underlying causality if that is all you see. We have ATRACE to watch every SEND, but those are not yet committed. ATRACE messages do show when and from whom a message has been sent. But it is often too much information. It takes a lot of patience to sift through a stack of ATRACE log messages. 

There really is no definite concept of a thread of execution that could be watched. Logical threads of execution arise merely as a consequence of an Actor doing multiple SENDs. Each one of those SENDs spawns a new logical thread of execution.

In a whole Actors system, even something as "simple" as the secure networking interface, produces a blizzard of logical threads. Pages and pages of ATRACE log messages arise from a single round-trip transaction. Thankfully, once you understand the rules of concurrency and full-blown asynchrony, it is really simple to write Actors systems that perform well, have amazing amounts of concurrency, enjoy full-on parallel execution with no system-thread management needed, no locking, and provide features that would boggle the mind if you had to use Call/Return programming.

Just as an aside, the Lispworks system has a distant cousin of Actors already in place. They call them Actions and Action Lists.


--- A Mental Model of an Ideal Actor Machine
---
It can be helpful to keep in mind, as you write Actors code, a mental picture of the ideal Actor machine model. In this machine there are only 3 operations: CREATE, SEND, BECOME, plus Actor activation from message events carrying arguments.

The machine, unlike a typical CPU, takes its instruction stream from a FIFO queue, not linear memory. So the PC (program counter) is not used here. The FIFO queue is affected by SEND operations becoming committed. An "Instruction Cycle" consists of sending the next message to an Actor and running the Actor body. 

There is no CALL/RETURN, except possibly at the microcode level. Actors can be told to send their result messages to some other Actor, e.g., CALL/FWD. Within an Actor we do have pattern matching, variable binding, conditional execution (IF), and basic blocks of instructions. But even these could be synthesized from a pure Actor machine sending messages to micro-Actors.

Within an Actor body, all operations within a program block (e.g., LET, PROGN, COND clauses) effectively execute in parallel. All happen at the same logical instant of time. The code is FPL pure, and so ordering of operations within program blocks is irrelevant. There is no SETF operation. Variables are bound just once and are foreverafter immutable.

The three operations (CREATE, SEND, BECOME) are staged for commit at the successful end of the Actor execution. On commit, SEND's add their messages to the FIFO queue, BECOME mutates the Actor behavior *(this is the sole mutation in the entire system)*, and CREATE reifies new Actors. But unless you tell someone about the newly CREATEd Actors, by SENDing a message containing a reference to them - either as SEND target or as message argument, or use them as arguments to a BECOME, they will simply vanish and become collected by the GC.

Failed Actor execution causes staged operations to be rolled back. On rollback, no observable change will have occurred to the Actor - the SENDs will be cancelled, the BECOMEs also cancelled, and the CREATEs cannot be seen by anything external to the Actor. The garbage collector will reclaim the messages constructed by SENDs, and Actors constructed from CREATEs. It will be as though the errant message were never delivered.

Notice that there is no mention of threads. No such concept is needed. And you cannot depend on specific ordering of message delivery. In some cases, messages might never be delivered - think about the behavior of real networks as the medium for message delivery. And you have to take active measures to protect order sensitive clusters of Actors against concurrent activities. That requires a different way of thinking about coding.

Such a machine is very unlike what we are accustomed to today. Perhaps someday such a machine will exist in Silicon. Today we have to emulate its behavior using lambda calculus. And of course, in the underlying microcode of the emulator you find plenty of use for SETF and mutation, and machine threads. We do have operation ordering. Not everything happens at the same physical instant of time. But externally, these are not visible.

It might well happen that Lisp is the ideal implementation language for Actor machine emulators. Actors are untyped. BECOME requires the formation of functional closures which get stored in the behavior slot of Actors. Messages are arbitrary lists or tuples of arguments, as are state vectors. 

Certainly, micro-ops in the Actor bodies can be typed with respect to their required arguments, or dynamically typed as with Lisp. CALL/RETURN is useful in the Actor bodies for performing assembly of new messages and state vectors. But the overriding important capability is that of allowing arbitrary lists of arbitrary items, and forming functional closures on demand - lambda forms in Lisp, capturing whatever free variables are needed in the lambda form body.

As I look around at the new iterations of languages: C++, Swift, Rust - none of these implement functional closures to the level of completeness, nor as succinctly as what you find in Lisp. There always seem to be a list of gotchas about using closures in those languges. There never is in Lisp. And you have to actively defeat the typing mechanisms in those languages to produce messages and state vectors.


--- Important References
---
Carl Hewitt (2012 ?) writing about the essential characteristics of what I refer to as "Classical Actors":

https://arxiv.org/pdf/1008.1459.pdf

These are *not* the kinds of "Actors" you find in my earliest works, nor in Swift, nor in just about every other extant system called "Actors". It is Hewitt's contention that Actors form a more universal description of computing than lambda calculus, simply because they (Actors concurrency) represent an instance of a computational system that cannot be described by lambda calculus. 

My own experience with Classical Actors is that they allow for breathtaking simplification of complex systems: an example is the remote radio control system in my laboratory here, and also in the asynchronous secure network (socket) interface distributed in the code found in folder xTActors.

Dale Schumacher's website is an important elucidation of the concepts discussed in Carl Hewitt's paper above:

http://www.dalnefre.com/wp/

Dale, mentioned in Hewitt's paper, is one of many collaborators with Hewitt in helping to refine the meaning of Actors computation.

Microsoft Research has been actively pursuing Actors, in particular their framework called Orleans:

https://docs.microsoft.com/en-us/shows/reactor/an-introduction-to-orleans

https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/Orleans-MSR-TR-2014-41.pdf

While Orleans shows clear heritage from the Actors world, it also shows distinctly OOPL design, and clearly does not abide by the notion of FPL purity inside the Actors. I can't say what their SEND/BECOME behavior is, whether transactional or not. But it appears much less fluid than my Classical Actors whose existence can range from single-message ephemeral, to permanent, and which never have a rigid OOPL Class structure. Classical Actors have no "Methods". They simply respond to message patterns, or not. There is no need on the part of Senders to know anything at all about what the recipient Actor looks like, nor in many instances what its purpose is. The recipient is often just some Customer to which an Actor has been directed to send its computed results, based on whatever message it is handling.

--- New Ideas on Classical Actors (2022/01/06)
---
Since the benefits of FPL pure Actors are so great, there is really no reason to avoid running Actors entirely in parallel. But there is still one race condition that can arise because ultimately the effect of BECOME is to mutate the behavior slot of an Actor. Now we can avoid that race condition entirely by demanding that Actors can run in only one thread at a time. Or else, we can adopt some of the ideas from Lock-Free programming, and enjoy the benefits of maximum parallel execution.

In Lock-Free, it is commonplace to use CAS to effect an atomic mutation. And when another thread wants to do the same thing at the same time, you can use a claim against the cell. When other threads see that claim, they can use it to help achieve your original goal, and then they can turn around and do their mutation. But that comes at relatively high performance cost. 

At the start of message dispatch, you have to construct a claim object that contains the old value of the behavior slot, and a function that may try to mutate that slot - the entire Actor body which might contain a BECOME. The claim is then inserted into the slot using a CAS spin loop with the aforementioned help-along algorithm. Every instance of mutation takes two CAS operations - one for the claim, and one for the mutation value.

This is only really needed if you are going to perform a BECOME, but we can't know from the dispatcher whether or not this will happen. And so it has to do this on every message dispatch. That is simply too much overhead on every message dispatch. And the probability of simultaneous mutating executions of Actors is much too low to justify this overhead.

So instead, the idea is that we go ahead and just try a direct mutation with CAS when committing BECOME. If that doesn't work, then simply retry the Actor at that point. Chances are, this will use a different execution path in the Actor, second time around, and that may lead to no BECOME operations. 

Because we stage SEND and BECOME for commit, and so they can be rolled back, and because we are committed to FPL pure Actor bodies, there is no harm in parallel execution, provided we use atomic mutation with execution retry on a failed mutation. But Lisp doesn't automatically enforce the use of FPL pure programming. That is up to us to be on our guard as we write code.

The speed of message dispatch has a significant impact on our Actors. Our Actors benefit from inidividually doing small jobs, and gluing together a lot of these little Actors to effect useful ensemble behaviors. This is also how you maximize concurrency, regardless of parallelism. So the runtime of many (most?) Actors will be very short, and possibly comparable to the duration of the message dispatch. Hence message dispatch sets an upper limit on how fast our Actor system can be.

A simple measuring tool that I built shows that on my Intel i7 and i9 iMac machines, I'm reaching a max rate of message dispatch around 20M/sec (50ns duration). The new M1 iMac shows a rate of 6-9 M/sec (110ns - 150ns), and it varies more strongly with the mix of background jobs in the iMac. These are single-thread timings. In multi-thread situations, which are far more commonplace, the two sets of machine architectures converge toward 250ns-300ns, or 3-4 M/sec.


-- A Commentary on Progress for the Actors Project
----
When I began the Actors project several years ago, my hope was to find an organizing principle and a collection of widgets that could enable one to more easily write complex real-world programs. I think I am now finally achieving that goal. 

My first cut was very much like the "Actors" you now find in other languages like Swift, or any which offer ASYNC/AWAIT. It was an organizing principle, but just barely simplified anything. Instead you end up with a tower of new complexity, CPS callback management, and the need for ASYNC/AWAIT-like behavior in the system. I look back on that now as a sad misunderstanding of what Hewitt originally intended. And I currently think other languages are going off the same deep end that I originally did. This is not to be applauded.

I think this tendency might be a consequence of the OOPL style of programming that you find in C++. There, one is expected to decompose the problem into Classes that provide a host of services. And complexity eventually builds up, services expand, and you end up with whole menus of features in primary Classes that rival the menus found in Chinese restaurants.

Along the way I discovered information provided by one of Hewitt's original collaborators, and he taught me a whole different approach, far simpler, no tower of new complexity, but requires a different mindset than I began with. Dale Shumacher (http://www.dalnefre.com/wp/) showed me that Actors should be tiny, intelligent little widgets, that each individually perform useful functions, but in combination can provide huge capabilities that others can only envy. Concurrency is maximized beyond your wildest dreams. It becomes possible, in a single thread of execution, to do things you always thought were absolutely use cases for multiprocessing, and perform at unbelievable speeds.

That's where we are today. There is never any need for ASYNC/AWAIT. Actors are always asynchronous widgets. Actors never need to wait on other Actors. Messages are not like Call/Return from imperative languages. Messages tell some Actor to activate itself with some provided arguments, and to send its results to some other Actor, usually not back to the SENDER. 

Actors can mutate their behavior based on updated state, but state is never mutated. Mutated behavior never changes the identity of an Actor. Functional purity pays off handsomely, and SEND/BECOME are transactional. In the event of any errors in an Actor execution, its scheduled SENDs and BECOMEs are rolled back. As long as the Actor body code is FPL pure, an exception looks like the message was never delivered in the first place.

This is radical simplification of complex problems. You solve the bigger problem using incremental small behaviors that can be combined in creative ways to effect the solution to the larger problem. Each little behavior is easy to test and verify that it is correct. Complex behaviors can hide behind public gateways that offer simple API's, and appear to the user as a unit. These units can themselves be combined into larger units, each of which were incrementally built up and tested along the way.


-- Lisp-Actors (xTActors) - Classical Actors (4 Jan 2022)
----------
Finally, a justifiable use-case for Sponsored Actors...

We now have 3 substrates for running Actors. The first one is the most general, and simply spawns a bunch of executive threads in a pool, each running a message dispatch loop, and each watching a communal mailbox for new messages. SEND sends new messages to the communal mailbox, whether messages originate from foreign threads or from local message processing.  

This default substrate offers maximum parallelism. Concurrency is assured, in any substrate, because our Actors use small building blocks, and messages from Actors are multiplexed from a FIFO queue, which offers breadth-first computation no matter the degree of parallelism.

For this substrate, the ARM M1 just slightly outperforms the substrate running on Intel i7 (1.04x faster) under best circumstances. But M1 on Apple seems more prone to background workload modulation, and can vary by 2:1 (SEND/dispatch timing of 250ns to 440ns) under typical situations. Intel i7 seems to vary from 260ns to 330ns for the same kinds of system workload variations.

The second one is an overt single-threaded substrate, which runs all its Actors using a fast thread-local event queue. It is very fast, almost 5x faster than the general pool substrate on Intel processors (2x on ARM). _*(Timing for SEND/dispatch on Intel i7 is 50ns, and 105ns on ARM M1.)*_ It offers the same degree of concurrency, but no parallelism if using only one instance of the substrate. 

This is most useful when you know that you have an Actor workload that is delimited and mostly 1-1 fanout of new messages. Examples would be processing pipelines (systolic arrays of Actors), and systems which don't rely heavily on outside asynchronous events, such as I/O ports, user interaction, and timer callbacks.

But even though this is a single-thread substrate, you can have as many running as you like, in different threads. They are triggered by either calling STSEND (single-thread SEND), or by wrapping your code with (WITH-SINGLE-THREAD &body body). In the wrapped situation, it dynamically rebinds SEND so that it becomes a substrate running the message and all subsequent messages arising from this processing. It runs until no more messages await, then returns to the SEND caller.

Finally we have Sponsored Actors - which are perfect for dedicated I/O port processing. In such case, we spend a lot of time, of indefinite duration, blocked and waiting for activity to arise on the I/O port. Since Actors can only run on one thread at a time, any Actor tied up waiting for I/O will block its use from other threads, causing them to enter weak spin-livelock in the worst case.

By isolating these I/O ports to their own Sponsors (threads) any message sent to the Sponsor is just a quick mailbox send. This frees up other threads who no longer have to wait in a spin-loop for the Actor (the Sponsor is also an Actor) to become available.

Sponsored Actors have runtime dispatch performance almost equal to the high-performance of single-thread substrates.

In every case, the same Actors are used. There is no such thing as a substrate-specific Actor. Even Sponsors, with that priveleged name, are nothing more than Actors of the same variety as any other Actor. Sponsors just happen to know the maibox and thread identity of a dispatching thread. And they know how to inject messages into that mailbox so the thread can take it from there. But Actors are simply code. Substrates are how they become executed.

The default substrate is the multi-threaded pool of dispatching threads. And in a pinch, it can always be used where the others might achieve better overall performance and efficiency. But when you know you are facing particular use cases, the other two substrates may perform better, and / or waste fewer CPU cycles.

Since Actors are just code, they have little notion of what thread they run on. And when they SEND additional messages, those messages are handled by the same substrate. Only a send to a Sponsor will cause explicit thread switching. In the default SMP pool substrate, Actors run on any and all of the substrate pool threads. In the single-thread and Sponsored substrates all messages are handled by the same dispatch thread.

When messsages are sent from foreign (non-Actor) threads, and from asynchronous timer and I/O callback routines, those messages are always first directed to the SMP pool dispatchers. From there they may go on to Sponsored Actors, or not. Foreign threads can never directly send to a single-thread substrate. 

But note that the same Actor can run on any thread. And this offers a back-channel between threads. A message can carry information to an Actor currently running in a single-thread substrate, while the same Actor can later be queried by ASK from a foreign non-Actor thread, and reveal that information. This could be good or bad, depending on your stance toward system security. Just something to be aware of...


-- Lisp-Actors (xTActors) - Classical Actors (2 Jan 2022)
----------

Major breakthrough - dispensing entirely with the concept of Sponsors. There are simply a number of threads in a pool, each running the message dispatch loop. All are feeding off of a central mailbox as *the* event queue. I have 4 such threads, chosen ad-hoc to match the most common number of CPU cores. But this isn't necessarily the right answer. More investigations to follow. But the notion of performing an Actor in a given Sponsor thread is now obviated. Actors just run, on whatever thread can run them.

At any rate, the speed remains very good, no measurable performance degradation. In part that is because of several considerations:
1. Most Actors SEND at least one message
2. Most Actors in a dynamic trace SEND only one message
3. We optimistically cache the SENDs pending successful return from an Actor. At that time, we can consider the SENDs as having been accomplished. They reside in a thread-local event queue.
4. When we cycle the dispatch loop looking for the next event, I pop one from the local event queue. But I also pop all remaining events and send those others over to the centrail mailbox event queue to give other threads a crack at them.
5. The thread dispatches on that first event for itself.

So, for the most part, the dispatch loop feeds from its own local event queue, until it runs dry, and then it waits on the central mailbox event queue.
The result has nearly the same performance characteristics as for when we used dedicatd Sponsor threads. But now the degree of parallelism is significantly higher. An Actor runs on whatever thread is available. 

_**[HOWEVER:**_

**_Performing your own first message can be shown to lead to potentially unlimited growth of the central message queue. This is tantamount to a depth-first execution path of potentially unlimited length, and where extra messages generated along the way will never be tended to. Bad news!!_**

**_The only controlled (and fair) approach is to dump all SEND messages into the central mailbox queue. That means that new messages are always at the tail of the queue, and any extra messages generated along the way will be incrementally handled. This becomes a breadth-first execution strategy, and will eventually drain the event queue, provided that we are dealing with a bounded execution graph. Bounded execution = NOT a fork-bomb, i.e., a bounded message fanout._** 

**_Sadly, in the face of SMP this implies a severe slowdown compared to single-thread execution. But frankly, the benefits of SMP outweigh the speed advantage in my mind, and the only way to incorporate fast, controlled SMP, is to reinvent Sponsors. I'm tired of Sponsors and I'd rather have parallelism and no bother in programming with Sponsors. Just my 2c...]_**

**So, what's the bloody problem with Sponsors? The problem is that Actors encapsulate functional closures. Sponsors are threads of execution. Code vs Threads - different dimensions of computing. The two have nothing in common. And once an Actor begins executing on a Sponsor thread, all its SENDs produce messages also destined for the same Sponsor. So you can easily construct a boundary Actor that will transfer messages on to some specified Sponsor thread. But how do you get back to the original sender's Sponsor? How do you decide if you even need to? Actor messages carry no indication of who sent the message. So you, the programmer, have to assume all the responsibility. Much like C-based manual memory management, there seems no best uniform strategy. That becomes fraught with uncertainty and that becomes tiresome.**

The only impediment to full parallel execution is the fact that Actors can only run on one thread at any moment. If two or more threads want to run the same Actor, only one of them succeeds. The others return the event to the tail of the central mailbox queue and look for another event to run.

So this is now a tuned variant of the very first original Actors system. It is tuned because of the local dynamic runtime characteristics matching a local thread event queue most of the time. Actors do not have mailboxes. Actors never block waiting on each other. The only time an Actor blocks is if some function it calls ends up blocking, as for I/O. If an Actor is blocked, there is a good likelihood that another dispatch thread is ready to run other Actors awaiting execution.

Actors can be logically blocked, which happens when they are a customer and no message has been sent to it, pending some other outcome. The customer Actor is logically blocked at that point, but not physically blocked. The concepts of AWAIT and ASYNC, as offered in other languages recently, seem unnecessary. There is never any need for an Actor to AWAIT. And Actors are inherently *always* ASYNC. 

Actors are inherently thread-safe, and can be programmed as though the system is entirely single-threaded. However, there are major benefits to transactional SEND and BECOME, and adhering to FPL principles, rather than using imperative mutation against local state. If local state needs to evolve, which it frequently does, then simply use functionally pure augmentation of state and make the new state an argument in a BECOME. The behavior and internal state of the Actor changes on successful completion, but the identity of the Actor never changes. If anything goes wrong in the Actor body, then the SENDs and BECOMEs are rolled back, and it is as though the errant message was never delivered.

So how many threads to place in the dispatch thread pool? Four is a good start. There may be good reason, due to I/O blocking activity to have more threads than physical CPU cores. OTOH, maybe 4 threads already reaches the point of diminishing returns? TBD...



-- Lisp-Actors (xTActors) - Classical Actors (Jan 2022)
----------

Actors are now completely thread-safe, and can perform with identical behavior on any Sponsor thread, but run on only one Sponsor at any moment. Sponsors are responsible for running the message dispatch loop and for setting up the Actor context before invoking its behavior code on messages. An Actor runs in a given Sponsor by virtue of a message arriving in the Sponsor event queue which specifies the Actor. The only possible distinction between an Actor running on different Sponsor threads is the queue response time. 

The base-sponsor is envisioned as the primary Sponsor thread. The slow-sponsor, which is identical to base-sponosor in all respects, was planned as the thread in which blocking I/O activities might be directed - hence the name "slow-sponsor". When you send a message from outside of the Actor framework, as from a foreign thread, or the REPL, that message is delivered to base-sponsor. By having two running Sponsors, muticore machines can potentially run these activities in parallel. But that is really up to the OS thread scheduler.

I bit the bullet after many variations, and after tiring of the constant need to be aware of the Sponsor thread. I use an ATOMIC-EXCHANGE in the run loop to simutaneously grab the Actor behavior address and null it out. Any other thread seeing that null behavior pointer knows to just put the message back in its event queue and go around for a later retry. The behavior pointer is restored on return, either by successful exit or by error, from the Actor. The runtime overhead of the ATOMIC-EXCHANGE isn't all that bad. And now we never need to concern ourselves with what Sponsor we are running on.

You should still adhere to the discipline of FPL. Even though we are thread safe, if you write imperative mutation code against your Actor parameters, instead of using BECOME, you violate the principle that upon any error the system behaves as though the message was never received. SENDs and BECOMEs are always backed out on error, but your imperative mutations would not be. If you write FPL code, then the only possible mutation in the system is the Actor behavior pointer that gets committed by the Sponsor when you return successfully, after having performed a BECOME.

The go-around behavior on seeing a null behavior pointer, potentially opens us up to spin-live-lock, where a long running Actor hogs the thread and other threads that want to run the Actor keep spinning, waiting for a non-null behavior pointer. This isn't quite as bad as spin-live-lock, since other messages that arrive can be handled during the go-around. You can alleviate this possibility by keeping Actor behaviors very short. If your Actor needs a significant computation, you can fling that off into a new ephemeral Actor, with its own copies of the parameters. If you, instead, tried using a continuation Actor via BETA, you would be sharing state between two different Actors, both of which could be executing in parallel. Once again, as long as you never mutate your parameters, that would be fine too.

In publicly deployed systems, as long as you only permit the operation of SEND, from outside of the Actor substrate, it becomes impossible for malicious clients to hose the system. They can only SEND to services whose Actor identity is made known to them. They cannot cripple the system by sending a message to arbitrary addresses. And Actors simply ignore ill-formed messages, and are incapable of providing feedback to the sender.



-- Lisp-Actors - Classical Actors (Bare Essentials in xTActors folder - Nov 2021)
--------------

What happens if we get rid of all the crud from our previous "2nd System Syndrome" and cut everything back to essentials? 

That's what we have in folder xTActors. Everything is an Actor, including Sponsors. No subtypes of Actors. An Actor is simply an encapsulated functional closure - called its Behavior - with code and state data. While the Behavior of an Actor can change by its calling BECOME, the Actor itself has unchanging identity. Once you have the address of an Actor, that will never change. 

Sponsors are ordinary Actors whose state consists of a mailbox and a thread that runs the RUN event dispatch loop. The thread shares that same mailbox. Any messages sent to a Sponsor Actor are stuffed into that mailbox and retrieved by its RUN loop. That allows us to have cross-Sponsor SEND without any extra baggage involving Actor subtypes.

Actors can be tied to a specific Sponsor by using `(IN-SPONSOR <sponsor> <actor>)` which creates another ordinary Actor. Anything sent to this new Actor gets shipped over to the indicated Sponsor before forwarding the message to the Actor. Actors, in themselves, have no notion of threads. They are just simple encapsulated functional closures. They can be executed on any thread.
  
In a multi-threaded environment, some Actors must not be permitted to execute in parallel on more than one thread. This is so when an Actor might execute a BECOME to mutate its behavior. Race conditions would result if multiple threads ran the Actor code at the same time. The easy solution to this is to simply use IN-SPONSOR, giving us an Actor tied to one agreed upon specific Sponsor. Operator `(PAR-SAFE <actor>)` does just that. Within each Sponsor you have a single thread of execution. Multiple threads wanting to execute the Actor will have their SENDS queued up in the event queue of that Sponsor, and given serialized access to the Actor code.

As before, SEND and BECOME are transactional, taking effect only at the successful exit of Actor code. Newly created Actors are hidden from the outside world until you send them to someone. If an uncaught error occurs in the body of executing Actor code, all SENDS and BECOMES since entry to the code will be discarded, and it will be as though the message that produced the error was never delivered. Functionally pure Actors. If an Actor needs to mutate its state, it is best to simply use BECOME to generate fresh behavior. The only mutated item is the behavior pointer inside the Actor encapsulation.

It is refreshing to chop away needless complexity...

Notes from the field:
---------------------

I have an application for these Actors in the lab, controlling lab equipment from a remote workstation. They work exceedingly well, with huge simplification to the logic of the program. And that field work has shown some lessons along the way.

One challenge with Actor concurrency: no guarantees on message delivery ordering, except that an Actor won't respond to a message before it has been sent. Your code must not make any assumptions about the order of arrival for related messages. We are generally unaccustomed to programming in such conditions, coming from imperative function oriented coding. This is different.

Probably the most important lesson has been the complexity, even here with Actors, provided by SMP multithreaded environments. In Actors we forego using Locks. There are still some locks implicit in the mailboxes used to converse across sponsor boundaries. But mostly we rely on the sponsor event queues to prevent parallel access to critical code. 

Each sponsor is a single thread, and so any code executing in a sponsor is also single threaded and thread-safe - provided only one copy of that code can be running in one sponsor at a time. Actors run to completion before allowing another message to be handled by the RUN loop. All SENDs and BECOMEs are deferred transactional actions that take effect at the successful exit of the Actor code.

But code is code, and it runs just fine on any thread, even in parallel (even simultaneously, with SMP). To prevent critical code from running in more than one sponsor, we can arrange that some particular behavior code is only allowed to run in one specific sponsor thread. If the code detects that the current thread belongs to a different sponsor, then it re-sends the message to itself in the desired sponsor and exits immediately. That queues up the message along with all other possible contenders in the chosen sponsor's event queue. Each event takes its turn in the code. We have concurrency, but not parallelism.

We have IN-SPONSOR, PAR-SAFE, and IO wrappers that do this sponsor switching ahead of running an Actor. But these solutions are often too coarse, and it becomes confusing to reason about which sponsor a particular Actor will be running on, especially when these are nested actions. A PAR-SAFE wrapping an Actor that has been wrapped by IN-SPONSOR, etc. The chains become potentially endless, occupies needless runtime with all the sponsor-switching re-sends, and SELF only refers to the final Actor actually running. It does not refer to any outermost IN-SPONSOR wrapper Actor, and becomes unsafe to hand out in message SEND, without rewrapping it first. It becomes very confusing to reason about.

Furthermore, there is nothing wrong with allowing some non-mutating sections of Actor code to run in arbitrary threads, and in parallel. And so it may be too severe to force the sponsor switching for all sections of the code. That really only needs to happen in sections that may be induced to perform a BECOME operation. Only those sections can lead to race conditions if allowed to run in parallel across multiple threads. (Assuming you are being good about writing FPL code)

The solution I finally chose, was to use WITH-SPONSOR in those critical message handlers of Actor behavior code, ahead of any code that performs BECOME. That macro tells the system which Sponsor the following critical code needs to be running on. If the code is being executed in a different sponsor then we re-send the message to ourself on the desired sponsor, and then exit immediately. By default it uses BASE-SPONSOR, but applications can specify which sponsor if they want to.

But there is a second possible solution, which might be simpler, so long as one abides by conventions. That is to consider that Actors are inherently single threaded. So program them that way entirely, and assume that they run in just one sponsor. This will generally be in the BASE-SPONSOR, unless wrapped with IN-SPONSOR. When you have an Actor that will be directed to run on another sponser, via IN-SPONSOR, wrap that Actor further with IOREQ, which arranges to send messages to the Actor with a customer argument likewise wrapped by IN-SPONSOR using the sender's own sponser. That way results are sent on to the customer in the original sender's sponsor, which will generally be the single thread of BASE-SPONSOR.

In practice both of these solutons work beautifully well. Gone is the confusion caused by nested IN-SPONSOR wrappers. For the solution using WITH-SPONSOR inside the message handlers, this method makes the Actor inherently capable of running properly in a multi-threaded system with lots of traffic going cross-sponsor. There is no question about which Actor the SELF refers to now. And no need to use PAR-SAFE, nor worry about whether we should. It becomes always safe to hand out the SELF to other actors via SEND. The Actor code, via WITH-SPONSOR, specifies exactly what needs to happen for just that section of code.

None of this would be necessary in a machine with only a single thread of execution. But multi-threaded applications, and especially SMP, pose a much higher level of complexity. I thought Actors would fix this, but it ends up being its own kind of complexity. I think the WITH-SPONSOR, and being careful to write FPL pure code, makes things about as simple as can be.

---

Fully multithread-capable example: from a database handler during write locking. While undergoing write modification (FPL style) the database remains intact and available for readers. Additional writers must be enqueued for later execution, after the current writer has finished. 

So the handler behavior code has selective use of WITH-SPONSOR, allowing readers to proceed in parallel, without any sponsor switching. Since the database is updated in one step, all update modifications from the writer are instantiated fully at once. No readers ever need to be blocked.

The underlying database uses a purely functional RB-Tree to store key/value pairs. So writers can freely and incrementally update the tree, while readers use the original intact.

The code can be called from multiple simultaneous sponsors. When critical updates are necessary we simply ensure that we are running in the single thread of BASE-SPONSOR.

```
(def-beh locked-db-beh (writer state sync pend-wr)
  (with-accessors ((kv-map  kv-state-map)) state
    (alambda
     
     ((cust :read queryfn)
      (with-worker
        (send cust (funcall queryfn kv-map) )))
      
     ((cust :write updatefn)
      (with-sponsor ()
        (become (locked-db-beh writer state sync
                               (addq pend-wr
                                     (cons cust updatefn) )))))
      
      ((cust :update new-map wr-cust) when (eq cust writer)
       (with-sponsor ()
         (let ((unchanged (eq kv-map new-map)))
           (send wr-cust self (not unchanged))
           (let ((new-state (if unchanged
                                state
                              (let ((new-state (copy-state-with state
                                                                :map new-map
                                                                :ver (new-ver))))
                                (send sync self :update new-state)
                                new-state))))
             (if (emptyq? pend-wr)
                 (become (kv-database-beh new-state sync))
               (multiple-value-bind (pair new-queue)
                   (popq pend-wr)
                 (destructuring-bind (new-cust . new-updatefn) pair
                   (let ((new-writer (make-writer new-cust new-updatefn new-map)))
                     (send new-writer (once self))
                     (become (locked-db-beh new-writer new-state sync new-queue))
                     )))
               )))))
      )))
```      
      
---------------

-- Lisp-Actors - Classical Actors (in TActors folder - Earlier in 2021) --
-------------
You will become amazed at this, but Classical Actors can run in a single thread, and yet, accomplish what we had all thought was a necessary use case for multi-threaded code. Classical Actors prove the contrary.

A Classical Actor has these features:

* Immutability, except for BECOME. The only way to effect a change of state for an Actor is to construct fresh state from a copy of existing state, and also possibly change its behavior. Actor identity remains immutable.

* Three fundamental operations: 
-   BECOME - change its state and/or behavior 
-   SEND - send a message to another Actor 
-   CREATE - construct a new Actor with some initial state and behavior

* All effects are validated within Actor bodies, but no visible change occurs until the Actor body finishes. It is transactional. Any errors revert the actions scheduled at exit. That means the externally visible effects of an Actor: CREATE, BECOME, and SEND, are delayed until Actor body exit.

The single-threaded implementation means that it is impossible for reentrant code to be simultaneously executed in parallel. There is no need for locking and taking measures to protect state against SMP parallel execution.

First of all, the only mutation permitted is via BECOME. All other state is expected to be immutable, and hence sharable. And single-thread implementation means that parallel execution and the race to BECOME never happens.

Actor behaviors are functional closures, which captures both the function of its behavior and its local state data.

SEND produces events, which are the combination of an Actor reference and a message containing data. These events are added to an event queue for later dispatch by the central RUN mechanism. RUN can be very fast, simply taking the next available event and applying the Actor behavior (a functional closure) to the message data. After the Actor returns to the RUN loop, its visible effects are reified. If an error occurs inside the Actor execution, this reification step is skipped.

Actor identities represent security capabilities. An Actor can become known to the wider system only by four mechanisms:

* An Actor knows its own identity as SELF.

* An Actor state may be constructed with references to other Actors.

* A SEND can include the identity of other Actors.

* An Actor that CREATES a new Actor knows the identity of the new Actor.

By being careful about disclosing the identity of newly constructed Actors, you can ensure safety and privacy. The only way to SEND a message to an Actor is to know its identity.

In a Classical Actor machine, nothing more than these capabilities will be present. In a Lisp simulation of an Actor machine, you do have access to mechanisms that could break these guidelines. So it becomes necessary to exert discipline in programming Actor bodies in Lisp.

Take care to avoid making visible system-wide state changes except through the mechanisms of CREATE, SEND, and BECOME. Do not mutate your local state, but rather, make a copy of it and BECOME a new behavior with new state. Your Actor identity will not change.

Try to live within the transactional boundaries by using retractable actions. BECOME and SEND are already retractable. CREATE does not become visible to the rest of the system until you SEND a message to the newly created Actor.

Because of the temporal separation of Actor closure executions, due to the event queue for SEND, it becomes completely painless to write code in a continuation-Actor style.

Actor behavior functions frequently accept a customer Actor argument, either as part of its local state, or supplied in a message, to which they can SEND message results. Those customers can be continuation Actors. They will never be called directly, but rather have to await SEND event delivery from the RUN mechanism.

Lacking a customer Actor, there is no other way for an Actor to return a result.

There is an initial collection of very useful Actor widgets. These building blocks can be connected to each other and your own Actors to effect very elaborate, safe, and secure subsystems of performant code.

Entire collections of interconnected Actor graphs can present a gateway Actor as its sole API. What happens behind that gateway Actor is of no concern to customer Actors. To them, it simply appears to be a single Actor providing some useful service.

A proper design style for Classical Actors is to avoid dumping huge state and capabilities all into one single Actor. Instead, a broad range of services can be provided better by separating actions into individual component Actors in a graph of interconnected Actors.

Doing this also keeps the system lively, and permits completely unrelated Actors to operate on other tasks, concurrently with the actions of an Actor service collection.

This last aspect of a many-component Actor system, coupled with the temporal separation of Actor continuations via the SEND event queue, enables a single-threaded implementation to provide the magic once thought only possible with multi-threaded systems.

As an example: the code in the TActors folder includes a fully performant TCP/IP Async Socket system for talking to Actor systems on other machines. You reach Actors on other machines by knowing their directory name, and the IP address of their machine. Just provide a USTI to your own Actors on your machine, or a PROXY address to Actors on other machines. Actor customers on your machine can receive message replies via SEND from Actors on the other machine, and vice versa.

For more insight into the use of Classical Actors, I highly recommend Dale Schumacher's BLOG pages at: http://www.dalnefre.com Dale has been working deeply with Actors for decades, beginning with Hewitt, and continuing in a very fruitful direction. 

In his Blog, he describes a virtual pure Actor machine, and his blog, entitled "It's Actors All The Way Down", is a very illuminating discussion of just what can be accomplished in a pure Actor machine. Imagine an FPGA implementation, where CREATE, SEND, and BECOME are single machine instructions of a native instruction set. 

Such a machine, coupled with the innate security of Actor identities as capabilities tokens, could become unhackable by malevolent adversaries. Dale has simulations of this written for ARM processors, using very fast and clever coding at the ARM Assembly level. It is really worth your time to study his writings.

--------------
Tons of experiments later, much careful tuning, and we now have an Actors machine in Lisp that performs basic SEND/activate of Actors at the rate of 20M Actors/sec [51 ns on Intel i7, 46 ns on Intel i9 -- all from high-level (optimized) compiled Lisp]. 

4.2 GHz 4-Core Intel i7 -- SEND/Activate Timing
<img width="400" alt="Screen Shot 2021-05-30 at 10 59 35 AM" src="https://user-images.githubusercontent.com/3160577/120114909-36fee000-c136-11eb-9198-bc0d16acae05.png">

3.6 GHz 8-Core Intel i9 -- SEND/Activate Timing
<img width="398" alt="Screen Shot 2021-05-30 at 11 02 32" src="https://user-images.githubusercontent.com/3160577/120114994-9e1c9480-c136-11eb-9df5-fb4228aabb16.png">


I looked closely at using a single thread of Actor activation vs multiple threads across up to 4 CPU cores on a 4 Core Intel i7. Any one Actor can only be active on one core at a time. But multiple activities across several Actors could feasibly be peformed in parallel on several CPU cores. 

The net result of my tests show that the highest performance comes from using only a single thread. Of course blocking I/O activity needs to run on a separate thread to avoid throttling the main Actors thread. But there is a considerable cost (> 2x) to spreading the Actors across multiple threads. You get the highest degree of concurrency by running a gazillion small Actors on a single thread, and relying on the inter-SEND interleaving of their activities in the event queue. Concurrent yes. Parallel no. There is no task switching, and no need for cooperative YIELD between them. Actors just send messages to each other and their messages get interleaved in the event queue.

[If you have separate tasks that perform with relatively infrequent communication between them, then it is completely feasible to run them as Actor subsystems in separate threads for true parallel performance. You can have any number of such parallel activities going on.]

But there is an obvious cost to interposing the event queue between Actors, compared to direct function call. Where is a sensible division between making lots of small Actors versus fewer larger Actors that do more work on each invocation? To test that, consider the extreme situation where every elemental data type in the program is represented by Actors - the pure Actors machine.

I wrote a $CONS Actor to represent the Lisp CONS cell. And then I wrote a ton of list operations in Actor form against chains of these $CONS cells. The graph below shows the result of timing tests on $APPEND of a 1,000 element $LIST. The graph shows a histogram of median-3 measurements of the $APPEND operation on these 1,000 $CONS cells, so the cost per $CONS is 1/1,000 of the abscissa values.

<img width="397" alt="Screen Shot 2021-05-30 at 5 40 36 AM" src="https://user-images.githubusercontent.com/3160577/120104504-9abee400-c109-11eb-8979-88a5589abcb3.png">

For comparison I did a test of the native (compiled) Lisp performance of APPEND on a list of 1,000 CONS cells:

<img width="401" alt="Screen Shot 2021-05-30 at 5 42 17 AM" src="https://user-images.githubusercontent.com/3160577/120104555-d35ebd80-c109-11eb-94ab-701fb69a7469.png">

So the answer is that using Actors for elementary data types costs about 400x in performance. For me, that is completely unacceptable. On a different hardware architecture you might get the reverse situation. But we are stuck for now with conventional Von Neumann computers. 

But it isn't quite as bad as that... I tested a manually written $APPEND against the high performance built-in APPEND from my Lispworks system. Suppose instead, we test the Actors against a similarly expressed Lisp function using the same kind of algorithm with CPS coding conventions, and against our own %CONS defined via DEFSTRUCT. We are testing Actors against CPS coding for some comparable higher-level datatype of our own making. This would be a more fair comparison. [Code for all of this is found in "TActors/cons-visitor.lisp"]

<img width="397" alt="Screen Shot 2021-05-30 at 7 22 16 AM" src="https://user-images.githubusercontent.com/3160577/120107938-d1036000-c117-11eb-9595-c4c7948edbfa.png">


Here we are comparing $APPEND (Actors) against %APPEND (CPS Lisp), and the performance ratio is now 26x, not 400x:
```
;; -------------------------------------------
;; Actors $CONS

(defun $append (cust $cons $lst)
  (actor-typecase $cons
    (nil-beh  () (send cust $lst))
    (cons-beh ($car $cdr)
              (beta ($ans)
                  ($append beta $cdr $lst)
                (send cust ($cons $car $ans))
                ))
    ))

;; -------------------------------------------
;; CPS Lisp %CONS

(defstruct (%nil
            (:constructor %nil ())))
(defstruct (%cons
            (:constructor %cons (%car %cdr)))
  %car %cdr)


(defun %append (cust %cons %lst)
  (typecase %cons
    (%nil (funcall cust %lst))
    (%cons 
     (flet ((k-cont (%ans)
              (funcall cust (%cons (%cons-%car %cons) %ans))))
       (%append #'k-cont (%cons-%cdr %cons) %lst)))
    ))
```

So on your own (higher level) data types, Actors might be a useful alternative. Not only is the reasoning simpler with Actors compared to CPS coding style, but you get automatic interleaved concurrency with Actors, and transactional semantics with (mostly) pure functional code. You get no concurrency from CPS coding, no transactional semantics, and pure functional is up to you to enforce.

Actors greatly simplify many programming tasks, and provide safe concurrency without concern for issues surrounding threading and multi-tasking. Just write simple single-threaded code and you automatically get a high degree of concurrency. You do have to exercise care in your algorithms to make them robust in the face of concurrent activity. You still have READ-MODIFY-WRITE concerns since, between a separated READ and WRITE, you may have any number of other Actors trying to do the same thing. There are no locks, semaphores, etc., but you do have to learn how to write concurrent Actor code.

So there are clear benefits to Actors programming. You just need to have them perform more substantial activity on balance, or invoke them with less intensity.

[The graphs shown above were generated by a generalized Actors dataflow with plug-and-play for the Device Under Test (DUT): it automates the process of performing the median-of-3 timings of iterated tests, and then shows the histogram of the collected timings.
```
(let* ((niter 1000)
       (npts  1000)
       (lst   (loop for ix from 0 below 1000 collect ix))
       (dut   (simple-collector npts niter
                                (med3
                                 (timing
                                  (make-append-self-tst lst) ;; <-- Plug in your test here...
                                  )))))
  (let ((act (actor (cust)
               (beta (arr)
                   (send dut beta niter)
                 (send cust)
                 (send (histogram) arr)
                 (send (statistics) println arr)
                 ))))
    (send (timing act) println)))
```
]

--------------------------------------------------------------
Closing the Performance Gap
-----
So what kind of workload in our Actors does it take to narrow the performance gap between direct function calls and Actor SEND? 

I created 3 versions of a multi-level lookup table. I can categorize data with any number of keys, and each intermediate level operates on another instance of the same kind of table. The leaf nodes contain the data. All three variations construct functionally pure data structures. The code for all of this is stored in file "TActors/lookup.lisp". 

The three versions are:

* An Actors version of one-element-per-Actor, which I connote by Spread-Actor Tables. This one has the highest degree of concurrency of the three variations. In a multi-key modification to the table, you have to queue up additional requests for modification while one is already under way. Lookups can continue during the modification, but we have to guard against the concurrent READ-MODIFY-WRITE - without resorting to locks. This provides an example of lock-free concurrent-safe READ-MODIFY-WRITE algorithms for Actors.

* An Actor version that uses a multi-level FP-pure ALIST. This is logically atomic for every operation and no need to write special concurrent access code as we did for the Spread-Actor tables. There is no concurrency happening during any table modification operation. All intermediate modifications are performed with function calls, not Actor SEND.

* An Actor version that uses a multi-level FP-pure Red-Black Tree. This has the same atomic semantics as our multi-level ALIST tables. The only difference, apart from morphology, is that while multi-level ALIST tables permit entries with duplicate keying (prepending the duplicates), our Red-Black Trees only allow one element per key, so ADD-ITEM is the same as REPLACE-ITEM. Again, this has no concurrency during the table modification operations.

I then generated 1,000 quads of random numbers between 0 and 10, and timed the duration that it took to construct a 3-key (3-levels) table, adding initial entries, and then additional iterations merely repeat the process against the populated table with REPLACE-ITEM.

All three of the lookup tables are FP, which means that REPLACE has to regenerate the table along the lookup path. And with 1,000 keysets using keys ranging from 0 to 10, I fully expect many replacements, both during initial table loading, and then most assuredly during the successive re-entry passes of the benchmark.

The results, per item timing, shows that the multi-level FP ALIST performs fastest, at about 0.9 microsec / element. The FP Red-Black Tree performs at around 4.6 microsec / item. And the multi-level Spread-Actor table came in just a bit faster than the RB-Tree version at around 3.9 microsec / element. All measurements showed a MAD of around 0.1 microsec. (MAD = median absolute deviation from measurement median)

Now I don't consider this sort of data structure to be extremely abstract and high level. I would consider it to be midway between a high-level data structure and an elemental data type in the language. And here we see a decent choice ahead of us. We can seek raw speed, but without concurrency during composite operations in the table, using data elements that are very close to the native Lisp data types (ALISTS). Or we can opt for maximum concurrency and only pay a factor of 4x in speed, which is about the same cost as using a more elaborate data structure (RB-Trees) without concurrency. In any case, because of the outer Actor wrapper on the data, we have transactional semantics with FP-pure data.

In terms of coding complexity, the most complex is for purely functional, non-concurrent, RB-Trees. The simplest was non-concurrent FP ALISTS. Spread-Actor tables are inherently FP clean, but have to deal with concurrency issues during their composite table updates. This takes only a moderate amount of very simple code. They are self-organizing, and self-trimming when table entries are removed. Fundamentally, the Spread-Actor tables are nearly the same as an Actor-ized implementation of ALIST tables. And they automatically produce and accommodate concurrency, where neither of the other two options provide any.

So, What are Actors Really About?
------------------------------------

Having spent some time with them, solving a variety of problems, and looking for a good partition between normal call/return coding style and Actors coding... What we actually have with Actors is another way to perform CPS coding style. But whereas conventional CPS coding merely replaces Call/Return with continuation jumps, the Actors event queue enables fine-grained concurrency among possibly unrelated tasks all within one machine thread. 

Pressure to invoke additional threads is reduced. Concurrency (but not parallelism) is increased over what you would get with cooperative multi-tasking where task switching is driven by manually planting YIELD at various locations in the code. With Actors there is no need to be aware of other tasks, no need for YIELD because it is implicit in the event queue dispatching routine.

Because Actors can be prevented from executing simultaneously in multiple threads (via ENSURE-PAR-SAFE-BEHAVIOR), we can write our code in single-thread style without worrying about simultaneous access to private state information. Indeed, with discipline, you can make all your Actor code into purely functional style, with the only mutation permitted being via BECOME. And with transactional semantics on BECOME and SEND, if anything goes wrong in the behavior code, the state remains in a predictable condition. Functional Actors without state mutation can be safely executed in parallel on multiple threads.

A message SEND is essentially a deferred function call; one that competes with other pending SENDS for execution. Activation order is non-deterministic. Between any two Actor activations other events may arrive from other threads via the Sponsor mailbox. Scheduling of activations is fair and free from starvation, but precise ordering cannot be predicted.

In detail, everything computed on a processor is via continuations. Call/Return style merely places the continuation address on the runtime stack (i.e., the return address). And Call/Return decides for you what the continuation will be. With Actor SEND you have the freedom to specify your own choice of Actor continuation.

On today's CPU architectures we already know that a pure Actors machine will suffer in comparison to CALL/RETURN style programming. But at some higher level of partitioning, many system level problems can be more easily solved with the asynchronous message SEND to Actors, compared to the complexity that ensues with CALL/RETURN and complex interwoven state information. (See the Async Network code for an example of this.) Actors partition the state into bite sized chunks about which can be more easily reasoned. Actor subsystems can more easily be proven correct. And larger assemblies can be constructed from already proven Actor components.
