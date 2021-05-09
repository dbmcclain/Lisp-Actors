# Lisp-Actors
-- Classical Actors (in TActors folder) --
-------------
You will become amazed at this, but Classical Actors run in a single thread, and yet, accomplish what we had all thought was a necessary use case for multi-threaded code. Classical Actors prove the contrary.

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

So now, let's re-invoke multiple threads to take advantage of parallel opportunities. The code in TActors now implements a single event queue, but feeds mutliple RUN dispatchers. Actors must be logically atomic. And so if an Actor is busy executing, new events arriving for that Actor must be delayed. RUN can then try to dispatch a different message to a different Actor.

Actor graphs must respond properly to messages that can arrive at any time, and in any order (or not at all). When you need a particular order for arrival, you have to arrange for that to happen with gatekeepers in the Actor graph.

Actor graphs (collections of cooperating Actors) describe a state / message dependent state machine, if you use this information to change state and behavior with BECOME. The Network Socket Reader is a good example. The outer Reader - the one seen by the Async Driver in Lisp, sequences the processing of arriving packets. Each packet is really 3 components: Prefix Length (4 bytes), Encrypted Data (ndata bytes), and HMAC (32 bytes). 

Data arriving from the Async Driver gets enqueued in a buffer manager whose job is to parcel out successive bytes into reader-supplied buffers for so many bytes in each buffer. You can't have the reader request the data portion until it knows the length, and you can't ask for HMAC bytes until all the data bytes have been taken. So sequential state is maintained by using BECOME in a circular manner.

Some Actor behaviors are inherently safe for parallel execution. These Actors do not call BECOME, and so no state changes. Examples are CONST-BEH, FWD-BEH, TAG-BEH, and LABEL-BEH. So I defined a subclass of FUNCTION called SAFE-BEH. If an event for one of these Actors is seen, it is permitted to execute even if it were already handling a prior event.

----------------------
After much experimentation with various ways of managing multiple CPU Cores, I have settled upon using Sponsors, which manage Actor threads and event queues. There are two Sponsors defined - Single Threaded and Multi-Threaded. Once an Actor is launched in one Sponsor, then all further activity occurs among that Sponsor's threads, unless an explicit Sponsor change is performed using SENDX. Jumping tracks from single-thread to multi-thread and vice versa. This is very useful behavior.

(To get accurate wall-clock timings, unpolluted by multi-core contributions, you must perform the measurement using a timer in a single thread. That's Apple, not me. But your test code can run in any Sponsor among any number of threads. The timer widget is provided to automatically manage this requirement.)

Liveness of an Actor system is guaranteed by making many small Actors and doing frequent Sends. The Actors are multiplexed from the event queue(s). You don't need multiple threads to peform actions that were previously thought to require threading. But when you do have multiple Sponsor threads, you get concurrency plus parallelism for some degree of speedup.

<img width="405" alt="Screen Shot 2021-05-09 at 7 50 54 AM" src="https://user-images.githubusercontent.com/3160577/117578055-62475f80-b0a1-11eb-8d5a-86a809956815.png">

This example shows a comparison of a do-nothing Fork Bomb that constructs up to 33 Million Actors, where every Actor sends a message back up the tree to the root node. The timings show the cost per Actor for MAKE-ACTOR / SEND / RUN-dispatch / SEND / RUN-dispatch. In the mutli-thread test, you can clearly see the overhead cost of thread switching when there is very light loading. 

The gradual incline in both tests at higher loads shows the increasing cost of GC. 33 Million Actors takes about 500 MB of memory (16 bytes / Actor in a 64-bit system), and the system is barely breaking a sweat with all 8 cores lit up to max utilization. The Lisp system remains responsive to keyboard and editing chores while the tests are running. Imagine running 33 Million live socket connections.

For direct comparison with CPS-style direct function-call code, we find that an equivalent CPS Fork Bomb runs about 400 times faster. But it opens the gates to callback-hell, while Actors remain so easy to use. Just imagine a machine in an FPGA, whose basic instructions are MAKE-ACTOR, SEND, BECOME. No stacks and stack frames to manage, no memory overruns, no dangling pointers, no shared mutable data, no crashes (ever!). Impossible to hack.
