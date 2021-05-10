v# Lisp-Actors
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
After much experimentation with various ways of managing multiple CPU Cores, I have settled upon using Sponsors, which manage Actor threads and event queues. Every event queue in the Sponsor has a dedicated thread running Actors. SEND multiplexes messages across event queues in round-robin fashion.

There are two Sponsors defined - Single Threaded and Multi-Threaded. Once an Actor is launched in one Sponsor, then all further activity occurs among that Sponsor's threads, unless an explicit Sponsor change is performed using SENDX. Jumping tracks from single-thread to multi-thread and vice versa. This is very useful behavior.

(To get accurate wall-clock timings, unpolluted by multi-core contributions, you must perform the measurement using a timer in a single thread. That's Apple, not me. But your test code can run in any Sponsor among any number of threads. The timer widget is provided to automatically manage this requirement.)

Liveness of an Actor system is guaranteed by making many small Actors and doing frequent Sends. The Actors are multiplexed from the event queue(s). You don't need multiple threads to peform actions that were previously thought to require threading. But when you do have multiple Sponsor threads, you get concurrency plus parallelism for some degree of speedup.

<img width="405" alt="Screen Shot 2021-05-09 at 7 50 54 AM" src="https://user-images.githubusercontent.com/3160577/117578055-62475f80-b0a1-11eb-8d5a-86a809956815.png">

This example shows a comparison of a do-nothing Fork Bomb that constructs up to 33 Million Actors, where every Actor sends a message back up the tree to the root node. The timings show the cost per Actor for [MAKE-ACTOR / MAKE-ACTOR / SEND / SEND / BECOME / RUN-dispatch / BECOME / RUN-dispatch / SEND / RUN-dispatch] at every interior Actor node, and [SEND / RUN-dispatch] at the leaves. At its largest, there are (2^24 -  1) interior Actor nodes, and 2^24 leaves. 

In the mutli-thread test, you can clearly see the overhead cost of OSX thread switching when there is very light loading. This overhead declines as the event queues become saturated.

The gradual incline in both tests at higher loads shows the increasing cost of GC. 33 Million Actors takes about 500 MB of memory (16 bytes / Actor in a 64-bit system), and the system is barely breaking a sweat with all 8 cores lit up to max utilization. The Lisp system remains responsive to keyboard and editing chores while the tests are running. Imagine running 33 Million live socket connections.

<img width="403" alt="Screen Shot 2021-05-09 at 10 21 50 AM" src="https://user-images.githubusercontent.com/3160577/117582357-806b8a80-b0b6-11eb-9881-215a83b87b32.png">

For direct comparison with CPS-style direct function-call code, we find that an equivalent CPS Fork Bomb runs about 200 times faster. But it opens the gates to callback-hell, while Actors remain so easy to use. And remember that these Fork Bomb Actors aren't loaded with any task computations. Any additional loading in the Actors and the equivalent CPS direct functions will decrese this disparity. 

Just imagine a machine in an FPGA, whose basic instructions are MAKE-ACTOR, SEND, BECOME. No stacks and stack frames to manage, no memory overruns, no dangling pointers, no shared mutable data, no thread switching overhead, no crashes (ever!). Impossible to hack.

-----------------
So lets FBomb the CPU again, but this time with a real workload. This time, I have all the interior nodes just generating two child nodes. But every leaf node is loaded down with computing Erfc(x) with 1,000 random values of x between 0.0 and 1.0.


<img width="399" alt="Screen Shot 2021-05-09 at 1 02 42 PM" src="https://user-images.githubusercontent.com/3160577/117585778-1b6d6000-b0c9-11eb-81c5-3c15dd5a5663.png">

That inner compute load nominally takes 2.3 microsec per Erfc(x) call. So from the REPL the workload should take about 2.3 ms. You see the single-threaded version of Actors taking pretty much that nominal time. Which means the effort constructing the tree and sending all those messages is buried by the workload and benefits from natural concurrency among Actors, even though it runs in a single thread.

The multi-threaded Actors gain further by parallel execution, but only by 2x, not the availabe 8x potential. (aw shucks..)

But notice that the native code CPS Funcall version, which is necessarily single-threaded, and has no inherent opportunities for concurrency, actually takes longer than any of the Actors versions. There is no 200x looping of the Erfc going on here. Just the same load of 1,000 random samples as for the Actors.  So my computer runs native code to accomplish absolutely nothing, 200 times faster than Actors can accomplish the same nothing. But when faced with real work, the Actors win out.

Further tests show that we can reduce the number of Erfc() executions all the way down to one, and the Actors consistently remain faster than direct CPS Funcall code. So the critical workload that equalizes the performance is considerably less than the work required to compute Erfc through a continue fraction expansion.

So dropping down to more elementary groups of instructions, I have found that instead of Erfc(), just generating 256 samples of RANDOM(1.0) is sufficient to equalize the performance of CPS Funcall with Single-threaded Actors:

<img width="401" alt="Screen Shot 2021-05-09 at 6 09 36 PM" src="https://user-images.githubusercontent.com/3160577/117593876-bbd77a80-b0f1-11eb-92c0-460accea22ee.png">

But now we have an anomaly in the multi-threaded timing. My code never performs any thread switching. A thread continuously runs the RUN dispatcher against its own dedicated event queue. At most my threads will pause while waiting for more events, or briefly, when there is a short contention period on the event queue between a sender and the event dispatcher thread. So this anomaly appears to be induced by OSX stealing unused remains of timeslices away from RUN dispatchers and giving them to unrelated background tasks on the machine.

