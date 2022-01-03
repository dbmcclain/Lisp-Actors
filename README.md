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

[HOWEVER: 
----
``Performing your own first message can be shown to lead to potentially unlimited growth of the central message queue. This is tantamount to a depth-first path of potentially unlimited length, and where extra messages generated along the way will never be tended to. Bad news!!

The only controlled (and fair) approach is to dump all SEND messages into the central mailbox queue. That means that new messages are always at the tail of the queue, and any extra messages generated along the way will be incrementally handled. This becomes a breadth-first execution strategy, and will eventually drain the event queue, provided that we are dealing with a bounded execution graph. Bounded execution = NOT a fork-bomb. 

Sadly, in the face of SMP this implies a severe slowdown compared to single-thread execution. But frankly, the benefits of SMP outweigh the speed advantage in my mind, and the only way to incorporate fast, controlled SMP, is to reinvent Sponsors. I'm tired of Sponsors and I'd rather have parallelism and no bother in programming with Sponsors. Just my 2c... '']

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
