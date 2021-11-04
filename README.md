-- Lisp-Actors - Classical Actors (Bare Essentials in xTActors folder - Nov 2021)
--------------

What happens if we get rid of all the crud from our previous "2nd System Syndrome" and cut everything back to essentials? 

That's what we have in folder xTActors. Everything is an Actor, including Sponsors. No subtypes of Actors. An Actor is simply an encapsulated functional closure - called its Behavior - with code and state data. While the Behavior of an Actor can change by its calling BECOME, the Actor itself has unchanging identity. Once you have the address of an Actor, that will never change. 

Sponsors are ordinary Actors whose state consists of a mailbox and a thread that runs the RUN event dispatch loop. The thread shares that same mailbox. Any messages sent to a Sponsor Actor are stuffed into that mailbox and retrieved by its RUN loop. That allows us to have cross-Sponsor SEND without any extra baggage involving Actor subtypes.

Actors can be tied to a specific Sponsor by using `(IN-SPONSOR <sponsor> <actor>)` which creates another ordinary Actor. Anything sent to this new Actor gets shipped over to the indicated Sponsor before forwarding the message to the Actor. Actors, in themselves, have no notion of threads. They are just simple encapsulated functional closures. They can be executed on any thread.
  
In a multi-threaded environment, some Actors must not be permitted to execute in parallel on more than one thread. This is so when an Actor might execute a BECOME to mutate its behavior. Race conditions would result if multiple threads ran the Actor code at the same time. The easy solution to this is to simply use IN-SPONSOR, giving us an Actor tied to one agreed upon specific Sponsor. Operator `(PAR-SAFE <actor>)` does just that. Within each Sponsor you have a single thread of execution. Multiple threads wanting to execute the Actor will be queued up in the event queue of that Sponsor, and given serialized access to the Actor code.

As before, SEND and BECOME are transactional. Newly created Actors are hidden from the outside world until you send them to someone. If an uncaught error occurs in the body of executing Actor code, all SENDS and BECOMES since entry to the code will be discarded, and it will be as though the message that produced the error was never delivered. Functionally pure Actors. If an Actor needs to mutate its state, it is best to simply use BECOME to generate fresh behavior. The only mutated item is the behavior pointer inside the Actor encapsulation.

It is refreshing to chop away needless complexity...



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
