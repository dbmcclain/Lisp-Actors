-- Lisp-Actors - Classical Actors (in TActors folder) --
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
Tons of experiments later, much careful tuning, and we now have an Actors machine in Lisp that performs basic SEND/activate of Actors at the rate of 20M Actors/sec. I looked closely at using a single thread of Actor activation vs multiple threads across up to 4 CPU cores on a 4 Core Intel i7. Any one Actor can only be active on one core at a time. But multiple activities across several Actors could feasibly be peformed in parallel on several CPU cores. 

The net result of my tests show that the highest performance comes from using only a single thread. Of course blocking I/O activity needs to run on a separate thread to avoid throttling the main Actors thread. But there is a considerable cost (> 2x) to spreading the Actors across multiple threads. You get the highest degree of concurrency by running a gazillion small Actors on a single thread, and relying on the inter-SEND interleaving of their activities in the event queue. Concurrent yes. Parallel no. There is no task switching, and no need for cooperative YIELD between them. Actors just send messages to each other and their messages get interleaved in the event queue.

But there is an obvious cost to interposing the event queue between Actors, compared to direct function call. Where is a sensible division between making lots of small Actors versus fewer larger Actors that do more work on each invocation? To test that, consider the extreme situation where every elemental data type in the program is represented by Actors - the pure Actors machine.

I wrote a $CONS Actor to represent the Lisp CONS cell. And then I wrote a ton of list operations in Actor form against chains of these $CONS cells. The graph below shows the result of timing tests on $APPEND of a 1,000 element $LIST. The graph shows a histogram of median-3 measurements of the $APPEND operation on these 1,000 $CONS cells, so the cost per $CONS is 1/1,000 of the abscissa values.

<img width="397" alt="Screen Shot 2021-05-30 at 5 40 36 AM" src="https://user-images.githubusercontent.com/3160577/120104504-9abee400-c109-11eb-8979-88a5589abcb3.png">

For comparison I did a test of the native (compiled) Lisp performance of APPEND on a list of 1,000 CONS cells:

<img width="401" alt="Screen Shot 2021-05-30 at 5 42 17 AM" src="https://user-images.githubusercontent.com/3160577/120104555-d35ebd80-c109-11eb-94ab-701fb69a7469.png">

So the answer is that using Actors for elementary data types costs about 400x in performance. For me, that is completely unacceptable. On a different hardware architecture you might get the reverse situation. But we are stuck for now with conventional Von Neumann computers. 

But it isn't quite as bad as that... I tested a manually written $APPEND against the high performance built-in APPEND from my Lispworks system. Suppose instead, we test the Actors against a similarly expressed Lisp function using the same kind of algorithm with CPS coding conventions, and against our own %CONS defined via DEFSTRUCT. We are testing Actors against CPS coding for some comparable higher-level datatype of our own making. This would be a more fair comparison.

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

Actors greatly simplify many programming tasks, and provide safe concurrency without bothering with issues surrounding threading and multi-tasking. Just write simple single-threaded code and you automatically get a high degree of concurrency. You do have to exercise care in your algorithms to make them robust in the face of concurrent activity. You still have READ-MODIFY-WRITE concerns since between a separated READ and WRITE you may have any number of other Actors trying to do the same thing. There are no locks, semaphores, etc. But you do have to learn how to write concurrent Actor code.

So there are clear benefits to Actors programming. You just have to have them perform more substantial activity on balance, or invoke them with less intensity.


