DM/RAL 10/15/25 -- Simplify your life!
---

Actors programming is the ultimate in luxurious simplicity, while rewarding your efforts with the maximum in parallel concurrent execution. It took me the better part of 20 years to finally achieve a simple and painless way to take maximum advantage of my multi-core processors. And here it is - _Transactional Conventional Hewitt Actors_.

Don't even bother thinking about threads and locks and even more complicated things. Just program like you own the machine, but do your future self a huge favor and program in a Functional Programming paradigm, and with pattern matching at your disposal. We are still Lisp, and if you violate functional protocols - i.e., treating shared data as mutable - then you open your future self to a world of hurt, and you make it almost impossible to fully flex the SMP multi-core architecture of your machine.

You can still have mutation in your program. After all, nothing gets done unless you have mutation. But by keeping state within Actors, and mutating solely through BECOME with fresh or copied and changed state, and treating all shared data as immutable otherwise, then you automatically obtain parallel concurrent code. And you can decide for yourself what granularity of concurrency you want.

All of your code will run the same on a single processor, without any mutithreading, as well as across all the CPU cores, each running a time-sliced OS. The only difference will be speed of overall execution.

Deadlocks will be a thing of the past. Priority inversion will cease to happen. If two "Tasks" face a standoff, the rest of the Actors system remains alive and responsive. A "Task" is not a machine thread, but rather a logical thread of activity produced by a coordinated series of actions that perform some or all of some computational chore that you need to accomplish.

There really could be multiple machine threads running beneath the Actors system - as Dispatch threads, delivering messages to Actors. A single Task may involve the execution of portions of Actor code by any number of arbitrary Dispatch threads. But your code is never aware of them.

And there are some hidden locks in the system to coordinate actions among multiple machine threads - in the Event Queue, a shared mailbox which is where all messages are sent, and in the committing of BECOME. But you never need to be aware of these locks.

Transactional means that all SEND and BECOME are staged for execution at successful exit when they will be committed. And the only way to keep a CREATEd Actor from being garbage collected is to share its identity in a committed SEND message or a BECOME state. No global effects from an Actor execution can become visible until its behavior code exits successfully. 

And best of all, the Transactional nature of Actors means that errors will not cascade any further than the abort of the currently running Actor code. All SEND, BECOME, and CREATEd Actors, are discarded, and it is as though the errant message were never delivered. 

Erlang was an early "Actor System", of sorts. But they conflate (Green) Threads with Actors. And they have the notion of killing an Actor. In our Transactional Conventional Hewitt Actor system, Actors are neither alive nor dead. They are just wrapped functional closures. Asking to kill an Actor has no more meaning than asking to kill the EVAL function.

And quite unlike Erlang, our Actors never request to read a message. Messages are just sent to them by Dispatch threads, which is another way of saying that if a message is extracted from the global Event Queue by a Dispatch thread, then it will take the message as function call arguments and apply the behavior function of the target Actor to those arguments. And if an Actor is already busy executing against a message from a different Dispatch thread, that's okay. We do parallel execution.

While Actors occupy a special place in programming, I do not believe they should be seen as an end-all, in the same way that Smalltalk views everything as an Object. Actor code is for managing and coordinating subsystems with asynchronous concurrent behavior. They are not a full substitute for the  performant Call/Return protocol most commonly used in Lisp programs. And by the same token, Call/Return can become very clumsy at managing asynchrony with multiple concurrent activities and numerous callbacks. Both paradims have their use.
___
