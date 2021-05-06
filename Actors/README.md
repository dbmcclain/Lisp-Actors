This folder contains the start of an experiment in minimalism. Just
how simple can an Actors system become? And what are the benefits of
it?

This is now a fully functioning version, sans ASK and Parallel launch
for mapping and iteration. Everything Actors runs in a single thread.
Concurrency is very much alive by chopping your tasks into bite-sized
comprehensible subtasks, and using SEND to activate the portions.

Two way conversations among Actors using mailbox schemes will lead to
deadlock. So, instead everthing is pay-it-forward. If you need to
operate on the result of an Actor computation, you should send the
result to a continuation or another Actor.

Continuations are still very much alive in this version, although now
with a single thread running through all Actors, the need for data
privacy and encapsulation disappear. It is still impossible for two
threads to be running in one Actor body code. And it is also
impossible for two threads to bang on the same data.

So this experiment is more about thinking in terms of more pure Actors
which offer three fundamental operations: MAKE, SEND, and BECOME. Make
them as small as you like, with minimal runtime overhead. Make more of
them too.

A benchmark test is shown in Examples/micro-actors.lisp at the bottom.
The test consists of an Actor which recursively creates a chain of up
to N Actors. Then when the chain has been constructed, the first Actor
records the NOW time, and tells the last Actor to send a message back
along the chain. When that message arrives back at the first Actor the
time is recorded, and the average time-cost of SEND/activate is
computed and printed.

For my machine, running compiled Lispworks Mac/64, a chain of 1
Million Actors show that the average SEND/activate time is 3
microseconds. In constrast, the fully SMP multithreaded version of
Actors in the Actors folder takes about 9 microsec. And to my dismay,
the version of SMP Actors based on Grand Central Dispatch takes 15
microsec. A captive Executive thread pool is desirable for maximum
speed.

- DM
