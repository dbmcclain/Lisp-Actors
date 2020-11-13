**Reppy Channels - adapted for Actors**

While Actors are inherently asynchronous, John Reppy's Channels and Events are aimed specifically at synchronous communications between threads. The huge advantage of John Reppy's Channels is that events and handlers are composable in an FPL sense. You can make some very elaborate multi-channel interfaces among multiple threads.

But with CPS style (=BIND, etc.) you can have Actors join into the fun. This code base is more of my exploratorium for Lisp. While my Actors code has seen production level field testing, my Reppy Channels really have not. But they have been evolving since the early 2000's after I first read John Reppy's CML.

Event handlers are composed of leaf-handlers conjoined with event meta-operators. Nothing happens until a composed event graph is called by SYNC. At that time, the events are entered into RECV/SEND queues, the queues are polled for available rendezvous, and when found, trigger a response from the composed handler graph.

On successful rendezvous, all other events in the graph are told to execute any wrap-abort handlers, and then the successful event performs its wrap handlers on the way to the top of the event graph. The final result is the data handed through the rendezvous, as modified by any wrap handlers. Rendezvous are always synchronous on both sides. Senders wait for receivers to pick up the data.

* Leaf Events:
	* SendEvt ch msg
	* RecvEvt ch
	* AlwaysEvt data
	* NeverEvt
	* ExecEvt fn &rest args
	* SendEvt* ch msg - a SendEvt with NAK
	* RecvEvt* ch - a RecvEvt with NAK
	* AbortEvt &optional val
	* TimerEvt dt
	* ErrorEvt fn
	* TimeoutEvt dt
	* JoinEvt thread
	* KeyEvt &key flush stream - awaits stream input char input
	* LineEvt &key flush stream - awaits stream text line input
	* SexpEvt &key flush stream - awaits stream SEXP input
	
* Meta-Event Composers - all produce Events from composition:
	* CHOOSE [evt]* - one of the events chosen in random order. Only one will rendezvous.
	* CHOOSE* [evt]* - one of the events in stated order. Only one will rendezvous.
	* WRAP-ABORT evt fn
	* WRAP evt fn
	* WRAP-HANDLER evt fn - used to wrap the graph with HANDLER-CASE, etc.
	* FailEvt evt 
	* GUARD fn - thunk fn must return an Event.
	* WRAP-ERROR evt fn - thunk fn should produce a condition specifier or object.
	* WRAP-TIMEOUT dt evt
	
* At the top of the graph, causing it to execute:
	* SYNC evt
	* RECV ch = (SYNC (RecvEvt ch))
	* SEND ch msg = (SYNC (SendEvt ch msg))
	* POKE ch msg = async version of SEND
	* PEEK ch = async version of RECV
	* SELECT [evt]* = (SYNC (CHOOSE evts...))
	* SELECT* [evt]* = (SYNC (CHOOSE* evts...))
	
* Possibly surrounding an event system:
	* WITH-CHANNEL ch &body body
	* WITH-CHANNELS [ch]* &body body
	* and, of course, MAKE-CHANNEL
	
Example compositions:

    (defun TimeoutEvt (dt)
       (WRAP-ERROR (TimerEvt dt)
           (lambda (arg)
             (declare (ignore arg))
             'TIMEOUT)))
	     
    (defun Wrap-Timeout (dt evt)
        (CHOOSE* evt
                 (TimeoutEvt dt)))
		 
Purpose of the NACK? While all pending events get cancelled and call their WRAP-ABORT handlers on failed rendezvous, that only happens if one of the events in the graph actually rendezvous. So imagine a situation where one thread has a SEND or RECV on one channel, and another thread has some pending events on that same channel as well as other events pending on another channel. 

If the other thread rendezvous on the other channel, its WRAP-ABORT handlers will be called on the first channel events. But unless those WRAP-HANLDERS perform a pseudo-rendezvous on the first channel, the first thread just hangs waiting for an event that will never arrive. We can deal with this situation by using timeouts on the channel comms. But by specifying a NACK handler SendEvt* or RecvEvt*, that pseudo-rendezvous will occur for us, telling the first thread to give up.

While the channels use lock-free FIFO queues, alas, we must resort to locks during polling when considering each potential rendezvous. Too many things changing at the same time, and without locking, it becomes possible for potential rendezvous to become missed. 

Honestly, this is relatively difficult concurrent code, and it really makes me appreciate Actors, with their single-thread semantics. But then our performance ought to be higher since the locking is very fine-grained here, whereas Actors can produce bottlenecks in highly-subscribed services in concurrent code, and they generally apply to wider conditions than our fine-grained locking. Calling on an Actor is heavier weight, (but not nearly as heavy as spawning new threads), and so they need to accomplish significant work to make it worthwhile.

- DM
