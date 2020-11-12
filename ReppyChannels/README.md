**Reppy Channels - adapted for Actors**

While Actors are inherently asynchronous, John Reppy's Channels and Events are aimed specifically at synchronous communications between threads. The huge advantage of John Reppy's Channels is that events and handlers are composable in an FPL sense. You can make some very elaborate multi-channel interfaces among multiple threads.

But with CPS style (=BIND, etc.) you can have Actors join into the fun. This code base is more of my exploratorium for Lisp. While my Actors code has seen production level field testing, my Reppy Channels really have not. But they have been evolving since the early 2000's after I first read John Reppy's CML.

Event handlers are composed of leaf-handlers conjoined with event-meta operators. Nothing happens until a composed event is called by SYNC. At that time, the events are entered into RECV/SEND queues, the queues are polled for available matching events, and when found, trigger a response from the composed handler graph.

On successful rendezvous, all other events in the graph are told to execute any wrap-abort handlers, and then the successful event performs its wrap handlers on the way to the top of the event tree. The final result is the data handed through the rendezvous, as modified by any wrap handlers.

Leaf Events:
	SendEvt ch msg
	RecvEvt ch
	AlwaysEvt data
	NeverEvt
	ExecEvt fn
	SendEvt* ch msg - a Send with NAK
	RecvEvt* ch - a Recv with NAK
	AbortEvt &optional val
	TimerEvt dt
	ErrorEvt fn
	TimeoutEvt dt
	JoinEvt thread
	KeyEvt &key flush stream - awaits stream input char input
	LineEvt &key flush stream - awaits stream text line input
	SexpEvt &key flush stream - awaits strean SEXP input
	
Meta-Event Composers - all produce Events from composition:
	CHOOSE [evt]* - one of the events chosen in random order
	CHOOSE* [evt]* - one of the events in stated order
	WRAP-ABORT evt fn
	WRAP evt fn
	WRAP-HANDLER evt fn - used to wrap the graph with HANDLER-CASE, etc.
	FailEvt evt 
	GUARD fn - fn must return an Event
	WRAP-ERROR evt fn - fn should produce a condition object
	WRAP-TIMEOUT dt evt
	
At the top of the graph:
	SYNC evt
	RECV ch = (SYNC (RecvEvt ch))
	SEND ch msg = (SYNC (SendEvt ch msg))
	POKE ch msg = async version of SEND
	PEEK ch = async version of RECV
	SELECT [evt]* = (SYNC (CHOOSE evts...))
	SELECT* [evt]* = (SYNC (CHOOSE* evts...))
	
Possibly surrounding an event system:
	WITH-CHANNEL ch &body body
	WITH-CHANNELS [ch]* &body body
	
Example composition:

  (defun TimeoutEvt (dt)
    (WRAP-ERROR (TimerEvt dt)
      (lambda (arg)
	    (declare (ignore arg))
		'TIMEOUT)))
	
- DM
