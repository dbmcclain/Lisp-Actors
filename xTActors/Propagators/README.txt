Propagators - an implementation of Radul and Sussman's Propagators in
Actors...

Actors represent an ideal implementation platform for these inherently
asynchronous networks. Using Actors removes the need for the
ALERT-PROPAGATORS function. These cells will just naturally invoke
each other without having to nudge them to do so.

We have CELLS as the only user visible components. These are
implemented as Actors. Their local state contains their current value
and a list of connections to other CELLS through operators. You affect
CELLS by setting new information into them with ADD-CONTENT, or into
another CELL connected via an underlying Propagator network.

You can examine the data content of a cell using CONTENT. The
underlying network remains hidden, except for the source code
specifying the operator interconnections between CELLS.

PROPAGATORS are also implemented as Actors beneath the network. They
use a pull-model to collext operator arguments from their input CELLS
(via :CONTENT messages), whenever they are given a :PROPAGATE message.
And they, in turn, send new information to their output CELL, which
might cause a cascade of asynchronous computation in other legs of the
network.

------------------
Start:

  To use: first load up propagators.lisp. Then manually execute the
exercises in section-2.lisp.

-------------------
Interval Arithmetic

  After loading propagators.lisp, load interval-cells.lisp and execute
the exercises in section-3.lisp and section-4.lisp. 

Section 4 introduces multi-directional evaluation, much like you find
in Prolog and Erlang. Only we have it much easier...

  ...just restate the cell arguments in different orders and use a
propagator in parallel with all the others that computes the inverse
functions. Also widen the compound-propagator cells list to include
the former output cell - it can now be an input cell in reverse. When
something can be computed and updated, it will be.

------------------
Ball (Statistical) Arithmetic:

  To try out using statistical Ball arithmetic, load up
ball-cells.lisp after propagators.lisp, and interval-cells.lisp, and
manually execute the exercises in section-3.lisp and section-4.lisp.

OTOH, just skip the exercises. There is a difference between
scientifically accounting for multiple measuremeents of varying
variance weight, and facing these Propagator networks which might
present duplicate data. Duplicate data is seen by Balls arithmetic as
a confirming instance, which further tightens the balls, when it
probably shouldn't be...

------------------ 
Generic Operations to support mixed Numbers, Intervals, and Ball
(Statistical) Arithmetic

  Next, load up generic-operations.lisp (needs propagators,
interval-cells, and ball-cells), and retry examples in section-3.lisp
and section-4.lisp. This version uses generic arithmetic operations to
make cells usable with any of balls, intervals, and/or number
arguments.

  Some care was also taken to ensure correct behavior in the complex
domain - setting the stage for electrical circuit simulations.

------------------ 
Supported Values and Provenance Tracking

  Now, load up supported-values.lisp (needs propagators,
interval-cells, ball-cells, generic-operations), and try out the
examples in section-6.1.lisp. This provides for a simple provenance
trail, but can arrive at some nonsensical supports.

------------------ 

We will extend the generic operations shortly for sections 6.2 and
beyond.

- DM/RAL 02/22

