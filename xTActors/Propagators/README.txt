Propagators - an implementation of Radul and Sussman's Propagators in
Actors...

Actors represent an ideal implementation platform for these inherently
asynchronous networks. Using Actors removes the need for the
ALERT-PROPAGATORS function. These cells will just naturally invoke
each other without having to nudge them to do so.

To use: first load up propagators.lisp. Then manually execute the
exercises in section-3.lisp and section-4.lisp.

To try out using statistical Ball arithmetic, load up ball-cells.lisp
after propagators.lisp, and manually execute the exercises in
section-3.lisp and section-4.lisp.

