Understanding β-Forms
---
β is a macro form analogous to MULTIPLE-VALUE-BIND. Following the opening β we have an argument list, a SEND form, and a macro body of one or more Sexprs. 

Its format is similar to that of MULTIPLE-VALUE-BIND, but in place of a VALUES-generating form or function call, the SEND form sends a message to some Actor, with β explicitly stated as a Customer arg in the message. 

You are telling the Actor to send its results to β as the customer in the message, and that β actually represents an unnamed Actor which expects a message containing the arguments shown in the β-form, and whose behavior body is made from the &BODY forms that follow the SEND.

To illustrate, suppose we have an Actor named PKEY-DATABASE which holds a repository of cryptographic keys indexed by some identifier. Then the β-form shown here, and with contours drawn around related expressions:

```
+-------------------------------------+
| (let ((id  "Server ID"))            |
+--------------------------------+    |
+------------+                   |    |
|  (β (pkey) |                   |    |
|  +---------+                   |    |
|  |  +--------------------------+    |
|  |  | (SEND PKEY-DATABASE β id)     |
|  |  +-------------------------------+
|  +---------------------------------------------------+
|    (do-something-with-the-returned-PKEY-item pkey))) |
+------------------------------------------------------+
```

The grouping of the expressions shows that the LET binding and the SEND are related by being executed, in succession, by the same thread at runtime. After sending, the thread goes on to do whatever follows the β-form (usually nothing) and then exits. 

Notice that we explicitly stated β in customer position of the SEND message. 

_[And remember that, since we have Transactional Hewitt Actors, no SENDs actually happen until we exit successfuly.]_

Meanwhile the message gets delivered to the database service, and that service sends its lookup result to the customer Actor (here the anonymous β Actor). So the second grouping shows that anonymous β-Actor. The arglist and body are grouped together, analogous to a λ-form. This anonymous β-Actor receives its message and gets executed by some arbitrary thread in the future.

Also, bear in mind that any bindings established in the scope of the SEND will also be seen by the behavior code of the anonymous β-Actor. Creating an anonymous Actor creates a lambda-closure for its behavior code.

---

Now, if you have a long succession of nested β-forms, your Editing indentation grows out of control. You can, instead, say the same thing using LET+ which keeps all of its binding clauses aligned to the left, and only its ultimate body forms get indented just once. 

E.g., the same code from above could be written as:
```
(LET+ ((id  "Server ID")
       (:β  (pkey)  (racurry PKEY-DATABASE id)))
   (do-something-with-the-returned-PKEY-item pkey))
```
The LET+ form has syntax:
```
(LET+ ((:β ARGS SERVICE)) &body body)
```
where SERVICE is an Actor argument that takes a single Customer argument - here, the Actor produced by a call to RACURRY. 

In this LET+ :β form, the customer is implied and not written explicitly. It will be provied as an anonymous β-Actor for the generated SEND.

When you have a target Actor that needs more arguments, as we have in this example, you can construct a single-argument Actor from it using the Actor analog to RCURRY, which is RACURRY. That packs up all the arguments as right-args, and leaves an Actor that expects at least one left-arg. This matches the expectations of our convention that customer Actors are always the first stated argument of any message.

Now one of the nice features of β-form syntax is that you can easily draw mental boundaries around the things that belong in the currently executing thread, like we did explicitly above, as distinct from those that will be executed in the future if a message is sent back to the anonymous β-Actor. 

But when you use LET+ with :β bindings, you lose this easy distinction. Just know that anything that follows the LET+ :β binding will only be executed, at some time in the future, if the anonymous β-Actor in that binding receives its message. And that also holds for successive LET+ bindings that may follow.

---

One important thing to remember, is that SELF always refers to the receiver Actor of the current message. That's why you see so much of my code making a copy for later use:
```
(let ((ME  SELF))
  (β  (...)
       (SEND AnActor β ...)
     ...
     (SEND ME ....)))
```

If, instead of sending to ME, I had mistakenly sent to SELF, in that final SEND, then that message would have gone right back to the anonymous β-Actor currently executing.

--- 

So, in summary, β-Forms look like MULTIPLE-VALUE-BIND, and they create anonymous Actors bound to lexical symbol β inside the β-form. These are the Actor equivalent of Lambda expressions. _[And yes, you can spell it out as BETA.]_

---


Understanding LET+
---

Lisp has so many different binding forms: LET, LET*, DESTRUCTURING-BIND, MULTIPLE-VALUE-BIND, SYMBOL-MACROLET, etc. Using them makes your code look like a veritable zoo of binding and cascading indentations. 

LET+ attempts to unify their syntax. Semantically, a succession of LET+ bindings is similar to LET*. Each binding completes before performing the next.

But LET+ can be used to subsume the variety into a compact, non-cascading, form.

```
(LET+ (bindings*) forms*)
```
where * denotes zero or more-of.

And for bindings we have:
```
(symbol    value-form)              -- same as LET*
(cons-form value-form)              -- implied DESTRUCTURING-BIND, so cons-form can represent an arbitrary tree.
(:β        args-form  service-form) -- implied β-Form
(:BETA     args-form  service-form) -- same as (:β )
(:MVB      arg-list   values-form)  -- implied MULTIPLE-VALUE-BIND
(:MVL      list-form  values-form)  -- implied MULTIPLE-VALUE-LIST [1]
(:ACC      (acc-form*) value-form)  -- implied WITH-ACCESSORS [2]
(:SLOTS    (slot-form*) value-form) -- implied WITH-SLOTS [3]
(:SYM      (sym-form*))             -- implied SYMBOL-MACROLET
(:FN       (fn-form*))              -- implied LABELS
(:MAC      (macrolet-form*))        -- implied MACROLET
(:DCL      (declaration*))          -- implied (LOCALLY (DECLARE ...) ...)
(:DB       arg-list property-list)  -- implied (apply (LAMBDA arg-list ...) property-list), as in "mini-database"
(:PAR      args-list expr-list)     -- implied parallel execution via β-Form with Fork-Join send.
```
Notes:

[1] Converts to (LET+ ((list-form (MULTIPLE-VALUE-LIST values-form))) ...), so if list-form is a symbol then it gets the list result of MULTIPLE-VALUE-LIST. If list-form is a cons, then we have a DESTRUCTURING-BIND on the list returned from MULTIPLE-VALUE-LIST.

[2] If acc-form is a symbol then it must be the name of the accessor. If acc-form is a pair, then the first must be a symbol that will become the name of the binding and the second must be the symbol naming the accessor. This is the same as for WITH-ACCESSORS.

[3] If slot-form is a symbol then it must be the name of the slot. If slot-form is a pair, then the first must be a symbol that will become the name of the binding and the second must be the symbol naming the slot. This is the same as for WITH-SLOTS.
