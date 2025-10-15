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

The grouping of the expressions shows that the LET binding and the SEND are related by being executed, in succession, by the same thread at runtime. After sending, the thread goes on to do whatever follows the β-form (usually nothing) and then exits. _[Remember that, since we have Transactional Hewitt Actors, no SENDS actually happen until we exit successfuly.]_

Meanwhile the message gets delivered to the database service, and that service sends its lookup result to the customer Actor (here the anonymous β Actor). So the second grouping shows that anonymous β-Actor. The arglist and body are grouped together, analogous to a λ-form. This anonymous β-Actor receives its message and gets executed by some arbitrary thread in the future.

---

Now, if you have a long succession of nested β-forms, your indentation grows out of control. You can, instead, say the same thing using LET+ which keeps all of its binding clauses aligned to the left, and only its ultimate body forms get indented just once. 

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
where SERVICE is an Actor argument that takes a single Customer argument - here, the anonymous β-Actor. 

When you have an Actor that actually needs more arguments, as we have in this example, you can construct a single-argument Actor from it using the Actor analog to RCURRY, which is RACURRY. That packs up all the arguments as right-args, and leaves an Actor that expects only a single lef-arg.

Now one of the nice features of β-form syntax is that you can easily draw mental boundaries around the things that belong in the currently executing thread, as distinct from those that will be executed in the future if a message is sent back to the anonymous β-Actor. 

But when you use LET+ with :β bindings, you lose this easy distinction. Just know that anything that follows the LET+ :β binding will only be executed, at some time in the future, if the anonymous β-Actor in that binding receives its message.

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
