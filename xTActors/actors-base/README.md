Understanding β-Forms
---
β is a macro form analogous to MULTIPLE-VALUE-BIND. Following the opening β we have an argument list, a SEND form, and a macro body of one or more Sexprs. 

Its format is similar to that of MULTIPLE-VALUE-BIND, but in place of a VALUES-generating form or function call, the SEND form sends a message to some Actor, with β as a Customer arg in the message. 

You are telling the Actor to send its results to β as the customer in the message, and that β actually is an unnamed Actor which expects a message containing the arguments shown in the β-form, and whose behavior body is made from the body forms that follow the SEND.

Suppose we have an Actor named PKEY-DATABASE which holds a repsitory of cryptographic keys indexed by some identifier. Then the β-form shown here, and with contours drawn around related expressions:

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
|  +----------------------------------------------+
|    (do-something with the returned PKEY item))) |
+-------------------------------------------------+
```

The grouping of the expressions shows that the LET binding and the SEND are related by being executed, in succession, by the same thread at runtime. After sending, the thread goes on to do whatever follows the β-form (usually nothing) and then exits. 

Meanwhile the message gets delivered to the database service, and that service sends its lookup result to the customer Actor (here the anonymous β Actor). So the second grouping shows that anonymous β-Actor. The arglist and body are grouped together, analogous to a λ-form.

