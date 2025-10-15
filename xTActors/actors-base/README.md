Understanding β-Forms
---
β is a macro form analogous to MULTIPLE-VALUE-BIND. Following the opening β we have an argument list, a SEND form, and a macro body of one or more Sexprs. Its format is similar to that of MULTIPLE-VALUE-BIND, but in place of a VALUES-generating form or function call, the SEND form sends a message to some Actor, with β as a Customer arg in the message. You are telling the Actor to send its results to β as the customer in the message, and that β actually is an unnamed Actor which expects a message containing the arguments shown in the β-form, and whose behavior body is made from the body forms that follow the SEND.

