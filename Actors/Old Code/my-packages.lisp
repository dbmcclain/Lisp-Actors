
(defpackage #:linda
  (:use #:common-lisp #:ac)
  (:import-from #:um
   #:if-let
   #:when-let
   #:foreach
   #:nlet
   #:nlet-tail
   #:group
   #:dlambda
   #:dlambda*
   #:curry
   #:rcurry
   #:defmacro!
   #:accum)
  (:export
   #:*linda*
   #:make-ts
   #:out
   #:rd
   #:in
   #:rdp
   #:inp
   #:outb
   #:rdb
   #:rdbp
   #:remove-bindings
   #:on-in
   #:on-inp
   #:on-rd
   #:on-rdp
   #:on-rdb
   #:on-rdbp
   #:remove-all-bindings
   #:remove-all-tuples
   #:reset
   #:srdp
   #:sinp
   #:srdbp
   #:remove-tuples
   #:remote-srdp
   #:remote-sinp
   #:remote-out
   #:remote-outb
   #:remote-srdbp
   #:remote-remove-bindings
   ))

