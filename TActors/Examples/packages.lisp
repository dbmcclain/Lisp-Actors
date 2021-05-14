
(defpackage #:rstkv-server
  (:use #:common-lisp #:actors)
  (:export
   #:*service-id*
   #:*writeback-delay*
   #:stkv-server
   #:open-trans
   #:get-database-key
   #:get-database-keys
   #:get-all-database-keys
   #:commit-trans
   #:revert-database
   #:save-database
   #:shutdown-server
   #:quit-server
   
   #:make-stkv-server
   ))

(defpackage #:rstkv-client
  (:use #:common-lisp #:actors)
  (:export
   #:open-trans
   #:get-database-key
   #:get-database-keys
   #:get-all-database-keys
   #:commit-trans
   #:revert-database
   #:save-database
   #:shutdown-server
   #:quit-server
   
   #:with-server
   #:*rstk-timeout*
   #:trans
   #:get-server
   #:rollback
   #:commit
   #:get-key
   #:get-keys
   #:get-all-keys
   #:set-key
   #:delete-key
   #:save
   #:revet
   ))

