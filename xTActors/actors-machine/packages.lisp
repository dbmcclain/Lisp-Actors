
(um:eval-always
  (project:defproject
   (#:actors-machine  #:com.ral.actors-machine)
   (#:am              #:actors-machine)))

(defpackage #:com.ral.actors-machine
  (:use #:common-lisp #:ac)
  (:export
   #:am-actor
   #:am-actor-beh
   #:send-message
   #:send-message*
   #:sending
   #:sending*
   #:becoming
   #:creating
   #:operating
   #:commit
   #:def-am-beh
   ))
