;; sync-msg.lisp -- Composable Synchronous Events in Actors (AKA Reppy Channels)
;;
;; This is amazing! In about a page of code we can reproduce the
;; essentials of Reppy Channels, using Actors!  Reppy Channels are
;; composable synchronous rendezvous events, and Actors are completely
;; asynchronous. This is a parallel, concurrent, lock-free, FPL
;; design, written with a fraction of the effort.
;;
;; DM/RAL  02/22
;; -----------------------------------------
;; Be mindful that information flows in two directions among these
;; composed event networks. Control signals, (:SYNC and :RESET) flow
;; into the network, while channel responses flow back out of the
;; network and into the customer Actor.
;;
;; The ultimate sink, for both senders and receivers, is generally a
;; Channel - a rendezvous point.
;;
;; In general, we should make sure to allow information to flow back
;; to customer only once. And we need to protect against multiple
;; logical readers or writers from entering a rendezvous. So we make
;; reader and writer ports to channels go throughn a serializer, and
;; many wrapper events have Once protection on information flowing
;; back to the customer.
;;
;; Customer Actors should be prepared to handle rendezvous failures in
;; addition to normal traffic - at both ends, sender and receiver.
;;
;; Events, E, (here Actors) are produced by mappings f: f(E) -> E,
;; with composition defined by, f • g = f(g(E)).
;;
;; SEND-EVT        : CHAN -> E
;; RECV-EVT        : CHAN -> E
;; WRAP-EVT        : E -> E
;; WRAP-ABORT-EVT  : E -> E
;; ON-EVT          : E -> E
;; CHOOSE-EVT      : E+ -> E
;; TIMEOUT-EVT     : E -> E

(in-package :ac)

;;------------------------------------------
;; Comoposable Synchronous Events in Actors

(defconstant +fail+ (list 'rendezvous-failure))

(defun sync-chan-beh ()
  (alambda
   ((cust :get)
    (become (get-waiting-beh cust)))
   
   ((sender :put . msg)
    (become (put-waiting-beh sender msg)))
   ))

(defun get-waiting-beh (cust)
  (alambda
   ((sender :put . msg)
    (send sender :ok)
    (send* cust msg)
    (become (sync-chan-beh)))

   ((:reset)
    (send cust +fail+)
    (become (sync-chan-beh)))
   ))

(defun put-waiting-beh (sender msg)
  (alambda
   ((cust :get)
    (send sender :ok)
    (send* cust msg)
    (become (sync-chan-beh)))

   ((:reset)
    (send sender +fail+)
    (become (sync-chan-beh)))
   ))

;; -------------------------------------------------------
;; Synchronous Rendezvous Channels

(defstruct chan
  read write ctrl)
            
(defun chan ()
  (let ((ch (make-actor (sync-chan-beh))))
    (make-chan
     :read  (serializer ch)
     :write (serializer ch)
     :ctrl  ch)
    ))

;; ------------------------------------------
;; Composable Events - Actors that expect a (customer :sync) to trigger their actions

(defun recv-evt (chan)
  ;; Define the receive side of a rendezvous event
  (make-actor
   (alambda
    ((:reset)
     (send (chan-ctrl chan) :reset))
    
    ((cust :sync)
     (send (chan-read chan) cust :get))
    )))

(defun send-evt (chan &rest msg)
  ;; Define the send side of a rendezvous event
  (make-actor
   (alambda
    ((:reset)
     (send (chan-ctrl chan) :reset))
    
    ((cust :sync)
     (send* (chan-write chan) cust :put msg))
    )))

(defun wrap-evt (evt actor)
  ;; on successful rendezvous, filter the transferred data through an
  ;; Actor on the way back to customer.
  (make-actor
   (alambda
    ((:reset)
     (send evt :reset))
    
    ((cust :sync)
     (β  ans
         (send evt (once β) :sync)
       (if (eq (car ans) +fail+)
           (send cust +fail+)
         (send* actor cust ans))))
    )))

(defun wrap-abort-evt (evt actor)
  ;; On a failed rendezvous, call on Actor
  (make-actor
   (alambda
    ((:reset)
     (send evt :reset))
    
    ((cust :sync)
     (β  ans
         (send evt (once β) :sync)
       (cond ((eq (car ans) +fail+)
              (send actor)
              (send cust +fail+))
             
             (t
              (send* cust ans))
             )))
    )))
          
(defun on-evt (evt actor)
  ;; On a successful rendezvous, send the transferred data to the
  ;; customer, and also perform Actor
  (make-actor
   (alambda
    ((:reset)
     (send evt :reset))

    ((cust :sync)
     (β  ans
         (send evt (once β) :sync)
       (cond ((eq (car ans) +fail+)
              (send cust +fail+))
             (t
              (send actor)
              (send* cust ans))
             )))
    )))

;; ---------------------------

(defun choose-evt (&rest evts)
  ;; Make a collection of events into a single event, If any one of
  ;; the collecton has a successful rendezvous we send the transferred
  ;; data back to customer, and cancel the rendezvous attempts on the
  ;; other events in the collection. If some event in the collection
  ;; has a failed rendezvous, we ignore that until all of them have
  ;; failed.
  (make-actor
   (alambda
    ((:reset)
     (send-to-all evts :reset))
    
    ((cust :sync)
     (labels ((rcvr-beh (ct)
                (λ ans
                  (cond ((eq (car ans) +fail+)
                         (let ((new-ct (1- ct)))
                           (cond ((zerop new-ct)
                                  (become (sink-beh))
                                  (send cust +fail+))
                                 (t
                                  (become (rcvr-beh new-ct)))
                                 )))
                        
                        (t
                         (become (sink-beh))
                         (send-to-all evts :reset)
                         (send* cust ans))
                        ))))
       (let ((rcvr (make-actor (rcvr-beh (length evts)))))
         (send-to-all evts rcvr :sync)
         ))))))

;; ---------------------------

(defun timeout-evt-beh (dt evt &optional tag)
  ;; Wrap an event with a timeout watchdog. If the timeout occurs
  ;; before a successful rendezvous or a rendezvous failure, we cancel
  ;; the rendezvous attempt and send failure back to the customer.
  (alambda
   ((cust :sync)
    (let ((kill (once
                 (α ans
                   (send evt :reset)
                   (send* cust ans)))))
      (become (timeout-evt-beh dt evt kill))
      (send evt kill :sync)
      (send-after dt kill +fail+)))
   
   ((:reset)
    (send tag +fail+))
   ))
    
(defun timeout-evt (dt evt)
  (make-actor (timeout-evt-beh dt evt)))

;; -----------------------------------------
;; Trigger an event, evaluate the event graph, by sending the customer

(defun sync (evt cust)
  ;; Trigger an event network, sending rendezvous data or failure
  ;; ultimately on to customer.
  (send evt (once cust) :sync))

;; -----------------------------------------
#|
(let ((ch (chan)))
  (sync (recv-evt ch) println)
  (sync (send-evt ch 1) println))

(let ((ch1 (chan))
      (ch2 (chan))
      (ch3 (chan)))
  (sync (timeout-evt 2
                     (choose-evt (recv-evt ch1)
                                 (recv-evt ch2)
                                 (recv-evt ch3)))
        println)
  (sleep 1)
  (sync (timeout-evt 2
                     (choose-evt (send-evt ch3 3)
                                 (send-evt ch2 2)
                                 (send-evt ch1 1)))
        println))

|#