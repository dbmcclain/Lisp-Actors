;; sync-msg.lisp -- Composable Synchronous Events in Actors (AKA Reppy Channels)
;;
;; This is amazing! In about a page of code we can reproduce the essentials of Reppy Channels, using Actors!
;; Reppy Channels are composable synchronous rendezvous events, and Actors are completely asynchronous.
;;
;; DM/RAL  02/22
;; -----------------------------------------

(in-package :ac)

;;------------------------------------------
;; Comoposable Synchronous Events in Actors

(defconstant cancellation (list 'cancellation))

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

   ((cancel?) / (eq cancel? cancellation)
    (send cust cancellation)
    (become (sync-chan-beh)))
   ))

(defun put-waiting-beh (sender msg)
  (alambda
   ((cust :get)
    (send sender :ok)
    (send* cust msg)
    (become (sync-chan-beh)))

   ((cancel?)  / (eq cancel? cancellation)
    (send sender cancellation)
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

(defun cancel (chan)
  (send (chan-ctrl chan) cancellation))

;; ------------------------------------------
;; Composable Events - Actors that expect a customer to trigger their actions

(defun recv-evt (chan)
  (make-actor
   (alambda
    ((:reset)
     (cancel chan))
    
    ((cust)
     (send (chan-read chan) cust :get))
    )))

(defun send-evt (chan &rest msg)
  (make-actor
   (alambda
    ((:reset)
     (cancel chan))
    
    ((sender)
     (send* (chan-write chan) sender :put msg))
    )))

(defun wrap-evt (evt actor)
  (make-actor
   (alambda
    ((:reset)
     (send evt :reset))
    
    ((cust)
     (β  ans
         (send evt β)
       (if (eq (car ans) cancellation)
           (send cust cancellation)
         (send* actor cust ans))))
    )))

(defun wrap-abort-evt (evt actor)
  (make-actor
   (alambda
    ((:reset)
     (send evt :reset))
    
    ((cust)
     (β  ans
         (send evt β)
       (cond ((eq (car ans) cancellation)
              (send actor)
              (send cust cancellation))
             
             (t
              (send* cust ans))
             )))
    )))
          
(defun on-evt (evt actor)
  (make-actor
   (alambda
    ((:reset)
     (send evt :reset))

    ((cust)
     (β  ans
         (send evt β)
       (cond ((eq (car ans) cancellation)
              (send cust cancellation))
             (t
              (send actor)
              (send* cust ans))
             )))
    )))

(defun choose-evt (&rest evts)
  (make-actor
   (alambda
    ((:reset)
     (send-to-all evts :reset))
    
    ((cust)
     (labels ((rcvr-beh (ct)
                (λ ans
                  (cond ((eq (car ans) cancellation)
                         (let ((new-ct (1- ct)))
                           (if (zerop new-ct)
                               (send cust cancellation)
                             (become (rcvr-beh new-ct)))
                           ))
                        
                        (t
                         (become (sink-beh))
                         (send-to-all evts :reset)
                         (send* cust ans))
                        ))))
       (let ((rcvr (make-actor (rcvr-beh (length evts)))))
         (send-to-all evts rcvr)
         ))))))

(defun timeout-evt (dt evt)
  (make-actor
   (alambda
    ((:reset)
     (send evt :reset))
    
    ((cust)
     (let ((kill (α ans
                   (become (sink-beh))
                   (send evt :reset)
                   (send* cust ans))
                 ))
       (send evt kill)
       (send-after dt kill cancellation)
       ))
    )))

;; -----------------------------------------
;; Trigger an event, evaluate the event graph, by sending the customer

(defun sync (evt cust)
  (send evt cust))

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
  (sleep 2)
  (sync (timeout-evt 2
                     (choose-evt (send-evt ch3 3)
                                 (send-evt ch2 2)
                                 (send-evt ch1 1)))
        println))

|#