
(in-package :com.ral.actors.encoding.self-sync)

;; -----------------------------------------------------------------
;; Stream Decoding

(defun stream-decoder (dest)
  ;; Construct an Actor that absorbs chunks of self-sync encoded input
  ;; stream and which triggers events to the dest actor as completed
  ;; decodings arrive.
  ;;
  ;; Incoming packets (chunks of encodings) are delivered along with a
  ;; chronological sequence number. Packets can arrive in any order,
  ;; starting from 1.
  ;;
  (let* ((machine (self-sync:make-reader-fsm (um:curry #'send dest)))
         (fsm     (create
                   (behav (cust buf)
                     (map nil machine buf)
                     (send cust :next)))
                  ))
    (labels
        ((stream-decoder-beh (wait-ix queue)
           (alambda
            ((:deliver bufix buf)
             (cond ((eql bufix wait-ix)
                    (>> fsm self buf)
                    (β! (busy-stream-decoder-beh (1+ wait-ix) queue)))
                   
                   (t
                    (β! (stream-decoder-beh wait-ix (acons bufix buf queue))))
                   ))
            ))
         ;; ----------------------------
         (busy-stream-decoder-beh (wait-ix queue)
           (alambda
            ((:next)
             (let ((pair (assoc wait-ix queue)))
               (cond (pair
                      (>> fsm self (cdr pair))
                      (β! (busy-stream-decoder-beh (1+ wait-ix) (remove pair queue))))
                     (t
                      (β! (stream-decoder-beh wait-ix queue)))
                     )))
            
            ((:deliver bufix buf)
             (β! (busy-stream-decoder-beh wait-ix (acons bufix buf queue))))
            )) )
      
      (create (stream-decoder-beh 1 nil))
      )))

;; ---------------------------------------------------
;; For manual testing...

(defun decode (vec)
  ;; decoding (as a function) for self-contained encoded vectors
  (ask (create
        (behav (cust &rest msg)
          (let ((decoder (stream-decoder cust)))
            (>>* decoder msg))))
       :deliver 1 vec))

