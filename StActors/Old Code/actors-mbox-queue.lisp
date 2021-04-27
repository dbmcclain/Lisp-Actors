
(in-package :actors-base)

;; --------------------------------------------------------------------
;; NOTE: MP:MAILBOX supports multiple readers, not just multiple
;; writers. So use a mailbox for the FIFO Actor ready queue.

(defun not-already-in-queue-p (actor)
  ;; return true if not in queue, but also mark it as being in the
  ;; ready queue if it wasn't
  (sys:compare-and-swap (car (actor-running-p-ref actor)) nil t))

(defun mark-not-in-queue (actor)
  (sys:compare-and-swap (car (actor-running-p-ref actor)) t nil))

(defmethod add-to-ready-queue ((actor actor))
  (cond ((not (has-behavior-p actor))
         (mark-not-in-queue actor))
        
        ((not (has-messages-p actor))
         ;; <-- a message could arrive right here
         (mark-not-in-queue actor)
         (when (has-messages-p actor)
           (add-to-ready-queue actor)))

        ((not-already-in-queue-p actor)
         (mailbox-send *actor-ready-queue* actor
                       :prio (actor-priority actor))
         (unless *executive-processes*
           (ensure-executives)))
        ))

(defun ready-queue-empty-p ()
  (mailbox-empty-p *actor-ready-queue*))

(defun empty-ready-queue ()
  (let ((old-mb  (sys:atomic-exchange *actor-ready-queue*
                                      (make-prio-mailbox))))
    ;; nudge any Executives waiting on the queue to move over to the
    ;; new one.
    (mapc (lambda (proc)
            (declare (ignore proc))
            (mailbox-send old-mb nil))
          *executive-processes*)
    ))

(defun pop-ready-queue ()
  (mailbox-read *actor-ready-queue*))

