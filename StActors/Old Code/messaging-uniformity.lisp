
;; A message should always be a list. Some handlers expect a single list
;; argument, others expect a message as &rest args. Some senders send
;; lists, others send naked single item messages - even if the single
;; item is really a list. What to do..?

(in-package :actors-base)

;; ---------------------------------------------
;;
;; Try to remove ambiguity from messages being sent.  Be explicit, or
;; allow SEND to assume multi-args are the elements of an intended
;; message.
#|
(defstruct msg
  args)

(defun message-list (args)
  ;; attempt to return a list containing the actual message args
  #F
  (cond ((null args) nil) 
        
        ((consp args)
         (cond ((msg-p (car args)) ;; the only unambiguous case
                (when (cdr args)
                  ;; if user went to trouble forming a MSG struct,
                  ;; then this is probably an error.
                  (error "Ill-formed message list: ~A" args))
                (msg-args (car args)))

               (t ;; an ambiguous case
                  ;; Is this a single argument, or a list of arguments?
                  ;; always better to be clear by using MESSAGE
                  ;; we assume it is a list of message args, not a single argument
                  args)
               ))

        ((msg-p args)
         (msg-args args))
        
        (t (list args))
        ))

(defun message (&rest args)
  ;; being explicit - idempotent even if args is already a singleton
  ;; list with a MSG arg
  (make-msg
   :args (message-list args)))
|#
;; ----------------------------------------------------------
;; General Case - FUNCALL/APPLY
#|
(defmethod has-rest-args-p ((fn function))
  #F
  (intersection '(&rest &key &optional)
                    (the list (lw:function-lambda-list fn))))
|#

(defun singleton (lst)
  #F
  (and (consp lst)
       (null (cdr (the cons lst)))))

(defun single-arg-p (fn)
  (declare (function fn))
  (singleton (lw:function-lambda-list fn)))

(defmethod funcall-adapter ((fn function))
  #F
  ;; identity for functions that expect a single list argument
  (cond ((single-arg-p fn) fn)
        
        ;; When fn has < 1 or > 1 arg, or &REST args
        ;; Offering the wrong number of args in the message will
        ;; generate an error for us.
        (t
         (lambda (msg)
           (apply fn msg)))
        ))

(defmethod apply-adapter ((fn function))
  #F
  ;; identity for functions that expect a &REST list
  (cond ((single-arg-p fn)
         (lambda (&rest message)
           (funcall fn message)))

        ;; When fn has &REST args, or else a fixed number of positional args < 1 or > 1.
        ;; If message has wrong number of components, the subsequent APPLY will produce
        ;; an error for us.
        (t  fn)
        ))
