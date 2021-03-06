
(in-package :core-crypto)

;; --------------------------------------------------------------------

#-(and :lispworks
       (not :windows))
(eval-when (:load-toplevel)
  (startup))

#+(and :lispworks
       (not :windows))
(eval-when (:load-toplevel)
  ;; 2 choices, if building-binary, don't load-dlls; else load-dlls
  ;; Cannot load-dlls during DELIVERY (since, multitasking not allowed during DELIVERY), must load-dlls later.
  ;; *performing-binary-build* is created in delivery.lisp, else it is not created and not BOUNDP

  ;; Trying to avoid the use of *features*.  We use a special, cl-user::*performing-binary-build*, set up
  ;; in emotiq/etc/deliver/deliver.lisp, then write Lisp code to decide which of the 2 cases to perform (at LOAD time).
  ;; This special is UNINTERNED in emotiq/src/startup.lisp/START.

  (let ((building-binary-p (boundp 'cl-user::*performing-binary-build*)))

    (format *standard-output* "~&building-binary-p ~A~&"
            building-binary-p)
    
    (unless building-binary-p
      ;; in all other cases, load-dlls at LOAD time.
      (startup))
    ))

