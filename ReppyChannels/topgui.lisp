;; topgui.lisp -- Convenience defs for toplevel application GUI's
;; These helpers allow toplevel application GUI's to either run within
;; an existing session, or to startup and shutdown properly when run as
;; standalone applications.
;;
;; DM/MCFA  10/01
;; ----------------------------------------------------

(in-package "TOPGUI")

(defclass <toplevel-app-interface> (capi:interface)
  ((app-killch
    :accessor app-killch
    :initarg :app-killch
    :initform nil)))

(defmethod initialize-instance :after ((self <toplevel-app-interface>)
                                       &rest args
                                       &key &allow-other-keys)
  (declare (ignore args))
  (let ((dcb (capi:interface-destroy-callback self)))
    (setf (capi:interface-destroy-callback self)
          #'(lambda (intf)
              (let ((ch (app-killch intf)))
                (when dcb
                  (funcall dcb intf))
                (when ch
                  (rch:poke ch t))))
          )))


(defmacro define-toplevel-app-interface (name superclasses &rest rest)
  ;; put <toplevel-app-interface> superclass as last one so that
  ;; all other superclasses get to establish the destroy-callback before
  ;; the final wrapup provided by <toplevel-app-interface>.
  `(capi:define-interface ,name (,@superclasses <toplevel-app-interface>)
     ,@rest))

(defun run-interface (intf before after)
  (when before
    (funcall before intf))
  (capi:display intf)
  (when after
    (funcall after intf)))
    
(defun run-toplevel-app-interface-with-killch (intfname before after)
  (let* ((ch   (rch:make-channel))
         (intf (make-instance intfname
                              :app-killch ch)))
    (run-interface intf before after)
    (rch:sync (rch:wrap (rch:recvEvt ch)
                        (lambda (_)
                          (declare (ignore _))
                          (lw:quit :status 0)))
              )))
  
(defun run-toplevel-app-interface (intfname &key before after)
  (if (mp:list-all-processes)
      (let ((intf (make-instance intfname)))
        (run-interface intf before after)
        intf)
    (progn
      (push `("Toplevel App Launcher"
              nil
              run-toplevel-app-interface-with-killch
              ,intfname ,before ,after)
            mp:*initial-processes*)
      (mp:initialize-multiprocessing))
    ))

#|
;; Example of use....

(topgui:define-toplevel-app-interface interface-1 ()
  ((image  :accessor interface-image :initform nil))  ;; slots
  (:panes
   .... ;; etc just like capi:define-interface
   ))

(defun doit ()
  (labels
      ((before (intf)
               .... ;; whatever you want after intf is created but before being displayed
               )
       (after (intf)
              ... ;; whatever you want after intf is created and displayed
              ))

    (topgui:run-toplevel-app-interface 'interface-1
                                       :before #'before
                                       :after #'after)))

|#

