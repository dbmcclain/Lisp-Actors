;; non-serializable.lisp -- Interned Symbols as stand-ins for non-serializable objects
;;
;; DM/RAL 10/20
;; ---------------------------------------------------------------------------------

;; Non-serializable objects can be represented by a symbol whose value
;; is the object. That symbol can be serialized. But such
;; representations are ephemeral and last only for as long as the
;; object is alive, and at most, for the duration of a Lisp session.

(in-package #:non-serializable)

(defvar *ns-pkg*  (find-package :ns-sym))

(defun make-ns-sym (obj &optional (pref "obj")
  ;; make a new, unique, symbol in package NS-SYM, whose value is the
  ;; object obj
  (let* ((name (concatenate 'string pref #\-
                            (uuid:uuid-string (uuid:make-v1-uuid))))
         (sym (intern name *ns-pkg*)))
    (setf sym obj)
    sym))

(defun ns-discard (sym)
  ;; if SYM is interned in the NS-SYM package, then unintern it, and
  ;; also remove any value or function binding it may have had. This
  ;; breaks a cycle of reference to an otherwise soon-to-be dead
  ;; object that carries the SYM.
  (when (eq (symbol-package sym) *ns-pkg*)
    (makunbound sym)
    (if (fboundp sym)
        (fmakunbound sym))
    (unintern sym *ns-pkg*)))

;; NS-SYM - override in other packages to provide an NS-SYM whose
;; value is OBJ and which symbol is somehow recoded in the OBJ for
;; later removal when OBJ is discarded.
(defgeneric ns-sym (obj))
