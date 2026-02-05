;; actor-state.lisp -- Alternative to BOA lists of state vars
;; DM/RAL  2023/12/07 10:08:39 UTC
;; ----------------------------------------------------------

(in-package :com.ral.actors.base)

;; ----------------------------------------------------------
;; What problem is this code trying to solve?
;;
;; Well, when you have a large number of state vars for an Actor
;; behavior, the use of BOA arglists (By Order of Argument) becomes
;; cumbersome to use. CommonLisp has keyword parameters in lambda
;; lists. But these suffer from two problems:
;;
;;   1. Any changes to arglists are usually done by pre-pending new
;;   keyword args in front of existing args. That leaves stale data
;;   laying around in addition to redefined values. -- and --
;;
;;   2. That stale data occupies memory preventing GC reclamation.
;;
;; So here we define ACTOR-STATE with clean augmentation semantics.
;;
;; Declare an ACTOR-STATE in your behavior code, stating the items by
;; name. Keep in mind that Actor state is visible to, and shared by,
;; all parallel executions.
;;
;; Use STATE-WITH to construct a fresh state from an existing state,
;; copying existing content, replacing old values by name, and
;; augmenting state with additonal values.
;;
;; Use STATE-WITHOUT to construct a fresh state from an existing
;; state, eliding keys and their values.
;;
;; These actions allows old values to be taken by GC.
;;
;; Pass existing state, in whole, to other functions, such as used in
;; CREATE for defining new behavior.
;;
;; We offer WITH-STATE-VALS as a convenient way to refer to the actual
;; state vars in the property list - much like WITH-ACCESSORS.
;;
;; You can also set default values in case the names were not already
;; in the plist. All of the accessor names are read-only.
;;
;; -------------------------------------------------------------------
;; No need for ACTOR-STATE... Just use PLists.
#|
(defun trim-plist (plist removals)
  (loop for key in removals do
          (remf plist key))
  plist)

(defun augment-plist (plist &rest props)
  (let* ((new-plist (copy-list plist))
         (elisions  (getf props :without new-plist)))
    (unless (eq elisions new-plist)
      (setf new-plist (trim-plist new-plist (um:mklist elisions))
            props     (copy-list props))
      (remf props :without))
    (loop for (key val) on props by #'cddr do
            (setf (getf new-plist key) val))
    new-plist))
|#
;; --------------------------------------------
;; Purely Functional PLIST operations.
;;
;; Try to be efficient, avoid copying entire PLIST if possible.
;; Is this really a speedup? Perhaps COPY-LIST is actually fast?

(defun plist-get (lst key &optional default)
  (getf lst key default))

(defun plist-put (lst &rest kv-args)
  ;; Add or change a PLIST binding. Returns a new list without mutating.
  (um:nlet iter ((tl  lst)
                 (hd  nil))
    (cond
     ((null kv-args)
      (append hd tl))
     ((null tl)
      (append hd kv-args))
     (t
      (let* ((key (car tl))
             (val (getf kv-args key #'putter)))
        (cond
         ((eq #'putter val)
          (go-iter (cddr tl) (list* key (cadr tl) hd)))
         (t
          (remf kv-args key)
          (go-iter (cddr tl) (list* key val hd)))
         )))
     )))

(defun plist-remove (lst &rest keys)
  ;; Remove entries from a PLIST. Returns a new list without mutating.
  (um:nlet iter ((tl  lst)
                 (hd  nil))
    (cond
     ((null keys)
      (append hd tl))
     ((null tl)
      hd)
     (t
      (let ((key (car tl)))
        (cond
         ((find key keys)
          (delete key keys)
          (go-iter (cddr tl) hd))
         (t
          (go-iter (cddr tl) (list* key (cadr tl) hd)))
         )))
     )))

;; --------------------------------------------

(defun augment-plist (plist &rest props)
  (let ((elision-keys (getf props :without)))
    (when elision-keys
      (remf props :without)
      (setf plist (apply #'plist-remove plist (um:mklist elision-keys))))
    (apply #'plist-put plist props)))
  
(defmethod um:with ((state list) &rest props)
  ;; State LIST is assumed to be a property list of alternating keywords and values.
  (apply #'augment-plist state props))

;; --------------------------------------------

(defmacro dictionary-bind (kw-args dict &body body)
  ;; Because we are providing a function arglist of &KEY args, this
  ;; also supports default values for missing items, as well as
  ;; present-p predicates.
  `(apply (lambda (&key ,@kw-args &allow-other-keys)
            ,@body)
          ,dict))

(defmethod do-let+ ((fst (eql :db)) list-form form)
  `(dictionary-bind ,(cadr list-form) ,(third list-form) ,form))

;; --------------------------------------------
#|
(defstruct actor-state
  (plist nil :read-only t))

(defun validate-plist (plist)
  (loop for key in plist by #'cddr do
          (assert (symbolp key)))
  plist)

(defun actor-state (&rest plist)
  (make-actor-state
   :plist (validate-plist plist)))

(defmethod state-with ((state actor-state) &rest props)
  (apply #'actor-state
         (apply #'augment-plist (actor-state-plist state) props)))

(defmethod state-without ((state actor-state) &rest removals)
  (let ((new-plist  (copy-list (actor-state-plist state))))
    (apply #'actor-state (trim-plist new-plist removals))
    ))

(defmethod state-val ((state actor-state) (key symbol) &optional default)
  (getf (actor-state-plist state) key default))

(defmacro with-state-vals (bindings state &body body)
  ;; bindings can include a default val following the plist symbol
  (let ((gstate (gensym)))
    `(let ((,gstate ,state))
       (symbol-macrolet 
           ,(mapcar #`(,(car a1) (state-val ,gstate ,@(cdr a1))) bindings)
         ,@body))
    ))

(defmethod um:with ((state actor-state) &rest props)
  ;; WITH state props [:WITHOUT (prop-name | list-of-prop-names)] -> new-state
  (apply #'state-with state props))

(defmacro with-actor-state (name &body body)
  ;; Let's try something a bit different...
  ;;
  ;; Declare an actor state binding which can then be used as a
  ;; function with various behaviors... namely, selecting content, and
  ;; producing a fresh state with modified bindings.
  ;;
  ;; E.g.,
  ;;
  ;;    (defun beh (STATE)
  ;;      (WITH-ACTOR-STATE STATE
  ;;        (alambda
  ;;           ((item) /. (eql item (STATE :item))
  ;;                   ...
  ;;                   (become (beh (STATE WITH
  ;;                                       :item new-item)))
  ;;         ))))
  ;;
  ;;  So WITH-ACTOR-STATE turns a plist binding into an intelligent plist.
  ;;  (Thanks go to Lisp for having separate function and value namespaces for each symbol!)
  ;;
  `(macrolet ((,name (cmd &rest args)
                (case cmd
                  (with
                      `(with ,',name ,@args))
                  (otherwise
                   `(state-val ,',name ,cmd ,@args))
                  )))
     ,@body))

#+:LISPWORKS
(editor:setup-indent "with-actor-state" 1)

;; --------------------------------------------

|#
