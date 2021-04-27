
(in-package :actors-globals)

;; ----------------------------------------------------------------------------

(defconstant +nbr-execs+          4)
(defconstant +max-pool+         100)
(defvar *executive-processes*   nil)
(defvar *executive-counter*       0)

(defconstant +heartbeat-interval+ 1)
(defconstant +maximum-age+        3)
(defvar *heartbeat-timer*       nil)
(defvar *last-heartbeat*          0)

;; --------------------------------------------------------------------

(defvar *actor-ready-queue*  (make-prio-mailbox))

(defvar *current-actor*      nil)

(defun current-actor ()
  *current-actor*)

;; --------------------------------------------------------------------

(defvar *actor-directory-manager* #'lw:do-nothing)

;; --------------------------------------------------------------------

(defun blind-print (cmd &rest items)
  (declare (ignore cmd))
  (dolist (item items)
    (print item)))

(defvar *shared-printer-actor*    #'blind-print)


