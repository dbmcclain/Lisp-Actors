;; globals.lisp
;;
;; DM/RAL  2022/12/14 06:20:16
;; ----------------------------------

(in-package #:com.ral.actors.base)

;; ----------------------------------

;; Per-Thread for Activated Actor
(defvar *current-actor*    nil) ;; Current Actor
(defvar *current-behavior* nil) ;; Current Actor's behavior
(defvar *current-message*  nil) ;; Current Event Message args
(defvar *current-message-frame*  nil) ;; Current Event Whole Message

(define-symbol-macro self         *current-actor*)
(define-symbol-macro self-beh     *current-behavior*)
(define-symbol-macro self-msg     *current-message*)

(mpc:defglobal *central-mail*  (mpc:make-mailbox :lock-name "Central Mail"))

(mpc:defglobal *nbr-pool*  8 )  ;; nbr threads in dispatch pool

(defconstant +ASK-TIMEOUT+  1)  ;; period of goal checking

