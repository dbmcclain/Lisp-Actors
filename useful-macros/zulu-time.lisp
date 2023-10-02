;; zulu-time.lisp
;;
;; DM/RAL  2023/10/02 11:00:19
;; ----------------------------------

(defpackage #:com.ral.zulu-time
  (:use #:common-lisp #:um))

(in-package #:com.ral.zulu-time)

;; ----------------------------------

(defun get-zulu-time ()
  (multiple-value-bind (sec min hr date mon yr day daylight-p zone)
      (get-decoded-time)
    (declare (ignore sec min hr date mon yr day daylight-p))
    (+ (get-universal-time)
       (* 3600 zone))))

(defun zulu-date-string ()
  (format nil "~A UTC" (hcl:date-string (get-zulu-time))))
