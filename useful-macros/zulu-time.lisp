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

#+:LISPWORKS
(defun zulu-date-string ()
  (format nil "~A UTC" (hcl:date-string (get-zulu-time))))

#-:LISPWORKS
(defun zulu-date-string ()
  (multiple-value-bind (sec min hr date mon yr day daylight-p zone)
      (decode-universal-time (get-zulu-time))
    (declare (ignore daylight-p day zone))
    (format nil "~{~2,'0D~^/~} ~{~2,'0D~^\:~} UTC"
            (list (mod yr 100) mon date)
            (list hr min sec))
    ))