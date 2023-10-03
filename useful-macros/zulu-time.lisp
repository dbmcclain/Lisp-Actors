;; zulu-time.lisp
;;
;; DM/RAL  2023/10/02 11:00:19
;; ----------------------------------

(defpackage #:com.ral.zulu-time
  (:use #:common-lisp #:um))

(in-package #:com.ral.zulu-time)

;; ----------------------------------

(defun get-zulu-time (&optional (universal-time (get-universal-time)))
  ;; The assumption here is that if you supply a time, it must have
  ;; originated on this system.
  (multiple-value-bind (sec min hr date mon yr day daylight-p zone)
      (get-decoded-time)
    (declare (ignore sec min hr date mon yr day daylight-p))
    (+ universal-time
       (* 3600 zone))))

#+:LISPWORKS
(defun zulu-date-string (&optional (universal-time (get-universal-time)))
  (format nil "~A UTC" (hcl:date-string (get-zulu-time universal-time))))

#-:LISPWORKS
(defun zulu-date-string (&optional (universal-time (get-universal-time)))
  (multiple-value-bind (sec min hr date mon yr day daylight-p zone)
      (decode-universal-time (get-zulu-time universal-time))
    (declare (ignore daylight-p day zone))
    (format nil "~{~2,'0D~^/~} ~{~2,'0D~^\:~} UTC"
            (list yr mon date)
            (list hr min sec))
    ))