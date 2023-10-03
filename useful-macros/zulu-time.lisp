;; zulu-time.lisp
;;
;; DM/RAL  2023/10/02 11:00:19
;; ----------------------------------

(defpackage #:com.ral.zulu-time
  (:use #:common-lisp #:um))

(in-package #:com.ral.zulu-time)

;; ----------------------------------

(defun zulu-date-string (&optional (zulu-time (get-universal-time)))
  ;; we have to over-correct because decode thinks we are local time
  (multiple-value-bind (sec min hr date mon yr day daylight-p zone)
      (decode-universal-time zulu-time 0)
    (declare (ignore daylight-p day zone))
    (format nil "~{~2,'0D~^/~} ~{~2,'0D~^\:~} UTC"
            (list yr mon date)
            (list hr min sec))
    ))

#|
(zulu-date-string 0) => "1900/01/01 00:00:00 UTC" 
 |#