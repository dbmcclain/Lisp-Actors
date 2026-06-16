;; zulu-time.lisp
;;
;; DM/RAL  2023/10/02 11:00:19
;; ----------------------------------

(defpackage #:com.ral.zulu-time
  (:use #:common-lisp #:um))

(in-package #:com.ral.zulu-time)

;; ----------------------------------

(defun local-date-string (&optional (local-time (get-universal-time)))
  ;; decode implicitly decodes to local time
  (multiple-value-bind (sec min hr date mon yr day daylight-p zone)
      (decode-universal-time local-time)
    (declare (ignore daylight-p day))
    (format nil "~{~2,'0D~^/~}T~{~2,'0D~^\:~}U~@d"
            (list yr mon date)
            (list hr min sec)
            (- zone))
    ))

(defun zulu-date-string (&optional (zulu-time (get-universal-time)))
  ;; we have to over-correct because decode thinks we are local time
  (multiple-value-bind (sec min hr date mon yr day daylight-p zone)
      (decode-universal-time zulu-time 0)
    (declare (ignore daylight-p day zone))
    (format nil "~{~2,'0D~^/~}T~{~2,'0D~^\:~}U"
            (list yr mon date)
            (list hr min sec))
    ))

#|
(zulu-date-string 0) => "1900/01/01T00:00:00U"
(local-date-string 0) => "1899/12/31T17:00:00U-7"

(local-date-string) => "2026/06/10T02:22:56U-7"
(zulu-date-string)  => "2026/06/10T09:23:13U"
 |#