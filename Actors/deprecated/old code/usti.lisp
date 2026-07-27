
;; unambiguous Actor identification

Machine: ip-addr [port]
Session: Session-ID
Sponsor: Sponsor-addr
Actor: Actor-addr
USTI: Machine Session Sponsor-addr Actor-addr

Machine: Arroyo -> Arroyo.local 65001
Session:

(defun session-id? ()
  (sys::getpid))

(defvar *session-id* (session-id?))
