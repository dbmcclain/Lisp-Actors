
(in-package :core-crypto)

(defun startup ()
  (mapc 'funcall (reverse (get 'startup 'startup-fns))))

(defun shutdown ()
  (mapc 'funcall (get 'shutdown 'shutdown-fns)))

(defun add-to-startups (fn)
  (pushnew fn (get 'startup 'startup-fns)))

(defun add-to-shutdowns (fn)
  (pushnew fn (get 'shutdown 'shutdown-fns)))

  