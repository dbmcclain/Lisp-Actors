
(in-package :actors)

(defclass hoare-monitor (actor)
  ())

(defmethod execute-in-monitor ((mon hoare-monitor) fn &rest args)
  (apply 'exec mon fn args))

(defmacro perform-in-monitor (mon &body body)
  `(execute-in-monitor ,mon (lambda ()
                              ,@body)))

#+:LISPWORKS
(editor:setup-indent "perform-in-monitor" 1)

