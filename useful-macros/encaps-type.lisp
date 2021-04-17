
(in-package :um)

(defmacro make-encapsulated-type (enc pred dec)
  `(defstruct (,enc
               (:conc-name)
               (:copier)
               (:constructor ,enc (,dec))
               (:predicate   ,pred))
     ,dec))

#|
(make-encapsulated-type e e? d)

(e 15)
(d (e 15))
(e? (e 15))
|#
