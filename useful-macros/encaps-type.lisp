
(in-package :um)

;; Define a construtor for encapsulated types. These envelopes become
;; useful for CLOS dispatching, and keep the contents opaque. In the
;; macro, arguments:
;;
;;  enc - is the name of the encapsulated type. As a function it
;;  constructs a new one whose opaque contents are provided as its
;;  sole constructor argument.
;;
;;  pred - is the name of a predicate for type testing
;;
;;  dec - is the deconstructor which returns its opaque contents.
;;
;; The envelopes are also convenient ref objects whose opaque contents
;; may be mutated (or not). But when the contents represent immutable
;; objects, the only way to record a mutation is to modify the
;; reference contained in the envelope, using (SETF (DEC obj)).

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
