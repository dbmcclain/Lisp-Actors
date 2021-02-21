;; fixmes.lisp - a collection of FIX-ME's to overcome some Lisp problems...
;;
;; DM/RAL 02/21
;; ----------------------------------------------------------------------

(defpackage :fixmes)

(in-package :fixmes)

;; -----------------------------------------------------------------
;; First up... DSPEC:Save-Tags-Database can run into problems when it
;; tags a method which uses EQL specializers.
;;
;; It currently attempts to serialize those EQL args, and will
;; reconstruct them on readback of the database. This is not a problem
;; for simple atoms - symbols, numbers, strings, etc.
;;
;; But if an EQL arg is a composite object, then it will produce a
;; deep copy of the original - if it can even save the original.
;; Sometimes those composite objects may contain a function, which is
;; not serializable, and will cause the saving to bomb out.
;;
;; What we actually need is an indirect form of EQL specializer where
;; it refers to a symbol, and the symbol value is the actual
;; specializer value. Serialization would only send across the symbol
;; by name.
;;
;; The name needs to be global and known - it can't be made up on the
;; fly during serialization because a later session wouldn't be able
;; to recognize that impromptu symbol.
;;
;; Hence we really need to disallow EQL specializers unless they are
;; simple atoms. Otherwise, a symbol should be defined to carry the
;; actual value, and an indirect form of EQL, perhaps @EQL should be
;; stated.
;;
;; But for now, we punt and send across a :DUMMY symbol in its
;; place...

(defvar *transient-protection* nil)
(defvar *warned* nil)

(lw:defadvice (system::dump-object :transient :around)
    (obj fasl-out)
  (handler-case
      (lw:call-next-advice obj fasl-out)
    (error (c)
      (if *transient-protection*
          (progn
            (unless *warned*
              (warn "Attempt to serialize a non-serializable object: ~S" obj)
              (setf *warned* t))
            (lw:call-next-advice :dummy fasl-out))
        (error c)))))
  
(lw:defadvice (dspec:save-tags-database :rescue-deep-copies :around)
    (fasl-file)
  (let ((*warned* nil)
        (*transient-protection* t))
    (lw:call-next-advice fasl-file)))

