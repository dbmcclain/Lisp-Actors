
(in-package :edec-ff)

;; equiv to #F
(declaim  (OPTIMIZE (SPEED 3) #|(SAFETY 0)|# #+:LISPWORKS (FLOAT 0)))

;; -------------------------------------------------
;; Structs used to hold working info on ECC curves

(defstruct portable-ed-curve
  name c d q h r gen)

(defstruct (ed-curve (:include portable-ed-curve))
  ff-embed ff-curve)

(defstruct (fast-ed-curve (:include ed-curve))
  affine-mul
  proj-mul
  proj-add
  to-affine)

;; -------------------------------------------------
;; For object serialization

(defmethod loenc:before-store ((obj ed-curve))
  ;; useful to elide non-portable items from ed-curve
  (with-slots (name c d q h r gen) obj
    (make-portable-ed-curve
     :name  name
     :c     c
     :d     d
     :q     q
     :h     h
     :r     r
     :gen   gen)))

;; -------------------------------------------------

#|
(defmethod hash:hashable ((obj ed-curve))
  (with-slots (name c d q h r gen) obj
    (hash:hashable `(ed-curve
                     :name ',name
                     :c    ,c
                     :d    ,d
                     :q    ,q
                     :h    ,h
                     :r    ,r
                     :gen  ,gen))))
|#

#+:SBCL
(defmethod make-load-form ((obj ed-curve) &optional environment)
  (make-load-form-saving-slots obj
                               :slot-names '(name c d q h r gen)
                               :environment environment))

#+:SBCL
(defmethod make-load-form ((obj fast-ed-curve) &optional environment)
  (make-load-form-saving-slots obj
                               :slot-names '(name c d q h r gen
                                                  affine-mul proj-mul
                                                  proj-add to-affine)
                               :environment environment))

;; -----------------------------------------------------------

(defstruct ecc-pt
  x y)

(defstruct ecc-proj-pt
  x y z)

(defstruct ecc-cmpr-pt
  cx)

#+:SBCL
(defmethod make-load-form ((obj ecc-pt) &optional environment)
  (make-load-form-saving-slots obj
                               :slot-names '(x y)
                               :environment environment))
#+:SBCL
(defmethod make-load-form ((obj ecc-proj-pt) &optional environment)
  (make-load-form-saving-slots obj
                               :slot-names '(x y z)
                               :environment environment))
#+:SBCL
(defmethod make-load-form ((obj ecc-cmpr-pt) &optional environment)
  (make-load-form-saving-slots obj
                               :slot-names '(cx)
                               :environment environment))

#+:ALLEGRO
(defmethod make-load-form ((point ecc-pt) &optional env)
  (make-load-form-saving-slots point :environment env))

#+:ALLEGRO
(defmethod make-load-form ((point ecc-proj-pt) &optional env)
  (make-load-form-saving-slots point :environment env))


(defmethod vec-repr:int ((pt ecc-pt))
  (vec-repr:int (ed-compress-pt pt)))

(defmethod vec-repr:int ((pt ecc-proj-pt))
  (vec-repr:int (ed-compress-pt pt)))

(defmethod vec-repr:int ((pt ecc-cmpr-pt))
  (vec-repr:int (ecc-cmpr-pt-cx pt)))


(defmethod vec-repr:bev ((pt ecc-cmpr-pt))
  (vec-repr:bev (ecc-cmpr-pt-cx pt)))

(defmethod vec-repr:bev ((pt ecc-pt))
  (vec-repr:bev (ed-compress-pt pt)))

(defmethod vec-repr:bev ((pt ecc-proj-pt))
  (vec-repr:bev (ed-compress-pt pt)))

