
(defun defunits-chaining (u units prev)
  (if (member u prev)
    (error "~{ ~a~^ depends on~}"
      (cons u prev)))
  (let ((spec (find u units :key #'car)))
    (if (null spec)
      (error "Unknown unit ~a" u)
      (let ((chain (cadr spec)))
        (if (listp chain)
          (* (car chain)
             (defunits-chaining
               (cadr chain)
               units
               (cons u prev)))
          chain)))))

(um:defmacro! defunits (quantity base-unit &rest units)
  `(defmacro ,(um:symb 'unit-of- quantity)
             (,g!val ,g!un)
     `(* ,,g!val
         ,(case ,g!un
           ((,base-unit) 1)
           ,@(mapcar (lambda (x)
                       `((,(car x))
                           ,(defunits-chaining
                              (car x)
                              (cons
                                `(,base-unit 1)
                                (um:group units 2))
                              nil)))
                     (um:group units 2))))))

;; -------------------------------------------
;; units of distance

(defunits length m
  km 1000
  cm 1/100
  mm (1/10 cm)
  um (1/1000 mm)
  nm (1/1000 um)
  
  angstrom (1/10 nm)
  
  yard 9144/10000 ; Defined in 1956
  foot (1/3 yard)
  inch (1/12 foot)
  mile (1760 yard)
  furlong (1/8 mile)

  fathom (2 yard) ; Defined in 1929
  nautical-mile 1852
  cable (1/10 nautical-mile)

  old-brit-nautical-mile ; Dropped in 1970
    (6080/3 yard)
  old-brit-cable
    (1/10 old-brit-nautical-mile)
  old-brit-fathom
    (1/100 old-brit-cable)

    r-earth (6371 km)
    r-sun     6.955d8
    au      (149.6d6 km)
    ly      (9.461d12 km)
    pc      (3.26156 ly)
    )

(defunits mass kg
          g 1/1000
          m-earth (5.9736d24 kg)
          m-sun   (1.9891d30 kg)
          )

(defunits time s
          ms (1/1000 s)
          us (1/1000 ms)
          ns (1/1000 us)
          ps (1/1000 ns)
          m  (60 s)
          h  (60 m)
          d  (24 h)
          sid-day (86164.090530833 s) ;; J2000(?)
          )

(defmacro [L] (val unit)
  `(unit-of-length ,val ,unit))

(defmacro [M] (val unit)
  `(unit-of-mass ,val ,unit))

(defmacro [T] (val unit)
  `(unit-of-time ,val ,unit))

(defunits frequency hz
          khz 1000
          mhz (10 khz)
          ghz (1000 mhz)
          thz (1000 ghz)
          )


          