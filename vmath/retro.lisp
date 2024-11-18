;; -------------------------------------------------------------
;; Higher Level (more abstract) Retro Gear Interface

(in-package "USER")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (fli:define-foreign-function (%read-nonmasked-bitmap
                                "lpReadNonmaskedBM")
      ((fname :pointer))
    :result-type (:pointer :void)
    :module sg::*plotter-dll*
    :calling-convention :cdecl)
  
  (defun read-nonmasked-bitmap (fname)
    (fli:with-foreign-string (fstr len blen) fname
      (declare (ignore len blen))
      (%read-nonmasked-bitmap fstr)))
  
  (fli:define-foreign-function (%read-masked-bitmap
                                "lpReadMaskedBM")
      ((fname  :pointer)
       (trans  :int))
    :result-type (:pointer :void)
    :module sg::*plotter-dll*
    :calling-convention :cdecl)

  (defun rgb-quad (red green blue)
    (+ (ash blue 16)
       (ash green 8)
       red))
  
  (defun read-masked-bitmap (fname trans)
    ;; fname should be the ASCII string filename for the bitmap
    ;; and trans should be an RGBQUAD color for the transparent part.
    (fli:with-foreign-string (fstr len blen) fname
      (declare (ignore len blen))
      (%read-masked-bitmap fstr trans))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter $x-org 0)
  (defparameter $y-org 0))

(fli:define-foreign-function (%draw-nonmasked
                              "lpDrawNonmasked")
    ((pbm     (:pointer :void))
     (ulc_x    :int)
     (ulc_y    :int)
     (bm_ulc_x :int)
     (bm_ulc_y :int)
     (bm_wd    :int)
     (bm_ht    :int))
  :result-type :void
  :module sg::*plotter-dll*
  :calling-convention :cdecl)

(defun draw-nonmasked (pbm ulc-x ulc-y bm-ulc-x bm-ulc-y bm-wd bm-ht)
  (%draw-nonmasked pbm
                   (+ ulc-x $x-org)
                   (+ ulc-y $y-org)
                   bm-ulc-x bm-ulc-y
                   bm-wd    bm-ht))

(fli:define-foreign-function (%draw-masked
                              "lpDrawMasked")
    ((pbm     (:pointer :void))
     (ulc_x    :int)
     (ulc_y    :int)
     (bm_ulc_x :int)
     (bm_ulc_y :int)
     (bm_wd    :int)
     (bm_ht    :int))
  :result-type :void
  :module sg::*plotter-dll*
  :calling-convention :cdecl)

(defun draw-masked (pbm ulc-x ulc-y bm-ulc-x bm-ulc-y bm-wd bm-ht)
  (%draw-masked pbm
                (+ ulc-x $x-org)
                (+ ulc-y $y-org)
                bm-ulc-x bm-ulc-y
                bm-wd    bm-ht))

(fli:define-foreign-function (%tile-bg
                              "lpTileBG")
    ((pbm     (:pointer :void))
     (ulc_x    :int)
     (ulc_y    :int)
     (wd       :int)
     (ht       :int)
     (bm_ulc_x :int)
     (bm_ulc_y :int)
     (bm_wd    :int)
     (bm_ht    :int))
  :result-type :void
  :module sg::*plotter-dll*
  :calling-convention :cdecl)

(defun tile-bg (pbm ulc-x ulc-y wd ht bm-ulc-x bm-ulc-y bm-wd bm-ht)
  (%tile-bg pbm
            (+ ulc-x $x-org)
            (+ ulc-y $y-org)
            wd ht
            bm-ulc-x bm-ulc-y
            bm-wd bm-ht))

(fli:define-foreign-function (%clip-drawing "lpClipDrawing")
    ((r_left   :int)
     (r_top    :int)
     (r_width  :int)
     (r_height :int))
  :result-type :pointer
  :module sg::*plotter-dll*
  :calling-convention :cdecl)

(defun clip-drawing (r-left r-top r-width r-height)
  (%clip-drawing (+ r-left $x-org)
                 (+ r-top  $y-org)
                 r-width r-height))

(fli:define-foreign-function (unclip-drawing "lpUnclipDrawing")
    ((sav :pointer))
  :result-type :void
  :module sg::*plotter-dll*
  :calling-convention :cdecl)

(fli:define-foreign-function (lock-drawing "lpLockDrawing")
    ()
  :result-type :void
  :module sg::*plotter-dll*
  :calling-convention :cdecl)

(fli:define-foreign-function (unlock-drawing "lpUnlockDrawing")
    ()
  :result-type :void
  :module sg::*plotter-dll*
  :calling-convention :cdecl)

(fli:define-foreign-function (%get-image-bits "lpGetImageBits")
    ((left   :int)
     (top    :int)
     (wd     :int)
     (ht     :int))
  :result-type :pointer
  :module sg::*plotter-dll*
  :calling-convention :cdecl)

(defun get-image-bits (left top wd ht)
  (%get-image-bits (+ left $x-org)
                   (+ top  $y-org)
                   wd ht))

(fli:define-foreign-function (%set-image-bits "lpSetImageBits")
    ((bits   :pointer)
     (left   :int)
     (top    :int)
     (wd     :int)
     (ht     :int))
  :result-type :void
  :module sg::*plotter-dll*
  :calling-convention :cdecl)

(defun set-image-bits (bits left top wd ht)
  (%set-image-bits bits
                   (+ left $x-org)
                   (+ top  $y-org)
                   wd ht))

(fli:define-foreign-function (discard-image-bits "lpDiscardImageBits")
    ((bits :pointer))
  :result-type :void
  :module sg::*plotter-dll*
  :calling-convention :cdecl)
     
;; --------------------------------------------------------------
;;
#| |#
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter $bg-file    "d:/projects/bitmap/bmbkgrnd.bmp")
  (defparameter $knob-file  "d:/projects/bitmap/bmwidget.bmp")
  (defparameter $frame-file "d:/projects/bitmap/bmframe.bmp")
  
  (defparameter $transparent-color (rgb-quad 255 0 255)) ;; magenta
  
  (defvar $bg-bmps    nil)
  (defvar $knob-bmps  nil)
  (defvar $frame-bmps nil)

  (defun init-bmps ()
    (setf $bg-bmps    (read-nonmasked-bitmap $bg-file)
          $knob-bmps  (read-masked-bitmap $knob-file
                                          $transparent-color)
          $frame-bmps (read-masked-bitmap $frame-file
                                          $transparent-color)))
  
  (unless $bg-bmps
    (init-bmps)))

;; ---------------------------------------------------------------
;; Background handling

(defclass bg-finish ()
  ((bg-bmp   :accessor bg-bmp   :initarg :bmp)
   (bg-ulc-x :accessor bg-ulc-x :initarg :ulc-x)
   (bg-ulc-y :accessor bg-ulc-y :initarg :ulc-y)
   (bg-wd    :accessor bg-wd    :initarg :wd)
   (bg-ht    :accessor bg-ht    :initarg :ht)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter $brushed-aluminum
    (make-instance 'bg-finish
                   :bmp   $bg-bmps
                   :ulc-x 0
                   :ulc-y 0
                   :wd    256
                   :ht    256))
  
  (defparameter $wrinkled-vinyl
    (make-instance 'bg-finish
                   :bmp   $bg-bmps
                   :ulc-x 256
                   :ulc-y 0
                   :wd    100
                   :ht    160)))

(defmethod tile ((bg bg-finish) left top width height)
  (tile-bg (bg-bmp bg)
           left top width height
           (bg-ulc-x bg) (bg-ulc-y bg)
           (bg-wd bg)    (bg-ht bg)))

(defmethod tile :after ((bg (eql $brushed-aluminum))
                        left top width height)
  ;; we need to smooth the top and bottom edges of this background
  ;; the smoothing constants are located in the frames bitmap
  (tile-bg $frame-bmps
           left top width 2
           3 91 24 2)
  (tile-bg $frame-bmps
           left (+ top height -3) width 3
           15 248 2 3))

(defmethod repair ((bg bg-finish) left top width height)
  ;; same as tile, but avoids edging when present
  (tile-bg (bg-bmp bg)
           left top width height
           (bg-ulc-x bg) (bg-ulc-y bg)
           (bg-wd bg)    (bg-ht bg)))

(defclass chassis ()
  ((chassis-bg   :accessor chassis-bg
                 :initarg  :bg
                 :initform $wrinkled-vinyl)
   (chassis-panels :accessor chassis-panels
                   :initarg  :panels
                   :initform (make-array 10))))

(defmacro with-origin ((x y) &body body)
  `(let (($x-org (+ $x-org ,x))
         ($y-org (+ $y-org ,y)))
     ,@body))

(defmethod draw ((chassis chassis))
  (tile (chassis-bg chassis)
        0 0 1600 1600)
  (map nil
       #'(lambda (lst)
           (dolist (panel lst)
             (with-origin ((panel-left panel)
                           (panel-top  panel))
                          (draw panel))))
       (chassis-panels chassis)))

(defmethod add-panel ((chassis chassis) panel &key (row 1))
  (let* ((rowm1 (1- (min 10 (max row 1))))
         (offy  (reduce #'+
                        (mapcar #'panel-width
                                (aref (chassis-panels chassis)
                                      rowm1)))))
    (setf (panel-chassis panel) chassis
          (panel-left panel) offy
          (panel-top  panel) (* 160 rowm1)
          (panel-height panel) 160)
  (push panel (aref (chassis-panels chassis) rowm1))))

(defmethod find-panel ((chassis chassis) name)
  (let* ((arr   (chassis-panels chassis))
         (nrows (array-total-size arr)))
    (labels
        ((find-in-row (ix)
           (if (< ix nrows)
               (or (find name (aref arr ix)
                         :key #'panel-name)
                   (find-in-row (1+ ix))))))
      (let ((panel (find-in-row 0)))
        (unless panel
          (error "panel not found: ~S" name))
        panel))))

;; ---------------------------------------------------------
;; Panel management

(defclass panel ()
  ((panel-chassis :accessor panel-chassis :initarg :chassis)
   (panel-name    :accessor panel-name   :initarg :name)
   (panel-left    :accessor panel-left   :initarg :left)
   (panel-top     :accessor panel-top    :initarg :top)
   (panel-width   :accessor panel-width  :initarg :width)
   (panel-height  :accessor panel-height :initarg :height)
   (panel-bg      :accessor panel-bg     :initarg :bg
                  :initform $brushed-aluminum)
   (panel-items   :accessor panel-items  :initarg :items
                  :initform nil)
   (panel-backing :accessor panel-backing
                  :initform nil)
   (panel-origin  :accessor panel-origin)
   ))

(defmethod cleanup-pointers (x)
  nil)

(defmethod cleanup-pointers ((it panel))
  (let ((bits (panel-backing it)))
    (if bits
        (discard-image-bits bits))
    (setf (panel-backing it) nil)))

(defmethod initialize-instance :after ((it panel) &rest args)
  (declare (ignore args))
  (hcl::add-special-free-action 'cleanup-pointers)
  (hcl::flag-special-free-action it))

(defmacro with-clipped-drawing ((left top width height) &body body)
  ;; use for drawing composite items
  ;; locks the bitmap so partial screen restoration won't happen
  (let ((sav (gensym)))
    `(let ((,sav (clip-drawing ,left ,top ,width ,height)))
       (unwind-protect
           (progn
             ,@body)
         (unclip-drawing ,sav)))
    ))

(defmethod draw ((panel panel))
  (let ((wd (panel-width  panel))
        (ht (panel-height panel)))
    ;; remember our location in case we have to repair anything
    (setf (panel-origin panel) (list $x-org $y-org))
    (with-clipped-drawing (0 0 wd ht)
      (tile (panel-bg panel) 0 0 wd ht)
      (if (panel-backing panel)
          (repair-panel panel)
        (progn
          (dolist (item (panel-items panel))
            (with-origin ((decoration-left item)
                          (decoration-top  item))
               (instantiate item)))
          (setf (panel-backing panel)
                (get-image-bits 0 0 wd ht))))
      (dolist (item (panel-items panel))
        (with-origin ((decoration-left item)
                      (decoration-top  item))
          (draw item)))
      )))
  
(defmethod repair-panel ((panel panel))
  (let ((bits (panel-backing panel))
        (org  (panel-origin panel)))
    (let (($x-org (first org))
          ($y-org (second org)))
      (if bits
          (set-image-bits bits
                          0 0 
                          (panel-width  panel)
                          (panel-height panel))
        (repair (panel-bg     panel)
                0 0
                (panel-width  panel)
                (panel-height panel)))
      )))

(defmethod repair-panel-subrect ((panel panel) left top width height)
  (with-clipped-drawing (left top width height)
    (repair-panel panel)))

(defmethod add-item ((panel panel) item)
  (setf (decoration-panel item) panel)
  (push item (panel-items panel)))

(defmethod find-item ((panel panel) name)
  (let ((item (find name (panel-items panel)
                    :key #'decoration-name)))
    (unless item
      (error "can't find item: ~S" name))
    item))

;; ---------------------------------------------------------
;; Frame decorations

(defclass decoration ()
  ((decoration-panel :accessor decoration-panel :initarg :panel)
   (decoration-name  :accessor decoration-name  :initarg :name
                     :initform "")
   (decoration-left  :accessor decoration-left  :initarg :left)
   (decoration-top   :accessor decoration-top   :initarg :top)
   (decoration-kind  :accessor decoration-kind  :initarg :kind)))

(defmethod instantiate ((d decoration))
  (draw-single-item (decoration-kind d)))

(defmethod draw ((d decoration))
  nil)

(defmethod redraw ((d decoration))
  (let ((panel (decoration-panel d)))
    (with-origin ((panel-left panel)
                  (panel-top  panel))
      (with-origin ((decoration-left d)
                    (decoration-top  d))
        (draw d)))
    ))

;; --------------------------------------------------------
;; Label decorations

(defclass label (decoration)
  ((label-text   :accessor label-text   :initarg :text)
   (label-anchor :accessor label-anchor :initarg :anchor
                 :initform :sw)))

(defmethod instantiate ((lbl label))
  (draw-string (decoration-kind lbl)
               (label-text lbl)
               (label-anchor lbl)))
                   
;; ---------------------------------------------------------
;; Indicators -- actually anything that is displayed in a panel

(defclass indicator (decoration)
  ((indicator-value :accessor indicator-value :initarg :value
                    :initform 0)))

(defmethod indicator-display-value ((indi indicator))
  ;; By default, just the indicator-value itself.
  (indicator-value indi))

(defmethod instantiate ((it indicator))
  nil)

(defmethod draw ((indi indicator))
  (draw-multi-item (decoration-kind indi)
                   (indicator-display-value indi)))

(defmethod set-value ((indi indicator) val)
  ;; Stores a new indicator value and redisplays the
  ;; indicator on screen.
  (unless (eql val (indicator-value indi))
    (setf (indicator-value indi) val)
    (redraw indi)))

;; -------------------------------------------------------------
;; Actuators -- widgets that respond to the mouse

(defclass actuator (indicator)
  ())

;; -------------------------------------------------------------
;; Bitmap widgets

(defclass bmp-owner-item ()
  ((bmp-item-bmp    :accessor bmp-item-bmp    :initarg :bmp
                    :initform $knob-bmps)
   (bmp-item-y0     :accessor bmp-item-y0     :initarg :y0)
   (bmp-item-height :accessor bmp-item-height :initarg :height)))


(defclass masked-mixin ()
  ())

(defmethod draw-bmp ((item masked-mixin) x0 y0 wd ht)
  (draw-masked (bmp-item-bmp item) 0 0 x0 y0 wd ht))

(defclass nonmasked-mixin ()
  ())

(defmethod draw-bmp ((item nonmasked-mixin) x0 y0 wd ht)
  (draw-nonmasked (bmp-item-bmp item) 0 0 x0 y0 wd ht))

;; ------------------------------------------------------------
;; String management

(defclass font-item (bmp-owner-item masked-mixin)
  ((font-starts      :accessor font-starts      :initarg :starts)
   (font-extras      :accessor font-extras      :initarg :extras)
   (font-space-width :accessor font-space-width :initarg :space-wd)))

(defun lookup-char (starts ix)
  ;; starts is a table of x offsets for each character
  (let ((start (aref starts ix))
        (end   (aref starts (1+ ix))))
    (values start (- end start))))

(defun lookup-extra (starts ch)
  ;; starts is an alist of (char . (xoffset width))
  ;; first cell is the default char info
  (let ((info (or (assoc ch starts)
                  (first starts))))
    (values (second info) (third info))))

(defmethod lookup-char-starts ((item font-item) ch)
  (cond
   ((and (char>= ch #\A)
         (char<= ch #\Z))
    (lookup-char
     (font-starts item)
     (- (char-code ch) #.(char-code #\A))))
   
   ((and (char>= ch #\a)
         (char<= ch #\z))
    (lookup-char
     (font-starts item)
     (+ 26 (- (char-code ch) #.(char-code #\a)))))
   
   ((and (char>= ch #\1)
         (char<= ch #\9))
    (lookup-char
     (font-starts item)
     (+ 52 (- (char-code ch) #.(char-code #\1)))))
   
   ((char= ch #\0)
    (lookup-char
     (font-starts item)
     61))
    
   (t (lookup-extra (font-extras item) ch))))
    
(defmethod draw-char ((item font-item) off-x ch)
  (multiple-value-bind (x0 wd)
      (lookup-char-starts item ch)
    (with-origin (off-x 0)
      (draw-bmp item
                x0 (bmp-item-y0 item)
                wd (bmp-item-height item)))
    wd))

(defmethod size-char ((item font-item) ch)
  (multiple-value-bind (x0 wd)
      (lookup-char-starts item ch)
    (declare (ignore x0))
    wd))

(defmethod size-string ((item font-item) str)
  (let ((pos 0))
    (map nil #'(lambda (ch)
                 (if (char= ch #\Space)
                     (incf pos (font-space-width item))
                   (incf pos (size-char item ch))))
         str)
    (values pos (bmp-item-height item))))

(defmacro with-locked-drawing (&body body)
  `(progn
     (lock-drawing)
     (unwind-protect
         (progn
           ,@body)
       (unlock-drawing))))

(defmethod draw-string ((item font-item) str anchor)
  (multiple-value-bind (wd ht)
      (size-string item str)
    (multiple-value-bind (lf tp)
        (ecase anchor
          ( :sw  (values 0 (- ht)))
          ( :w   (values 0 (- (truncate ht 2))))
          ( :nw  (values 0 0))
          ( :se  (values (- wd) (- ht)))
          ( :e   (values (- wd) (- (truncate ht 2))))
          ( :ne  (values (- wd) 0))
          ( :n   (values (- (truncate wd 2)) 0))
          ( :s   (values (- (truncate wd 2)) (- ht)))
          ( :ctr (values (- (truncate wd 2))
                         (- (truncate ht 2)))))
      (with-origin (lf tp)
        (with-locked-drawing
          (let ((pos 0))
            (map nil #'(lambda (ch)
                         (if (char= ch #\Space)
                             (incf pos (font-space-width item))
                           (incf pos (draw-char item pos ch))))
                 str))))
      )))

(defmethod draw-string-at ((item font-item) left top str anchor)
  (with-origin (left top)
     (draw-string item str anchor)))

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defparameter $small-font-extras
    `((#\?  85 4)  ;; default char
      (#\. 399 2)
      (#\, 401 3)))
  
  (defparameter $small-font-starts
    #( 89  ;; #\A
       94
       99
      104
      109
      
      114
      119
      124
      129
      134

      139
      144
      149
      154
      159
      
      164
      169
      174
      179
      184

      189
      194
      199
      204
      209

      214  ;; #\Z

      219  ;; #\a
      224
      229
      234
      239

      244
      249
      254
      259
      264

      269
      275
      279
      284
      289

      294
      299
      304
      309
      314

      319
      324
      329
      334
      339

      344  ;; #\z

      349  ;; #\1
      354  ;; #\2
      359
      364
      369

      374 ;; #\6
      379
      384
      389 ;; #\9
      394 ;; #\0

      399)) ;; next beyond #\0
  
  (defparameter $large-font-extras
    '((#\/ 220 5)
      (#\- 569 6)
      (#\+ 574 7)
      (#\. 579 3)
      (#\, 582 4)
      (#\_ 585 5)))
  
  (defparameter $large-font-starts
    #(225 ;; #\A
      231
      237
      243
      249
      
      254
      259
      266
      272
      274
      
      279
      285
      290
      298
      304

      312
      318
      326
      332
      338

      344
      350
      356
      366
      372

      378 ;; #\Z
      
      383 ;; #\a
      388
      393
      399
      403

      408
      413
      417
      422
      424

      427
      432
      434
      442
      447
      
      452
      457
      462
      467
      472
      
      476
      481
      487
      495
      501
      
      507 ;; #\z
      
      512 ;; #\1
      515 ;; #\2
      521 ;; #\3
      527
      533

      539 ;; #\6
      545
      551
      557 ;; #\9
      563 ;; #\0

      569)) ;; next beyond #\0
  
  (defparameter $large-font-item
    (make-instance 'font-item
                   :bmp    $knob-bmps
                   :starts $large-font-starts
                   :extras $large-font-extras
                   :space-wd 5
                   :y0     636
                   :height  11))

  (defparameter $large-font-embossed-gray-item
    (make-instance 'font-item
                   :bmp    $knob-bmps
                   :starts $large-font-starts
                   :extras $large-font-extras
                   :space-wd 5
                   :y0     624
                   :height  11))
  
  (defparameter $large-font-embossed-white-item
    (make-instance 'font-item
                   :bmp    $knob-bmps
                   :starts $large-font-starts
                   :extras $large-font-extras
                   :space-wd 5
                   :y0     648
                   :height  11))

  (defparameter $small-font-item
    (make-instance 'font-item
                   :bmp    $knob-bmps
                   :starts $small-font-starts
                   :extras $small-font-extras
                   :space-wd 3
                   :y0     555
                   :height   5)))

;; --------------------------------------------------------------
;; Widget management

(defclass bmp-item (bmp-owner-item)
  ((bmp-item-x0     :accessor bmp-item-x0     :initarg :x0)
   (bmp-item-width  :accessor bmp-item-width  :initarg :width)))

(defmethod draw-single-item ((item bmp-item))
  (draw-bmp item
            (bmp-item-x0 item)
            (bmp-item-y0 item)
            (bmp-item-width item)
            (bmp-item-height item)))

(defclass multi-bmp-item (bmp-item)
  ((bmp-item-count  :accessor bmp-item-count  :initarg :count)
   (bmp-item-mod    :accessor bmp-item-mod    :initarg :mod)))

(defmethod draw-multi-item ((item multi-bmp-item) val)
  (let ((val (max 0 (min (round val)
                         (1- (bmp-item-count item)))
                  )))
    (multiple-value-bind (quot rem)
        (truncate val (bmp-item-mod item))
      (draw-bmp item
                (+ (bmp-item-x0 item)
                   (* rem (bmp-item-width item)))
                (+ (bmp-item-y0 item)
                   (* quot (bmp-item-height item)))
                (bmp-item-width item)
                (bmp-item-height item)))
    ))

(defclass odometer-item (multi-bmp-item nonmasked-mixin)
  ())

(defclass meter-face-item (bmp-item masked-mixin)
  ())

(defclass meter-needle-item (multi-bmp-item nonmasked-mixin)
  ())

(defmethod draw-multi-item :around ((item meter-needle-item) val)
  ;; The meter needle movements are just bitmaps that are copied
  ;; over the face of the meter, and so they need to be offset
  ;; from the ULC of the meter face.
  (with-origin (11 14)
      (call-next-method)))

(defclass led-item (multi-bmp-item masked-mixin)
  ())

(defclass lever-switch-item (multi-bmp-item masked-mixin)
  ((label-x :accessor label-x :initarg :label-x
            :initform nil)
   (label-y :accessor label-y :initarg :label-y
            :initform nil)))

(defmethod draw-labels ((it lever-switch-item) labels)
  (let ((x0 (label-x it))
        (y0 (1- (bmp-item-height it))))
    (map nil
         #'(lambda (offy lbl)
             (with-origin (x0 (- y0 offy))
               (draw-string $small-font-item lbl :e)))
         (label-y it) labels)))
                        
(defclass push-button-item (multi-bmp-item masked-mixin)
  ())

(defclass knob-item (multi-bmp-item masked-mixin)
  ())

(defclass rotary-switch-item (knob-item)
  ((label-x       :accessor label-x       :initarg :label-x
                  :initform nil)
   (label-y       :accessor label-y       :initarg :label-y
                  :initform nil)
   (label-anchors :accessor label-anchors :initarg :anchor
                  :initform nil)))

(defmethod draw-labels ((it rotary-switch-item) labels)
  (let ((y0 (1- (bmp-item-height it))))
    (map nil
         #'(lambda (offx offy lbl anchor)
             (with-origin (offx (- y0 offy))
               (draw-string $small-font-item lbl anchor)))
         (label-x it) (label-y it) labels (label-anchors it))))
                        
(defclass tristate-push-button-item (multi-bmp-item masked-mixin)
  ())

(defclass masked-decoration-item (bmp-item masked-mixin)
  ())

(defclass blob-item (bmp-item masked-mixin)
  ())

(defclass slider-slot-item (bmp-item nonmasked-mixin)
  ())

(defclass slider-knob-item (multi-bmp-item nonmasked-mixin)
  ())

(defclass slider-scale-item (bmp-item masked-mixin)
  ())

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defparameter $meter-face-item
    (make-instance 'meter-face-item
                   :bmp     $knob-bmps
                   :count    1
                   :mod      1
                   :x0       0
                   :width   84
                   :y0     478
                   :height  84))
  
  (defparameter $meter-needle-item
    (make-instance 'meter-needle-item
                   :bmp    $knob-bmps
                   :count   32
                   :mod     14
                   :x0       0
                   :y0     310
                   :width   56
                   :height  56))
  
  (defparameter $red-led-item
    (make-instance 'led-item
                   :bmp    $knob-bmps
                   :count  2
                   :mod    2
                   :x0     721
                   :y0     249
                   :width  13
                   :height 13))

  (defparameter $green-led-item
    (make-instance 'led-item
                   :bmp    $knob-bmps
                   :count  2
                   :mod    2
                   :x0     721
                   :y0     262
                   :width  13
                   :height 13))
  
  (defparameter $yellow-led-item
    (make-instance 'led-item
                   :bmp    $knob-bmps
                   :count  2
                   :mod    2
                   :x0     721
                   :y0     275
                   :width  13
                   :height 13))

  (defparameter $green-triangular-led-item
    (make-instance 'led-item
                   :bmp    $knob-bmps
                   :count  2
                   :mod    1
                   :x0     553
                   :y0     438
                   :width  11
                   :height 11))

  (defparameter $toggle-switch-item
    (make-instance 'lever-switch-item
                   :bmp    $knob-bmps
                   :count   3
                   :mod     3
                   :x0    627
                   :y0    423
                   :width  36
                   :height 46))

  (defparameter $push-button-item
    (make-instance 'push-button-item
                   :bmp    $knob-bmps
                   :count    2
                   :mod      1
                   :x0     748
                   :y0     249
                   :width   34
                   :height  21))

  (defparameter $large-knob-item
    (make-instance 'knob-item
                   :bmp    $knob-bmps
                   :count   64
                   :mod     13
                   :x0       0
                   :y0       0
                   :width   60
                   :height  62))

  (defparameter $medium-knob-item
    (make-instance 'knob-item
                   :bmp    $knob-bmps
                   :count   32
                   :mod     19
                   :x0      85
                   :y0     478
                   :width   36
                   :height  38))

  (defparameter $small-knob-item
    (make-instance 'knob-item
                   :bmp    $knob-bmps
                   :count   32
                   :mod     32
                   :x0       1
                   :y0     737
                   :width   24
                   :height  23))

  (defparameter $tiny-knob-item
    (make-instance 'knob-item
                   :bmp    $knob-bmps
                   :count   32
                   :mod     16
                   :x0     280
                   :y0     437
                   :width   17
                   :height  17))

  (defparameter $4-pos-rotary-switch-item
    (make-instance 'rotary-switch-item
                   :bmp    $knob-bmps
                   :count    4
                   :mod      4
                   :x0     220
                   :y0     584
                   :width   39
                   :height  39
                   :label-x '(-1 10 28 40)
                   :label-y '(26 39 39 26)
                   :anchor '(:e :se :sw :w)))

  (defparameter $3-pos-rotary-switch-item
    (make-instance 'rotary-switch-item
                   :bmp    $knob-bmps
                   :count    3
                   :mod      3
                   :x0     378
                   :y0     584
                   :width   39
                   :height  39
                   :label-x '(1 16 33)
                   :label-y '(34 39 34)
                   :anchor '(:se :s :sw)))

  (defparameter $5-pos-rotary-switch-item
    (make-instance 'rotary-switch-item
                   :bmp    $knob-bmps
                   :count    5
                   :mod      5
                   :x0     495
                   :y0     584
                   :width   39
                   :height  39
                   :label-x '(-1 3 18 35 41)
                   :label-y '(18 33 39 33 18)
                   :anchor '(:e :se :s :sw :w)))

  (defparameter $9-pos-rotary-switch-item
    (make-instance 'rotary-switch-item
                   :bmp    $knob-bmps
                   :count    9
                   :mod      9
                   :x0       1
                   :y0     659
                   :width   50
                   :height  52
                   :label-x '(1 4 9 17 22 29 37 41 46)
                   :label-y '(41 45 50 53 54 53 50 45 41)
                   :anchor  '(:e :se :se :se :s :sw :sw :sw :w)))

  (defparameter $3-pos-lever-switch-item
    (make-instance 'lever-switch-item
                   :bmp    $knob-bmps
                   :count    3
                   :mod      3
                   :x0     592
                   :y0     624
                   :width   19
                   :height  30
                   :label-x -1
                   :label-y '(10 17 24)))

  (defparameter $4-pos-lever-switch-item
    (make-instance 'lever-switch-item
                   :bmp    $knob-bmps
                   :count    4
                   :mod      4
                   :x0     548
                   :y0     660
                   :width   19
                   :height  37
                   :label-x -1
                   :label-y '(10 17 24 31)))

  (defparameter $5-pos-lever-switch-item
    (make-instance 'lever-switch-item
                   :bmp    $knob-bmps
                   :count    5
                   :mod      5
                   :x0     452
                   :y0     660
                   :width   19
                   :height  44
                   :label-x -1
                   :label-y '(10 17 24 31 38)))

  (defparameter $rectangular-tristate-push-button
    (make-instance 'tristate-push-button-item
                   :bmp    $knob-bmps
                   :count    3
                   :mod      3
                   :x0     625
                   :y0     704
                   :width   35
                   :height  21))

  (defparameter $square-tristate-push-button
    (make-instance 'tristate-push-button-item
                   :bmp    $knob-bmps
                   :count    3
                   :mod      3
                   :x0     674
                   :y0     517
                   :width   20
                   :height  21))

  (defparameter $left-trim-item
    (make-instance 'masked-decoration-item
                   :bmp   $frame-bmps
                   :x0       0
                   :y0      91
                   :width   15
                   :height 160))

  (defparameter $right-trim-item
    (make-instance 'masked-decoration-item
                   :bmp   $frame-bmps
                   :x0      17  
                   :y0      91
                   :width   13
                   :height 160))

  (defparameter $red-blob-item
    (make-instance 'blob-item
                   :bmp    $knob-bmps
                   :x0    729
                   :y0    585
                   :width  36
                   :height 38))

  (defparameter $blue-blob-item
    (make-instance 'blob-item
                   :bmp    $knob-bmps
                   :x0    723
                   :y0    546
                   :width  36
                   :height 38))

  (defparameter $white-blob-item
    (make-instance 'blob-item
                   :bmp    $knob-bmps
                   :x0    693
                   :y0    585
                   :width  35
                   :height 38))

  (defparameter $green-blob-item
    (make-instance 'blob-item
                   :bmp    $knob-bmps
                   :x0    686
                   :y0    624
                   :width  36
                   :height 38))

  (defparameter $yellow-blob-item
    (make-instance 'blob-item
                   :bmp    $knob-bmps
                   :x0    649
                   :y0    624
                   :width  36
                   :height 38))

  (defparameter $purple-blob-item
    (make-instance 'blob-item
                   :bmp    $knob-bmps
                   :x0    723
                   :y0    624
                   :width  36
                   :height 38))

  (defparameter $large-odometer-item
    (make-instance 'odometer-item
                   :bmp    $knob-bmps                   
                   :count  30
                   :mod    30
                   :x0      0
                   :y0    711
                   :width  15
                   :height 22))

  (defparameter $small-odometer-item
    (make-instance 'odometer-item
                   :bmp    $knob-bmps
                   :count   30
                   :mod     30
                   :x0     280
                   :y0     422
                   :width   10
                   :height  15))

  (defparameter $slider-slot-item
    (make-instance 'slider-slot-item
                   :bmp    $knob-bmps
                   :count    1
                   :mod      1
                   :x0     770
                   :y0     422
                   :width    4
                   :height 109))

  (defparameter $slider-scale-item
    (make-instance 'slider-scale-item
                   :bmp   $knob-bmps
                   :count     1
                   :mod       1
                   :x0      774
                   :y0      422
                   :width     4
                   :height  109))

  (defparameter $slider-knob-item
    (make-instance 'slider-knob-item
                   :bmp   $knob-bmps
                   :count    2
                   :mod      2
                   :x0     584
                   :y0     424
                   :width   21
                   :height  32)))

;; ---------------------------------------------------------
;; Specific panel classes

(defclass panel-with-trim (panel)
  ())

(defmethod initialize-instance :after ((panel panel-with-trim)
                                       &rest args)
  (declare (ignore args))
  (add-item panel
            (make-instance 'decoration
                           :left 0
                           :top  0
                           :kind $left-trim-item))
  (add-item panel
            (make-instance 'decoration
                           :left (- (panel-width panel)
                                    (bmp-item-width $right-trim-item))
                           :top  0
                           :kind $right-trim-item)))

;; --------------------------------------------------------------
;; Odometer management

(defclass odometer (indicator)
  ((odo-digits  :accessor odo-digits  :initarg :ndigits)))

(defclass small-odometer (odometer)
  ((decoration-kind  :initform $small-odometer-item
                     :allocation :class)))

(defclass large-odometer (odometer)
  ((decoration-kind :initform $large-odometer-item
                    :allocation :class)))

(defun fractional-part (v)
  (multiple-value-bind (q r)
      (truncate v)
    (declare (ignore q))
    r))

(defmethod draw ((odo odometer))
  (let ((digits '(0))
        (nd     (odo-digits odo)))
    (let ((v    (mod (indicator-value odo) (expt 10 nd))))
      (dotimes (ix nd)
        (multiple-value-bind (q r)
            (truncate v 10)
          (push (mod (+ (truncate (* 3 r))
                        (max 0 (- (first digits) 27)))
                     30)
                digits)
          (setf v q))))
    (let* ((left     0)
           (odo-item (decoration-kind odo))
           (wd       (bmp-item-width   odo-item)))
      (with-locked-drawing
        (dotimes (ix nd)
          (with-origin (left 0)
            (draw-multi-item odo-item (pop digits)))
          (incf left wd)))
      )))


;; --------------------------------------------------------------
;; Reparing indicators -- those leaving after-effects that need
;; to be cleaned up before being redrawn...

(defclass repairing-mixin ()
  ;; A class that describes panel items that need
  ;; to repair their damage before being redrawn.
  ())

(defmethod get-damage-box ((it decoration))
  (let ((item (decoration-kind it)))
    (list 0 0
          (bmp-item-width  item)
          (bmp-item-height item))))

(defmethod draw :around ((ts repairing-mixin))
  (with-locked-drawing
    (apply #'repair-panel-subrect
           (decoration-panel ts)
           (get-damage-box ts))
    (call-next-method)))

;; ------------------------------------------------------
;; Specific decorations

(defclass small-label (label)
  ((decoration-kind :initform $small-font-item
                    :allocation :class)))

(defclass large-label (label)
  ((decoration-kind :initform $large-font-item
                    :allocation :class)))

(defclass large-label-embossed-gray (label)
  ((decoration-kind :initform $large-font-embossed-gray-item
                    :allocation :class)))

(defclass large-label-embossed-white (label)
  ((decoration-kind :initform $large-font-embossed-white-item
                    :allocation :class)))

;; ---------------------------------------------------------
;; Specific indicator classes

(defclass panel-meter (indicator)
  ((decoration-kind :initform $meter-needle-item
                    :allocation :class)
   (needle-tc       :accessor needle-tc   :initarg :tconst
                    :initform 10)
   (meter-fsval     :accessor meter-fsval :initarg :fsval
                    :initform 1.0)))

(defmethod instantiate ((m panel-meter))
  (draw-single-item $meter-face-item))

(defmethod indicator-display-value ((m panel-meter))
  (round (indicator-value m) (/ (meter-fsval m) 31)))

(defmethod set-value ((m panel-meter) val)
  (let ((alpha (/ (needle-tc m))))
    (do* ((nval (indicator-value m)
                (+ (* (- 1 alpha) (indicator-value m))
                   (* alpha val)))
          (dv  (abs (- nval val)) (abs (- nval val))))
         ((< dv (/ (meter-fsval m) 64)))
      (call-next-method m nval)
      (sleep 0)
    ;(print nval)
    ;;(sleep 0.1)
    )
    (call-next-method m val)))

(defclass led (repairing-mixin indicator)
  ())

(defclass red-led (led)
  ((decoration-kind :initform $red-led-item
                   :allocation :class)))

(defclass green-led (led)
  ((decoration-kind :initform $green-led-item
                   :allocation :class)))

(defclass yellow-led (led)
  ((decoration-kind :initform $yellow-led-item
                   :allocation :class)))

(defclass green-triangular-led (led)
  ((decoration-kind :initform $green-triangular-led-item
                    :allocation :class)))

;; -----------------------------------------------------
;; Specific actuators...

(defclass labeled-mixin ()
  ((decoration-labels :accessor decoration-labels
                      :initarg  :labels
                      :initform nil)))

(defmethod instantiate :after ((it labeled-mixin))
  (draw-labels (decoration-kind   it)
               (decoration-labels it)))

(defclass toggle-switch (repairing-mixin actuator)
  ((decoration-kind :initform $toggle-switch-item
                   :allocation :class)))

(defclass lever-switch (repairing-mixin labeled-mixin actuator)
  ())

(defclass 3-pos-lever-switch (lever-switch)
  ((decoration-kind :initform $3-pos-lever-switch-item
                    :allocation :class)))

(defclass 4-pos-lever-switch (lever-switch)
  ((decoration-kind :initform $4-pos-lever-switch-item
                    :allocation :class)))

(defclass 5-pos-lever-switch (lever-switch)
  ((decoration-kind :initform $5-pos-lever-switch-item
                    :allocation :class)))

(defclass knob (actuator)
  ())

(defclass large-knob (knob)
  ((decoration-kind :initform $large-knob-item
                    :allocation :class)))

(defclass medium-knob (knob)
  ((decoration-kind :initform $medium-knob-item
                    :allocation :class)))

(defclass small-knob (knob)
  ((decoration-kind :initform $small-knob-item
                    :allocation :class)))

(defclass tiny-knob (knob)
  ((decoration-kind :initform $tiny-knob-item
                    :allocation :class)))

(defclass rotary-switch (labeled-mixin actuator)
  ())

(defclass 3-pos-rotary-switch (rotary-switch)
  ((decoration-kind :initform $3-pos-rotary-switch-item
                    :allocation :class)))

(defclass 4-pos-rotary-switch (rotary-switch)
  ((decoration-kind :initform $4-pos-rotary-switch-item
                    :allocation :class)))

(defclass 5-pos-rotary-switch (rotary-switch)
  ((decoration-kind :initform $5-pos-rotary-switch-item
                    :allocation :class)))

(defclass 9-pos-rotary-switch (repairing-mixin rotary-switch)
  ((decoration-kind :initform $9-pos-rotary-switch-item
                    :allocation :class)))

(defclass push-button (actuator)
  ((decoration-kind :initform $push-button-item
                    :allocation :class)))

(defclass tristate-push-button (actuator)
  ())

(defclass rectangular-tristate-push-button (tristate-push-button)
  ((decoration-kind :initform $rectangular-tristate-push-button
                    :allocation :class)))

(defclass square-tristate-push-button (tristate-push-button)
  ((decoration-kind :initform $square-tristate-push-button
                    :allocation :class)))

(defclass colored-blob-mixin ()
  ((blob-item  :accessor blob-item  :initarg :blob)))

(defmethod initialize-instance :after ((it colored-blob-mixin)
                                       &key color &allow-other-keys)
  (setf (blob-item it)
        (ecase color
          (:red    $red-blob-item)
          (:blue   $blue-blob-item)
          (:green  $green-blob-item)
          (:yellow $yellow-blob-item)
          (:purple $purple-blob-item)
          (:white  $white-blob-item))))

(defmethod draw :around ((it colored-blob-mixin))
  (with-locked-drawing
    (call-next-method)
    (with-origin (0 -1)
      (draw-single-item (blob-item it)))
    ))

(defclass 3-pos-colored-rotary-switch (colored-blob-mixin
                                       3-pos-rotary-switch)
  ())

(defclass 4-pos-colored-rotary-switch (colored-blob-mixin
                                       4-pos-rotary-switch)
  ())

(defclass 5-pos-colored-rotary-switch (colored-blob-mixin
                                       5-pos-rotary-switch)
  ())

;; ----------------------------------------------------------
;; Slider management

(defclass slider (actuator)
  ((slider-hilighted :accessor slider-hilighted
                     :initform nil)
   (slider-old-value :accessor slider-old-value
                     :initform 0)
   (decoration-kind  :initform $slider-knob-item
                     :allocation :class)))

(defmethod instantiate ((sl slider))
  (draw-single-item $slider-slot-item))

(defmethod draw ((sl slider))
  (let* ((item  (decoration-kind sl))
         (wd    (bmp-item-width item))
         (ht    (bmp-item-height item))
         (sht   (bmp-item-height $slider-slot-item))
         (v     (max 0 (min (truncate (indicator-value sl))
                            (1- sht)))))
    (with-locked-drawing
      (let ((x   (- (truncate wd 2)))
            (ht2 (truncate ht 2)))
        (repair-panel-subrect (decoration-panel sl)
                              x (- sht (slider-old-value sl) ht2)
                              wd ht)
        (setf (slider-old-value sl) v)
        (with-origin (x (- sht v ht2))
          (draw-multi-item item
                           (if (slider-hilighted sl)
                               1
                             0)))
        ))))

(defclass slider-with-scale (slider)
  ())

(defmethod instantiate :around ((sl slider-with-scale))
  (with-locked-drawing
    (call-next-method)
    (with-origin ((bmp-item-width $slider-slot-item) 0)
      (draw-single-item $slider-scale-item))
    ))

(defmethod set-value :around ((sl slider) val)
  (let* ((vmax (bmp-item-height $slider-slot-item))
         (v    (max 0 (min vmax (truncate (indicator-value sl)))))
         (val  (max 0 (min vmax (truncate val))))
         (dir  (if (>= val v) 1 -1)))
    (do ((v v (+ dir v)))
        ((= v val))
      (call-next-method sl v))
    ))

;; -----------------------------------------------------------

(defmacro defpanel (name class &key width items)
  (let ((panel (gensym)))
    `(let ((,panel (make-instance ',class
                                 :name   ',name
                                 :left   0
                                 :top    0
                                 :width  ,width
                                 :height 160)))
       (dolist (item ',items)
         (add-item ,panel
                   (apply #'make-instance (second item)
                          :name (first item)
                          (nthcdr 2 item))))
       ,panel)))
#|
(defparameter $chassis
  (let ((c  (make-instance 'chassis))
        (p1 (defpanel panel-1 panel-with-trim
                      :width   120
                      :items
                      ((meter-1 panel-meter
                                :left    20
                                :top     10
                                :fsval  1.0
                                :tconst 300)
                       (led-1 red-led
                              :left  90
                              :top   10)
                       (sw-1  toggle-switch
                              :left   70
                              :top   100))))
        (p2 (defpanel panel-2 panel-with-trim
                      :width  150
                      :items
                      ((knob-1 large-knob
                               :left 20
                               :top  40)
                       (knob-2 medium-knob
                               :left 90
                               :top  40)
                       (knob-3 small-knob
                               :left 80
                               :top  90)
                       (knob-4 tiny-knob
                               :left 115
                               :top   95)
                       (led-1 yellow-led
                              :left 25
                              :top  20)
                       (led-2 green-led
                              :left 60
                              :top  20)
                       (led-3 green-triangular-led
                              :left 103
                              :top  30)
                       (pb-1  push-button
                              :left 30
                              :top  110)
                       (odo-1 small-odometer
                              :left 30
                              :top  135
                              :ndigits 6
                              :value 12345.9))))
        (p3 (defpanel panel-3 panel-with-trim
                      :width 130
                      :items
                      ((sw-1 9-pos-rotary-switch
                             :left 10
                             :top  20
                             :labels ("1" "2" "3" "4" "5"
                                          "6" "7" "8" "9"))
                       (sw-2 5-pos-colored-rotary-switch
                             :color :purple
                             :left 70
                             :top  20
                             :labels ("one" "two" "three"
                                            "four" "five"))
                       (sw-3 4-pos-colored-rotary-switch
                             :color :green
                             :left 10
                             :top  90
                             :labels ("one" "two" "three" "four"))
                       (sw-4 3-pos-colored-rotary-switch
                             :color :red
                             :left 70
                             :top  90
                             :labels ("one" "two" "three")))))
        (p4 (defpanel panel-4 panel-with-trim
                      :width 120
                      :items
                      ((sw-1 3-pos-lever-switch
                            :left 40
                            :top  20
                            :labels ("ONE" "TWO" "THREE"))
                       (sw-2 4-pos-lever-switch
                             :left 90
                             :top 20
                             :labels ("one" "two" "three" "four"))
                       (sw-3 5-pos-lever-switch
                             :left 40
                             :top  70
                             :labels ("one" "two" "three"
                                            "four" "five"))
                       (sw-4 square-tristate-push-button
                             :left 20
                             :top  120)
                       (sw-5 rectangular-tristate-push-button
                             :left 70
                             :top  120)
                       (led-1 green-triangular-led
                              :left 80
                              :top  65)
                       (kn-1  medium-knob
                              :left 67
                              :top  75))))
        (p5 (defpanel panel-5 panel-with-trim
                      :width 200
                      :items
                      ((lbl-1 large-label
                              :text "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                              :left 15
                              :top  20)
                       (lbl-2 large-label
                              :text "abcdefghijklmnopqrstuvwxyz"
                              :left 15
                              :top  32)
                       (lbl-3 large-label
                              :text "+012,345.67E-89 /?"
                              :left 15
                              :top  44)
                       (lbl-4 large-label-embossed-gray
                              :text "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                              :left 15
                              :top  60)
                       (lbl-5 large-label-embossed-gray
                              :text "abcdefghijklmnopqrstuvwxyz"
                              :left 15
                              :top  72)
                       (lbl-6 large-label-embossed-gray
                              :text "+012,345.67E-89 /?"
                              :left 15
                              :top  84)
                       (lbl-7 large-label-embossed-white
                              :text "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                              :left 15
                              :top  100)
                       (lbl-8 large-label-embossed-white
                              :text "abcdefghijklmnopqrstuvwxyz"
                              :left 15
                              :top  112)
                       (lbl-9 large-label-embossed-white
                              :text "+012,345.67E-89 /?"
                              :left 15
                              :top  124)
                       (odo-1 large-odometer
                              :left 20
                              :top  128
                              :ndigits 6
                              :value 12349.7))))
        (p6 (defpanel panel-6 panel-with-trim
                      :width 180
                      :items
                      ((lbl-1 small-label
                              :text "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                              :left 15
                              :top  20)
                       (lbl-2 small-label
                              :text "abcdefghijklmnopqrstuvwxyz"
                              :left 15
                              :top  32)
                       (lbl-3 small-label
                              :text "+012,345.67E-89 /?"
                              :left 15
                              :top  44)
                       (sw-1 3-pos-colored-rotary-switch
                             :color :yellow
                             :left  30
                             :top   100
                             :labels ("1" "2" "3"))
                       (sw-2 3-pos-colored-rotary-switch
                             :color :white
                             :left  80
                             :top   100
                             :labels ("1" "2" "3"))
                       (sw-3 3-pos-colored-rotary-switch
                             :color :blue
                             :left  50
                             :top   60
                             :labels ("1" "2" "3")))))
        (p7 (defpanel panel-7 panel-with-trim
                      :width 100
                      :items
                      ((sl-1 slider-with-scale
                             :left  45
                             :top   20
                             :value 50)))))
    (add-panel c p1 :row 1)
    (add-panel c p2 :row 1)
    (add-panel c p3 :row 1)
    (add-panel c p4 :row 2)
    (add-panel c p5 :row 2)
    (add-panel c p6 :row 3)
    (add-panel c p7 :row 3)
    c))

(sg:window 0 :xsize 400 :ysize (* 3 160))
;(with-locked-drawing
;    (draw $chassis))
(draw $chassis)

;(let ((p (find-panel $chassis 'panel-5)))
;  (with-locked-drawing
;    (draw p)))

(defun doit ()
  (let* ((panel (find-panel $chassis 'panel-1))
         ($my-onoff (find-item panel 'sw-1))
         ($my-meter (find-item panel 'meter-1))
         ($my-led   (find-item panel 'led-1)))
    (set-value $my-onoff 1)
    (set-value $my-meter 0.48)
    (set-value $my-onoff 2)
    (set-value $my-meter 0.83)
    (set-value $my-led 1)
    (set-value $my-meter 0.96)
    (sleep 3)
    (set-value $my-onoff 1)
    (set-value $my-meter 0.7)
    (set-value $my-led 0)
    (set-value $my-meter 0.48)
    (set-value $my-onoff 0)
    (set-value $my-meter 0)))


(defun tstit ()
  (let* ((panel (find-panel $chassis 'panel-2))
         (pb    (find-item panel 'pb-1))
         (bk    (find-item panel 'knob-1))
         (mk    (find-item panel 'knob-2))
         (sk    (find-item panel 'knob-3))
         (tk    (find-item panel 'knob-4))
         (yl    (find-item panel 'led-1))
         (gl    (find-item panel 'led-2))
         (gl2   (find-item panel 'led-3))
         (odo   (find-item panel 'odo-1)))
    (dotimes (ix 64)
      (let ((v (mod ix 2)))
        (set-value yl v)
        (set-value gl (- 1 v))
        (set-value gl2 (if (= (truncate ix 2) 16) 1 0))
        (set-value pb v)
        (set-value bk ix)
        (set-value mk (truncate ix 2))
        (set-value sk (truncate ix 2))
        (set-value tk (truncate ix 2))
        (dotimes (jx (+ ix 100))
          (set-value odo (+ (indicator-value odo) 0.1)))
        ))
    ))

(defun tstsw ()
  (let* ((panel (find-panel $chassis 'panel-3))
         (sw1   (find-item panel 'sw-1))
         (sw2   (find-item panel 'sw-2))
         (sw3   (find-item panel 'sw-3))
         (sw4   (find-item panel 'sw-4))
         (panel2 (find-panel $chassis 'panel-4))
         (sw5   (find-item panel2 'sw-1))
         (sw6   (find-item panel2 'sw-2))
         (sw7   (find-item panel2 'sw-3))
         (sw8   (find-item panel2 'sw-4))
         (sw9   (find-item panel2 'sw-5))
         (led1  (find-item panel2 'led-1))
         (odo   (find-item (find-panel $chassis 'panel-5) 'odo-1)))
    (dotimes (ix 9)
      (set-value sw1 ix)
      (set-value sw2 (mod ix 5))
      (set-value sw3 (mod ix 4))
      (set-value sw4 (mod ix 3))
      (set-value sw5 (mod ix 3))
      (set-value sw6 (mod ix 4))
      (set-value sw7 (mod ix 5))
      (set-value sw8 (mod ix 3))
      (set-value sw9 (mod ix 3))
      (set-value led1 (mod ix 2))
      (dotimes (jx (+ ix 100))
        (set-value odo (+ (indicator-value odo) 0.1)))
      )))

(defun tstodo ()
  (let* ((panel (find-panel $chassis 'panel-5))
         (odo   (find-item panel 'odo-1)))
    (set-value odo -11111) ;; 19999)
    (dotimes (ix 121)
      (set-value odo (+ (indicator-value odo) 0.1))
      (sleep 0.1))
    (dotimes (ix 121)
      (set-value odo (- (indicator-value odo) 0.1))
      (sleep 0.1))))


(defun tstslider ()
  (let* ((panel (find-panel $chassis 'panel-7))
         (sl    (find-item panel 'sl-1)))
    (setf (slider-hilighted sl) t)
    (dotimes (ix 110)
      (set-value sl ix)
      (sleep 0.02))
    (setf (slider-hilighted sl) nil)
    (dotimes (ix 110)
      (set-value sl (- 109 ix))
      (sleep 0.01))))

(defun tstall ()
  (doit)
  (tstit)
  (tstsw)
  (tstodo)
  (tstslider))

(tstall)

|#

