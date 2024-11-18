#| DATE           : 28Dec1 
 | USER           : Dave 
 | PROCESSED FILE : D:\Projects\LispWorks\vmath\scigraph_intf.h
 |#

(in-package #:com.ral.scigraph)

;;; Derived from file : "C:\\TEMP\\PCL19E.h"
;;; Hand edited for module linkage - DM/MCFA 12/01

(defvar *plotter-dll*
  (translate-logical-pathname "PROJECTS:LIB;libLispAquaTermPlotter.dylib"))

(fli:define-c-struct (twindow-setup (:foreign-name "Twindow_setup"))
  (wid     :int)
  (xpos    :int)
  (ypos    :int)
  (xsize   :int)
  (ysize   :int)
  (title   (:pointer :char))
  (bgcolor :int))

(fli:define-c-struct (timage-parms (:foreign-name "Timage_parms"))
  (parr   (:pointer :float))
  (xsiz   :int)
  (ysiz   :int)
  (xpos   :int)
  (ypos   :int)
  (minval :float)
  (maxval :float)
  (xmagn  :float)
  (ymagn  :float)
  (neg    :int)
  (flipv  :int)
  (fliph  :int)
  (xmin   :float)
  (xmax   :float)
  (ymin   :float)
  (ymax   :float)
  (xlog   :int)
  (ylog   :int)
  (zlog   :int)
  (xoff   :int))

(fli:define-c-struct (taxes-parms (:foreign-name "Taxes_parms"))
  (xmin    :float)
  (xmax    :float)
  (ymin    :float)
  (ymax    :float)
  (aspect  :float)
  (xlog    :int)
  (ylog    :int)
  (bgcolor :int)
  (fgcolor :int)
  (grid    :int)
  (ticks-inside :int)
  (xtitle  (:pointer :char))
  (ytitle  (:pointer :char))
  (title   (:pointer :char)))

(fli:define-c-struct (tdata-parms (:foreign-name "Tdata_parms"))
  (npts   :int)
  (pxarr  (:pointer :float))
  (pyarr  (:pointer :float))
  (sym    :int)
  (color  :int)
  (thick  :int)
  (clip   :int)
  (penpat :int)
  (alpha  :int))

(fli:define-c-struct (tscale-parms (:foreign-name "Tscale_parms"))
  (nel    :int)
  (parr   (:pointer :float))
  (log    :int)
  (minval :float)
  (maxval :float))

(fli:define-c-struct (ttext-parms (:foreign-name "Ttext_parms"))
  (text     (:pointer :char))
  (org-type :int)
  (xorg     :float)
  (yorg     :float)
  (color    :int)
  (alpha    :int)
  (fontname (:pointer :char))
  (fontsize :float)
  (clip     :int)
  (anchor   :int)
  (angle    :float))

(fli:define-c-struct (point (:foreign-name "POINT"))
  (x :int)
  (y :int))

(fli:define-c-struct (vertinfo (:foreign-name "VertInfo"))
  (m-color  :int)
  (m-nverts :int)
  (m-pts    (:pointer point)))

(fli:define-c-struct (tpolys-parms (:foreign-name "Tpolys_parms"))
  (npolys :int)
  (polys  (:pointer vertinfo)))

(fli:define-c-struct (twindow-info (:foreign-name "Twindow_info"))
  (xsize :int)
  (ysize :int)
  (left  :int)
  (top   :int))

(fli:define-c-struct (tmouse-info (:foreign-name "Tmouse_info"))
  (mousex :float)
  (mousey :float)
  (mousez :float))

(fli:define-c-typedef (tnonmaskedbm (:foreign-name "TNonmaskedBM"))
  (:pointer :void))

(fli:define-c-typedef (tmaskedbm (:foreign-name "TMaskedBM"))
  (:pointer :void))

(fli:define-c-typedef (hbitmap (:foreign-name "HBITMAP")) :int)

(fli:define-c-typedef (hwnd (:foreign-name "HWND")) :int)

;; --------------------------------------------------------------------
;;
(fli:define-foreign-function (_lpInitSG "lpInitAQT" :source)
			     ()
			     :result-type :int
			     :language    :ansi-c)


(defvar *sci-graph-init* nil)

(defun proxy-request (fn)
  (unless *sci-graph-init*
    (fli:register-module *plotter-dll*)
    (when (zerop (aqt::by-proxy (_lpInitSG)))
      (error "Can't initialize SciGraphics"))
    (setf *sci-graph-init* t))
  (aqt::proxy-request fn))

;; ------------------------------------------------------------------
;;
(def-proxy-fli-function (lpselectwindow "lpSelectWindow" :source)
    ((wid :int))
  :result-type :void
  :language    :ansi-c)

(def-proxy-fli-function (lpshowwindow "lpShowWindow" :source)
    ((wid :int))
  :result-type :void
  :language    :ansi-c)

(def-proxy-fli-function (lperasewindow "lpEraseWindow" :source)
    ((bg :int))
  :result-type :void
  :language    :ansi-c)

(def-proxy-fli-function (lpkillwindow "lpKillWindow" :source)
    ((wid :int))
  :result-type :void
  :language    :ansi-c)

(def-proxy-fli-function (lpopenwindow "lpOpenWindow" :source)
    ((p (:pointer twindow-setup)))
  :result-type :void
  :language    :ansi-c)

#|
(def-proxy-fli-function (lpattachwindow "lpAttachWindow" :source)
    ((hwnd hwnd))
  :result-type
  :void
  :language
  :ansi-c)
|#

(def-proxy-fli-function (lpshowimage "lpShowImage" :source)
    ((p (:pointer timage-parms)))
  :result-type :void
  :language    :ansi-c)

(def-proxy-fli-function (lpplotimage "lpPlotImage" :source)
    ((p (:pointer timage-parms)))
  :result-type :void
  :language    :ansi-c)

(def-proxy-fli-function (lpplotaxes "lpPlotAxes" :source)
    ((p (:pointer taxes-parms)))
  :result-type :void
  :language    :ansi-c)

(def-proxy-fli-function (lpdrawgrid "lpDrawGrid" :source)
    ((grid-color :int))
  :result-type :void
  :language    :ansi-c)

(def-proxy-fli-function (lpplotdata "lpPlotData" :source)
    ((p (:pointer tdata-parms)))
  :result-type :void
  :language    :ansi-c)

(def-proxy-fli-function (lpautoscale "lpAutoScale" :source)
    ((p (:pointer tscale-parms)))
  :result-type :void
  :language    :ansi-c)

#|
(def-proxy-fli-function (lpplotpolys "lpPlotPolys" :source)
    ((p (:pointer tpolys-parms)))
  :result-type :void
  :language    :ansi-c)
|#
(def-proxy-fli-function (lpplotpolys "lpPlotPolys" :source)
    ((p (:pointer :int)))
  :result-type :void
  :language    :ansi-c)

(def-proxy-fli-function (lpupdatewindow "lpUpdateWindow" :source)
    nil
  :result-type :void
  :language    :ansi-c)

(def-proxy-fli-function (lpdelayupdatewindow
                              "lpDelayUpdateWindow"
                              :source)
    nil
  :result-type :void
  :language    :ansi-c)

(def-proxy-fli-function (lpgetwindowsize
                              "lpGetWindowSize"
                              :source)
    ((p (:pointer twindow-info)))
  :result-type :void
  :language    :ansi-c)

(def-proxy-fli-function (lpsetcmap "lpSetCMap" :source)
    ((cred (:pointer (:unsigned :char)))
     (cgreen (:pointer (:unsigned :char)))
     (cblue (:pointer (:unsigned :char))))
  :result-type :void
  :language    :ansi-c)

(def-proxy-fli-function (lpgetmouse "lpGetMouse" :source)
    ((p (:pointer tmouse-info)))
  :result-type :void
  :language    :ansi-c)

(def-proxy-fli-function (lpdrawtext "lpDrawText" :source)
    ((p (:pointer ttext-parms)))
    :result-type :void
    :language    :ansi-c)

(def-proxy-fli-function (lpSavePlot "lpSavePlot" :source)
    ((filename (:reference-pass
                (:ef-mb-string :limit 256))))
    :result-type :void
    :language    :ansi-c)

#|
(def-proxy-fli-function (lpgetcoordvalues
                              "lpGetCoordValues"
                              :source)
    ((p (:pointer tmouse-info)))
  :result-type
  :void
  :language
  :ansi-c)

(def-proxy-fli-function (lpcopytoclipboard
                              "lpCopyToClipboard"
                              :source)
    nil
  :result-type
  :void
  :language
  :ansi-c)

(def-proxy-fli-function (lpsavetobacking
                              "lpSaveToBacking"
                              :source)
    nil
  :result-type
  :void
  :language
  :ansi-c)

(def-proxy-fli-function (lpgetcursorhandle
                              "lpGetCursorHandle"
                              :source)
    ((cursor-filename (:pointer (:unsigned :char))))
  :result-type
  :int
  :language
  :ansi-c)

(def-proxy-fli-function (lpdirectredraw "lpDirectRedraw" :source)
    ((hwnd hwnd))
  :result-type
  :void
  :language
  :ansi-c)

(def-proxy-fli-function (lppromptformultiplefiles
                              "lpPromptForMultipleFiles"
                              :source)
    ((title (:pointer :char))
     (buf (:pointer :char))
     (buflen :int))
  :result-type
  :void
  :language
  :ansi-c)

(def-proxy-fli-function (lpreadnonmaskedbm
                              "lpReadNonmaskedBM"
                              :source)
    ((fname (:pointer (:const :char))))
  :result-type
  (:pointer tnonmaskedbm)
  :language
  :ansi-c)

(def-proxy-fli-function (lpreadmaskedbm "lpReadMaskedBM" :source)
    ((fname (:pointer (:const :char)))
     (trans :int))
  :result-type
  (:pointer tmaskedbm)
  :language
  :ansi-c)

(def-proxy-fli-function (lpdrawnonmasked
                              "lpDrawNonmasked"
                              :source)
    ((pbm (:pointer tnonmaskedbm))
     (ulc-x :int)
     (ulc-y :int)
     (bm-ulc-x :int)
     (bm-ulc-y :int)
     (bm-wd :int)
     (bm-ht :int))
  :result-type
  :void
  :language
  :ansi-c)

(def-proxy-fli-function (lpdrawmasked "lpDrawMasked" :source)
    ((pbm (:pointer tmaskedbm))
     (ulc-x :int)
     (ulc-y :int)
     (bm-ulc-x :int)
     (bm-ulc-y :int)
     (bm-wd :int)
     (bm-ht :int))
  :result-type
  :void
  :language
  :ansi-c)

(def-proxy-fli-function (lptilebg "lpTileBG" :source)
    ((pbm (:pointer tnonmaskedbm))
     (ulc-x :int)
     (ulc-y :int)
     (wd :int)
     (ht :int)
     (bm-ulc-x :int)
     (bm-ulc-y :int)
     (bm-wd :int)
     (bm-ht :int))
  :result-type :void
  :language    :ansi-c)

(def-proxy-fli-function (lpdiscardbitmap
                              "lpDiscardBitmap"
                              :source)
    ((pbm (:pointer tnonmaskedbm)))
  :result-type :void
  :language    :ansi-c)

(def-proxy-fli-function (lpclipdrawing "lpClipDrawing" :source)
    ((clip-left :int)
     (clip-top :int)
     (clip-width :int)
     (clip-height :int))
  :result-type :int
  :language    :ansi-c)

(def-proxy-fli-function (lpunclipdrawing
                              "lpUnclipDrawing"
                              :source)
    ((sav :int))
  :result-type :void
  :language    :ansi-c)

(def-proxy-fli-function (lplockdrawing "lpLockDrawing" :source)
    nil
  :result-type :void
  :language    :ansi-c)

(def-proxy-fli-function (lpunlockdrawing
                              "lpUnlockDrawing"
                              :source)
			    nil
			    :result-type :void
			    :language    :ansi-c)

(def-proxy-fli-function (lpgetimagebits "lpGetImageBits" :source)
    ((left :int)
     (top  :int)
     (wd   :int)
     (ht   :int))
  :result-type hbitmap
  :language    :ansi-c)

(def-proxy-fli-function (lpsetimagebits "lpSetImageBits" :source)
    ((hbm  hbitmap)
     (left :int)
     (top  :int)
     (wd   :int)
     (ht   :int))
  :result-type :void
  :language    :ansi-c)

(def-proxy-fli-function (lpdiscardimagebits
                              "lpDiscardImageBits"
                              :source)
    ((hbm hbitmap))
  :result-type
  :void
  :language
  :ansi-c)

(def-proxy-fli-function (lpmovememory
                              "lpMoveMemory"
                              :source)
    ((pdst   (:pointer :void))
     (psrc   (:pointer :void))
     (nbytes :int))
  :result-type :void
  :language    :ansi-c)

(def-proxy-fli-function (lpclearmemory
                              "lpClearMemory"
                              :source)
    ((pdst   (:pointer :void))
     (nbytes :int))
  :result-type :void
  :language    :ansi-c)

(def-proxy-fli-function (lpshiftimageleft
                              "lpShiftImageLeft"
                              :source)
    ((pdst         (:pointer :void))
     (nshift-bytes :int)
     (ncol-bytes   :int)
     (nrows        :int))
  :result-type :void
  :language    :ansi-c)
|#

#|
(def-proxy-fli-function (test-add-label "lpTestAddLabel" :source)
                             ((text  (:reference-pass
				      (:ef-mb-string :limit 256)))
                              (x     :float)
                              (y     :float)
                              (angle :float)
                              (align :int))
                             :result-type :void
                             :language    :ansi-c)

|#
