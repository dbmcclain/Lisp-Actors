#| DATE           : 28Dec1 
 | USER           : Dave 
 | PROCESSED FILE : D:\Projects\LispWorks\vmath\scigraph_intf.h
 |#

(in-package "SCIGRAPH")

;;; Derived from file : "C:\\TEMP\\PCL19E.h"
;;; Handed edited for module linkage - DM/MCFA 12/01

(defvar *plotter-dll* "lisp_plotter.dll")

(fli:define-c-struct (twindow-setup (:foreign-name "Twindow_setup"))
  (wid :long)
  (xpos :long)
  (ypos :long)
  (xsize :long)
  (ysize :long)
  (title (:pointer :char))
  (bgcolor :long))
(fli:define-c-struct (timage-parms (:foreign-name "Timage_parms"))
  (parr (:pointer :float))
  (xsiz :long)
  (ysiz :long)
  (xpos :long)
  (ypos :long)
  (filler :long)
  (minval :float)
  (maxval :float)
  (xmagn :float)
  (ymagn :float)
  (neg :long)
  (flipv :long)
  (fliph :long)
  (xmin  :float)
  (xmax  :float)
  (ymin  :float)
  (ymax  :float)
  (xlog  :long)
  (ylog  :long)
  (zlog  :long))
(fli:define-c-struct (taxes-parms (:foreign-name "Taxes_parms"))
  (xmin :float)
  (xmax :float)
  (ymin :float)
  (ymax :float)
  (aspect :float)
  (xlog :long)
  (ylog :long)
  (bgcolor :long)
  (fgcolor :long)
  (grid    :long)
  (ticks-inside :long)
  (xtitle (:pointer :char))
  (ytitle (:pointer :char))
  (title  (:pointer :char)))
(fli:define-c-struct (tdata-parms (:foreign-name "Tdata_parms"))
  (npts :long)
  (pxarr (:pointer :float))
  (pyarr (:pointer :float))
  (sym :long)
  (color :long)
  (thick :long)
  (clip :long)
  (penpat :long))
(fli:define-c-struct (tscale-parms (:foreign-name "Tscale_parms"))
  (nel :long)
  (parr (:pointer :float))
  (log :long)
  (filler :long)
  (minval :float)
  (maxval :float))
(fli:define-c-struct (ttext-parms (:foreign-name "Ttext_parms"))
  (text     (:pointer :char))
  (org-type :long)
  (xorg     :float)
  (yorg     :float)
  (color    :long)
  (alpha    :long)
  (fontName (:pointer :char))
  (fontsize :float)
  (clip     :long)
  (anchor   :long)
  (angle    :float))

(fli:define-c-struct (point (:foreign-name "POINT"))
  (x :long)
  (y :long))
(fli:define-c-struct (vertinfo (:foreign-name "VertInfo"))
  (m-color :long)
  (m-nverts :long)
  (m-pts (:pointer point)))
(fli:define-c-struct (tpolys-parms (:foreign-name "Tpolys_parms"))
  (npolys :long)
  (polys (:pointer vertinfo)))
(fli:define-c-struct (twindow-info (:foreign-name "Twindow_info"))
  (xsize :long)
  (ysize :long)
  (left  :long)
  (top   :long))
(fli:define-c-struct (tmouse-info (:foreign-name "Tmouse_info"))
  (mousex :float)
  (mousey :float)
  (mousez :float))
(fli:define-c-typedef (tnonmaskedbm (:foreign-name "TNonmaskedBM"))
  (:pointer :void))
(fli:define-c-typedef (tmaskedbm (:foreign-name "TMaskedBM"))
  (:pointer :void))
(fli:define-c-typedef (hbitmap (:foreign-name "HBITMAP")) :long)
(fli:define-c-typedef (hwnd (:foreign-name "HWND")) :long)
"linkage-specifier -> \"C\" (NIL NIL) "
(fli:define-foreign-function (lpselectwindow "lpSelectWindow" :source)
    ((wid :long))
  :result-type
  :void
  :module *plotter-dll*
  :language
  :c
  :calling-convention
  :cdecl)
(fli:define-foreign-function (lpshowwindow "lpShowWindow" :source)
    ((wid :long))
  :result-type
  :void
  :module *plotter-dll*
  :language
  :c
  :calling-convention
  :cdecl)
(fli:define-foreign-function (lperasewindow "lpEraseWindow" :source)
    ((bg :long))
  :result-type
  :void
  :module *plotter-dll*
  :language
  :c
  :calling-convention
  :cdecl)
(fli:define-foreign-function (lpkillwindow "lpKillWindow" :source)
    ((wid :long))
  :result-type
  :void
  :module *plotter-dll*
  :language
  :c
  :calling-convention
  :cdecl)
(fli:define-foreign-function (lpopenwindow "lpOpenWindow" :source)
    ((p (:pointer twindow-setup)))
  :result-type
  :void
  :module *plotter-dll*
  :language
  :c
  :calling-convention
  :cdecl)
(fli:define-foreign-function (lpattachwindow "lpAttachWindow" :source)
    ((hwnd hwnd))
  :result-type
  :void
  :module *plotter-dll*
  :language
  :c
  :calling-convention
  :cdecl)
(fli:define-foreign-function (lpshowimage "lpShowImage" :source)
    ((p (:pointer timage-parms)))
  :result-type
  :void
  :module *plotter-dll*
  :language
  :c
  :calling-convention
  :cdecl)
(fli:define-foreign-function (lpplotimage "lpPlotImage" :source)
    ((p (:pointer timage-parms)))
  :result-type
  :void
  :module *plotter-dll*
  :language
  :c
  :calling-convention
  :cdecl)
(fli:define-foreign-function (lpplotaxes "lpPlotAxes" :source)
    ((p (:pointer taxes-parms)))
  :result-type
  :void
  :module *plotter-dll*
  :language
  :c
  :calling-convention
  :cdecl)
(fli:define-foreign-function (lpdrawgrid "lpDrawGrid" :source)
    ((grid-color :LONG))
  :result-type
  :void
  :module *plotter-dll*
  :language
  :c
  :calling-convention
  :cdecl)
(fli:define-foreign-function (lpplotdata "lpPlotData" :source)
    ((p (:pointer tdata-parms)))
  :result-type
  :void
  :module *plotter-dll*
  :language
  :c
  :calling-convention
  :cdecl)
(fli:define-foreign-function (lpautoscale "lpAutoscale" :source)
    ((p (:pointer tscale-parms)))
  :result-type
  :void
  :module *plotter-dll*
  :language
  :c
  :calling-convention
  :cdecl)
(fli:define-foreign-function (lpplotpolys "lpPlotPolys" :source)
    ((p (:pointer tpolys-parms)))
  :result-type
  :void
  :module *plotter-dll*
  :language
  :c
  :calling-convention
  :cdecl)
(fli:define-foreign-function (lpupdatewindow "lpUpdateWindow" :source)
    nil
  :result-type
  :void
  :module *plotter-dll*
  :language
  :c
  :calling-convention
  :cdecl)
(fli:define-foreign-function (lpdelayupdatewindow
                              "lpDelayUpdateWindow"
                              :source)
    nil
  :result-type
  :void
  :module *plotter-dll*
  :language
  :c
  :calling-convention
  :cdecl)
(fli:define-foreign-function (lpgetwindowsize
                              "lpGetWindowSize"
                              :source)
    ((p (:pointer twindow-info)))
  :result-type
  :void
  :module *plotter-dll*
  :language
  :c
  :calling-convention
  :cdecl)
(fli:define-foreign-function (lpsetcmap "lpSetCMap" :source)
    ((cred (:pointer (:unsigned :char)))
     (cgreen (:pointer (:unsigned :char)))
     (cblue (:pointer (:unsigned :char))))
  :result-type
  :void
  :module *plotter-dll*
  :language
  :c
  :calling-convention
  :cdecl)
(fli:define-foreign-function (lpgetmouse "lpGetMouse" :source)
    ((p (:pointer tmouse-info)))
  :result-type
  :void
  :module *plotter-dll*
  :language
  :c
  :calling-convention
  :cdecl)
(fli:define-foreign-function (lpdrawtext "lpDrawText" :source)
    ((p (:pointer ttext-parms)))
  :result-type :void
  :module *plotter-dll*
  :language
  :c
  :calling-convention
  :cdecl)

(fli:define-foreign-function (lpgetcoordvalues
                              "lpGetCoordValues"
                              :source)
    ((p (:pointer tmouse-info)))
  :result-type
  :void
  :module *plotter-dll*
  :language
  :c
  :calling-convention
  :cdecl)
(fli:define-foreign-function (lpcopytoclipboard
                              "lpCopyToClipboard"
                              :source)
    nil
  :result-type
  :void
  :module *plotter-dll*
  :language
  :c
  :calling-convention
  :cdecl)
(fli:define-foreign-function (lpsavetobacking
                              "lpSaveToBacking"
                              :source)
    nil
  :result-type
  :void
  :module *plotter-dll*
  :language
  :c
  :calling-convention
  :cdecl)
(fli:define-foreign-function (lpgetcursorhandle
                              "lpGetCursorHandle"
                              :source)
    ((cursor-filename (:pointer (:unsigned :char))))
  :result-type
  :long
  :module *plotter-dll*
  :language
  :c
  :calling-convention
  :cdecl)
(fli:define-foreign-function (lpdirectredraw "lpDirectRedraw" :source)
    ((hwnd hwnd))
  :result-type
  :void
  :module *plotter-dll*
  :language
  :c
  :calling-convention
  :cdecl)
(fli:define-foreign-function (lppromptformultiplefiles
                              "lpPromptForMultipleFiles"
                              :source)
    ((title (:pointer :char))
     (buf (:pointer :char))
     (buflen :long))
  :result-type
  :void
  :module *plotter-dll*
  :language
  :c
  :calling-convention
  :cdecl)
(fli:define-foreign-function (lpreadnonmaskedbm
                              "lpReadNonmaskedBM"
                              :source)
    ((fname (:pointer (:const :char))))
  :result-type
  (:pointer tnonmaskedbm)
  :module *plotter-dll*
  :language
  :c
  :calling-convention
  :cdecl)
(fli:define-foreign-function (lpreadmaskedbm "lpReadMaskedBM" :source)
    ((fname (:pointer (:const :char)))
     (trans :long))
  :result-type
  (:pointer tmaskedbm)
  :module *plotter-dll*
  :language
  :c
  :calling-convention
  :cdecl)
(fli:define-foreign-function (lpdrawnonmasked
                              "lpDrawNonmasked"
                              :source)
    ((pbm (:pointer tnonmaskedbm))
     (ulc-x :long)
     (ulc-y :long)
     (bm-ulc-x :long)
     (bm-ulc-y :long)
     (bm-wd :long)
     (bm-ht :long))
  :result-type
  :void
  :module *plotter-dll*
  :language
  :c
  :calling-convention
  :cdecl)
(fli:define-foreign-function (lpdrawmasked "lpDrawMasked" :source)
    ((pbm (:pointer tmaskedbm))
     (ulc-x :long)
     (ulc-y :long)
     (bm-ulc-x :long)
     (bm-ulc-y :long)
     (bm-wd :long)
     (bm-ht :long))
  :result-type
  :void
  :module *plotter-dll*
  :language
  :c
  :calling-convention
  :cdecl)
(fli:define-foreign-function (lptilebg "lpTileBG" :source)
    ((pbm (:pointer tnonmaskedbm))
     (ulc-x :long)
     (ulc-y :long)
     (wd :long)
     (ht :long)
     (bm-ulc-x :long)
     (bm-ulc-y :long)
     (bm-wd :long)
     (bm-ht :long))
  :result-type
  :void
  :module *plotter-dll*
  :language
  :c
  :calling-convention
  :cdecl)
(fli:define-foreign-function (lpdiscardbitmap
                              "lpDiscardBitmap"
                              :source)
    ((pbm (:pointer tnonmaskedbm)))
  :result-type
  :void
  :module *plotter-dll*
  :language
  :c
  :calling-convention
  :cdecl)
(fli:define-foreign-function (lpclipdrawing "lpClipDrawing" :source)
    ((clip-left :long)
     (clip-top :long)
     (clip-width :long)
     (clip-height :long))
  :result-type
  :long
  :module *plotter-dll*
  :language
  :c
  :calling-convention
  :cdecl)
(fli:define-foreign-function (lpunclipdrawing
                              "lpUnclipDrawing"
                              :source)
    ((sav :long))
  :result-type
  :void
  :module *plotter-dll*
  :language
  :c
  :calling-convention
  :cdecl)
(fli:define-foreign-function (lplockdrawing "lpLockDrawing" :source)
    nil
  :result-type
  :void
  :module *plotter-dll*
  :language
  :c
  :calling-convention
  :cdecl)
(fli:define-foreign-function (lpunlockdrawing
                              "lpUnlockDrawing"
                              :source)
    nil
  :result-type
  :void
  :module *plotter-dll*
  :language
  :c
  :calling-convention
  :cdecl)
(fli:define-foreign-function (lpgetimagebits "lpGetImageBits" :source)
    ((left :long)
     (top :long)
     (wd :long)
     (ht :long))
  :result-type
  hbitmap
  :module *plotter-dll*
  :language
  :c
  :calling-convention
  :cdecl)
(fli:define-foreign-function (lpsetimagebits "lpSetImageBits" :source)
    ((hbm hbitmap)
     (left :long)
     (top :long)
     (wd :long)
     (ht :long))
  :result-type
  :void
  :module *plotter-dll*
  :language
  :c
  :calling-convention
  :cdecl)
(fli:define-foreign-function (lpdiscardimagebits
                              "lpDiscardImageBits"
                              :source)
    ((hbm hbitmap))
  :result-type
  :void
  :module *plotter-dll*
  :language
  :c
  :calling-convention
  :cdecl)
(fli:define-foreign-function (lpmovememory
                              "lpMoveMemory"
                              :source)
    ((pdst   (:pointer :void))
     (psrc   (:pointer :void))
     (nbytes :long))
  :result-type :void
  :module *plotter-dll*
  :language :c
  :calling-convention :cdecl)
(fli:define-foreign-function (lpclearmemory
                              "lpClearMemory"
                              :source)
    ((pdst   (:pointer :void))
     (nbytes :long))
  :result-type :void
  :module *plotter-dll*
  :language :c
  :calling-convention :cdecl)
(fli:define-foreign-function (lpshiftimageleft
                              "lpShiftImageLeft"
                              :source)
    ((pdst         (:pointer :void))
     (nshift-bytes :long)
     (ncol-bytes   :long)
     (nrows        :long))
  :result-type :void
  :module *plotter-dll*
  :language :c
  :calling-convention :cdecl)
"end-of-linkage-specifier (NIL)"
