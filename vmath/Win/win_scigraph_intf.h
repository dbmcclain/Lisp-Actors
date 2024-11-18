// scigraph_intf.h -- Interface to Lisp_Plotter DLL
//
// DM/MCFA  12/01
// ---------------------------------------------------

struct Twindow_setup
{
  long wid;
  long xpos;
  long ypos;
  long xsize;
  long ysize;
  char *title;
  long bgcolor;
};

struct Timage_parms
{
  float  *parr;
  long    xsiz;
  long    ysiz;
  long    xpos;
  long    ypos;
  long    filler;
  float   minval;
  float   maxval;
  float   magn;
  long    neg;
  long    flipv;
  long    fliph;
};

struct Taxes_parms
{
  float  xmin;
  float  xmax;
  float  ymin;
  float  ymax;
  float  aspect;
  long   xlog;
  long   ylog;
  char  *xtitle;
  char  *ytitle;
  char  *title;
  long   bgcolor;
  long   fgcolor;
};

struct Tdata_parms
{
  long    npts;
  float *pxarr;
  float *pyarr;
  long    sym;
  long    color;
  long    thick;
  long    clip;
  long    penpat;
};

struct Tscale_parms
{
  long    nel;
  float  *parr;
  long    log;
  long    filler;
  float   minval;
  float   maxval;
};

struct POINT
{
  long  x;
  long  y;
};

struct VertInfo
{
  long   m_color;
  long   m_nverts;
  POINT *m_pts;     // (long x, long y)
};

struct Tpolys_parms
{
  long       npolys;
  VertInfo  *polys;
};

struct Twindow_info
{
  long xsize;
  long ysize;
};

struct Tmouse_info {
  float mousex;
  float mousey;
  float mousez;
};

typedef void *TNonmaskedBM;
typedef void *TMaskedBM;
typedef long  HBITMAP;
typedef long  HWND;

#define LISP_PLOTTER_API  __declspec(dllexport)

extern "C" {
  
  LISP_PLOTTER_API void lpSelectWindow(long wid);
  LISP_PLOTTER_API void lpShowWindow(long wid);
  LISP_PLOTTER_API void lpEraseWindow(long bg);
  LISP_PLOTTER_API void lpKillWindow(long wid);
  LISP_PLOTTER_API void lpOpenWindow(Twindow_setup *p);
  LISP_PLOTTER_API void lpAttachWindow(HWND hWnd);
  LISP_PLOTTER_API void lpShowImage(Timage_parms *p);
  LISP_PLOTTER_API void lpPlotAxes(Taxes_parms *p);
  LISP_PLOTTER_API void lpPlotData(Tdata_parms *p);
  LISP_PLOTTER_API void lpAutoscale(Tscale_parms *p);
  LISP_PLOTTER_API void lpPlotPolys(Tpolys_parms *p);
  LISP_PLOTTER_API void lpUpdateWindow();
  LISP_PLOTTER_API void lpDelayUpdateWindow();
  LISP_PLOTTER_API void lpGetWindowSize(Twindow_info *p);
  LISP_PLOTTER_API void lpSetCMap(unsigned char *cred, 
				  unsigned char *cgreen,
				  unsigned char *cblue);
  LISP_PLOTTER_API void lpGetMouse(Tmouse_info *p);
  LISP_PLOTTER_API void lpGetCoordValues(Tmouse_info *p);
  LISP_PLOTTER_API void lpCopyToClipboard();
  LISP_PLOTTER_API void lpSaveToBacking();
  LISP_PLOTTER_API long lpGetCursorHandle(char *cursor_filename);
  LISP_PLOTTER_API void lpDirectRedraw(HWND hWnd);
  LISP_PLOTTER_API void lpPromptForMultipleFiles(char *title, char *buf,
						 long buflen);
  
  // -----------------------------------------------------------------
  // Retro stuff...
  
  LISP_PLOTTER_API TNonmaskedBM* lpReadNonmaskedBM(const char* fname);
  LISP_PLOTTER_API TMaskedBM* lpReadMaskedBM(const char* fname, long trans);
  LISP_PLOTTER_API void lpDrawNonmasked(TNonmaskedBM* pbm,
					long ulc_x, long ulc_y,
					long bm_ulc_x, long bm_ulc_y,
					long bm_wd, long bm_ht);
  LISP_PLOTTER_API void lpDrawMasked(TMaskedBM* pbm,
				     long ulc_x, long ulc_y,
				     long bm_ulc_x, long bm_ulc_y,
				     long bm_wd, long bm_ht);
  LISP_PLOTTER_API void lpTileBG(TNonmaskedBM* pbm,
				 long ulc_x, long ulc_y, long wd, long ht,
				 long bm_ulc_x, long bm_ulc_y,
				 long bm_wd, long bm_ht);
  LISP_PLOTTER_API void lpDiscardBitmap(TNonmaskedBM *pbm);
  LISP_PLOTTER_API long lpClipDrawing(long clip_left, long clip_top,
				      long clip_width, long clip_height);
  LISP_PLOTTER_API void lpUnclipDrawing(long sav);
  LISP_PLOTTER_API void lpLockDrawing();
  LISP_PLOTTER_API void lpUnlockDrawing();
  LISP_PLOTTER_API HBITMAP lpGetImageBits(long left, long top,
					  long wd, long ht);
  LISP_PLOTTER_API void lpSetImageBits(HBITMAP hbm, long left,
				       long top, long wd, long ht);
  LISP_PLOTTER_API void lpDiscardImageBits(HBITMAP hbm);
  
};  // extern "C"

// -- end of scigraph_intf.h -- //

