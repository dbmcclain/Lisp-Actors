/* curve-e521_intf.h -- Interface between Lisp and support for Curve-E521 */
/* DM/RAL 01/24 */

#ifndef __curve_e521_intf_h__
#define __curve_e521_intf_h__

#include <strings.h>
#include <stdlib.h>
#include <memory.h>

// ---------------------------------------------------
// for initial interface testing...

extern "C" {
  void Curve1174_affine_mul(unsigned char* ptx,
			    unsigned char* pty,
			    unsigned char* ptz,
			    unsigned char* nv);
  
  void Curve1174_projective_mul(unsigned char* ptx,
				unsigned char* pty,
				unsigned char* ptz,
				unsigned char* nv);
  
  void Curve1174_projective_add(unsigned char* lp1x,
				unsigned char* lp1y,
				unsigned char* lp1z,
				unsigned char* lp2x,
				unsigned char* lp2y,
				unsigned char* lp2z);
  
  void Curve1174_to_affine(unsigned char* lpx,
			   unsigned char* lpy,
			   unsigned char* lpz);

  bool g1173_sqrt(unsigned char* lpsrc, 
                  unsigned char* lpdst);
}

#endif // __curve_e521_intf_h__

// -- end of curve-e521_intf.h -- //
