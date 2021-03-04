/* curve1174_intf.h -- Interface between Lisp and support for Curve1174 */
/* DM/Emotiq 07/18 */
/*
Copyright (c) 2018 Emotiq AG

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
 */

#ifndef __curve1174_intf_h__
#define __curve1174_intf_h__

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

#endif // __curve1174_intf_h__

// -- end of curve1174_intf.h -- //
