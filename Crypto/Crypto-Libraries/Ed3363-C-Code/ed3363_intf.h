/* ed3363_intf.h -- Interface between Lisp and support for curve Ed3363 */
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

#ifndef __ed3363_intf_h__
#define __ed3363_intf_h__

#include <strings.h>
#include <stdlib.h>
#include <memory.h>

// ---------------------------------------------------
// for initial interface testing...

extern "C" {
  void Ed3363_affine_mul(unsigned char* ptx,
			 unsigned char* pty,
			 unsigned char* ptz,
			 unsigned char* nv);

  void Ed3363_projective_mul(unsigned char* ptx,
			     unsigned char* pty,
			     unsigned char* ptz,
			     unsigned char* nv);
  
  void Ed3363_projective_add(unsigned char* lp1x,
			     unsigned char* lp1y,
			     unsigned char* lp1z,
			     unsigned char* lp2x,
			     unsigned char* lp2y,
			     unsigned char* lp2z);
  
  void Ed3363_to_affine(unsigned char* lpx,
			unsigned char* lpy,
			unsigned char* lpz);
}

#endif // __ed3363_intf_h__

// -- end of ed3363_intf.h -- //
