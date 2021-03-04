// ldb-dpb.c -- C implementation for Load bits and Deposit bits
//
// DM/Stegos  03/19
//
/* ------------------------------------------------------------------
The MIT License

Copyright (c) 2017-2018 Refined Audiometrics Laboratory, LLC

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
 ------------------------------------------------------------------------ */

u64 ldb(u64 *v, usize len, usize pos) {
  // for len <= 64
  uint  byte = pos >> 6;
  uint  off  = pos & 63;
  u64   mask = (1 << len) - 1;
  
  u64  xlo = v[byte] >> off;
  uint nlo = 64 - off;
  if(nlo >= len) { 
    return (xlo & mask); 
  }
  u64 xhi = v[byte+1] << nlo;
  return ((xhi | xlo) & mask);
}

void dbp(u64 x, u64 *v, usize len, usize pos) {
  // for len <= 64
  uint byte = pos >> 6;
  uint off  = pos & 63;
  u64  mask = (1 << len) - 1;

  u64 xbits = (x & mask);
  v[byte] &= ~(mask << off);
  v[byte] |= (xbits << off);

  uint nlo = 64 - off;
  if(nlo < len) {
    usize nsh = len - nlo;
    v[byte+1] &= ~(mask >> nsh);
    v[byte+1] |= (xbits >> nsh);
  }
}

  
  
