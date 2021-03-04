/* pbc-intf.h -- Interface between Lisp and PBC libs */
/* DM/Emotiq 03/18 */
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

#ifndef __pbc_intf_h__
#define __pbc_intf_h__

#include <strings.h>
#include <stdlib.h>
#include <memory.h>
#include <stdint.h>

#include <pbc/pbc.h>

// ---------------------------------------------------
// for initial interface testing...

extern "C" {
  uint64_t echo(uint64_t nel, char* msg_in, char* msg_out);

  int64_t init_pairing(uint64_t ctxt, char* param_str, uint64_t nel, uint64_t* psize);
		     
  int64_t set_g2(uint64_t ctxt, uint8_t* pbuf);
  int64_t set_g1(uint64_t ctxt, uint8_t* pbuf);
		     
  uint64_t get_g2(uint64_t ctxt, uint8_t* pbuf, uint64_t buflen);
  uint64_t get_g1(uint64_t ctxt, uint8_t* pbuf, uint64_t buflen);

  void make_key_pair(uint64_t ctxt, uint8_t* pskey, uint8_t* ppkey,
		     uint8_t* phash, uint64_t nhash);
  void sign_hash(uint64_t ctxt, uint8_t* psig, uint8_t* pskey,
		 uint8_t* phash, uint64_t nhash);

  int64_t check_signature(uint64_t ctxt,
		       uint8_t* psig,
		       uint8_t* phash, uint64_t nhash,
		       uint8_t *pkey);
  
  void make_public_subkey(uint64_t ctxt, uint8_t* abuf,
			  uint8_t* pkey,
			  uint8_t* phash_id, uint64_t nhash);
  void make_secret_subkey(uint64_t ctxt, uint8_t* abuf,
			  uint8_t* skey,
			  uint8_t* phash_id, uint64_t nhash);

  void compute_pairing(uint64_t ctxt, uint8_t* gtbuf,
		       uint8_t* hbuf,
		       uint8_t* gbuf);

  void sakai_kasahara_encrypt(uint64_t ctxt, uint8_t* rbuf, // R result in G2
			      uint8_t* pbuf, // pairing result in GT
			      uint8_t* pkey, // public subkey in G2
			      uint8_t* phash, uint64_t nhash);
  void sakai_kasahara_decrypt(uint64_t ctxt, uint8_t* pbuf, // pairing result in GT
			      uint8_t* rbuf, // R pt in G2
			      uint8_t* sbuf); // secret subkey in G1
  int64_t sakai_kasahara_check(uint64_t ctxt, uint8_t* rkey, // R in G2
			    uint8_t* pkey, // public subkey in G2
			    uint8_t* phash, uint64_t nhash);

  void add_G1_pts(uint64_t ctxt, uint8_t* pt1, uint8_t* pt2);
  void sub_G1_pts(uint64_t ctxt, uint8_t* pt1, uint8_t* pt2);
  void mul_G1_pts(uint64_t ctxt, uint8_t* pt1, uint8_t* pt2);
  void div_G1_pts(uint64_t ctxt, uint8_t* pt1, uint8_t* pt2);
  void exp_G1z(uint64_t ctxt, uint8_t* g1, uint8_t* zr);
  void neg_G1_pt(uint64_t ctxt, uint8_t* pt);
  void inv_G1_pt(uint64_t ctxt, uint8_t* pt);

  void add_G2_pts(uint64_t ctxt, uint8_t* pt1, uint8_t* pt2);
  void sub_G2_pts(uint64_t ctxt, uint8_t* pt1, uint8_t* pt2);
  void mul_G2_pts(uint64_t ctxt, uint8_t* pt1, uint8_t* pt2);
  void div_G2_pts(uint64_t ctxt, uint8_t* pt1, uint8_t* pt2);
  void exp_G2z(uint64_t ctxt, uint8_t* g2, uint8_t* zr);
  void neg_G2_pt(uint64_t ctxt, uint8_t* pt);
  void inv_G2_pt(uint64_t ctxt, uint8_t* pt);
  
  void add_Zr_vals(uint64_t ctxt, uint8_t* zr1, uint8_t* zr2);
  void sub_Zr_vals(uint64_t ctxt, uint8_t* zr1, uint8_t* zr2);
  void mul_Zr_vals(uint64_t ctxt, uint8_t* zr1, uint8_t* zr2);
  void div_Zr_vals(uint64_t ctxt, uint8_t* zr1, uint8_t* zr2);
  void exp_Zr_vals(uint64_t ctxt, uint8_t* zr1, uint8_t* zr2);
  void neg_Zr_val(uint64_t ctxt, uint8_t* zr);
  void inv_Zr_val(uint64_t ctxt, uint8_t* zr);

  void mul_GT_vals(uint64_t ctxt, uint8_t* gt1, uint8_t* gt2);
  void div_GT_vals(uint64_t ctxt, uint8_t* gt1, uint8_t* gt2);
  void exp_GTz(uint64_t ctxt, uint8_t* gt, uint8_t* zr);
  void inv_GT_val(uint64_t ctxt, uint8_t* gt);
  
  void get_G1_from_hash(uint64_t ctxt, uint8_t *g1_pt, uint8_t *phash, uint64_t nhash);
  void get_G2_from_hash(uint64_t ctxt, uint8_t *g2_pt, uint8_t *phash, uint64_t nhash);
  void get_Zr_from_hash(uint64_t ctxt, uint8_t *zr_val, uint8_t *phash, uint64_t nhash);
}

#endif // __pbc_intf_h__

// -- end of pbc_intf.h -- //
