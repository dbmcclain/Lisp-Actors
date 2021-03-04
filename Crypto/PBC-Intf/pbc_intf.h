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

#include <strings.h>
#include <stdlib.h>
#include <memory.h>

#include <pbc/pbc.h>

// ---------------------------------------------------
// for initial interface testing...

extern "C" {
  long echo(long nel, char* msg_in, char* msg_out);

  long init_pairing(long ctxt, char* param_str, long nel, long* psize);
		     
  long set_g2(long ctxt, unsigned char* pbuf);
  long set_g1(long ctxt, unsigned char* pbuf);
		     
  long get_g2(long ctxt, unsigned char* pbuf, long buflen);
  long get_g1(long ctxt, unsigned char* pbuf, long buflen);

  void make_key_pair(long ctxt, unsigned char* pskey, unsigned char* ppkey,
		     unsigned char* phash, long nhash);
  void sign_hash(long ctxt, unsigned char* psig, unsigned char* pskey,
		 unsigned char* phash, long nhash);

  long check_signature(long ctxt,
		       unsigned char* psig,
		       unsigned char* phash, long nhash,
		       unsigned char *pkey);
  
  void make_public_subkey(long ctxt, unsigned char* abuf,
			  unsigned char* pkey,
			  unsigned char* phash_id, long nhash);
  void make_secret_subkey(long ctxt, unsigned char* abuf,
			  unsigned char* skey,
			  unsigned char* phash_id, long nhash);

  void compute_pairing(long ctxt, unsigned char* gtbuf,
		       unsigned char* hbuf,
		       unsigned char* gbuf);

  void sakai_kasahara_encrypt(long ctxt, unsigned char* rbuf, // R result in G2
			      unsigned char* pbuf, // pairing result in GT
			      unsigned char* pkey, // public subkey in G2
			      unsigned char* phash, long nhash);
  void sakai_kasahara_decrypt(long ctxt, unsigned char* pbuf, // pairing result in GT
			      unsigned char* rbuf, // R pt in G2
			      unsigned char* sbuf); // secret subkey in G1
  long sakai_kasahara_check(long ctxt, unsigned char* rkey, // R in G2
			    unsigned char* pkey, // public subkey in G2
			    unsigned char* phash, long nhash);

  void add_G1_pts(long ctxt, unsigned char* pt1, unsigned char* pt2);
  void sub_G1_pts(long ctxt, unsigned char* pt1, unsigned char* pt2);
  void mul_G1_pts(long ctxt, unsigned char* pt1, unsigned char* pt2);
  void div_G1_pts(long ctxt, unsigned char* pt1, unsigned char* pt2);
  void exp_G1z(long ctxt, unsigned char* g1, unsigned char* zr);
  void neg_G1_pt(long ctxt, unsigned char* pt);
  void inv_G1_pt(long ctxt, unsigned char* pt);

  void add_G2_pts(long ctxt, unsigned char* pt1, unsigned char* pt2);
  void sub_G2_pts(long ctxt, unsigned char* pt1, unsigned char* pt2);
  void mul_G2_pts(long ctxt, unsigned char* pt1, unsigned char* pt2);
  void div_G2_pts(long ctxt, unsigned char* pt1, unsigned char* pt2);
  void exp_G2z(long ctxt, unsigned char* g2, unsigned char* zr);
  void neg_G2_pt(long ctxt, unsigned char* pt);
  void inv_G2_pt(long ctxt, unsigned char* pt);
  
  void add_Zr_vals(long ctxt, unsigned char* zr1, unsigned char* zr2);
  void sub_Zr_vals(long ctxt, unsigned char* zr1, unsigned char* zr2);
  void mul_Zr_vals(long ctxt, unsigned char* zr1, unsigned char* zr2);
  void div_Zr_vals(long ctxt, unsigned char* zr1, unsigned char* zr2);
  void exp_Zr_vals(long ctxt, unsigned char* zr1, unsigned char* zr2);
  void neg_Zr_val(long ctxt, unsigned char* zr);
  void inv_Zr_val(long ctxt, unsigned char* zr);

  void mul_GT_vals(long ctxt, unsigned char* gt1, unsigned char* gt2);
  void div_GT_vals(long ctxt, unsigned char* gt1, unsigned char* gt2);
  void exp_GTz(long ctxt, unsigned char* gt, unsigned char* zr);
  void inv_GT_val(long ctxt, unsigned char* gt);
  
  void get_G1_from_hash(long ctxt, unsigned char *g1_pt, unsigned char *phash, long nhash);
  void get_G2_from_hash(long ctxt, unsigned char *g2_pt, unsigned char *phash, long nhash);
  void get_Zr_from_hash(long ctxt, unsigned char *zr_val, unsigned char *phash, long nhash);
}

// -- end of pbc_intf.h -- //



