/* pbc-intf.c -- Interface between Lisp and PBC libs */
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

#include <stdint.h>
#include "pbc_intf.h"

// ---------------------------------------------------
// for initial interface testing...

extern "C"
long echo(long nel, char* msg_in, char* msg_out)
{
  memcpy(msg_out, msg_in, nel);
  return nel;
}

// ---------------------------------------------------

typedef struct pairing_context {
  bool       init_flag;
  pairing_t  pairing;
  element_t  g1_gen;
  element_t  g2_gen;
} pairing_context_t;

pairing_context_t context[16];

bool& IsInit(int ctxt)
{
  return context[ctxt].init_flag;
}

pairing_t& Pairing(int ctxt)
{
  return context[ctxt].pairing;
}

element_t& G1_gen(int ctxt)
{
   return context[ctxt].g1_gen;
}

element_t& G2_gen(int ctxt)
{
  return context[ctxt].g2_gen;
}

// -------------------------------------------------

extern "C"
long init_pairing(long ctxt, char* param_str, long nel, long* psize)
{
  long ans;
  
  if(IsInit(ctxt))
    {
      element_clear(G1_gen(ctxt));
      element_clear(G2_gen(ctxt));
      pairing_clear(Pairing(ctxt));
      IsInit(ctxt) = false;
    }
  ans = pairing_init_set_buf(Pairing(ctxt), param_str, nel);
  if(0 == ans)
    {
      element_t z, pair;
      
      element_init_G1(G1_gen(ctxt), Pairing(ctxt));
      element_init_G2(G2_gen(ctxt), Pairing(ctxt));
      
      element_init_GT(pair,    Pairing(ctxt)); // archetypes for sizing info
      element_init_Zr(z,       Pairing(ctxt));
      
      element_random(G1_gen(ctxt)); // default random values
      element_random(G2_gen(ctxt));

      psize[0] = element_length_in_bytes_compressed(G1_gen(ctxt));
      psize[1] = element_length_in_bytes_compressed(G2_gen(ctxt));
      psize[2] = element_length_in_bytes(pair);
      psize[3] = element_length_in_bytes(z);

      element_clear(pair);
      element_clear(z);

      IsInit(ctxt) = true;
    }
  return ans;
}

extern "C"
long set_g2(long ctxt,
	    unsigned char* pbuf)
{
  // Changing G2 generator invalidates all keying,
  // so remake default random key pair
  return element_from_bytes_compressed(G2_gen(ctxt), pbuf);
}

extern "C"
long set_g1(long ctxt,
	    unsigned char* pbuf)
{
  return element_from_bytes_compressed(G1_gen(ctxt), pbuf);
}

// --------------------------------------------

extern "C"
void make_key_pair(long ctxt,
		   unsigned char *pskey, unsigned char* ppkey,
		   unsigned char* phash, long nhash)
{
  element_t skey, pkey;
  element_init_Zr(skey, Pairing(ctxt));
  element_init_G2(pkey, Pairing(ctxt));
  
  element_from_hash(skey, phash, nhash);
  element_pow_zn(pkey, G2_gen(ctxt), skey);
  element_to_bytes(pskey, skey);
  element_to_bytes_compressed(ppkey, pkey);

  element_clear(skey);
  element_clear(pkey);
}
 
extern "C"
void sign_hash(long ctxt,
	       unsigned char* psig, unsigned char* pskey,
	       unsigned char* phash, long nhash)
{
  element_t sig, skey;

  element_init_G1(sig,  Pairing(ctxt));
  element_init_Zr(skey, Pairing(ctxt));
  element_from_hash(sig, phash, nhash);
  element_from_bytes(skey, pskey);
  element_pow_zn(sig, sig, skey);
  element_to_bytes_compressed(psig, sig);
  element_clear(sig);
  element_clear(skey);
}

extern "C"
void make_public_subkey(long ctxt,
			unsigned char* abuf,
			unsigned char* pkey,
			unsigned char* phash_id, long nhash)
{
  element_t z;
  element_t gx, gp;
  
  element_init_Zr(z, Pairing(ctxt));
  element_init_G2(gx, Pairing(ctxt));
  element_init_G2(gp, Pairing(ctxt));
  element_from_bytes_compressed(gp, pkey);
  element_from_hash(z, phash_id, nhash);
  element_pow_zn(gx, G2_gen(ctxt), z);
  element_mul(gp, gx, gp);
  element_to_bytes_compressed(abuf, gp); // ans is G2
  element_clear(z);
  element_clear(gx);
  element_clear(gp);
}

extern "C"
void make_secret_subkey(long ctxt,
			unsigned char* abuf,
			unsigned char* skey,
			unsigned char* phash_id, long nhash)
{
  element_t z, zs, s;

  element_init_Zr(z, Pairing(ctxt));
  element_init_Zr(zs, Pairing(ctxt));
  element_init_G1(s, Pairing(ctxt));
  element_from_hash(z, phash_id, nhash);   // get ID
  element_from_bytes(zs, skey);            // user's secret key
  element_add(z, z, zs);
  element_invert(z, z);
  element_pow_zn(s, G1_gen(ctxt), z);
  element_to_bytes_compressed(abuf, s);    // ans is G1
  element_clear(z);
  element_clear(zs);
  element_clear(s);
}

extern "C"
void compute_pairing(long ctxt,
		     unsigned char* gtbuf,
		     unsigned char* hbuf,
		     unsigned char* gbuf)
{
  element_t hh, gg, pair;

  element_init_G1(hh,   Pairing(ctxt));
  element_init_G2(gg,   Pairing(ctxt));
  element_init_GT(pair, Pairing(ctxt));
  
  element_from_bytes_compressed(hh, hbuf);
  element_from_bytes_compressed(gg, gbuf);
  pairing_apply(pair, hh, gg, Pairing(ctxt));
  element_to_bytes(gtbuf, pair);
  element_clear(pair);
  element_clear(hh);
  element_clear(gg);
}

extern "C"
void sakai_kasahara_encrypt(long ctxt,
			    unsigned char* rbuf, // R result in G2
			    unsigned char* pbuf, // pairing result in GT
			    unsigned char* pkey, // public subkey in G2
			    unsigned char* phash, long nhash)
{
  element_t zr, gt, pk;

  /* pk, pkey is the public-subkey */
  /* phash, zr is the hash(ID || Tstamp || msg) */
  /* result R = zr*Psubkey */
  /* result pairing e(zr*U,Psubkey) = e(U,zr*Psubkey) */
  
  element_init_G2(pk, Pairing(ctxt));
  element_init_Zr(zr, Pairing(ctxt));
  element_init_GT(gt, Pairing(ctxt));
  element_from_bytes_compressed(pk, pkey);
  element_from_hash(zr, phash, nhash);
  element_pow_zn(pk, pk, zr);
  element_to_bytes_compressed(rbuf, pk);

  element_pow_zn(pk, G2_gen(ctxt), zr);
  pairing_apply(gt, G1_gen(ctxt), pk, Pairing(ctxt));
  element_to_bytes(pbuf, gt);

  element_clear(zr);
  element_clear(gt);
  element_clear(pk);
}

extern "C"
void sakai_kasahara_decrypt(long ctxt,
			    unsigned char* pbuf, // pairing result in GT
			    unsigned char* rbuf, // R pt in G2
			    unsigned char* sbuf) // secret subkey in G1
{
  element_t gt, sk, rk;

  /* rk, rbuf is the R value from encryption */
  /* sk, sbuf is the secret_subkey */
  
  element_init_G1(sk, Pairing(ctxt));
  element_init_G2(rk, Pairing(ctxt));
  element_init_GT(gt, Pairing(ctxt));
  element_from_bytes_compressed(sk, sbuf);
  element_from_bytes_compressed(rk, rbuf);
  pairing_apply(gt, sk, rk, Pairing(ctxt));
  element_to_bytes(pbuf, gt);
  element_clear(sk);
  element_clear(rk);
  element_clear(gt);
}
			    
extern "C"
long sakai_kasahara_check(long ctxt,
			  unsigned char* rkey, // R in G2
			  unsigned char* pkey, // public subkey in G2
			  unsigned char* phash, long nhash)
{
  element_t zr, pk1, pk2;
  long      ans;

  /* rkey, pk2 is the R value from encryption */
  /* pkey, pk1 is the public_subkey */
  /* phash is hash(ID || Tstamp || msg) */
  
  element_init_G2(pk1, Pairing(ctxt));
  element_init_G2(pk2, Pairing(ctxt));
  element_init_Zr(zr, Pairing(ctxt));
  element_from_bytes_compressed(pk1, pkey);
  element_from_bytes_compressed(pk2, rkey);
  element_from_hash(zr, phash, nhash);
  element_pow_zn(pk1, pk1, zr);
  ans = element_cmp(pk1, pk2);
  element_clear(pk1);
  element_clear(pk2);
  element_clear(zr);
  return ans;
}

// -----------------------------------------------------------------

static long get_datum(element_t elt, unsigned char* pbuf, long buflen, bool cmpr = true)
{
  long len;

  if(cmpr)
    len = element_length_in_bytes_compressed(elt);
  else
    len = element_length_in_bytes(elt);

  if (NULL != pbuf)
    {
      if(buflen < len)
	return 0;
      if(cmpr)
	element_to_bytes_compressed(pbuf, elt);
      else
	element_to_bytes(pbuf, elt);
    }
  return len;
}

extern "C"
long get_g2(long ctxt,
	    unsigned char* pbuf, long buflen)
{
  return get_datum(G2_gen(ctxt), pbuf, buflen);
}

extern "C"
long get_g1(long ctxt,
	    unsigned char* pbuf, long buflen)
{
  return get_datum(G1_gen(ctxt), pbuf, buflen);
}

// ------------------------------------------------

extern "C"
long check_signature(long ctxt,
		     unsigned char* psig,
		     unsigned char* phash, long nhash,
		     unsigned char *pkey)
{
  element_t ptHash, ptPKey, ptSig, pair1, pair2;
  long      tf;
  element_init_G1(ptHash, Pairing(ctxt));
  element_init_G1(ptSig,  Pairing(ctxt));
  element_init_G2(ptPKey, Pairing(ctxt));
  element_init_GT(pair1,  Pairing(ctxt));
  element_init_GT(pair2,  Pairing(ctxt));
  
  element_from_bytes_compressed(ptSig, psig);
  element_from_hash(ptHash, phash, nhash);
  element_from_bytes_compressed(ptPKey, pkey);
  pairing_apply(pair1, ptSig,  G2_gen(ctxt), Pairing(ctxt));
  pairing_apply(pair2, ptHash, ptPKey,  Pairing(ctxt));
  tf = element_cmp(pair1, pair2);
  
  element_clear(ptHash);
  element_clear(ptPKey);
  element_clear(ptSig);
  element_clear(pair1);
  element_clear(pair2);
  
  return tf;
}

// ----------------------------------------------
// PBC Library does not handle incoming zero (identity) values very
// well, often returning total garbage in such cases. Instead, we must
// take precautions ourselves.

bool tst_nonzero (unsigned char* ptr, long nel)
{
  // search operand for a non-zero byte
  // this version assumes at least 8 bytes of memory in buffer
  return ((0 != ((uint64_t*)ptr)[0]) ||
	  (0 != memcmp(ptr, ptr+8, nel-8)));
  
  /*
  uint64_t *p64 = (uint64_t*)ptr;
  for(long ix = (nel >> 3); --ix >= 0; )
    if(*p64++)
      return true;
  uint32_t *p32 = (uint32_t*)p64;
  if((nel & 4) && (*(uint32_t*)ptr++))
    return true;
  uint16_t *p16 = (uint16_t*)p32;
  if((nel & 2) && (*(uint16_t*)ptr++))
    return true;
  uint8_t *p8 = (uint8_t*)p16;
  if((nel & 1) && *p8)
    return true;
  return false;
  */
  /*  
  for(long ix = nel; --ix >= 0;)
    if(ptr[ix])
      return true;
  return false;
  */
}

// ----------------------------------------------

extern "C"
void add_G1_pts(long ctxt,
		unsigned char* pt1, unsigned char* pt2)
{
  element_t p1, p2;
  long      nel;
  element_init_G1(p1, Pairing(ctxt));
  nel = element_length_in_bytes_compressed(p1);
  if(tst_nonzero(pt1, nel))
    {
      if(tst_nonzero(pt2, nel))
	{
	  element_init_G1(p2, Pairing(ctxt));
	  element_from_bytes_compressed(p1, pt1);
	  element_from_bytes_compressed(p2, pt2);
	  element_add(p1, p1, p2);
	  element_clear(p2);
	  if(element_is0(p1))
	    memset(pt1,0,nel);
	  else
	    element_to_bytes_compressed(pt1, p1);
	}
    }
  else if(tst_nonzero(pt2, nel))
    memcpy(pt1, pt2, nel);
  element_clear(p1);
}
  
extern "C"
void sub_G1_pts(long ctxt,
		unsigned char* pt1, unsigned char* pt2)
{
  element_t p1, p2;
  long      nel;
  element_init_G1(p1, Pairing(ctxt));
  nel = element_length_in_bytes_compressed(p1);
  if(tst_nonzero(pt1, nel))
    {
      if(tst_nonzero(pt2, nel))
	{
	  element_init_G1(p2, Pairing(ctxt));
	  element_from_bytes_compressed(p1, pt1);
	  element_from_bytes_compressed(p2, pt2);
	  element_sub(p1, p1, p2);
	  element_clear(p2);
	  if(element_is0(p1))
	    memset(pt1,0,nel);
	  else
	    element_to_bytes_compressed(pt1, p1);
	}
    }
  else if(tst_nonzero(pt2, nel))
    {
      element_from_bytes_compressed(p1, pt2);
      element_neg(p1, p1);
      element_to_bytes_compressed(pt1, p1);
    }
  element_clear(p1);
}
  
extern "C"
void mul_G1_pts(long ctxt,
		unsigned char* pt1, unsigned char* pt2)
{
  element_t p1, p2;
  long      nel;
  element_init_G1(p1, Pairing(ctxt));
  nel = element_length_in_bytes_compressed(p1);
  if(tst_nonzero(pt1, nel))
    {
      if(tst_nonzero(pt2, nel))
	{
	  element_init_G1(p2, Pairing(ctxt));
	  element_from_bytes_compressed(p1, pt1);
	  element_from_bytes_compressed(p2, pt2);
	  element_mul(p1, p1, p2);
	  element_clear(p2);
	  if(element_is0(p1))
	    memset(pt1,0,nel);
	  else
	    element_to_bytes_compressed(pt1, p1);
	}
    }
  else if(tst_nonzero(pt2, nel))
    memcpy(pt1, pt2, nel);
  element_clear(p1);
}
  
extern "C"
void div_G1_pts(long ctxt,
		unsigned char* pt1, unsigned char* pt2)
{
  element_t p1, p2;
  long      nel;
  element_init_G1(p1, Pairing(ctxt));
  nel = element_length_in_bytes_compressed(p1);
  if(tst_nonzero(pt1, nel))
    {
      if(tst_nonzero(pt2, nel))
	{
	  element_init_G1(p2, Pairing(ctxt));
	  element_from_bytes_compressed(p1, pt1);
	  element_from_bytes_compressed(p2, pt2);
	  element_div(p1, p1, p2);
	  element_clear(p2);
	  if(element_is0(p1))
	    memset(pt1,0,nel);
	  else
	    element_to_bytes_compressed(pt1, p1);
	}
    }
  else if(tst_nonzero(pt2, nel))
    {
      element_from_bytes_compressed(p1, pt2);
      element_invert(p1, p1);
      element_to_bytes_compressed(pt1, p1);
    }
  element_clear(p1);
}
  
extern "C"
void neg_G1_pt(long ctxt,
	       unsigned char* pt1)
{
  element_t p1;
  long      nel;
  element_init_G1(p1, Pairing(ctxt));
  nel = element_length_in_bytes_compressed(p1);
  if(tst_nonzero(pt1, nel))
    {
      element_from_bytes_compressed(p1, pt1);
      element_neg(p1, p1);
      element_to_bytes_compressed(pt1, p1);
    }
  element_clear(p1);
}
  
extern "C"
void inv_G1_pt(long ctxt,
	       unsigned char* pt1)
{
  element_t p1;
  long      nel;
  element_init_G1(p1, Pairing(ctxt));
  nel = element_length_in_bytes_compressed(p1);
  if(tst_nonzero(pt1, nel))
    {
      element_from_bytes_compressed(p1, pt1);
      element_invert(p1, p1);
      element_to_bytes_compressed(pt1, p1);
    }
  element_clear(p1);
}
  
// ----------------------------------------------

extern "C"
void add_G2_pts(long ctxt,
		unsigned char* pt1, unsigned char* pt2)
{
  element_t p1, p2;
  long      nel;
  element_init_G2(p1, Pairing(ctxt));
  nel = element_length_in_bytes_compressed(p1);
  if(tst_nonzero(pt1, nel))
    {
      if(tst_nonzero(pt2, nel))
	{
	  element_init_G2(p2, Pairing(ctxt));
	  element_from_bytes_compressed(p1, pt1);
	  element_from_bytes_compressed(p2, pt2);
	  element_add(p1, p1, p2);
	  element_clear(p2);
	  if(element_is0(p1))
	    memset(pt1,0,nel);
	  else
	    element_to_bytes_compressed(pt1, p1);
	}
    }
  else if(tst_nonzero(pt2, nel))
    memcpy(pt1, pt2, nel);
  element_clear(p1);
}
  
extern "C"
void sub_G2_pts(long ctxt,
		unsigned char* pt1, unsigned char* pt2)
{
  element_t p1, p2;
  long      nel;
  element_init_G2(p1, Pairing(ctxt));
  nel = element_length_in_bytes_compressed(p1);
  if(tst_nonzero(pt1, nel))
    {
      if(tst_nonzero(pt2, nel))
	{
	  element_init_G2(p2, Pairing(ctxt));
	  element_from_bytes_compressed(p1, pt1);
	  element_from_bytes_compressed(p2, pt2);
	  element_sub(p1, p1, p2);
	  element_clear(p2);
	  if(element_is0(p1))
	    memset(pt1,0,nel);
	  else
	    element_to_bytes_compressed(pt1, p1);
	}
    }
  else if(tst_nonzero(pt2, nel))
    {
      element_from_bytes_compressed(p1, pt2);
      element_neg(p1, p1);
      element_to_bytes_compressed(pt1, p1);
    }
  element_clear(p1);
}
  
extern "C"
void mul_G2_pts(long ctxt,
		unsigned char* pt1, unsigned char* pt2)
{
  element_t p1, p2;
  long      nel;
  element_init_G2(p1, Pairing(ctxt));
  nel = element_length_in_bytes_compressed(p1);
  if(tst_nonzero(pt1, nel))
    {
      if(tst_nonzero(pt2, nel))
	{
	  element_init_G2(p2, Pairing(ctxt));
	  element_from_bytes_compressed(p1, pt1);
	  element_from_bytes_compressed(p2, pt2);
	  element_mul(p1, p1, p2);
	  element_clear(p2);
	  if(element_is0(p1))
	    memset(pt1,0,nel);
	  else
	    element_to_bytes_compressed(pt1, p1);
	}
    }
  else if(tst_nonzero(pt2, nel))
    memcpy(pt1, pt2, nel);
  element_clear(p1);
}
  
extern "C"
void div_G2_pts(long ctxt,
		unsigned char* pt1, unsigned char* pt2)
{
  element_t p1, p2;
  long      nel;
  element_init_G2(p1, Pairing(ctxt));
  nel = element_length_in_bytes_compressed(p1);
  if(tst_nonzero(pt1, nel))
    {
      if(tst_nonzero(pt2, nel))
	{
	  element_init_G2(p2, Pairing(ctxt));
	  element_from_bytes_compressed(p1, pt1);
	  element_from_bytes_compressed(p2, pt2);
	  element_div(p1, p1, p2);
	  element_clear(p2);
	  if(element_is0(p1))
	    memset(pt1,0,nel);
	  else
	    element_to_bytes_compressed(pt1, p1);
	}
    }
  else if(tst_nonzero(pt2, nel))
    {
      element_from_bytes_compressed(p1, pt2);
      element_invert(p1, p1);
      element_to_bytes_compressed(pt1, p1);
    }
  element_clear(p1);
}
  
extern "C"
void neg_G2_pt(long ctxt,
	       unsigned char* pt1)
{
  element_t p1;
  long      nel;
  element_init_G2(p1, Pairing(ctxt));
  nel = element_length_in_bytes_compressed(p1);
  if(tst_nonzero(pt1, nel))
    {
      element_from_bytes_compressed(p1, pt1);
      element_neg(p1, p1);
      element_to_bytes_compressed(pt1, p1);
    }
  element_clear(p1);
}
  
extern "C"
void inv_G2_pt(long ctxt,
	       unsigned char* pt1)
{
  element_t p1;
  long      nel;
  element_init_G2(p1, Pairing(ctxt));
  nel = element_length_in_bytes_compressed(p1);
  if(tst_nonzero(pt1, nel))
    {
      element_from_bytes_compressed(p1, pt1);
      element_invert(p1, p1);
      element_to_bytes_compressed(pt1, p1);
    }
  element_clear(p1);
}
  
// ----------------------------------------------

extern "C"
void add_Zr_vals(long ctxt,
		 unsigned char* zr1, unsigned char* zr2)
{
  element_t z1, z2;
  element_init_Zr(z1, Pairing(ctxt));
  element_init_Zr(z2, Pairing(ctxt));
  element_from_bytes(z1, zr1);
  element_from_bytes(z2, zr2);
  element_add(z1, z1, z2);
  element_to_bytes(zr1, z1);
  element_clear(z1);
  element_clear(z2);
}
  
extern "C"
void sub_Zr_vals(long ctxt,
		 unsigned char* zr1, unsigned char* zr2)
{
  element_t z1, z2;
  element_init_Zr(z1, Pairing(ctxt));
  element_init_Zr(z2, Pairing(ctxt));
  element_from_bytes(z1, zr1);
  element_from_bytes(z2, zr2);
  element_sub(z1, z1, z2);
  element_to_bytes(zr1, z1);
  element_clear(z1);
  element_clear(z2);
}
  
extern "C"
void mul_Zr_vals(long ctxt,
		 unsigned char* zr1, unsigned char* zr2)
{
  element_t z1, z2;
  element_init_Zr(z1, Pairing(ctxt));
  element_init_Zr(z2, Pairing(ctxt));
  element_from_bytes(z1, zr1);
  element_from_bytes(z2, zr2);
  element_mul(z1, z1, z2);
  element_to_bytes(zr1, z1);
  element_clear(z1);
  element_clear(z2);
}
  
extern "C"
void div_Zr_vals(long ctxt,
		 unsigned char* zr1, unsigned char* zr2)
{
  element_t z1, z2;
  element_init_Zr(z1, Pairing(ctxt));
  element_init_Zr(z2, Pairing(ctxt));
  element_from_bytes(z1, zr1);
  element_from_bytes(z2, zr2);
  element_div(z1, z1, z2);
  element_to_bytes(zr1, z1);
  element_clear(z1);
  element_clear(z2);
}

extern "C"
void exp_Zr_vals(long ctxt,
		 unsigned char* zr1, unsigned char* zr2)
{
  element_t z1, z2;
  element_init_Zr(z1, Pairing(ctxt));
  element_init_Zr(z2, Pairing(ctxt));
  element_from_bytes(z1, zr1);
  element_from_bytes(z2, zr2);
  element_pow_zn(z1, z1, z2);
  element_to_bytes(zr1, z1);
  element_clear(z1);
  element_clear(z2);
}
  
extern "C"
void inv_Zr_val(long ctxt,
		unsigned char* zr)
{
  element_t z;
  long nel;
  element_init_Zr(z, Pairing(ctxt));
  nel = element_length_in_bytes(z);
  if(tst_nonzero(zr, nel))
    {
      element_from_bytes(z, zr);
      element_invert(z, z);
      element_to_bytes(zr, z);
    }
  element_clear(z);
}

extern "C"
void neg_Zr_val(long ctxt,
		  unsigned char* zr)
{
  element_t z;
  long nel;
  element_init_Zr(z, Pairing(ctxt));
  nel = element_length_in_bytes(z);
  if(tst_nonzero(zr, nel))
    {
      element_from_bytes(z, zr);
      element_neg(z, z);
      element_to_bytes(zr, z);
    }
  element_clear(z);
}

// ----------------------------------------------

extern "C"
void mul_G1z(long ctxt,
	       unsigned char* g1, unsigned char* zr)
{
  element_t z, g;
  long nelg, nelz;
  
  element_init_G1(g, Pairing(ctxt));
  nelg = element_length_in_bytes_compressed(g);
  if(tst_nonzero(g1, nelg))
    {
      element_init_Zr(z, Pairing(ctxt));
      nelz = element_length_in_bytes(z);
      if(tst_nonzero(zr, nelz))
	{
	  element_from_bytes(z, zr);
	  element_from_bytes_compressed(g, g1);
	  element_mul_zn(g, g, z);
	  if(element_is0(g))
	    memset(g1, 0, nelg);
	  else
	    element_to_bytes_compressed(g1, g);
	}
      else
	memset(g1, 0, nelg);
      element_clear(z);
    }
  element_clear(g);
}
  
extern "C"
void exp_G1z(long ctxt,
	       unsigned char* g1, unsigned char* zr)
{
  element_t z, g;
  long nelg, nelz;
  
  element_init_G1(g, Pairing(ctxt));
  nelg = element_length_in_bytes_compressed(g);
  if(tst_nonzero(g1, nelg))
    {
      element_init_Zr(z, Pairing(ctxt));
      nelz = element_length_in_bytes(z);
      if(tst_nonzero(zr, nelz))
	{
	  element_from_bytes(z, zr);
	  element_from_bytes_compressed(g, g1);
	  element_pow_zn(g, g, z);
	  if(element_is0(g))
	    memset(g1, 0, nelg);
	  else
	    element_to_bytes_compressed(g1, g);
	}
      else
	memset(g1, 0, nelg);
      element_clear(z);
    }
  element_clear(g);
}
  
// ----------------------------------------------

extern "C"
void mul_G2z(long ctxt,
	       unsigned char* g1, unsigned char* zr)
{
  element_t z, g;
  long nelg, nelz;
  
  element_init_G2(g, Pairing(ctxt));
  nelg = element_length_in_bytes_compressed(g);
  if(tst_nonzero(g1, nelg))
    {
      element_init_Zr(z, Pairing(ctxt));
      nelz = element_length_in_bytes(z);
      if(tst_nonzero(zr, nelz))
	{
	  element_from_bytes(z, zr);
	  element_from_bytes_compressed(g, g1);
	  element_mul_zn(g, g, z);
	  if(element_is0(g))
	    memset(g1, 0, nelg);
	  else
	    element_to_bytes_compressed(g1, g);
	}
      else
	memset(g1, 0, nelg);
      element_clear(z);
    }
  element_clear(g);
}
  
extern "C"
void exp_G2z(long ctxt,
	       unsigned char* g1, unsigned char* zr)
{
  element_t z, g;
  long nelg, nelz;
  
  element_init_G2(g, Pairing(ctxt));
  nelg = element_length_in_bytes_compressed(g);
  if(tst_nonzero(g1, nelg))
    {
      element_init_Zr(z, Pairing(ctxt));
      nelz = element_length_in_bytes(z);
      if(tst_nonzero(zr, nelz))
	{
	  element_from_bytes(z, zr);
	  element_from_bytes_compressed(g, g1);
	  element_pow_zn(g, g, z);
	  if(element_is0(g))
	    memset(g1, 0, nelg);
	  else
	    element_to_bytes_compressed(g1, g);
	}
      else
	memset(g1, 0, nelg);
      element_clear(z);
    }
  element_clear(g);
}
  
// ----------------------------------------------

extern "C"
void mul_GT_vals(long ctxt,
		   unsigned char* gt1, unsigned char* gt2)
{
  element_t z1, z2;
  long nel;
  element_init_GT(z1, Pairing(ctxt));
  nel = element_length_in_bytes(z1);
  if(tst_nonzero(gt1, nel))
    {
      if(tst_nonzero(gt2, nel))
	{
	  element_init_GT(z2, Pairing(ctxt));
	  element_from_bytes(z1, gt1);
	  element_from_bytes(z2, gt2);
	  element_mul(z1, z1, z2);
	  element_clear(z2);
	  if(element_is0(z1))
	    memset(gt1, 0, nel);
	  else
	    element_to_bytes(gt1, z1);
	}
      else
	memset(gt1, 0, nel);
    }
  element_clear(z1);
}
  
extern "C"
void div_GT_vals(long ctxt,
		   unsigned char* gt1, unsigned char* gt2)
{
  element_t z1, z2;
  long nel;
  
  element_init_GT(z1, Pairing(ctxt));
  nel = element_length_in_bytes(z1);
  if(tst_nonzero(gt1, nel))
    {
      if(tst_nonzero(gt2, nel))
	{
	  element_init_GT(z2, Pairing(ctxt));
	  element_from_bytes(z1, gt1);
	  element_from_bytes(z2, gt2);
	  element_div(z1, z1, z2);
	  element_clear(z2);
	  if(element_is0(z1))
	    memset(gt1, 0, nel);
	  else
	    element_to_bytes(gt1, z1);
	}
      else
	memset(gt1, 0, nel);
    }
  element_clear(z1);
}
  
extern "C"
void inv_GT_val(long ctxt,
		unsigned char* gt)
{
  element_t z1;
  long nel;
  
  element_init_GT(z1, Pairing(ctxt));
  nel = element_length_in_bytes(z1);
  if(tst_nonzero(gt, nel))
    {
      element_from_bytes(z1, gt);
      element_invert(z1, z1);
      element_to_bytes(gt, z1);
    }
  element_clear(z1);
}
  
extern "C"
void exp_GTz(long ctxt,
	     unsigned char* gt, unsigned char* zr)
{
  element_t z1, z2;
  long nelg, nelz;
  
  element_init_GT(z1, Pairing(ctxt));
  nelg = element_length_in_bytes(z1);
  if(tst_nonzero(gt, nelg))
    {
      element_init_Zr(z2, Pairing(ctxt));
      nelz = element_length_in_bytes(z2);
      if(tst_nonzero(zr, nelz))
	{
	  element_from_bytes(z1, gt);
	  element_from_bytes(z2, zr);
	  element_pow_zn(z1, z1, z2);
	  if(element_is0(z1))
	    memset(gt, 0, nelg);
	  else
	    element_to_bytes(gt, z1);
	}
      else
	memset(gt, 0, nelg);
      element_clear(z2);
    }
  element_clear(z1);
}
  
// ----------------------------------------------

extern "C"
void get_G1_from_hash(long ctxt,
		      unsigned char *g1_pt, unsigned char *phash, long nhash)
{
  element_t g;

  element_init_G1(g, Pairing(ctxt));
  element_from_hash(g, phash, nhash);
  element_to_bytes_compressed(g1_pt, g);
  element_clear(g);
}

extern "C"
void get_G2_from_hash(long ctxt,
		      unsigned char *g2_pt, unsigned char *phash, long nhash)
{
  element_t g;

  element_init_G2(g, Pairing(ctxt));
  element_from_hash(g, phash, nhash);
  element_to_bytes_compressed(g2_pt, g);
  element_clear(g);
}

extern "C"
void get_Zr_from_hash(long ctxt,
		      unsigned char *zr_val, unsigned char *phash, long nhash)
{
  element_t z;

  element_init_Zr(z, Pairing(ctxt));
  element_from_hash(z, phash, nhash);
  element_to_bytes(zr_val, z);
  element_clear(z);
}

// -- end of pbc_intf.cpp -- //



