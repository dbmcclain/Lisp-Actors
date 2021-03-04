// ecc.cpp -- NIST B571 ECC over F_2^571
// DMcClain/Acudora  11/11
// ---------------------------------------------

#include "ecc.h"

// ------------------------------------------------------------
// GF(2^128)
// ------------------------------------------------------------

static_gf128_opnd::static_gf128_opnd()
{
  opnd = bytes;
}

static_gf128_opnd::static_gf128_opnd(const gf128_opnd &op)
{
  opnd = bytes;
  gf128_copy(op);
}

static_gf128_opnd::static_gf128_opnd(const ubyte *pnum, uint nb)
{
  opnd = bytes;
  if(nb > sizeof(gf128_opnd_bytes))
    nb = sizeof(gf128_opnd_bytes);
  memmove(opnd, pnum, nb);
  if(nb < sizeof(gf128_opnd_bytes))
    memset(opnd + nb, 0, sizeof(gf128_opnd_bytes)-nb);
}

static_gf128_opnd::static_gf128_opnd(const char *str)
{
  opnd = bytes;
  init_from_string(str);
}

void init_from_string_for_nbytes(const char *str,
				 ubyte      *opnd,
				 const int  maxbytes)
{
  int ix, jx;

  for(jx = 0, ix = strlen(str);
      (ix -= 2, (jx < maxbytes) && (ix >= 0));
      ++jx)
    {
      int x;
      sscanf(&str[ix], "%02x", &x);
      opnd[jx] = x;
    }
  if((jx < maxbytes) && (-1 == ix))
    {
      int x;
      sscanf(str, "%01x", &x);
      opnd[jx] = x;
    }
}

gf128_opnd& gf128_opnd::init_from_string(const char *str)
{
  zero();
  init_from_string_for_nbytes(str, opnd, sizeof(gf128_opnd_bytes));
  return *this;
}

// -----------------------------------------------------------

inline
gf128_opnd& gf128_add(const gf128_opnd &op1, const gf128_opnd &op2, gf128_opnd &opdst)
{
  ulong* pdst = (ulong*)opdst.opnd;
  ulong* p1   = (ulong*)op1.opnd;
  ulong* p2   = (ulong*)op2.opnd;
  pdst[0] = p1[0] ^ p2[0];
  pdst[1] = p1[1] ^ p2[1];
  return opdst;
}

inline
void gf128_opnd::zero()
{
  ulong* p = (ulong*)opnd;
  p[0] = 0;
  p[1] = 0;
}

inline
void gf128_opnd::one()
{
  ulong* p = (ulong*)opnd;
  p[1] = 0;
  p[0] = 1;
}

inline
void gf128_opnd::two()
{
  ulong* p = (ulong*)opnd;
  p[1] = 0;
  p[0] = 2;
}

inline
void gf128_opnd::prim()
{
  ulong* p = (ulong*)opnd;
  p[1] = 0;
  p[0] = 0x87;
}

inline
bool gf128_opnd::is_one() const
{
  ulong *p = (ulong*)opnd;

  return ((0 == p[1]) &&
	  (1 == p[0]));
}

inline
void gf128_opnd::step()
{
  ulong *p = (ulong*)opnd;
  int hibit = (opnd[sizeof(gf128_opnd_bytes)-1] & 0x080);

  p[1] = (p[1] << 1) | (opnd[7] >> 7);
  p[0] <<= 1;
  if(hibit)
    opnd[0] ^= 0x087;
}

void gf128_opnd::first_bit(int &ix, uint &mask) const
{
  ulong *p = (ulong*)opnd;
  if(p[1])
    ix = 2*sizeof(ulong);
  else if(p[0])
    ix = sizeof(ulong);
  else
    {
      ix = -1;
      return;
    }
  ubyte b;
  for(; 0 == (b = opnd[--ix]);) // assignment intended
    ;
  for(mask = 0x080; 0 == (mask & b); mask >>= 1)
    ;
}

int gf128_opnd::integer_length() const
{
  int  ix;
  uint mask;

  first_bit(ix, mask);
  if(ix < 0)
    return 0;
  ix = 8*(ix+1);
  for(; --ix, mask <<= 1;)
    ;
  return (ix+1);
}

inline
gf128_opnd& gf128_opnd::gf128_copy(const gf128_opnd &op)
{
  if(opnd != op.opnd)
    {
      ulong* psrc = (ulong*)op.opnd;
      ulong* pdst = (ulong*)opnd;
      pdst[0] = psrc[0];
      pdst[1] = psrc[1];
    }
  return *this;
}
      
gf128_opnd& gf128_mul(const gf128_opnd &op1, const gf128_opnd &op2, gf128_opnd &opdst)
{
  int  ix;
  uint mask;
  
  op2.first_bit(ix, mask);
  if(ix < 0)
    {
      opdst.zero();
      return opdst;
    }
  
  static_gf128_opnd ans(op1);

  for(; ix >= 0; --ix, mask = 0x100)
    {
      ubyte b = op2.opnd[ix];
      for(; mask >>= 1; )
	{
	  ans.step();
	  if(mask & b)
	    gf128_add(ans, op1, ans);
	} 
    }
  opdst.gf128_copy(ans);
  return opdst;
}

#if 0
gf128_opnd& gf128_inv(const gf128_opnd &op, gf128_opnd &opdst)
{
  static_gf128_opnd p(op);
  
  opdst.one();
  for(int ix = 1; ix < 128; ++ix)
    {
      gf128_mul(p, p, p);
      gf128_mul(p, opdst, opdst);
    }
  return opdst;
}
#else
gf128_opnd& gf128_inv(const gf128_opnd &op, gf128_opnd &opdst)
{
  if(op.is_one())
    {
      opdst.one();
      return opdst;
    }
  
  // using extended Euclidean algorithm
  static_gf128_opnd u;
  static_gf128_opnd v(op);
  static_gf128_opnd g1;
  static_gf128_opnd g2;
  static_gf128_opnd tmp;

  g2.one();

  {
    int j = 129 - v.integer_length();
    gf128_shiftl(v, j, u);
    u.opnd[0] ^= 0x087;
    gf128_shiftl(g2, j, g1);
  }
		 
  while(!(u.is_one()))
    {
      // if(!u)
      //   throw("gf128_inv: zero divisor");
     
      int j = u.integer_length() - v.integer_length();
      if(j < 0)
	{
	  ubyte *pb = u.opnd;
	  u.opnd = v.opnd;
	  v.opnd = pb;

	  pb = g1.opnd;
	  g1.opnd = g2.opnd;
	  g2.opnd = pb;
	
	  j = -j;
	}
      gf128_shiftl(v, j, tmp);
      gf128_add(u, tmp, u);
      gf128_shiftl(g2, j, tmp);
      gf128_add(g1, tmp, g1);
    }
  opdst.gf128_copy(g1);
  return opdst;
}
#endif

gf128_opnd& gf128_div(const gf128_opnd& op1, const gf128_opnd& op2, gf128_opnd& opdst)
{
  static_gf128_opnd tmp;
  gf128_inv(op2, tmp);
  return gf128_mul(op1, tmp, opdst);
}

inline
bool gf128_opnd::operator!() const
{
  ulong *p = (ulong*)opnd;
  return ((0 == p[0]) &&
	  (0 == p[1]));
}

inline
gf128_opnd& gf128_oneplus(const gf128_opnd &op, gf128_opnd &opdst)
{
  ulong* psrc = (ulong*)op.opnd;
  ulong* pdst = (ulong*)opdst.opnd;
  pdst[1] = psrc[1];
  pdst[0] = psrc[0] ^ 1;
  return opdst;
}

inline
bool gf128_opnd::operator==(const gf128_opnd &op) const
{
  if(opnd == op.opnd)
    return true;
  ulong *p1 = (ulong*)opnd;
  ulong *p2 = (ulong*)op.opnd;
  return ((p1[0] == p2[0]) &&
	  (p1[1] == p2[1]));
}

gf128_opnd& gf128_shiftl(const gf128_opnd &op, int nsh, gf128_opnd &opdst)
{
  ulong* psrc = (ulong*)op.opnd;
  ulong* pdst = (ulong*)opdst.opnd;

  switch(nsh >> 6)
    {
    case 1:
      {
	if(nsh & 63)
	  {
	    int nl = nsh - 64;
	    int nr = 128 - nsh;
	    
	    pdst[1] = (psrc[0] << nl);
	    pdst[0] = 0;
	  }
	else
	  {
	    pdst[1] = psrc[0];
	    pdst[0] = 0;
	  }
      }
      break;

    case 0:
      {
	if(nsh & 63)
	  {
	    int nl = nsh;
	    int nr = 64 - nsh;
	    
	    pdst[1] = ((psrc[1] << nl) | (psrc[0] >> nr));
	    pdst[0] = (psrc[0] << nl);
	  }
	else
	  {
	    pdst[1] = psrc[1];
	    pdst[0] = psrc[0];
	  }
      }
      break;

    default:
      {
	pdst[1] = 0;
	pdst[2] = 0;
      }
      break;
    }
  return opdst;
}

void gf128_opnd::show()
{
  for(int ix = sizeof(gf128_opnd_bytes); --ix >= 0; )
    printf("%02X", opnd[ix]);
}

// ------------------------------------------------------------
// GF(2^571)
// ------------------------------------------------------------

static_gf571_opnd::static_gf571_opnd()
{
  opnd = bytes;
}

static_gf571_opnd::static_gf571_opnd(const gf571_opnd &op)
{
  opnd = bytes;
  gf571_copy(op);
}

static_gf571_opnd::static_gf571_opnd(const ubyte *pnum, uint nb)
{
  opnd = bytes;
  if(nb > sizeof(gf571_opnd_bytes))
    nb = sizeof(gf571_opnd_bytes);
  memmove(opnd, pnum, nb);
  if(nb < sizeof(gf571_opnd_bytes))
    memset(opnd + nb, 0, sizeof(gf571_opnd_bytes) - nb);
  opnd[sizeof(gf571_opnd_bytes)-1] &= 0x07;
}

static_gf571_opnd::static_gf571_opnd(const char *str)
{
  opnd = bytes;
  init_from_string(str);
}

gf571_opnd& gf571_opnd::init_from_string(const char *str)
{
  zero();
  init_from_string_for_nbytes(str, opnd, sizeof(gf571_opnd_bytes));
  opnd[sizeof(gf571_opnd_bytes)-1] &= 0x07;
  return *this;
}

// -----------------------------------------------------------

inline
gf571_opnd& gf571_add(const gf571_opnd &op1, const gf571_opnd &op2, gf571_opnd &opdst)
{
  ulong* pdst = (ulong*)opdst.opnd;
  ulong* p1   = (ulong*)op1.opnd;
  ulong* p2   = (ulong*)op2.opnd;

  pdst[0] = p1[0] ^ p2[0];
  pdst[1] = p1[1] ^ p2[1];
  pdst[2] = p1[2] ^ p2[2];
  pdst[3] = p1[3] ^ p2[3];
  pdst[4] = p1[4] ^ p2[4];
  pdst[5] = p1[5] ^ p2[5];
  pdst[6] = p1[6] ^ p2[6];
  pdst[7] = p1[7] ^ p2[7];
  pdst[8] = p1[8] ^ p2[8];
  return opdst;
}

inline
void gf571_opnd::zero()
{
  ulong* pdst = (ulong*)opnd;
  
  pdst[8] = 0;
  pdst[7] = 0;
  pdst[6] = 0;
  pdst[5] = 0;
  pdst[4] = 0;
  pdst[3] = 0;
  pdst[2] = 0;
  pdst[1] = 0;
  pdst[0] = 0;
}

inline
void gf571_opnd::one()
{
  zero();
  opnd[0] = 1;
}

inline
void gf571_opnd::two()
{
  zero();
  opnd[0] = 2;
}

inline
void gf571_opnd::prim()
{
  zero();
  opnd[sizeof(gf571_opnd_bytes)-1] = 0x08;
  opnd[1] = 0x04;
  opnd[0] = 0x25;
}

inline
bool gf571_opnd::is_one() const
{
  ulong *p = (ulong*)opnd;

  return ((0 == p[8]) &&
	  (0 == p[7]) &&
	  (0 == p[6]) &&
	  (0 == p[5]) &&
	  (0 == p[4]) &&
	  (0 == p[3]) &&
	  (0 == p[2]) &&
	  (0 == p[1]) &&
	  (1 == p[0]));
}

inline
void gf571_opnd::step()
{
  ulong *p = (ulong*)opnd;
  int hibit = (opnd[sizeof(gf571_opnd_bytes)-1] & 0x0fc);
  static const int nrsh = 8*sizeof(ulong)-1;

  p[8] = ((p[8] << 1) | (p[7] >> nrsh));
  p[7] = ((p[7] << 1) | (p[6] >> nrsh));
  p[6] = ((p[6] << 1) | (p[5] >> nrsh));
  p[5] = ((p[5] << 1) | (p[4] >> nrsh));
  p[4] = ((p[4] << 1) | (p[3] >> nrsh));
  p[3] = ((p[3] << 1) | (p[2] >> nrsh));
  p[2] = ((p[2] << 1) | (p[1] >> nrsh));
  p[1] = ((p[1] << 1) | (p[0] >> nrsh));
  p[0] = (p[0] << 1);
  opnd[sizeof(gf571_opnd_bytes)-1] &= 0x07;
  if(hibit)
    {
      opnd[1] ^= 0x04;
      opnd[0] ^= 0x25;
    }
}

void gf571_opnd::first_bit(int &ix, uint &mask) const
{
  ubyte b;
  ulong *p = (ulong*)opnd;

  ix = sizeof(gf571_opnd_bytes) / sizeof(ulong);
  for(;;)
    {
      if(--ix < 0)
	return;
      if(p[ix])
	break;
    }
  ix = (ix + 1) * sizeof(ulong);
  for(; 0 == (b = opnd[--ix]);) // assignment intended
    ;
  for(mask = 0x080; 0 == (mask & b); mask >>= 1)
    ;
}

int gf571_opnd::integer_length() const
{
  int  ix;
  uint mask;

  first_bit(ix, mask);
  if(ix < 0)
    return 0;
  ix = 8*(ix+1);
  for(; --ix, (mask <<= 1);)
    ;
  return (1+ix);
}

inline
gf571_opnd& gf571_opnd::gf571_copy(const gf571_opnd &op)
{
  ulong* pdst = (ulong*)opnd;
  ulong* psrc = (ulong*)op.opnd;
  
  if(pdst != psrc)
    {
      pdst[0] = psrc[0];
      pdst[1] = psrc[1];
      pdst[2] = psrc[2];
      pdst[3] = psrc[3];
      pdst[4] = psrc[4];
      pdst[5] = psrc[5];
      pdst[6] = psrc[6];
      pdst[7] = psrc[7];
      pdst[8] = psrc[8];
    }
  return *this;
}
      
gf571_opnd& gf571_mul(const gf571_opnd &op1, const gf571_opnd &op2, gf571_opnd &opdst)
{
  int ix;
  uint mask;
  
  op2.first_bit(ix, mask);
  if(ix < 0)
    {
      opdst.zero();
      return opdst;
    }
  
  static_gf571_opnd ans(op1);

  for(; ix >= 0; --ix, mask = 0x100)
    {
      ubyte b = op2.opnd[ix];
      for(; (mask >>= 1); )
	{
	  ans.step();
	  if(mask & b)
	    gf571_add(ans, op1, ans);
	} 
    }
  opdst.gf571_copy(ans);
  return opdst;
}

#if 0
gf571_opnd& gf571_inv(const gf571_opnd &op, gf571_opnd &opdst)
{
  static_gf571_opnd p(op);
  
  opdst.one();
  for(int ix = 1; ix < 571; ++ix)
    {
      gf571_mul(p, p, p);
      gf571_mul(p, opdst, opdst);
    }
  return opdst;
}
#else
gf571_opnd& gf571_inv(const gf571_opnd &op, gf571_opnd &opdst)
{
  // using extended Euclidean algorithm
  static_gf571_opnd u(op);
  static_gf571_opnd v;
  static_gf571_opnd g1;
  static_gf571_opnd g2;
  static_gf571_opnd tmp;

  v.prim();
  g1.one();
  g2.zero();

  while(!(u.is_one()))
    {
      // if(!u)
      //   throw("gf571_inv: zero divisor");
     
      int j = u.integer_length() - v.integer_length();
      if(j < 0)
	{
	  ubyte *pb = u.opnd;
	  u.opnd = v.opnd;
	  v.opnd = pb;

	  pb = g1.opnd;
	  g1.opnd = g2.opnd;
	  g2.opnd = pb;
	
	  j = -j;
	}
      gf571_shiftl(v, j, tmp);
      gf571_add(u, tmp, u);
      gf571_shiftl(g2, j, tmp);
      gf571_add(g1, tmp, g1);
    }
  opdst.gf571_copy(g1);
  return opdst;
}
#endif

gf571_opnd& gf571_div(const gf571_opnd& op1, const gf571_opnd& op2, gf571_opnd& opdst)
{
  static_gf571_opnd tmp;
  gf571_inv(op2, tmp);
  return gf571_mul(op1, tmp, opdst);
}

inline
bool gf571_opnd::operator!() const
{
  ulong *p = (ulong*)opnd;
  
  return !(p[8] || p[7] || p[6] || p[5] || p[4] || p[3] || p[2] || p[1] || p[0]);
}

inline
gf571_opnd& gf571_oneplus(const gf571_opnd &op, gf571_opnd &opdst)
{
  opdst.gf571_copy(op);
  opdst.opnd[0] ^= 1;
  return opdst;
}

inline
bool gf571_opnd::operator==(const gf571_opnd &op) const
{
  ulong *p1 = (ulong*)opnd;
  ulong *p2 = (ulong*)op.opnd;
  return ((p1 == p2) ||
	  ((p1[0] == p2[0]) &&
	   (p1[1] == p2[1]) &&
	   (p1[2] == p2[2]) &&
	   (p1[3] == p2[3]) &&
	   (p1[4] == p2[4]) &&
	   (p1[5] == p2[5]) &&
	   (p1[6] == p2[6]) &&
	   (p1[7] == p2[7]) &&
	   (p1[8] == p2[8])));
}

gf571_opnd& gf571_shiftl(const gf571_opnd &op, int nsh, gf571_opnd &opdst)
{
  ulong* psrc = (ulong*)op.opnd;
  ulong* pdst = (ulong*)opdst.opnd;

  switch(nsh >> 6)
    {
    case 8:
      {
	if(nsh & 63)
	  {
	    int nl = nsh - 512;
	    
	    pdst[8] = (psrc[0] << nl);
	    pdst[7] = 0;
	    pdst[6] = 0;
	    pdst[5] = 0;
	    pdst[4] = 0;
	    pdst[3] = 0;
	    pdst[2] = 0;
	    pdst[1] = 0;
	    pdst[0] = 0;
	  }
	else
	  {
	    pdst[8] = psrc[0];
	    pdst[7] = 0;
	    pdst[6] = 0;
	    pdst[5] = 0;
	    pdst[4] = 0;
	    pdst[3] = 0;
	    pdst[2] = 0;
	    pdst[1] = 0;
	    pdst[0] = 0;
	  }
      }		
      break;
	    
    case 7:
      {
	if(nsh & 63)
	  {
	    int nl = nsh - 448;
	    int nr = 512 - nsh;
		  
	    pdst[8] = ((psrc[1] << nl) | (psrc[0] >> nr));
	    pdst[7] = (psrc[0] << nl);
	    pdst[6] = 0;
	    pdst[5] = 0;
	    pdst[4] = 0;
	    pdst[3] = 0;
	    pdst[2] = 0;
	    pdst[1] = 0;
	    pdst[0] = 0;
	  }
	else
	  {
	    pdst[8] = psrc[1];
	    pdst[7] = psrc[0];
	    pdst[6] = 0;
	    pdst[5] = 0;
	    pdst[4] = 0;
	    pdst[3] = 0;
	    pdst[2] = 0;
	    pdst[1] = 0;
	    pdst[0] = 0;
	  }
      }
      break;
	    
    case 6:
      {
	if(nsh & 63)
	  {
	    int nl = nsh - 384;
	    int nr = 448 - nsh;
		  
	    pdst[8] = ((psrc[2] << nl) | (psrc[1] >> nr));
	    pdst[7] = ((psrc[1] << nl) | (psrc[0] >> nr));
	    pdst[6] = (psrc[0] << nl);
	    pdst[5] = 0;
	    pdst[4] = 0;
	    pdst[3] = 0;
	    pdst[2] = 0;
	    pdst[1] = 0;
	    pdst[0] = 0;
	  }
	else
	  {
	    pdst[8] = psrc[2];
	    pdst[7] = psrc[1];
	    pdst[6] = psrc[0];
	    pdst[5] = 0;
	    pdst[4] = 0;
	    pdst[3] = 0;
	    pdst[2] = 0;
	    pdst[1] = 0;
	    pdst[0] = 0;
	  }
      }
      break;
	    
    case 5:
      {
	if(nsh & 63)
	  {
	    int nl = nsh - 320;
	    int nr = 384 - nsh;
		  
	    pdst[8] = ((psrc[3] << nl) | (psrc[2] >> nr));
	    pdst[7] = ((psrc[2] << nl) | (psrc[1] >> nr));
	    pdst[6] = ((psrc[1] << nl) | (psrc[0] >> nr));
	    pdst[5] = (psrc[0] << nl);
	    pdst[4] = 0;
	    pdst[3] = 0;
	    pdst[2] = 0;
	    pdst[1] = 0;
	    pdst[0] = 0;
	  }
	else
	  {
	    pdst[8] = psrc[3];
	    pdst[7] = psrc[2];
	    pdst[6] = psrc[1];
	    pdst[5] = psrc[0];
	    pdst[4] = 0;
	    pdst[3] = 0;
	    pdst[2] = 0;
	    pdst[1] = 0;
	    pdst[0] = 0;
	  }
      }
      break;
	    
    case 4:
      {
	if(nsh & 63)
	  {
	    int nl = nsh - 256;
	    int nr = 320 - nsh;
		  
	    pdst[8] = ((psrc[4] << nl) | (psrc[3] >> nr));
	    pdst[7] = ((psrc[3] << nl) | (psrc[2] >> nr));
	    pdst[6] = ((psrc[2] << nl) | (psrc[1] >> nr));
	    pdst[5] = ((psrc[1] << nl) | (psrc[0] >> nr));
	    pdst[4] = (psrc[0] << nl);
	    pdst[3] = 0;
	    pdst[2] = 0;
	    pdst[1] = 0;
	    pdst[0] = 0;
	  }
	else
	  {
	    pdst[8] = psrc[4];
	    pdst[7] = psrc[3];
	    pdst[6] = psrc[2];
	    pdst[5] = psrc[1];
	    pdst[4] = psrc[0];
	    pdst[3] = 0;
	    pdst[2] = 0;
	    pdst[1] = 0;
	    pdst[0] = 0;
	  }
      }
      break;
	    
    case 3:
      {
	if(nsh & 63)
	  {
	    int nl = nsh - 192;
	    int nr = 256 - nsh;
		  
	    pdst[8] = ((psrc[5] << nl) | (psrc[4] >> nr));
	    pdst[7] = ((psrc[4] << nl) | (psrc[3] >> nr));
	    pdst[6] = ((psrc[3] << nl) | (psrc[2] >> nr));
	    pdst[5] = ((psrc[2] << nl) | (psrc[1] >> nr));
	    pdst[4] = ((psrc[1] << nl) | (psrc[0] >> nr));
	    pdst[3] = (psrc[0] << nl);
	    pdst[2] = 0;
	    pdst[1] = 0;
	    pdst[0] = 0;
	  }
	else
	  {
	    pdst[8] = psrc[5];
	    pdst[7] = psrc[4];
	    pdst[6] = psrc[3];
	    pdst[5] = psrc[2];
	    pdst[4] = psrc[1];
	    pdst[3] = psrc[0];
	    pdst[2] = 0;
	    pdst[1] = 0;
	    pdst[0] = 0;
	  }
      }		
      break;
	    
    case 2:
      {
	if(nsh & 63)
	  {
	    int nl = nsh - 128;
	    int nr = 192 - nsh;
		  
	    pdst[8] = ((psrc[6] << nl) | (psrc[5] >> nr));
	    pdst[7] = ((psrc[5] << nl) | (psrc[4] >> nr));
	    pdst[6] = ((psrc[4] << nl) | (psrc[3] >> nr));
	    pdst[5] = ((psrc[3] << nl) | (psrc[2] >> nr));
	    pdst[4] = ((psrc[2] << nl) | (psrc[1] >> nr));
	    pdst[3] = ((psrc[1] << nl) | (psrc[0] >> nr));
	    pdst[2] = (psrc[0] << nl);
	    pdst[1] = 0;
	    pdst[0] = 0;
	  }
	else
	  {
	    pdst[8] = psrc[6];
	    pdst[7] = psrc[5];
	    pdst[6] = psrc[4];
	    pdst[5] = psrc[3];
	    pdst[4] = psrc[2];
	    pdst[3] = psrc[1];
	    pdst[2] = psrc[0];
	    pdst[1] = 0;
	    pdst[0] = 0;
	  }
      }
      break;
	    
    case 1:
      {
	if(nsh & 63)
	  {
	    int nl = nsh - 64;
	    int nr = 128 - nsh;
		  
	    pdst[8] = ((psrc[7] << nl) | (psrc[6] >> nr));
	    pdst[7] = ((psrc[6] << nl) | (psrc[5] >> nr));
	    pdst[6] = ((psrc[5] << nl) | (psrc[4] >> nr));
	    pdst[5] = ((psrc[4] << nl) | (psrc[3] >> nr));
	    pdst[4] = ((psrc[3] << nl) | (psrc[2] >> nr));
	    pdst[3] = ((psrc[2] << nl) | (psrc[1] >> nr));
	    pdst[2] = ((psrc[1] << nl) | (psrc[0] >> nr));
	    pdst[1] = (psrc[0] << nl);
	    pdst[0] = 0;
	  }
	else
	  {
	    pdst[8] = psrc[7];
	    pdst[7] = psrc[6];
	    pdst[6] = psrc[5];
	    pdst[5] = psrc[4];
	    pdst[4] = psrc[3];
	    pdst[3] = psrc[2];
	    pdst[2] = psrc[1];
	    pdst[1] = psrc[0];
	    pdst[0] = 0;
	  }
      }
      break;
	    
    case 0:
      {
	if(nsh & 63)
	  {
	    int nl = nsh;
	    int nr = 64 - nsh;
		  
	    pdst[8] = ((psrc[8] << nl) | (psrc[7] >> nr));
	    pdst[7] = ((psrc[7] << nl) | (psrc[6] >> nr));
	    pdst[6] = ((psrc[6] << nl) | (psrc[5] >> nr));
	    pdst[5] = ((psrc[5] << nl) | (psrc[4] >> nr));
	    pdst[4] = ((psrc[4] << nl) | (psrc[3] >> nr));
	    pdst[3] = ((psrc[3] << nl) | (psrc[2] >> nr));
	    pdst[2] = ((psrc[2] << nl) | (psrc[1] >> nr));
	    pdst[1] = ((psrc[1] << nl) | (psrc[0] >> nr));
	    pdst[0] = (psrc[0] << nl);
	  }
	else
	  opdst.gf571_copy(op);
      }
      break;
	    
    default:
      opdst.zero();
      break;
    }
  // needs to be a raw shift, so no masking of top byte
  return opdst;
}

void gf571_opnd::show()
{
  for(int ix = sizeof(gf571_opnd_bytes); --ix >= 0;)
    printf("%02X", opnd[ix]);
}

// ------------------------------------------------------------------
// ECC primitives for curve NIST-B571
// ------------------------------------------------------------

static_gf571_opnd gEcc571_a("1");
static_gf571_opnd gEcc571_b("2f40e7e2221f295de297117b7f3d62f5c6a97ffcb8ceff1cd6ba8ce4a9a18ad84ffabbd8efa59332be7ad6756a66e294afd185a78ff12aa520e4de739baca0c7ffeff7f2955727a");
static const char* genStr[2] = {
  "303001d34b856296c16c0d40d3cd7750a93d1d2955fa80aa5f40fc8db7b2abdbde53950f4c0d293cdd711a35b67fb1499ae60038614f1394abfa3b4c850d927e1e7769c8eec2d19",
  "37bf27342da639b6dccfffeb73d69d78c6c27a6009cbbca1980f8533921e8a684423e43bab08a576291af8f461bb2a8b3531d2f0485c19b16e2f1516e23dd3c1a4827af1b8ac15b"
};
const static_ecc571_affine_pt gGen(genStr);
// ------------------------------------------------------------

static_ecc571_affine_pt::static_ecc571_affine_pt()
{
  x.opnd = xbytes;
  y.opnd = ybytes;
}

static_ecc571_affine_pt::static_ecc571_affine_pt(const ecc571_affine_pt &pt)
{
  x.opnd = xbytes;
  y.opnd = ybytes;

  ecc571_copy(pt);
}

static_ecc571_affine_pt::static_ecc571_affine_pt(const char* str[2])
{
  x.opnd = xbytes;
  y.opnd = ybytes;

  x.init_from_string(str[0]);
  y.init_from_string(str[1]);
}

// ------------------------------------------------------------

ecc571_affine_pt::ecc571_affine_pt(ubyte* px, ubyte* py)
{
  x.opnd = px;
  y.opnd = py;
}

ecc571_affine_pt &ecc571_affine_pt::infinite()
{
  x.zero();
  y.zero();
  return *this;
}

bool ecc571_affine_pt::infinite_p() const
{
  return ((!x) && (!y));
}

ecc571_affine_pt& ecc571_affine_pt::ecc571_copy(const ecc571_affine_pt &pt)
{
  x.gf571_copy(pt.x);
  y.gf571_copy(pt.y);
  return *this;
}

ecc571_affine_pt& ecc571_neg(const ecc571_affine_pt &pt, ecc571_affine_pt &ptdst)
{
  ptdst.x.gf571_copy(pt.x);
  gf571_add(pt.x, pt.y, ptdst.y);
  return ptdst;
}

ecc571_affine_pt& ecc571_sub(const ecc571_affine_pt &pt1, const ecc571_affine_pt &pt2, ecc571_affine_pt &ptdst)
{
  static_ecc571_affine_pt tmp;
  
  ecc571_neg(pt2, tmp);
  return ecc571_add(pt1, tmp, ptdst);
}

ecc571_affine_pt& ecc571_double(const ecc571_affine_pt &pt, ecc571_affine_pt &ptdst)
{
  if(!pt.x)
    {
      ptdst.infinite();
      return ptdst;
    }
  
  static_gf571_opnd tmp1, tmp2;
  static_ecc571_affine_pt ans;

  // --- LISP MACHINE GENERATED CODE --- //
  gf571_div(pt.y, pt.x, ans.x);
  gf571_add(pt.x, ans.x, tmp1);
  gf571_mul(tmp1, tmp1, ans.x);
  gf571_add(tmp1, gEcc571_a, tmp2);
  gf571_add(ans.x, tmp2, ans.x);
  gf571_oneplus(tmp1, ans.y);
  gf571_mul(ans.y, ans.x, ans.y);
  gf571_mul(pt.x, pt.x, tmp1);
  gf571_add(ans.y, tmp1, ans.y);
  // --- LISP MACHINE GENERATED CODE --- //
  
  ptdst.ecc571_copy(ans);
  return ptdst;
}

ecc571_affine_pt &ecc571_add(const ecc571_affine_pt &pt1, const ecc571_affine_pt &pt2, ecc571_affine_pt &ptdst)
{
  if(pt1.infinite_p())
    {
      ptdst.ecc571_copy(pt2);
      return ptdst;
    }
  
  if(pt2.infinite_p())
    {
      ptdst.ecc571_copy(pt1);
      return ptdst;
    }
  
  if(pt1.x == pt2.x)
    {
      if((!pt1.x) ||
         (pt1.y != pt2.y))
        return ptdst.infinite();

      return ecc571_double(pt1, ptdst);
    }
  
  static_gf571_opnd tmp1;
  static_gf571_opnd tmp2;
  static_ecc571_affine_pt ans;

  // --- LISP MACHINE GENERATED CODE --- //
  gf571_add(pt1.y, pt2.y, ans.x);
  gf571_add(pt2.x, pt1.x, tmp1);
  gf571_div(ans.x, tmp1, tmp2);
  gf571_mul(tmp2, tmp2, ans.x);
  gf571_add(ans.x, tmp2, ans.x);
  gf571_add(tmp1, gEcc571_a, ans.y);
  gf571_add(ans.x, ans.y, ans.x);
  gf571_add(pt1.x, ans.x, ans.y);
  gf571_mul(tmp2, ans.y, ans.y);
  gf571_add(ans.x, ans.y, ans.y);
  gf571_add(pt1.y, ans.y, ans.y);
  // --- LISP MACHINE GENERATED CODE --- //

  ptdst.ecc571_copy(ans);
  return ptdst;
}

void ecc571_affine_pt::show()
{
  printf("{affine\n  x:");
  x.show();
  printf("\n  y:");
  y.show();
  printf(" }\n");
}

// -------------------------------------------------------
// ------------------------------------------------------------

static_ecc571_projective_pt::static_ecc571_projective_pt()
{
  x.opnd = xbytes;
  y.opnd = ybytes;
  z.opnd = zbytes;
}

static_ecc571_projective_pt::static_ecc571_projective_pt(const ecc571_projective_pt &pt)
{
  x.opnd = xbytes;
  y.opnd = ybytes;
  z.opnd = zbytes;

  ecc571_copy(pt);
}

static_ecc571_projective_pt::static_ecc571_projective_pt(const ecc571_affine_pt &pt, const gf571_opnd &rnd)
{
  x.opnd = xbytes;
  y.opnd = ybytes;
  z.opnd = zbytes;
  
  static_gf571_opnd tmp1;
  static_gf571_opnd tmp2;

  gf571_mul(rnd,rnd,tmp1);
  gf571_mul(rnd,tmp1,tmp2);
  gf571_mul(tmp1,pt.x,tmp1);
  gf571_mul(tmp2,pt.y,tmp2);

  x.gf571_copy(tmp1);
  y.gf571_copy(tmp2);
  z.gf571_copy(rnd);
}

// -------------------------------------------------------

ecc571_affine_pt& ecc571_projective_pt::convert_to_affine(ecc571_affine_pt &ptdst)
{
  // using Jacobi projective coordinates

  if(infinite_p())
    ptdst.infinite();
  else
    {
      static_gf571_opnd tmp1;
      static_gf571_opnd tmp2;

      gf571_inv(z, tmp1);          // tmp1 = 1/z
      gf571_mul(tmp1,tmp1,tmp2);   // tmp2 = 1/z^2
      gf571_mul(tmp2, tmp1, tmp1); // tmp1 = 1/z^3

      gf571_mul(x, tmp2, ptdst.x);
      gf571_mul(y, tmp1, ptdst.y);
    }
  return ptdst;
}

ecc571_projective_pt &ecc571_projective_pt::ecc571_copy(const ecc571_projective_pt &pt)
{
  x.gf571_copy(pt.x);
  y.gf571_copy(pt.y);
  z.gf571_copy(pt.z);
  return *this;
}

ecc571_projective_pt &ecc571_projective_pt::infinite()
{
  x.one();
  y.one();
  z.zero();
  return *this;
}

bool ecc571_projective_pt::infinite_p() const
{
  return !z;
}

ecc571_projective_pt& ecc571_double(const ecc571_projective_pt &pt, ecc571_projective_pt &ptdst)
{
  if(pt.infinite_p())
      return ptdst.infinite();

  static_gf571_opnd tmp1, tmp2;
  static_ecc571_projective_pt ans;

  // --- LISP MACHINE GENERATED CODE --- //
  gf571_mul(pt.z, pt.z, ans.z);
  gf571_mul(pt.x, ans.z, ans.z);
  gf571_mul(pt.y, pt.z, ans.y);
  gf571_mul(pt.x, pt.x, tmp1);
  gf571_add(ans.y, tmp1, tmp2);
  gf571_mul(tmp2, tmp2, ans.y);
  gf571_mul(gEcc571_a, ans.z, ans.x);
  gf571_add(ans.x, tmp2, ans.x);
  gf571_mul(ans.z, ans.x, ans.x);
  gf571_add(ans.y, ans.x, ans.x);
  gf571_mul(pt.x, tmp1, tmp1);
  gf571_mul(pt.x, tmp1, tmp1);
  gf571_add(ans.x, tmp1, ans.y);
  gf571_mul(ans.z, ans.y, ans.y);
  gf571_mul(tmp2, ans.x, tmp1);
  gf571_add(ans.y, tmp1, ans.y);
  // --- LISP MACHINE GENERATED CODE --- //

  ptdst.ecc571_copy(ans);
  return ptdst;
}


ecc571_projective_pt& ecc571_add(const ecc571_projective_pt &pt1, const ecc571_projective_pt &pt2, ecc571_projective_pt &ptdst)
{
  if(pt1.infinite_p())
    {
      ptdst.ecc571_copy(pt2);
      return ptdst;
    }
  if(pt2.infinite_p())
    {
      ptdst.ecc571_copy(pt1);
      return ptdst;
    }

  static_gf571_opnd tmp1, tmp2, tmp3, tmp4;
  static_ecc571_projective_pt ans;

  // --- LISP MACHINE GENERATED CODE --- //
  gf571_mul(pt2.z, pt2.z, tmp1);
  gf571_mul(pt1.x, tmp1, ans.z);
  gf571_mul(pt1.z, pt1.z, tmp2);
  gf571_mul(pt2.x, tmp2, ans.y);
  gf571_add(ans.z, ans.y, tmp3); // ta
  
  gf571_mul(pt2.z, tmp3, tmp4);
  gf571_mul(pt1.z, tmp4, ans.z); // ans.z
  
  gf571_mul(tmp3, tmp3, ans.y);
  gf571_mul(tmp3, ans.y, ans.y); // ta^3
  gf571_mul(ans.z, ans.z, tmp3);
  gf571_mul(gEcc571_a, tmp3, tmp3); // A*z3^2
  
  gf571_mul(pt2.z, tmp1, ans.x); // z2^3
  gf571_mul(pt1.y, ans.x, ans.x); // y1*z2^2
  gf571_mul(pt1.z, tmp2, tmp1);  // z1^3
  gf571_mul(pt2.y, tmp1, tmp1);
  gf571_add(ans.x, tmp1, tmp1); // tb
  
  if(!ans.y)
    {
      if(!tmp1)
	return ecc571_double(pt2, ptdst);
      else
	{
	  ptdst.infinite();
	  return ptdst;
	}
    }

  gf571_mul(tmp1, ans.z, ans.x); // tb*z3
  gf571_mul(tmp1, tmp1, tmp2);   //  (tb*z3)^2
  gf571_add(ans.x, tmp2, ans.x); // y1*z2^2*(tb*z3)^2
  gf571_add(tmp3, ans.x, ans.x);
  gf571_add(ans.y, ans.x, ans.x);
  gf571_mul(pt1.z, ans.x, ans.y);
  gf571_mul(tmp4, tmp4, tmp2);
  gf571_mul(pt1.y, tmp2, tmp3);
  gf571_add(ans.y, tmp3, ans.y);
  gf571_mul(tmp4, ans.y, ans.y);
  gf571_mul(pt1.x, tmp2, tmp2);
  gf571_add(ans.x, tmp2, tmp2);
  gf571_mul(tmp1, tmp2, tmp1);
  gf571_add(ans.y, tmp1, ans.y);
  // --- LISP MACHINE GENERATED CODE --- //

  ptdst.ecc571_copy(ans);
  return ptdst;
}

ecc571_projective_pt& ecc571_mul(const ecc571_projective_pt &pt, const gf571_opnd &n, ecc571_projective_pt &ptdst)
{
  if(!n)
    {
      ptdst.infinite();
      return ptdst;
    }
  if(pt.infinite_p())
    {
      ptdst.infinite();
      return ptdst;
    }
  
  static_ecc571_projective_pt ans(pt);
  static_ecc571_projective_pt tmp1;
  
  int ix;
  uint mask;
  n.first_bit(ix, mask);
  for(; ix >= 0; --ix, mask = 0x100)
    {
      ubyte b = n.opnd[ix];
      for(; mask >>= 1;)
	{
	  ecc571_double(ans, ans);
          ecc571_add(ans, pt, (mask & b) ? ans : tmp1);
	}
    }
  ptdst.ecc571_copy(ans);
  return ptdst;
}

ecc571_affine_pt& ecc571_mul(const ecc571_affine_pt &pt, const gf571_opnd &n, 
                             ecc571_affine_pt &ptdst, const gf571_opnd &rnd)
{
  static_ecc571_projective_pt p(pt, rnd);
  static_ecc571_projective_pt ans;
  ecc571_mul(p, n, ans);
  ans.convert_to_affine(ptdst);
  return ptdst;
}

ecc571_affine_pt& ecc571_mulGen(const gf571_opnd &n, ecc571_affine_pt &ptdst)
{
  static_gf571_opnd rnd;
  rnd.one();
  return ecc571_mul(gGen, n, ptdst, rnd);
}

// ------------------------------------------------------------
// Library Entry Points
// ------------------------------------------------------------
extern "C" {

  void gf128_add(ubyte *op1, ubyte *op2, ubyte *opdst)
  {
    gf128_opnd p1(op1);
    gf128_opnd p2(op2);
    gf128_opnd pdst(opdst);
    gf128_add(p1, p2, pdst);
  }

  void gf128_mul(ubyte *op1, ubyte *op2, ubyte *opdst)
  {
    gf128_opnd p1(op1);
    gf128_opnd p2(op2);
    gf128_opnd pdst(opdst);
    gf128_mul(p1, p2, pdst);
  }

  void gf128_inv(ubyte *op, ubyte *opdst)
  {
    gf128_opnd p(op);
    gf128_opnd pdst(opdst);
    gf128_inv(p, pdst);
  }

  void gf128_div(ubyte *op1, ubyte *op2, ubyte *opdst)
  {
    gf128_opnd p1(op1);
    gf128_opnd p2(op2);
    gf128_opnd pdst(opdst);
    gf128_div(p1, p2, pdst);
  }

  // -------------------------------------------------------
  
  void gf571_add(ubyte *op1, ubyte *op2, ubyte *opdst)
  {
    gf571_opnd p1(op1);
    gf571_opnd p2(op2);
    gf571_opnd pdst(opdst);
    gf571_add(p1, p2, pdst);
  }

  void gf571_mul(ubyte *op1, ubyte *op2, ubyte *opdst)
  {
    gf571_opnd p1(op1);
    gf571_opnd p2(op2);
    gf571_opnd pdst(opdst);
    gf571_mul(p1, p2, pdst);
  }

  void gf571_inv(ubyte *op, ubyte *opdst)
  {
    gf571_opnd p(op);
    gf571_opnd pdst(opdst);
    gf571_inv(p, pdst);
  }

  void gf571_div(ubyte *op1, ubyte *op2, ubyte *opdst)
  {
    gf571_opnd p1(op1);
    gf571_opnd p2(op2);
    gf571_opnd pdst(opdst);
    gf571_div(p1, p2, pdst);
  }

  // -------------------------------------------------------
  
#if 0
  void gf571_shiftl(ubyte *op, int nsh, ubyte *opdst)
  {
    gf571_opnd p(op);
    gf571_opnd pdst(opdst);
    gf571_shiftl(p, nsh, pdst);
  }

  void gf571_prim(ubyte *opdst)
  {
    gf571_opnd pdst(opdst);
    pdst.prim();
  }

  int gf571_is_one(ubyte *opnd)
  {
    gf571_opnd p(opnd);
    return p.is_one();
  }

  int gf571_sizeof_opnd()
  {
    return sizeof(ulong);
  }
#endif

  // -------------------------------------------------------
  
  void c_ecc571_add(ubyte *op1x, ubyte *op1y,
		    ubyte *op2x, ubyte *op2y,
		    ubyte *ansx, ubyte *ansy)
  {
    ecc571_affine_pt p1(op1x, op1y);
    ecc571_affine_pt p2(op2x, op2y);
    ecc571_affine_pt ans(ansx, ansy);
    ecc571_add(p1,p2,ans);
  }

  void c_ecc571_sub(ubyte *op1x, ubyte *op1y,
		    ubyte *op2x, ubyte *op2y,
		    ubyte *ansx, ubyte *ansy)
  {
    ecc571_affine_pt p1(op1x, op1y);
    ecc571_affine_pt p2(op2x, op2y);
    ecc571_affine_pt ans(ansx, ansy);
    ecc571_sub(p1,p2,ans);
  }

  void c_ecc571_mul(ubyte *op1x, ubyte *op1y,
		    ubyte *op2,
		    ubyte *ansx, ubyte *ansy,
                    ubyte *noise)
  {
    ecc571_affine_pt p1(op1x, op1y);
    ecc571_affine_pt ans(ansx, ansy);
    gf571_opnd p2(op2);
    gf571_opnd rnd(noise);
    ecc571_mul(p1,p2,ans,rnd);
  }

  void c_ecc571_setCurve(ubyte *a, ubyte *b)
  {
    if(a)
      {
	gf571_opnd pa(a);
	gEcc571_a = pa;
      }
    if(b)
      {
	gf571_opnd pb(b);
	gEcc571_b = pb;
      }
  }
};
