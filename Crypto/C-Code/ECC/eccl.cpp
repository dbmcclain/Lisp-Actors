// ecc.cpp -- NIST B571 ECC over F_2^571
// DMcClain/Acudora  11/11
// ---------------------------------------------

#include "ecc.h"
// ------------------------------------------------------------

static const int lmask = sizeof(ulong)-1;

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
  if(nb < sizeof(gf128_opnd_bytes))
    {
      uint nrem = sizeof(gf128_opnd_bytes) - nb;
      int ix;
      for(ix = 0; ix < nrem; ++ix)
	opnd[ix ^ lmask] = 0;
      for(int jx = 0; ix < sizeof(gf128_opnd_bytes); ++ix, ++jx)
	opnd[ix ^ lmask] = pnum[jx ^ lmask];
    }
  else
    {
      uint nrem = nb - sizeof(gf128_opnd_bytes);
      memmove(opnd, pnum + nrem, sizeof(gf128_opnd_bytes));
    }
}

static_gf128_opnd::static_gf128_opnd(const char *str)
{
  opnd = bytes;
  init_from_string(str);
}

gf128_opnd& gf128_opnd::init_from_string(const char *str)
{
  int ix, jx;

  zero();
  for(jx = sizeof(gf128_opnd_bytes), ix = strlen(str);
      (--jx, ix -= 2, (jx >= 0) && (ix >= 0));
      )
    {
      int x;
      sscanf(&str[ix], "%02x", &x);
      opnd[jx ^ lmask] = x;
    }
  if((jx >= 0) && (-1 == ix))
    {
      int x;
      sscanf(str, "%01x", &x);
      opnd[jx ^ lmask] = x;
    }
}

// -----------------------------------------------------------

gf128_opnd& gf128_add(const gf128_opnd &op1, const gf128_opnd &op2, gf128_opnd &opdst)
{
  ulong* pdst = (ulong*)opdst.opnd;
  ulong* p1   = (ulong*)op1.opnd;
  ulong* p2   = (ulong*)op2.opnd;
  pdst[0] = p1[0] ^ p2[0];
  pdst[1] = p1[1] ^ p2[1];
  return opdst;
}

void gf128_opnd::zero()
{
  memset(opnd, 0, sizeof(gf128_opnd_bytes));
}

void gf128_opnd::one()
{
  zero();
  opnd[(sizeof(gf128_opnd_bytes)-1)^lmask] = 1;
}

void gf128_opnd::two()
{
  zero();
  opnd[(sizeof(gf128_opnd_bytes)-1)^lmask] = 2;
}

void gf128_opnd::prim()
{
  zero();
  opnd[(sizeof(gf128_opnd_bytes)-1) ^ lmask] = 0x87;
}

bool gf128_opnd::is_one() const
{
   ulong *p = (ulong*)opnd;

   if(p[0])
     return false;
   return (1 == p[1]);
}

void gf128_opnd::step()
{
  ulong *p = (ulong*)opnd;
  int hibit = (opnd[0 ^ lmask] & 0x080);
  static const int nrsh = 8*sizeof(ulong)-1;

  p[0] = (p[0] << 1) | (p[1] >> nrsh);
  p[1] <<= 1;
  if(hibit)
    opnd[(sizeof(gf128_opnd_bytes)-1) ^ lmask] ^= 0x087;
}

void gf128_opnd::first_bit(int &ix, ubyte &mask) const
{
  ulong *p = (ulong*)opnd;
  if(p[0])
    ix = 0;
  else if(p[1])
    ix = sizeof(ulong);
  else
    {
      ix = -1;
      return;
    }
  ubyte b;
  for(; ix < sizeof(gf128_opnd_bytes); ++ix)
    if((b = opnd[ix ^ lmask])) // assignment intended
      break;
  for(mask = 0x080; 0 == (mask & b); mask >>= 1)
    ;
}

int gf128_opnd::integer_length() const
{
    int ix;
    ubyte mask;

    first_bit(ix, mask);
    if(ix < 0)
      return 0;
    ix = 8*(sizeof(gf128_opnd_bytes) - ix);
    for(mask <<= 1; mask; mask <<= 1)
      --ix;
    return ix;
}

gf128_opnd& gf128_opnd::gf128_copy(const gf128_opnd &op)
{
  if(opnd != op.opnd)
    memmove(opnd, op.opnd, sizeof(gf128_opnd_bytes));
  return *this;
}
      
gf128_opnd& gf128_mul(const gf128_opnd &op1, const gf128_opnd &op2, gf128_opnd &opdst)
{
  int ix;
  ubyte mask;
  
  op2.first_bit(ix, mask);
  if(ix < 0)
    {
      opdst.zero();
      return opdst;
    }
  
  static_gf128_opnd ans(op1);

  mask >>= 1;
  if(0 == mask)
    {
      mask = 0x080;
      ++ix;
    }
  for(; ix < sizeof(gf128_opnd_bytes); ++ix, mask = 0x080)
    {
      for(; mask; mask >>= 1)
	{
	  ans.step();
	  if(mask & op2.opnd[ix^lmask])
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
     u.opnd[(sizeof(gf128_opnd_bytes)-1) ^ lmask] ^= 0x087;
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

bool gf128_opnd::operator!() const
{
  ulong *p = (ulong*)opnd;
  return ((p[0] == 0) && (p[1] == 0));
}

gf128_opnd& gf128_oneplus(const gf128_opnd &op, gf128_opnd &opdst)
{
  opdst.gf128_copy(op);
  opdst.opnd[(sizeof(gf128_opnd_bytes)-1)^lmask] ^= 1;
  return opdst;
}

bool gf128_opnd::operator==(const gf128_opnd &op) const
{
  if(opnd == op.opnd)
    return true;
  ulong *p1 = (ulong*)opnd;
  ulong *p2 = (ulong*)op.opnd;
  return ((p1[0] == p2[0]) && (p1[1] == p2[1]));
}

gf128_opnd& gf128_shiftl(const gf128_opnd &op, int nsh, gf128_opnd &opdst)
{
  int nw = nsh / (8*sizeof(ulong));

  if(nw > 0)
    {
      int nb = nw*sizeof(ulong);
      nsh -= 8 * nb;
      int nrem = sizeof(gf128_opnd_bytes) - nb;
      memmove(opdst.opnd, op.opnd + nb, nrem);
      memset(opdst.opnd+nrem, 0, nb);
    }
  else
    opdst.gf128_copy(op);
  
  int nb = nsh / 8;
  
  if(nb > 0)
    {
      nsh -= 8 * nb;
      int ix, jx;
      for(ix = 0, jx = nb; jx < sizeof(gf128_opnd_bytes); ++ix, ++jx)
	opdst.opnd[ix ^ lmask] = opdst.opnd[jx ^ lmask];
      for(; ix < sizeof(gf128_opnd_bytes); ++ix)
	opdst.opnd[ix ^ lmask] = 0;
    }
  
  if(nsh > 0)
  {
    int ix;
    int rsh = 8*sizeof(ulong) - nsh;
    ulong *p = (ulong*)opdst.opnd;

    for(ix = 0; ix < sizeof(gf128_opnd_bytes)/sizeof(ulong)-1; ++ix)
      p[ix] = ((p[ix] << nsh) |
	       (p[ix+1] >> rsh));
    p[ix] <<= nsh;
  }
  return opdst;
}


void gf128_opnd::show()
{
  for(int ix = 0; ix < sizeof(gf128_opnd_bytes); ++ix)
    printf("%02X", opnd[ix ^ lmask]);
}

// ------------------------------------------------------------
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
  if(nb < sizeof(gf571_opnd_bytes))
    {
      uint nrem = sizeof(gf571_opnd_bytes) - nb;
      int ix;
      for(ix = 0; ix < nrem; ++ix)
	opnd[ix ^ lmask] = 0;
      for(int jx = 0; ix < sizeof(gf571_opnd_bytes); ++ix, ++jx)
	opnd[ix ^ lmask] = pnum[jx ^ lmask];
    }
  else
    {
      uint nrem = nb - sizeof(gf571_opnd_bytes);
      memmove(opnd, pnum + nrem, sizeof(gf571_opnd_bytes));
      opnd[0 ^ lmask] &= 0x07;
    }
}

static_gf571_opnd::static_gf571_opnd(const char *str)
{
  opnd = bytes;
  init_from_string(str);
}

gf571_opnd& gf571_opnd::init_from_string(const char *str)
{
  int ix, jx;

  zero();
  for(jx = sizeof(gf571_opnd_bytes), ix = strlen(str);
      (--jx, ix -= 2, (jx >= 0) && (ix >= 0));
      )
    {
      int x;
      sscanf(&str[ix], "%02x", &x);
      opnd[jx ^ lmask] = x;
    }
  if((jx >= 0) && (-1 == ix))
    {
      int x;
      sscanf(str, "%01x", &x);
      opnd[jx ^ lmask] = x;
    }
}

// -----------------------------------------------------------

gf571_opnd& gf571_add(const gf571_opnd &op1, const gf571_opnd &op2, gf571_opnd &opdst)
{
  ulong* pdst = (ulong*)opdst.opnd;
  ulong* p1   = (ulong*)op1.opnd;
  ulong* p2   = (ulong*)op2.opnd;
  for(int ix = sizeof(gf571_opnd_bytes)/sizeof(ulong); --ix >= 0;)
    *pdst++ = *p1++ ^ *p2++;
  return opdst;
}

void gf571_opnd::zero()
{
  memset(opnd, 0, sizeof(gf571_opnd_bytes));
}

void gf571_opnd::one()
{
  zero();
  opnd[(sizeof(gf571_opnd_bytes)-1)^lmask] = 1;
}

void gf571_opnd::two()
{
  zero();
  opnd[(sizeof(gf571_opnd_bytes)-1)^lmask] = 2;
}

void gf571_opnd::prim()
{
  zero();
  opnd[ 0 ^ lmask] = 0x08;
  opnd[(sizeof(gf571_opnd_bytes)-2) ^ lmask] = 0x04;
  opnd[(sizeof(gf571_opnd_bytes)-1) ^ lmask] = 0x25;
}

bool gf571_opnd::is_one() const
{
   int ix;
   ulong *p = (ulong*)opnd;

   for(ix = 0; ix < sizeof(gf571_opnd_bytes)/sizeof(ulong)-1; ++ix)
      if(p[ix])
        return false;
   return (1 == p[ix]);
}

void gf571_opnd::step()
{
  int ix;
  
  ulong *p = (ulong*)opnd;
  static const int nrsh = 8*sizeof(ulong)-1;
  
  for(ix = 0; ix < sizeof(gf571_opnd_bytes)/sizeof(ulong)-1; ++ix)
    p[ix] = ((p[ix] << 1) |
	     (p[ix+1] >> nrsh));
  p[ix] <<= 1;
  if(opnd[0 ^ lmask] & 0x08)
    {
      opnd[ 0 ^ lmask]  ^= 0x08;
      opnd[(sizeof(gf571_opnd_bytes)-2) ^ lmask] ^= 0x04;
      opnd[(sizeof(gf571_opnd_bytes)-1) ^ lmask] ^= 0x25;
    }
}

void gf571_opnd::first_bit(int &ix, ubyte &mask) const
{
  ulong *p = (ulong*)opnd;
  for(ix = 0; ix < sizeof(gf571_opnd_bytes)/sizeof(ulong); ++ix)
    if(p[ix])
      break;
  if(ix >= sizeof(gf571_opnd_bytes)/sizeof(ulong))
    {
      ix = -1;
      return;
    }
  ix *= sizeof(ulong);
  ubyte b;
  for(; ix < sizeof(gf571_opnd_bytes); ++ix)
    if(b = opnd[ix ^ lmask]) // assignment intended
      break;
  for(mask = 0x080; 0 == (mask & b); mask >>= 1)
    ;
}

int gf571_opnd::integer_length() const
{
    int ix;
    ubyte mask;

    first_bit(ix, mask);
    if(ix < 0)
      return 0;
    ix = 8*(sizeof(gf571_opnd_bytes) - ix);
    for(mask <<= 1; mask; mask <<= 1)
      --ix;
    return ix;
}

gf571_opnd& gf571_opnd::gf571_copy(const gf571_opnd &op)
{
  if(opnd != op.opnd)
    memmove(opnd, op.opnd, sizeof(gf571_opnd_bytes));
  return *this;
}
      
gf571_opnd& gf571_mul(const gf571_opnd &op1, const gf571_opnd &op2, gf571_opnd &opdst)
{
  int ix;
  ubyte mask;
  
  op2.first_bit(ix, mask);
  if(ix < 0)
    {
      opdst.zero();
      return opdst;
    }
  
  static_gf571_opnd ans(op1);

  mask >>= 1;
  if(0 == mask)
    {
      mask = 0x080;
      ++ix;
    }
  for(; ix < sizeof(gf571_opnd_bytes); ++ix, mask = 0x080)
    {
      for(; mask; mask >>= 1)
	{
	  ans.step();
	  if(mask & op2.opnd[ix^lmask])
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

bool gf571_opnd::operator!() const
{
  ulong *p = (ulong*)opnd;
  for(int ix = sizeof(gf571_opnd_bytes)/sizeof(ulong); --ix >= 0; ++p)
    if(*p)
      return false;
  return true;
}

gf571_opnd& gf571_oneplus(const gf571_opnd &op, gf571_opnd &opdst)
{
  opdst.gf571_copy(op);
  opdst.opnd[(sizeof(gf571_opnd_bytes)-1)^lmask] ^= 1;
  return opdst;
}

bool gf571_opnd::operator==(const gf571_opnd &op) const
{
  if(opnd == op.opnd)
    return true;
  ulong *p1 = (ulong*)opnd;
  ulong *p2 = (ulong*)op.opnd;
  for(int ix = sizeof(gf571_opnd_bytes)/sizeof(ulong); --ix >= 0; ++p1, ++p2)
    if(*p1 != *p2)
      return false;
  return true;
}

gf571_opnd& gf571_shiftl(const gf571_opnd &op, int nsh, gf571_opnd &opdst)
{
  int nw = nsh / (8*sizeof(ulong));

  if(nw > 0)
    {
      int nb = nw*sizeof(ulong);
      nsh -= 8 * nb;
      int nrem = sizeof(gf571_opnd_bytes) - nb;
      memmove(opdst.opnd, op.opnd + nb, nrem);
      memset(opdst.opnd+nrem, 0, nb);
    }
  else
    opdst.gf571_copy(op);
  
  int nb = nsh / 8;
  
  if(nb > 0)
    {
      nsh -= 8 * nb;
      int ix, jx;
      for(ix = 0, jx = nb; jx < sizeof(gf571_opnd_bytes); ++ix, ++jx)
	opdst.opnd[ix ^ lmask] = opdst.opnd[jx ^ lmask];
      for(; ix < sizeof(gf571_opnd_bytes); ++ix)
	opdst.opnd[ix ^ lmask] = 0;
    }
  
  if(nsh > 0)
  {
    int ix;
    int rsh = 8*sizeof(ulong) - nsh;
    ulong *p = (ulong*)opdst.opnd;

    for(ix = 0; ix < sizeof(gf571_opnd_bytes)/sizeof(ulong)-1; ++ix)
      p[ix] = ((p[ix] << nsh) |
	       (p[ix+1] >> rsh));
    p[ix] <<= nsh;
  }
  return opdst;
}


void gf571_opnd::show()
{
  for(int ix = 0; ix < sizeof(gf571_opnd_bytes); ++ix)
    printf("%02X", opnd[ix ^ lmask]);
}

// ------------------------------------------------------------------

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
  return !x;
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
  if(pt.infinite_p())
    {
      ptdst.infinite();
      return ptdst;
    }
  
  static_gf571_opnd s;
  static_gf571_opnd tmp1, tmp2;
  static_ecc571_affine_pt ans;
  
  gf571_div(pt.y, pt.x, s);
  gf571_add(pt.x, s, s);
  
  gf571_mul(s, s, tmp1);
  gf571_oneplus(s, tmp2);
  gf571_add(tmp1, tmp2, ans.x);
  
  gf571_mul(tmp2, ans.x, tmp1);
  gf571_mul(pt.x, pt.x, tmp2);
  gf571_add(tmp1, tmp2, ans.y);
  
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
      if(pt1.y == pt2.y)
	return ecc571_double(pt1, ptdst);
      
      static_gf571_opnd tmp1;
      
      gf571_add(pt1.x, pt1.y, tmp1);
      if(pt2.y == tmp1)
	return ptdst.infinite();
      
      gf571_add(pt2.x, pt2.y, tmp1);
      if(pt1.y == tmp1)
	return ptdst.infinite();
    }
  
  static_gf571_opnd s, tmp1, tmp2;
  static_ecc571_affine_pt ans;
  
  gf571_add(pt1.y, pt2.y, tmp1);
  gf571_add(pt1.x, pt2.x, tmp2);
  gf571_div(tmp1, tmp2, s);
  
  gf571_add(tmp2, s, tmp1);
  gf571_mul(s, s, tmp2);
  gf571_add(tmp2, tmp1, tmp1);
  gf571_oneplus(tmp1, ans.x);
  
  gf571_add(pt1.x, ans.x, tmp1);
  gf571_mul(s, tmp1, tmp2);
  gf571_add(tmp2, ans.x, tmp1);
  gf571_add(tmp1, pt1.y, ans.y);
  
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

static_ecc571_projective_pt::static_ecc571_projective_pt(const ecc571_affine_pt &pt)
{
  x.opnd = xbytes;
  y.opnd = ybytes;
  z.opnd = zbytes;
  
  x.gf571_copy(pt.x);
  y.gf571_copy(pt.y);
  z.one();
}

// -------------------------------------------------------

ecc571_affine_pt& ecc571_projective_pt::convert_to_affine(ecc571_affine_pt &ptdst)
{
  if(infinite_p())
    ptdst.infinite();
  else
    {
      static_gf571_opnd tmp1;
      
      gf571_inv(z, tmp1);
      gf571_mul(x, tmp1, ptdst.x);
      
      gf571_mul(tmp1, tmp1, tmp1);
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
  y.zero();
  z.zero();
  return *this;
}

bool ecc571_projective_pt::infinite_p() const
{
  return !z;
}

static_gf571_opnd gEcc571_b("2f40e7e2221f295de297117b7f3d62f5c6a97ffcb8ceff1cd6ba8ce4a9a18ad84ffabbd8efa59332be7ad6756a66e294afd185a78ff12aa520e4de739baca0c7ffeff7f2955727a");

ecc571_projective_pt& ecc571_double(const ecc571_projective_pt &pt, ecc571_projective_pt &ptdst)
{
  if(pt.infinite_p())
    {
      ptdst = pt;
      return ptdst;
    }
  static_gf571_opnd x1sq;
  static_gf571_opnd z1sq;
  static_gf571_opnd bz1sqsq;
  static_gf571_opnd tmp1, tmp2;
  static_ecc571_projective_pt ans;
  
  gf571_mul(pt.x, pt.x, x1sq);
  gf571_mul(pt.z, pt.z, z1sq);
  gf571_mul(z1sq, z1sq, bz1sqsq);
  gf571_mul(bz1sqsq, gEcc571_b, bz1sqsq);
  gf571_mul(x1sq, z1sq, ans.z);
  gf571_mul(x1sq, x1sq, tmp1);
  gf571_add(tmp1, bz1sqsq, ans.x);
  
  gf571_mul(pt.y, pt.y, tmp1);
  gf571_add(tmp1, bz1sqsq, tmp1);
  gf571_add(tmp1, ans.z, tmp1);
  gf571_mul(ans.x, tmp1, tmp1);
  gf571_mul(bz1sqsq, ans.z, tmp2);
  gf571_add(tmp1, tmp2, ans.y);
  
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
  static_gf571_opnd t1;
  static_gf571_opnd t2;
  static_gf571_opnd t3;
  static_ecc571_projective_pt ans;
  
  gf571_mul(pt1.z, pt2.x, t1);
  gf571_mul(pt1.z, pt1.z, t2);
  gf571_add(pt1.x, t1, ans.x);
  gf571_mul(pt1.z, ans.x, t1);
  gf571_mul(t2, pt2.y, t3);
  gf571_add(pt1.y, t3, ans.y);
  if(!ans.x)
    {
      if(!ans.y)
	return ecc571_double(pt2, ptdst);
      else
	{
	  ptdst.infinite();
	  return ptdst;
	}
    }
  else
    {
      gf571_mul(t1, t1, ans.z);
      gf571_mul(t1, ans.y, t3);
      gf571_add(t1, t2, t1);
      
      gf571_mul(ans.x, ans.x, t2);
      gf571_mul(t2, t1, ans.x);
      gf571_mul(ans.y, ans.y, t2);
      
      gf571_add(ans.x, t2, ans.x);
      gf571_add(ans.x, t3, ans.x);
      gf571_mul(pt2.x, ans.z, t2);
      
      gf571_add(t2, ans.x, t2);
      gf571_mul(ans.z, ans.z, t1);
      gf571_add(t3, ans.z, t3);
      
      gf571_mul(t3, t2, ans.y);
      gf571_add(pt2.x, pt2.y, t2);
      gf571_mul(t1, t2, t3);
      
      gf571_add(ans.y, t3, ans.y);
      
      ptdst.ecc571_copy(ans);
      return ptdst;
    }
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
  
  int ix;
  ubyte mask;
  n.first_bit(ix, mask);
  mask >>= 1;
  if(0 == mask)
    {
      mask = 0x080;
      ++ix;
    }
  
  for(; ix < sizeof(gf571_opnd_bytes); ++ix, mask = 0x080)
    {
      for(; mask; mask >>= 1)
	{
	  ecc571_double(ans, ans);
	  if(mask & n.opnd[ix ^ lmask])
	    ecc571_add(ans, pt, ans);
	}
    }
  ptdst.ecc571_copy(ans);
  return ptdst;
}

ecc571_affine_pt& ecc571_mul(const ecc571_affine_pt &pt, const gf571_opnd &n, ecc571_affine_pt &ptdst)
{
  static_ecc571_projective_pt p(pt);
  static_ecc571_projective_pt ans;
  ecc571_mul(p, n, ans);
  ans.convert_to_affine(ptdst);
  return ptdst;
}

static const char* genStr[2] = {
  "303001d34b856296c16c0d40d3cd7750a93d1d2955fa80aa5f40fc8db7b2abdbde53950f4c0d293cdd711a35b67fb1499ae60038614f1394abfa3b4c850d927e1e7769c8eec2d19",
  "37bf27342da639b6dccfffeb73d69d78c6c27a6009cbbca1980f8533921e8a684423e43bab08a576291af8f461bb2a8b3531d2f0485c19b16e2f1516e23dd3c1a4827af1b8ac15b"
};

const static_ecc571_affine_pt gGen(genStr);

ecc571_affine_pt& ecc571_mulGen(const gf571_opnd &n, ecc571_affine_pt &ptdst)
{
  return ecc571_mul(gGen, n, ptdst);
}

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
		 ubyte *ansx, ubyte *ansy)
  {
    ecc571_affine_pt p1(op1x, op1y);
    ecc571_affine_pt ans(ansx, ansy);
    gf571_opnd p2(op2);
    ecc571_mul(p1,p2,ans);
  }

  
};
