// ecc.cpp -- NIST B571 ECC over F_2^571
// DMcClain/Acudora  11/11
// ---------------------------------------------

#include "ecc.h"
// ------------------------------------------------------------

gf_opnd::gf_opnd()
{
	zero();
}

gf_opnd::gf_opnd(const gf_opnd &op)
{
	memmove(opnd, op.opnd, sizeof(opnd));
}

#if 0
gf_opnd& gf_opnd::operator=(const gf_opnd &op)
{
	memmove(opnd, op.opnd, sizeof(opnd));
	return *this;
}
#endif

gf_opnd::gf_opnd(const ubyte *pnum, uint nb)
{
	if(nb < sizeof(opnd))
	{
		uint nrem = sizeof(opnd) - nb;
                memset(opnd, 0, nrem);
		memmove(opnd + nrem, pnum, nb);
	}
	else
	{
		uint nrem = nb - sizeof(opnd);
		memmove(opnd, pnum + nrem, sizeof(opnd));
		opnd[0] &= 0x07;
	}
}

gf_opnd::gf_opnd(const char *str)
{
	int ix, jx;
	zero();
	for(jx = sizeof(opnd), ix = strlen(str);
		(--jx, ix -= 2, (jx >= 0) && (ix >= 0));
		)
	{
		int x;
		sscanf(&str[ix], "%02x", &x);
		opnd[jx] = x;
	}
	if((jx >= 0) && (-1 == ix))
	{
		int x;
		sscanf(str, "%01x", &x);
		opnd[jx] = x;
	}
}

gf_opnd& gf_add(const gf_opnd &op1, const gf_opnd &op2, gf_opnd &opdst)
{
	ulong* pdst = (ulong*)opdst.opnd;
	ulong* p1   = (ulong*)op1.opnd;
	ulong* p2   = (ulong*)op2.opnd;
	for(int ix = sizeof(opdst.opnd)/sizeof(ulong); --ix >= 0;)
		*pdst++ = *p1++ ^ *p2++;
	return opdst;
}

void gf_opnd::zero()
{
	memset(opnd, 0, sizeof(opnd));
}

void gf_opnd::one()
{
	zero();
	opnd[sizeof(opnd)-1] = 1;
}

void gf_opnd::two()
{
	zero();
	opnd[sizeof(opnd)-1] = 2;
}

void gf_opnd::prim()
{
  zero();
  opnd[ 0] = 0x08;
  opnd[70] = 0x04;
  opnd[71] = 0x25;
}

bool gf_opnd::is_one() const
{
   int ix;
   ulong *p = (ulong*)opnd;

   for(ix = 0; ix < sizeof(opnd)/sizeof(ulong)-1; ++ix)
      if(p[ix])
        return false;
   return (0x0100000000000000 == p[ix]);
}

void gf_opnd::step()
{
	int ix;

	for(ix = 0; ix < sizeof(opnd)-1; ++ix)
		opnd[ix] = (opnd[ix] << 1) | ((opnd[ix+1] & 0x080) >> 7);
	opnd[ix] <<= 1;
	if(opnd[0] & 0x08)
	{
		opnd[0]    ^= 0x08;
		opnd[ix-1] ^= 0x04;
		opnd[ix]   ^= 0x25;
	}
}

void gf_opnd::first_bit(int &ix, ubyte &mask) const
{
	for(ix = 0; ix < sizeof(opnd); ++ix)
		if(opnd[ix])
			break;
	if(ix >= sizeof(opnd))
	{
		ix = -1;
		return;
	}
	for(mask = 0x080; 0 == (mask & opnd[ix]); mask >>= 1)
		;
}

int gf_opnd::integer_length() const
{
    int ix;
    ubyte mask;

    first_bit(ix, mask);
    if(ix < 0)
      return 0;
    ix = 8*(sizeof(opnd) - ix);
    for(mask <<= 1; mask; mask <<= 1)
      --ix;
    return ix;
}

gf_opnd& gf_mul(const gf_opnd &op1, const gf_opnd &op2, gf_opnd &opdst)
{
	int ix;
	ubyte mask;
	gf_opnd ans;
	
	op2.first_bit(ix, mask);
	if(ix < 0)
	{
		opdst.zero();
		return opdst;
	}

        ans = op1;
        mask >>= 1;
        if(0 == mask)
        {
           mask = 0x080;
           ++ix;
        }
	for(; ix < sizeof(op2.opnd); ++ix, mask = 0x080)
	{
		for(; mask; mask >>= 1)
		{
			ans.step();
			if(mask & op2.opnd[ix])
				gf_add(ans, op1, ans);
		} 
	}
	opdst = ans;
	return opdst;
}

#if 0
gf_opnd& gf_inv(const gf_opnd &op, gf_opnd &opdst)
{
	gf_opnd p(op);

	opdst.one();
	for(int ix = 1; ix < 571; ++ix)
	{
		gf_mul(p, p, p);
		gf_mul(p, opdst, opdst);
	}
	return opdst;
}
#else
gf_opnd& gf_inv(const gf_opnd &op, gf_opnd &opdst)
{
   // using extended Euclidean algorithm
   gf_opnd u(op);
   gf_opnd v;
   gf_opnd g1;
   gf_opnd g2;
   gf_opnd tmp;
   g1.one();
   v.prim();

   gf_opnd *pu = &u;
   gf_opnd *pv = &v;
   gf_opnd *pg1 = &g1;
   gf_opnd *pg2 = &g2;
   gf_opnd *ptmp;

   while(!(pu->is_one()))
   {
     // if(!u)
     //   throw("gf_inv: zero divisor");
     
      int j = pu->integer_length() - pv->integer_length();
      if(j < 0)
      {
	ptmp = pu;
	pu = pv;
	pv = ptmp;
        
	ptmp = pg1;
	pg1 = pg2;
	pg2 = ptmp;
	
	j = -j;
      }
      gf_shiftl(*pv, j, tmp);
      gf_add(*pu, tmp, *pu);
      gf_shiftl(*pg2, j, tmp);
      gf_add(*pg1, tmp, *pg1);
   }
   opdst = *pg1;
   return opdst;
}
#endif

gf_opnd& gf_div(const gf_opnd& op1, const gf_opnd& op2, gf_opnd& opdst)
{
  gf_opnd tmp;
  gf_inv(op2, tmp);
  return gf_mul(op1, tmp, opdst);
}

bool gf_opnd::operator!() const
{
	ulong *p = (ulong*)opnd;
	for(int ix = sizeof(opnd)/sizeof(ulong); --ix >= 0; ++p)
		if(*p)
			return false;
	return true;
}

gf_opnd& gf_oneplus(const gf_opnd &op, gf_opnd &opdst)
{
  gf_opnd ans = op;
  ans.opnd[sizeof(ans.opnd)-1] ^= 1;
  opdst = ans;
  return opdst;
}

bool gf_opnd::operator==(const gf_opnd &op) const
{
	if(&opnd == &op.opnd)
		return true;
	ulong *p1 = (ulong*)opnd;
	ulong *p2 = (ulong*)op.opnd;
	for(int ix = sizeof(opnd)/sizeof(ulong); --ix >= 0; ++p1, ++p2)
		if(*p1 != *p2)
			return false;
	return true;
}

gf_opnd& gf_shiftl(const gf_opnd &op, int nsh, gf_opnd &opdst)
{
  int nb = nsh / 8;
  gf_opnd ans;

  if(nb > 0)
  {
    nsh -= 8 * nb;
    int nrem = sizeof(ans.opnd) - nb;

    memmove(ans.opnd, op.opnd+nb, nrem);
    memset(ans.opnd+nrem, 0, nb);
  }
  else
    ans = op;
  
  if(nsh > 0)
  {
    int ix;
    int rsh = 8 - nsh;
    
    for(ix = 0; ix < sizeof(ans.opnd)-1; ++ix)
      ans.opnd[ix] = (ans.opnd[ix] << nsh) | (ans.opnd[ix+1] >> rsh);
    ans.opnd[ix] <<= nsh;
  }
  opdst = ans;
  return opdst;
}


void gf_opnd::show()
{
	for(int ix = 0; ix < sizeof(opnd); ++ix)
		printf("%02X", opnd[ix]);
}

// ------------------------------------------------------------------

// ------------------------------------------------------------

ecc_affine_pt::ecc_affine_pt()
{
	infinite();
}

ecc_affine_pt &ecc_affine_pt::infinite()
{
	x.zero();
	y.zero();
	return *this;
}

bool ecc_affine_pt::infinite_p() const
{
	return !x;
}

ecc_affine_pt::ecc_affine_pt(const ecc_projective_pt &pt)
{
	if(pt.infinite_p())
		infinite();
	else
	{
		gf_opnd tmp1;
		gf_opnd tmp2;

		gf_inv(pt.z, tmp1);
		gf_mul(pt.x, tmp1, x);

		gf_mul(tmp1, tmp1, tmp2);
		gf_mul(pt.y, tmp2, y);
	}
}

ecc_affine_pt::ecc_affine_pt(const char* str[2])
{
	gf_opnd xs(str[0]);
	gf_opnd ys(str[1]);
	x = xs;
	y = ys;
}

ecc_affine_pt::ecc_affine_pt(const ubyte* px, const ubyte* py, uint nb)
{
  gf_opnd gx(px, nb);
  gf_opnd gy(py, nb);
  x = gx;
  y = gy;
}


ecc_affine_pt& ecc_neg(const ecc_affine_pt &pt, ecc_affine_pt &ptdst)
{
	ptdst.x = pt.x;
	gf_add(pt.x, pt.y, ptdst.y);
	return ptdst;
}

ecc_affine_pt& ecc_sub(const ecc_affine_pt &pt1, const ecc_affine_pt &pt2, ecc_affine_pt &ptdst)
{
	ecc_affine_pt tmp;
	ecc_neg(pt2, tmp);
	return ecc_add(pt1, tmp, ptdst);
}

ecc_affine_pt& ecc_double(const ecc_affine_pt &pt, ecc_affine_pt &ptdst)
{
	if(pt.infinite_p())
	{
		ptdst.infinite();
		return ptdst;
	}

	gf_opnd s;
	gf_opnd tmp1, tmp2;
	ecc_affine_pt ans;

	gf_div(pt.y, pt.x, s);
	gf_add(pt.x, s, s);

	gf_mul(s, s, tmp1);
	gf_oneplus(s, tmp2);
	gf_add(tmp1, tmp2, ans.x);

	gf_mul(tmp2, ans.x, tmp1);
	gf_mul(pt.x, pt.x, tmp2);
	gf_add(tmp1, tmp2, ans.y);

	ptdst = ans;
	return ptdst;
}

ecc_affine_pt &ecc_add(const ecc_affine_pt &pt1, const ecc_affine_pt &pt2, ecc_affine_pt &ptdst)
{
	if(pt1.infinite_p())
	{
		ptdst = pt2;
		return ptdst;
	}

	if(pt2.infinite_p())
	{
		ptdst = pt1;
		return ptdst;
	}

	if(pt1.x == pt2.x)
	{
		if(pt1.y == pt2.y)
			return ecc_double(pt1, ptdst);

		gf_opnd tmp1;

		gf_add(pt1.x, pt1.y, tmp1);
		if(pt2.y == tmp1)
			return ptdst.infinite();

		gf_add(pt2.x, pt2.y, tmp1);
		if(pt1.y == tmp1)
			return ptdst.infinite();
	}

	gf_opnd s, tmp1, tmp2;
	ecc_affine_pt ans;

	gf_add(pt1.y, pt2.y, tmp1);
	gf_add(pt1.x, pt2.x, tmp2);
	gf_div(tmp1, tmp2, s);

	gf_add(tmp2, s, tmp1);
	gf_mul(s, s, tmp2);
	gf_add(tmp2, tmp1, tmp1);
	gf_oneplus(tmp1, ans.x);

	gf_add(pt1.x, ans.x, tmp1);
	gf_mul(s, tmp1, tmp2);
	gf_add(tmp2, ans.x, tmp1);
	gf_add(tmp1, pt1.y, ans.y);

	ptdst = ans;
	return ptdst;
}

void ecc_affine_pt::show()
{
	printf("{affine\n  x:");
	x.show();
	printf("\n  y:");
	y.show();
	printf(" }\n");
}

// -------------------------------------------------------

ecc_projective_pt::ecc_projective_pt()
{
	z.one();
}

ecc_projective_pt::ecc_projective_pt(const ecc_affine_pt &pt)
{
	x = pt.x;
	y = pt.y;
	z.one();
}

ecc_projective_pt &ecc_projective_pt::infinite()
{
	x.one();
	y.zero();
	z.zero();
	return *this;
}

bool ecc_projective_pt::infinite_p() const
{
	return !z;
}

gf_opnd gEcc_b("2f40e7e2221f295de297117b7f3d62f5c6a97ffcb8ceff1cd6ba8ce4a9a18ad84ffabbd8efa59332be7ad6756a66e294afd185a78ff12aa520e4de739baca0c7ffeff7f2955727a");

ecc_projective_pt& ecc_double(const ecc_projective_pt &pt, ecc_projective_pt &ptdst)
{
	if(pt.infinite_p())
	{
		ptdst = pt;
		return ptdst;
	}
	gf_opnd x1sq;
	gf_opnd z1sq;
	gf_opnd bz1sqsq;
	gf_opnd tmp1, tmp2;
	ecc_projective_pt ans;

	gf_mul(pt.x, pt.x, x1sq);
	gf_mul(pt.z, pt.z, z1sq);
	gf_mul(z1sq, z1sq, bz1sqsq);
	gf_mul(bz1sqsq, gEcc_b, bz1sqsq);
	gf_mul(x1sq, z1sq, ans.z);
	gf_mul(x1sq, x1sq, tmp1);
	gf_add(tmp1, bz1sqsq, ans.x);

	gf_mul(pt.y, pt.y, tmp1);
	gf_add(tmp1, bz1sqsq, tmp1);
	gf_add(tmp1, ans.z, tmp1);
	gf_mul(ans.x, tmp1, tmp1);
	gf_mul(bz1sqsq, ans.z, tmp2);
	gf_add(tmp1, tmp2, ans.y);

	ptdst = ans;
	return ptdst;
}

ecc_projective_pt& ecc_add(const ecc_projective_pt &pt1, const ecc_projective_pt &pt2, ecc_projective_pt &ptdst)
{
	if(pt1.infinite_p())
	{
		ptdst = pt2;
		return ptdst;
	}
	gf_opnd t1;
	gf_opnd t2;
	gf_opnd t3;
	ecc_projective_pt ans;

	gf_mul(pt1.z, pt2.x, t1);
	gf_mul(pt1.z, pt1.z, t2);
	gf_add(pt1.x, t1, ans.x);
	gf_mul(pt1.z, ans.x, t1);
	gf_mul(t2, pt2.y, t3);
	gf_add(pt1.y, t3, ans.y);
	if(!ans.x)
	{
		if(!ans.y)
			return ecc_double(pt2, ptdst);
		else
		{
			ptdst.infinite();
			return ptdst;
		}
	}
	else
	{
		gf_mul(t1, t1, ans.z);
		gf_mul(t1, ans.y, t3);
		gf_add(t1, t2, t1);

		gf_mul(ans.x, ans.x, t2);
		gf_mul(t2, t1, ans.x);
		gf_mul(ans.y, ans.y, t2);

		gf_add(ans.x, t2, ans.x);
		gf_add(ans.x, t3, ans.x);
		gf_mul(pt2.x, ans.z, t2);
		
		gf_add(t2, ans.x, t2);
		gf_mul(ans.z, ans.z, t1);
		gf_add(t3, ans.z, t3);
		
		gf_mul(t3, t2, ans.y);
		gf_add(pt2.x, pt2.y, t2);
		gf_mul(t1, t2, t3);
		
		gf_add(ans.y, t3, ans.y);

		ptdst = ans;
		return ptdst;
	}
}

ecc_projective_pt& ecc_mul(const ecc_projective_pt &pt, const gf_opnd &n, ecc_projective_pt &ptdst)
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
  
  ecc_projective_pt ans = pt;
  
  int ix;
  ubyte mask;
  n.first_bit(ix, mask);
  mask >>= 1;
  if(0 == mask)
    {
      mask = 0x080;
      ++ix;
    }
  
  for(; ix < sizeof(n.opnd); ++ix, mask = 0x080)
    {
      for(; mask; mask >>= 1)
	{
	  ecc_double(ans, ans);
	  if(mask & n.opnd[ix])
	    ecc_add(ans, pt, ans);
	}
    }
  ptdst = ans;
  return ptdst;
}

ecc_affine_pt& ecc_mul(const ecc_affine_pt &pt, const gf_opnd &n, ecc_affine_pt &ptdst)
{
	ecc_projective_pt p(pt);
	ecc_projective_pt ans;
	ecc_mul(p, n, ans);
	ecc_affine_pt xans(ans);
	ptdst = xans;
	return ptdst;
}

static const char* genStr[2] = {
	"303001d34b856296c16c0d40d3cd7750a93d1d2955fa80aa5f40fc8db7b2abdbde53950f4c0d293cdd711a35b67fb1499ae60038614f1394abfa3b4c850d927e1e7769c8eec2d19",
	"37bf27342da639b6dccfffeb73d69d78c6c27a6009cbbca1980f8533921e8a684423e43bab08a576291af8f461bb2a8b3531d2f0485c19b16e2f1516e23dd3c1a4827af1b8ac15b"
};

const ecc_affine_pt gGen(genStr);

ecc_affine_pt& ecc_mulGen(const gf_opnd &n, ecc_affine_pt &ptdst)
{
	return ecc_mul(gGen, n, ptdst);
}

extern "C" {

  void gf_add(const ubyte *op1, const ubyte *op2, ubyte *opdst)
  {
    gf_opnd p1(op1, 72);
    gf_opnd p2(op2, 72);
    gf_opnd pdst;
    gf_add(p1, p2, pdst);
    memmove(opdst, pdst.opnd, 72);
  }

  void gf_mul(const ubyte *op1, const ubyte *op2, ubyte *opdst)
  {
    gf_opnd p1(op1, 72);
    gf_opnd p2(op2, 72);
    gf_opnd pdst;
    gf_mul(p1, p2, pdst);
    memmove(opdst, pdst.opnd, 72);
  }

  void gf_inv(const ubyte *op, ubyte *opdst)
  {
    gf_opnd p(op, 72);
    gf_opnd pdst;
    gf_inv(p, pdst);
    memmove(opdst, pdst.opnd, 72);
  }

  void gf_div(const ubyte *op1, const ubyte *op2, ubyte *opdst)
  {
    gf_opnd p1(op1, 72);
    gf_opnd p2(op2, 72);
    gf_opnd pdst;
    gf_div(p1, p2, pdst);
    memmove(opdst, pdst.opnd, 72);
  }

  #if 0
  void gf_shiftl(const ubyte *op, int nsh, ubyte *opdst)
  {
    gf_opnd p(op, 72);
    gf_opnd pdst;
    gf_shiftl(p, nsh, pdst);
    memmove(opdst, pdst.opnd, 72);
  }

  void gf_prim(ubyte *opdst)
  {
    gf_opnd pdst;
    pdst.prim();
    memmove(opdst, pdst.opnd, 72);
  }

  int gf_is_one(const ubyte *opnd)
  {
    gf_opnd p(opnd, 72);
    return p.is_one();
  }

  int gf_sizeof_opnd()
  {
    return sizeof(ulong);
  }
  #endif

  void c_ecc_add(const ubyte *op1x, const ubyte *op1y,
		 const ubyte *op2x, const ubyte *op2y,
		 ubyte *ansx, ubyte *ansy)
  {
    ecc_affine_pt p1(op1x, op1y, 72);
    ecc_affine_pt p2(op2x, op2y, 72);
    ecc_affine_pt ans;
    ecc_add(p1,p2,ans);
    memmove(ansx, ans.x.opnd, 72);
    memmove(ansy, ans.y.opnd, 72);
  }

  void c_ecc_mul(const ubyte *op1x, const ubyte *op1y,
		 const ubyte *op2,
		 ubyte *ansx, ubyte *ansy)
  {
    ecc_affine_pt p1(op1x, op1y, 72);
    ecc_affine_pt ans;
    gf_opnd p2(op2,72);
    ecc_mul(p1,p2,ans);
    memmove(ansx, ans.x.opnd, 72);
    memmove(ansy, ans.y.opnd, 72);
  }

  
};
