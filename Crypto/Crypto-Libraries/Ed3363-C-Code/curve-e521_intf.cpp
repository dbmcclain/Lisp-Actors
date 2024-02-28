// curve-e521_intf.cpp -- Interface for Lisp to Curve-E521 support
//
// Shamelessly lifted from MIRACL web pages and adapted to Curve-E521
// DM/RAL  01/24
// ----------------------------------------------------------------------
// Based on...
// Test program for ed336 scalar point multiplication
// Uses Bernstein et al.s point multiplication method from Curve41417 paper
// Cache safety thanks to ed25519
// Fully Tested and debugged
// g++ -O3 ed336.cpp -o ed336
// M.Scott 23/09/2015

// 64-bit version

#include <iostream>
#include <ctime>
#include <inttypes.h>
#include "curve-e521_intf.h"

// curve-E521 needs 521 bits for curve points = 10 groups of 53 bits
// scalar multiples of curve points (q = 2^519-big) need 130 groups of 4 bits

// Group multiplication by field scalars. Scalars have size 519-bits
#define WINDOW   4  // mult curves by 4-bits at a time
#define PANES  132  // nbr of windows in multiplier

using namespace std;

#define NGRP  10  // 10 cells of 53-bits

typedef __int128 type128;  /* non-standard type */
typedef int64_t  type64;
typedef type64 coord_t[NGRP];

// q = 2^521-1
// 2^521 mod q = 1
// 521 = 10 groups of 53-bits
// 2^(10*53) mod q = 512 (9 bits)
static const type64 bot53bits = 0x1FFFFFFFFFFFFF;
static const type64 bot44bits = 0x000FFFFFFFFFFF;
static const int    kCurveD   = -376014;
static const int    kBPW      = 53;
static const int    kBPWFinal = 44; // = 521 - 9*53
static const type64 kwrap     = 512; // = 2^(10*53) mod q, 9 bits

#include <stdint.h>

#define gcopy(src,dst)   memcpy(dst,src,sizeof(coord_t))
#define gzap(dst)        memset(dst,0,sizeof(coord_t))

#if 0
   __inline__ uint64_t rdtsc() {
   uint32_t lo, hi;
   __asm__ __volatile__ (      // serialize
     "xorl %%eax,%%eax \n        cpuid"
     ::: "%rax", "%rbx", "%rcx", "%rdx");
   
   __asm__ __volatile__ ("rdtsc" : "=a" (lo), "=d" (hi));
   return (uint64_t)hi << 32 | lo;
   }
#else
inline uint64_t rdtsc() {
    return 0;
}
#endif

// w=x+y
static
void gadd(coord_t& x,coord_t& y,coord_t& w)
{
    for(int i = 0; i < NGRP; ++i)
        w[i] = x[i] + y[i];
}

// w=x-y
static
void gsub(coord_t& x,coord_t& y,coord_t& w)
{
    for(int i = 0; i < NGRP; ++i)
        w[i] = x[i] - y[i];
}

// w-=x
static
void gdec(coord_t& x,coord_t& w)
{
    for(int i = 0; i < NGRP; ++i)
        w[i] -= x[i];
}

// w*=2
static
void gmul2(coord_t& w)
{
    for(int i = 0; i < NGRP; ++i)
        w[i] *= 2;
}

// w-=2*x
static
void gsb2(coord_t& x,coord_t& w)
{
    for(int i = 0; i < NGRP; ++i)
        w[i] -= 2*x[i];
}

// reduce w - Short Coefficient Reduction
static
void scr(coord_t& w)
{
	type64 t0,t1,t2;
	t0=w[0]&bot53bits;

	t1=w[1]+(w[0]>>kBPW);
	w[1]=t1&bot53bits;

	t2=w[2]+(t1>>kBPW);
	w[2]=t2&bot53bits;

	t1=w[3]+(t2>>kBPW);
	w[3]=t1&bot53bits;

	t2=w[4]+(t1>>kBPW);
	w[4]=t2&bot53bits;

	t1=w[5]+(t2>>kBPW);
	w[5]=t1&bot53bits;

	t2=w[6]+(t1>>kBPW);
	w[6]=t2&bot53bits;

	t1=w[7]+(t2>>kBPW);
	w[7]=t1&bot53bits;

	t2=w[8]+(t1>>kBPW);
	w[8]=t2&bot53bits;

	t1=w[9]+(t2>>kBPW);
	w[9]=t1&bot44bits;

	w[0]=t0+(t1>>kBPWFinal);
}

// multiply w by a constant, w*=i

static
void gmuli(coord_t& w,int i)
{
	type128 t;

	t=(type128)w[0]*i;
	w[0]=((type64)t)&bot53bits;

	t=(type128)w[1]*i+(t>>kBPW);
	w[1]=((type64)t)&bot53bits;

	t=(type128)w[2]*i+(t>>kBPW);
	w[2]=((type64)t)&bot53bits;

	t=(type128)w[3]*i+(t>>kBPW);
	w[3]=((type64)t)&bot53bits;

	t=(type128)w[4]*i+(t>>kBPW);
	w[4]=((type64)t)&bot53bits;

	t=(type128)w[5]*i+(t>>kBPW);
	w[5]=((type64)t)&bot53bits;

	t=(type128)w[6]*i+(t>>kBPW);
	w[6]=((type64)t)&bot53bits;

	t=(type128)w[7]*i+(t>>kBPW);
	w[7]=((type64)t)&bot53bits;

	t=(type128)w[8]*i+(t>>kBPW);
	w[8]=((type64)t)&bot53bits;

	t=(type128)w[9]*i+(t>>kBPW);
	w[9]=((type64)t)&bot44bits;

	w[0]+=(t>>kBPWFinal);
}

// z=x^2
#define prod(a,ia,b,ib)  ((type128)((a)[ia]))*((type128)((b)[ib]))
#define sqx(x,ia,ib)     prod(x,ia,x,ib)

static
void gsqr(coord_t& x,coord_t& z)
{
    type128 t0,t1;
    
    t0=2*(sqx(x,0,9)+sqx(x,1,8)+sqx(x,2,7)+sqx(x,3,6)+sqx(x,4,5));
    z[9]=((type64) t0)&bot44bits;
    
    t1=sqx(x,0,0)+
       kwrap*(2*(sqx(x,1,9)+sqx(x,2,8)+sqx(x,3,7)+sqx(x,4,6))+
              sqx(x,5,5))+
       (t0>>kBPWFinal);
    z[0]=((type64) t1)&bot53bits;
    
    t0=2*(sqx(x,0,1)+
          kwrap*(sqx(x,2,9)+sqx(x,3,8)+sqx(x,4,7)+sqx(x,5,6)))+
       (t1>>kBPW);
    z[1]=((type64) t0)&bot53bits;
    
    t1=2*sqx(x,0,2)+
       sqx(x,1,1)+
       kwrap*(2*(sqx(x,3,9)+sqx(x,4,8)+sqx(x,5,7))+sqx(x,6,6))+
       (t0>>kBPW);
    z[2]=((type64) t1)&bot53bits;
    
    t0=2*(sqx(x,0,3)+sqx(x,1,2))+
       2*kwrap*(sqx(x,4,9)+sqx(x,5,8)+sqx(x,6,7))+
       (t1>>kBPW);
    z[3]=((type64) t0)&bot53bits;
    
    t1=2*(sqx(x,0,4)+sqx(x,1,3))+
       sqx(x,2,2)+
       kwrap*(2*(sqx(x,5,9)+sqx(x,6,8))+sqx(x,7,7))+
       (t0>>kBPW);
    z[4]=((type64) t1)&bot53bits;
    
    t0=2*(sqx(x,0,5)+sqx(x,1,4)+sqx(x,2,3))+
       2*kwrap*(sqx(x,6,9)+sqx(x,7,8))+
       (t1>>kBPW);
    z[5]=((type64) t0)&bot53bits;
    
    t1=2*(sqx(x,0,6)+sqx(x,1,5)+sqx(x,2,4))+
       sqx(x,3,3)+
       kwrap*(2*sqx(x,7,9)+sqx(x,8,8))+
       (t0>>kBPW);
    z[6]=((type64) t1)&bot53bits;
    
    t0=2*(sqx(x,0,7)+sqx(x,1,6)+sqx(x,2,5)+sqx(x,3,4))+
       2*kwrap*sqx(x,8,9)+
       (t1>>kBPW);
    z[7]=((type64) t0)&bot53bits;
    
    t1=2*(sqx(x,0,8)+sqx(x,1,7)+sqx(x,2,6)+sqx(x,3,5))+
       sqx(x,4,4)+
       kwrap*sqx(x,9,9)+
       (t0>>kBPW);
    z[8]=((type64) t1)&bot53bits;
    
    t0=z[9]+(t1>>kBPW);
    z[9]=((type64) t0)&bot44bits;
    z[0]+=(t0>>kBPWFinal);
}

static
void gmul(coord_t& x,coord_t& y,coord_t& z)
{
    type128 t0,t1;
    
    t0=prod(x,0,y,9)+prod(x,9,y,0)+
       prod(x,1,y,8)+prod(x,8,y,1)+
       prod(x,2,y,7)+prod(x,7,y,2)+
       prod(x,3,y,6)+prod(x,6,y,3)+
       prod(x,4,y,5)+prod(x,5,y,4);
    z[9]=((type64) t0)&bot44bits;
    
    t1=prod(x,0,y,0)+
       kwrap*(prod(x,1,y,9)+prod(x,9,y,1)+
              prod(x,2,y,8)+prod(x,8,y,2)+
              prod(x,3,y,7)+prod(x,7,y,3)+
              prod(x,4,y,6)+prod(x,6,y,4)+
              prod(x,5,y,5))+
      (t0>>kBPWFinal);
    z[0]=((type64) t1)&bot53bits;
    
    t0=prod(x,0,y,1)+prod(x,1,y,0)+
       kwrap*(prod(x,2,y,9)+prod(x,9,y,2)+
              prod(x,3,y,8)+prod(x,8,y,3)+
              prod(x,4,y,7)+prod(x,7,y,4)+
              prod(x,5,y,6)+prod(x,6,y,5))+
       (t1>>kBPW);
    z[1]=((type64) t0)&bot53bits;
    
    t1=prod(x,0,y,2)+prod(x,2,y,0)+
       prod(x,1,y,1)+
       kwrap*(prod(x,3,y,9)+prod(x,9,y,3)+
              prod(x,4,y,8)+prod(x,8,y,4)+
              prod(x,5,y,7)+prod(x,7,y,5)+
              prod(x,6,y,6))+
       (t0>>kBPW);
    z[2]=((type64) t1)&bot53bits;
    
    t0=prod(x,0,y,3)+prod(x,3,y,0)+
       prod(x,1,y,2)+prod(x,2,y,1)+
       kwrap*(prod(x,4,y,9)+prod(x,9,y,4)+
              prod(x,5,y,8)+prod(x,8,y,5)+
              prod(x,6,y,7)+prod(x,7,y,6))+
    (t1>>kBPW);
    z[3]=((type64) t0)&bot53bits;
    
    t1=prod(x,0,y,4)+prod(x,4,y,0)+
       prod(x,1,y,3)+prod(x,3,y,1)+
       prod(x,2,y,2)+
       kwrap*(prod(x,5,y,9)+prod(x,9,y,5)+
              prod(x,6,y,8)+prod(x,8,y,6)+
              prod(x,7,y,7))+
       (t0>>kBPW);
    z[4]=((type64) t1)&bot53bits;
    
    t0=prod(x,0,y,5)+prod(x,5,y,0)+
       prod(x,1,y,4)+prod(x,4,y,1)+
       prod(x,2,y,3)+prod(x,3,y,2)+
       kwrap*(prod(x,6,y,9)+prod(x,9,y,6)+
              prod(x,7,y,8)+prod(x,8,y,7))+
       (t1>>kBPW);
    z[5]=((type64) t0)&bot53bits;
    
    t1=prod(x,0,y,6)+prod(x,6,y,0)+
       prod(x,1,y,5)+prod(x,5,y,1)+
       prod(x,2,y,4)+prod(x,4,y,2)+
       prod(x,3,y,3)+
       kwrap*(prod(x,7,y,9)+prod(x,9,y,7)+
              prod(x,8,y,8))+
       (t0>>kBPW);
    z[6]=((type64) t1)&bot53bits;
    
    t0=prod(x,0,y,7)+prod(x,7,y,0)+
       prod(x,1,y,6)+prod(x,6,y,1)+
       prod(x,2,y,5)+prod(x,5,y,2)+
       prod(x,3,y,4)+prod(x,4,y,3)+
       kwrap*(prod(x,8,y,9)+prod(x,9,y,8))+
       (t1>>kBPW);
    z[7]=((type64) t0)&bot53bits;
    
    t1=prod(x,0,y,8)+prod(x,8,y,0)+
       prod(x,1,y,7)+prod(x,7,y,1)+
       prod(x,2,y,6)+prod(x,6,y,2)+
       prod(x,3,y,5)+prod(x,5,y,3)+
       prod(x,4,y,4)+
       kwrap*prod(x,9,y,9)+
       (t0>>kBPW);
    z[8]=((type64) t1)&bot53bits;
    
    t0=z[9]+(t1>>kBPW);
    z[9]=((type64) t0)&bot44bits;
    z[0]+=(t0>>kBPWFinal);
}

// Inverse x = 1/x = x^(p-2) mod p
// the exponent (p-2) = "1FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFD"
// p-2 = 2^521-3 = (2^2)*(2^519-1)+1
// (129 F's)
//
// This calls for repeated application of the identity:
//    (2^(2*N) - 1) = (2^N - 1) * (2^N + 1)
//                  = (2^N-1)*2^N + (2^N-1) 
// Shift arg width of arg using SQR and then mult with arg

static void shmul(coord_t &x, int nsh, coord_t &y, coord_t &dst) {
    // Shift arg X left by nsh using gsqr, then mult by Y.
    // We make copies of x, y locally, so that dst can overlap with either of them.
    // That isn't allowed with gmul - all gmul args must be distinct from dst.
    coord_t t,t2;
    
    if(1 & nsh)
        gsqr(x,t);
    else
        gcopy(x,t);
    for(int ix = (nsh >> 1); ix-- > 0;) {
        gsqr(t,t2);
        gsqr(t2,t);
    }
    gcopy(y,t2);
    gmul(t2,t,dst);
}

static
void ginv(coord_t& x)
{
    coord_t w;

    shmul(x,   1, x, w); //   2-bits
    shmul(w,   2, w, w); //   4-bits
    shmul(w,   4, w, w); //   8-bits
    shmul(w,   8, w, w); //  16-bits
    shmul(w,  16, w, w); //  32-bits
    shmul(w,  32, w, w); //  64-bits
    shmul(w,  64, w, w); // 128-bits
    shmul(w,   1, x, w); // 129-bits
    shmul(w, 129, w, w); // 258-bits
    shmul(w,   1, x, w); // 259-bits
    shmul(w, 259, w, w); // 518-bits
    shmul(w,   1, x, w); // 519-bits
    shmul(w,   2, x, x);
}

static
void gneg(coord_t& x, coord_t& y) {
    for(int i = 0; i < NGRP; ++i)
        y[i] = -x[i];
}
   
static
void gnorm(coord_t& x, coord_t& y) {
    coord_t tmp;
    gneg(x, tmp);
    scr(tmp);
    gneg(tmp, y);
    scr(y);
    scr(y);
}

static
bool geq(coord_t& x, coord_t& y) {
    // compare x, y for equality
    coord_t z1, z2;
    gsub(x, y, z1);
    gnorm(z1, z2);
    type64 m = 0;
    for(int i = 0; i < NGRP; ++i)
        m |= z2[i];
    return (0 == m);
}

// -----------------------------------------------------
// Curve-E521 has modulus q = 2^521-1
// q mod 4 = 3
static
bool gsqrt(coord_t& x, coord_t& y) {
    // Sqrt(x) -> y
    //
    // By Fermat's theorem, for prime |Fq|, |Fq| mod 4 = 3
    // when X is a quadratic residue in the field,
    // we have Sqrt(X) = X^((|Fq|+1)/4) mod |Fq|
    // where |Fq| = 2^521-1, (|Fq|+1)/4 = 2^519
    // Return true if X was a quadratic-residue of Fq.
    //
    coord_t one, tmp;
    
    gzap(one);
    one[0] = 1;
    shmul(x,519,one,y);
    
    gsqr(y,tmp);
    return geq(tmp,x);
}

// Point Structure

typedef struct {
    coord_t x;
    coord_t y;
    coord_t z;
    coord_t t;
} ECp;

#define zap(pt)        memset(pt,0,sizeof(ECp))
#define copy(src,dst)  memcpy(dst,src,sizeof(ECp))

// P+=P

static
void double_1(ECp *P)
{
	coord_t a,b,e,f,g,h;
	gsqr(P->x,a);
	gsqr(P->y,b);
	gcopy(P->t,e);
	gmul2(e);
	gadd(a,b,g);
	gcopy(g,f); f[0]-=2;
	gsub(a,b,h);
	gmul(e,f,P->x); 
	gmul(g,h,P->y); 
	gsqr(g,P->z);
	gsb2(g,P->z);
	gmul(e,h,P->t);  
	scr(P->z);
}

static
void double_2(ECp *P)
{
    coord_t a,b,c,e,f,g,h;
	gsqr(P->x,a);
	gsqr(P->y,b); 
	gsqr(P->z,c); gmul2(c); 
	gadd(P->x,P->y,g); gsqr(g,e); gdec(a,e); gdec(b,e); 
	gadd(a,b,g); 
	gsub(g,c,f); 
	gsub(a,b,h); 
	gmul(e,f,P->x); 
	gmul(g,h,P->y);
	gmul(f,g,P->z); 
}

static
void double_3(ECp *P)
{
    coord_t a,b,c,e,f,g,h;
	gsqr(P->x,a);
	gsqr(P->y,b);
	gsqr(P->z,c); gmul2(c);
	gadd(P->x,P->y,g); gsqr(g,e); gdec(a,e); gdec(b,e); 
	gadd(a,b,g);
	gsub(g,c,f);
	gsub(a,b,h);
	gmul(e,f,P->x); 
	gmul(g,h,P->y); 
	gmul(f,g,P->z);
	gmul(e,h,P->t); 
}

//P+=Q;

static
void add_1(ECp *Q,ECp *P)
{
    coord_t a,b,c,d,e,f,g,h;
	gmul(P->x,Q->x,a);
	gmul(P->y,Q->y,b);
	gmul(P->t,Q->t,c);
	gsub(P->z,c,f);  /* reversed sign as d is negative */
	gadd(P->z,c,g);
	gsub(b,a,h);
	gadd(P->x,P->y,c); gadd(Q->x,Q->y,d); gmul(c,d,e); gdec(a,e); gdec(b,e);
	gmul(e,f,P->x); 
	gmul(g,h,P->y); 
	gmul(f,g,P->z); 
	gmul(e,h,P->t); 
}

static
void add_2(ECp *Q,ECp *P)
{
    coord_t a,b,c,d,e,f,g,h;
	gmul(P->x,Q->x,a);
	gmul(P->y,Q->y,b);
	gmul(P->t,Q->t,c);
	gmul(P->z,Q->z,d);
	gsub(d,c,f);  /* reversed sign as d is negative */
	gadd(d,c,g);
	gsub(b,a,h);
	gadd(P->x,P->y,c); gadd(Q->x,Q->y,d); gmul(c,d,e); gdec(a,e); gdec(b,e);
	gmul(e,f,P->x); 
	gmul(g,h,P->y); 
	gmul(f,g,P->z); 
}

//P=0

static
void inf(ECp *P)
{
    zap(P);
	P->y[0]=P->z[0]=1;
}

// Initialise P
static
void init(coord_t& x,coord_t& y,ECp *P)
{
    gcopy(x,P->x);
    gcopy(y,P->y);
    gzap(P->z);
	P->z[0]=1;
	gmul(x,y,P->t);
}

// P=-Q

static
void neg(ECp *Q,ECp *P)
{
    gneg(Q->x,P->x);
    gcopy(Q->y,P->y);
    gcopy(Q->z,P->z);
    gneg(Q->t,P->t);
}
   
/* Make Affine */

static
void norm(ECp *P)
{
    coord_t w,t;
    gcopy(P->z,w);
	ginv(w);
	gmul(P->x,w,t); scr(t); gcopy(t,P->x);
	gmul(P->y,w,t); scr(t); gcopy(t,P->y);
	gmul(P->z,w,t); scr(t); gcopy(t,P->z);
	gmul(P->t,w,t); scr(t); gcopy(t,P->t);
}

/* Precomputation */

static
void precomp(ECp *P,ECp W[])
{
	inf(&W[0]);
	copy(P,&W[1]); gmuli(W[1].t,kCurveD);
	copy(P,&W[2]); double_1(&W[2]);
	copy(&W[2],&W[3]); add_1(&W[1],&W[3]);

	copy(&W[2],&W[4]); double_3(&W[4]);
	copy(&W[4],&W[5]); add_1(&W[1],&W[5]);
	copy(&W[3],&W[6]); double_3(&W[6]);
	copy(&W[6],&W[7]); add_1(&W[1],&W[7]);
	copy(&W[4],&W[8]); double_3(&W[8]);

/* premultiply t parameter by curve constant */
    for(int i = 2; i <= 8; ++i)
        gmuli(W[i].t, kCurveD);
}

/* Window of width 4 */

static
void window(ECp *Q,ECp *P)
{
	double_2(P);
	double_2(P);
	double_2(P);
	double_3(P);
	add_2(Q,P);
}

/*
Constant time table look-up - borrowed from ed25519 
*/

static
void fe_cmov(type64 f[],type64 g[],int ib)
{
    type64 b=ib;
    b=-b;
    for(int i = 0; i < NGRP; ++i)
        f[i] ^= (f[i] ^ g[i]) & b;
}

static void cmov(ECp *w,ECp *u,int b)
{
  fe_cmov(w->x,u->x,b);
  fe_cmov(w->y,u->y,b);
  fe_cmov(w->z,u->z,b);
  fe_cmov(w->t,u->t,b);
}

// return 1 if b==c, no branching
static int equal(int b,int c)
{
	int x=b^c;
	x-=1;  // if x=0, x now -1
	return ((x>>31)&1);
}

static void select(ECp *T,ECp W[],int b)
{
    ECp MT;
    int m=b>>31;
    int babs=(b^m)-m;
    
    for(int i = 0; i <= 8; ++i)
        cmov(T,&W[i],equal(babs,i));
    neg(T,&MT);  // minus t
    cmov(T,&MT,m&1);
}

#define testing 0
#if testing
#define TEST  /* define to multiply by group order */

/* Point Multiplication - exponent is 333 bits */

static
void mul(int *w,ECp *P)
{
    ECp W[1+(1<<(WINDOW-1))],Q;
    precomp(P,W);

    copy(&W[w[PANES-1]],P);
    for (int i=PANES-2;i>=0;i--)
    {
        select(&Q,W,w[i]);
        window(&Q,P);
    }
    norm(P);
}

int main()
{
	uint64_t bef,aft;
	int w[PANES];
	int ii,lpz=10000;
	type64 xs[6],ys[6];
	ECp P;

/* Base point on Curve  */

	xs[0]=0xcLL;
	xs[1]=0x0LL;
	xs[2]=0x0LL;
	xs[3]=0x0LL;
	xs[4]=0x0LL;
	xs[5]=0x0LL;

	ys[0]=0xBEC68505FE8632;
	ys[1]=0x5650CA0365DB12;
	ys[2]=0x1C7EF435B6DB5D;
	ys[3]=0x53D1B14B46C381;
	ys[4]=0xE18E1C161D0078;
	ys[5]=0x0C0DC616B56502;

// random multiplier arranged in signed windows of size 6
#ifndef TEST
void output(ECp *P)
{
	cout << "x[0]= " << hex << P->x[0] << endl;
	cout << "x[1]= " << hex << P->x[1] << endl;
	cout << "x[2]= " << hex << P->x[2] << endl;
	cout << "x[3]= " << hex << P->x[3] << endl;
	cout << "x[4]= " << hex << P->x[4] << endl;
	cout << endl;

	cout << "y[0]= " << hex << P->y[0] << endl;
	cout << "y[1]= " << hex << P->y[1] << endl;
	cout << "y[2]= " << hex << P->y[2] << endl;
	cout << "y[3]= " << hex << P->y[3] << endl;
	cout << "y[4]= " << hex << P->y[4] << endl;
	cout << endl;
}

#if WINDOW==4

w[0]= 5; w[1]= -5; w[2]= 2; w[3]= -5; w[4]= -8;
w[5]= -3; w[6]= 5; w[7]= -2; w[8]= -4; w[9]= 3; 
w[NGRP]= -5; w[NGRP]= -4; w[12]= -6; w[13]= -4; w[14]= -7;
w[15]= -8; w[16]= -5; w[17]= -2; w[18]= 4; w[19]= -2;
w[20]= -7; w[21]= 2; w[22]= 7; w[23]= -5; w[24]= -8;
w[25]= -4; w[26]= -1; w[27]= -8; w[28]= 7; w[29]= -6;
w[30]= -7; w[31]= -5; w[32]= -4; w[33]= -5; w[34]= -3;
w[35]= 6; w[36]= -2; w[37]= -3; w[38]= -3; w[39]= -7;
w[40]= -4; w[41]= -2; w[42]= 3; w[43]= 7; w[44]= -1;
w[45]= 0; w[46]= -1; w[47]= -5; w[48]= -1; w[49]= -4;
w[50]= -4; w[51]= 5; w[52]= -1; w[53]= -6; w[54]= 7;
w[55]= 4; w[56]= 6; w[57]= -4; w[58]= -2; w[59]= -2; 
w[60]= 2; w[61]= -4; w[62]= 5; w[63]= 1; w[64]= 3;
w[65]= -6; w[66]= -7; w[67]= -2; w[68]= 1; w[69]= 7;
w[70]= 2; w[71]= -6; w[72]= 6; w[73]= 3; w[74]= -5;
w[75]= 6; w[76]= -7; w[77]= -6; w[78]= -4; w[79]= 6;
w[80]= -6; w[81]= -8; w[82]= -3; w[83]= 1;


#endif

// Group Order - for testing
#else

#if WINDOW==4
w[0]= 5; w[1]= 0; w[2]= -8; w[3]= -5; w[4]= 0;
w[5]= -6; w[6]= -1; w[7]= 4; w[8]= 7; w[9]= -7;
w[NGRP]= 6; w[NGRP]= -7; w[12]= 0; w[13]= 3; w[14]= -5;
w[15]= -8; w[16]= -5; w[17]= -5; w[18]= -4; w[19]= 4;
w[20]= -7; w[21]= 0; w[22]= -8; w[23]= -7; w[24]= -4;
w[25]= 7; w[26]= -3; w[27]= -4; w[28]= 1; w[29]= -4;
w[30]= 1; w[31]= 5; w[32]= -8; w[33]= -6; w[34]= -5;
w[35]= 0; w[36]= 6; w[37]= 1; w[38]= 4; w[39]= 1;
w[40]= 7; w[41]= 0; w[42]= 0; w[43]= 0; w[44]= 0;
w[45]= 0; w[46]= 0; w[47]= 0; w[48]= 0; w[49]= 0;
w[50]= 0; w[51]= 0; w[52]= 0; w[53]= 0; w[54]= 0;
w[55]= 0; w[56]= 0; w[57]= 0; w[58]= 0; w[59]= 0;
w[60]= 0; w[61]= 0; w[62]= 0; w[63]= 0; w[64]= 0;
w[65]= 0; w[66]= 0; w[67]= 0; w[68]= 0; w[69]= 0;
w[70]= 0; w[71]= 0; w[72]= 0; w[73]= 0; w[74]= 0;
w[75]= 0; w[76]= 0; w[77]= 0; w[78]= 0; w[79]= 0;
w[80]= 0; w[81]= 0; w[82]= 0; w[83]= 2;
#endif

#endif

	bef=rdtsc();
	for (ii=0;ii<lpz;ii++)
	{
		init(xs,ys,&P);
		mul(w,&P);
	}
	aft=rdtsc();
	cout << "Clock cycles= " << (aft-bef)/(lpz) << endl;

	output(&P);

	return 0;
}
#endif // testing

// -------------------------------------------------------------
// Lisp Interface

/* Point Multiplication - exponent is 519 bits */

static
void mul_to_proj(int *w,ECp *P)
{
	ECp W[1+(1<<(WINDOW-1))],Q;
	precomp(P,W);

	copy(&W[w[PANES-1]],P);  
	for (int i=PANES-2;i>=0;i--)
	{
		select(&Q,W,w[i]);
		window(&Q,P);
	}
    norm(P);
}

static
void win4(unsigned char* nv, int *w)
{
  // convert incoming N to bipolar 4-bit window vector - no branching
  int ix, jx;
  int cy = 0; // carry bit
  for(ix = 0, jx = 0; ix < PANES;)
    {
      unsigned int byt;
      int v;

      byt = nv[jx++];
      v = cy + (byt & 15);
      cy = (v >> 3);
      cy |= (cy >> 1);
      cy &= 1;
      v -= (cy << 4);
      w[ix++] = v;
      v = cy + (byt >> 4);
      cy = (v >> 3);
      cy |= (cy >> 1);
      cy &= 1;
      v -= (cy << 4);
      w[ix++] = v;
    }
}

static
void gfetch(unsigned char* v, coord_t& w)
{
    // fetch 53-bit words from consecutively stored 64-bit words in v.
    // Assumes input value is < 2^521
    
    uint64_t *pv = (uint64_t*)v;
    uint64_t *pw = (uint64_t*)w;
    pw[0] =   pv[0] & bot53bits;
    pw[1] = ((pv[1] << 11) | (pv[0] >> 53)) & bot53bits;
    pw[2] = ((pv[2] << 22) | (pv[1] >> 42)) & bot53bits;
    pw[3] = ((pv[3] << 33) | (pv[2] >> 31)) & bot53bits;
    pw[4] = ((pv[4] << 44) | (pv[3] >> 20)) & bot53bits;
    pw[5] =                  (pv[4] >>  9)  & bot53bits;
    pw[6] = ((pv[5] <<  2) | (pv[4] >> 62)) & bot53bits;
    pw[7] = ((pv[6] << 13) | (pv[5] >> 51)) & bot53bits;
    pw[8] = ((pv[7] << 24) | (pv[6] >> 40)) & bot53bits;
    pw[9] = ((pv[8] << 35) | (pv[7] >> 29)) & bot44bits;
}

static
void gstore(coord_t& w, unsigned char* v)
{
    // store 53-bit words into consecutively stored 64-bit words in v.
    coord_t wn;
    
    gnorm(w, wn);

    uint64_t *pv = (uint64_t*)v;
    uint64_t *pw = (uint64_t*)wn;
    pv[0] = (pw[1] << 53) | pw[0];
    pv[1] = (pw[2] << 42) | (pw[1] >> 11);
    pv[2] = (pw[3] << 31) | (pw[2] >> 22);
    pv[3] = (pw[4] << 20) | (pw[3] >> 33);
    pv[4] = (pw[6] << 62) | (pw[5] <<  9) | (pw[4] >> 44);
    pv[5] = (pw[7] << 51) | (pw[6] >>  2);
    pv[6] = (pw[8] << 40) | (pw[7] >> 13);
    pv[7] = (pw[9] << 29) | (pw[8] >> 24);
    pv[8] =                 (pw[9] >> 35);
}

// -----------------------------------------------------
// Lisp Intetface

extern "C"
void CurveE521_affine_mul(unsigned char* ptx,
                          unsigned char* pty,
                          unsigned char* ptz,
                          unsigned char* nv)
{
    // Multiplies point pt by scalar n and returns in pt as projective coords
    //
    // lpx, lpy, lpz, and nv are stored as consecutive 64-bit values
    // in little-endian order (assumes Intel conventions)
    //
    // nv should be a little-endian encoding of scalar n. n should be
    // mod q. (0 <= n < q)
    
    int w[PANES];
    ECp P;
    coord_t px, py;
    
    win4(nv, w);
    gfetch(ptx, px);
    gfetch(pty, py);
    
    init(px, py, &P);
    mul_to_proj(w, &P);
    
    gstore(P.x, ptx);
    gstore(P.y, pty);
    gstore(P.z, ptz);
}

static
void normz(coord_t& px, coord_t& py, coord_t& pz)
{
    coord_t t;
    
    ginv(pz);
    gmul(px, pz, t); scr(t); gcopy(t, px);
    gmul(py, pz, t); scr(t); gcopy(t, py);
}

extern "C"
void CurveE521_projective_mul(unsigned char* ptx,
                              unsigned char* pty,
                              unsigned char* ptz,
                              unsigned char* nv)
{
    // Multiplies point pt by scalar n and returns in pt as projective coords
    //
    // lpx, lpy, lpz, and nv are stored as consecutive 64-bit values
    // in little-endian order (assumes Intel conventions)
    //
    // nv should be a little-endian encoding of scalar n. n should be
    // mod q. (0 <= n < q)
    
    int w[PANES];
    ECp P;
    coord_t px, py, pz;
    
    win4(nv, w);
    gfetch(ptx, px);
    gfetch(pty, py);
    gfetch(ptz, pz);
    
    normz(px, py, pz);
    init(px, py, &P);
    mul_to_proj(w, &P);
    
    gstore(P.x, ptx);
    gstore(P.y, pty);
    gstore(P.z, ptz);
}

// Initialise P

static
void init_proj(coord_t& x,coord_t& y,coord_t& z, ECp *P)
{
    gcopy(x,P->x);
    gcopy(y,P->y);
    gcopy(z,P->z);
    gzap(P->t);
}

static
void add_proj(ECp *Q, ECp *P)
{
    // Add Q to P, both in projective (X,Y,Z) coordinates. We don't use T here.
    //
    // careful here... the 3-address code cannot share output with input
    coord_t A,B,C,D,E,F,G,X3,Y3,Z3;
    
    gmul(Q->z, P->z, A);
    gsqr(A,B);
    gmul(Q->x, P->x, C);
    gmul(Q->y, P->y, D);
    gmul(C,D,E); gmuli(E, kCurveD);
    gsub(B,E,F);
    gadd(B,E,G);
    gadd(Q->x, Q->y, X3);
    gadd(P->x, P->y, Y3);
    gmul(X3, Y3, Z3);
    gsub(Z3, C, Y3);
    gsub(Y3, D, X3);
    gmul(F, X3, Y3);
    gmul(A, Y3, X3);
    gsub(D, C, Y3);
    gmul(G, Y3, Z3);
    gmul(A, Z3, Y3);
    gmul(F, G, Z3); // c = 1
    init_proj(X3, Y3, Z3, P);
}

extern "C"
void CurveE521_projective_add(unsigned char* lp1x,
                              unsigned char* lp1y,
                              unsigned char* lp1z,
                              unsigned char* lp2x,
                              unsigned char* lp2y,
                              unsigned char* lp2z)
{
    // Add two Projective points returning result in first one (pt)
    //
    // lpx, lpy, and lpz are stored as consecutive 64-bit values
    // in little-endian order (assumes Intel conventions)
    //
    
    ECp P, Q;
    coord_t px, py, pz, qx, qy, qz;
    
    gfetch(lp1x,px);
    gfetch(lp1y,py);
    gfetch(lp1z,pz);
    
    gfetch(lp2x,qx);
    gfetch(lp2y,qy);
    gfetch(lp2z,qz);
    
    init_proj(px, py, pz, &P);
    init_proj(qx, qy, qz, &Q);
    
    add_proj(&Q, &P); // Q + P -> P
    
    gstore(P.x, lp1x);
    gstore(P.y, lp1y);
    gstore(P.z, lp1z);
}

extern "C"
void CurveE521_to_affine(unsigned char* lpx,
                         unsigned char* lpy,
                         unsigned char* lpz)
{
    // Convert incoming projective coordinates to affine form. Return
    // result in-place.
    //
    // lpx, lpy, and lpz are stored as consecutive 64-bit values
    // in little-endian order (assumes Intel conventions)
    //
    
    coord_t px, py, pz, t;
    
    gfetch(lpx, px);
    gfetch(lpy, py);
    gfetch(lpz, pz);
    
    ginv(pz);
    gmul(pz, px, t);
    gstore(t, lpx);
    gmul(pz, py, t);
    gstore(t, lpy);
    gstore(pz, lpz); // in case anyone wants 1/z
}

extern "C"
bool g521_sqrt(unsigned char* lpsrc, unsigned char* lpdst)
{
    // Square root of Curve-Field numbers (p = 2^521-1).
    // NOTE: Not applicable to Field on the curve, but to point-space.
    coord_t src,dst;
    
    gfetch(lpsrc, src);
    bool ans = gsqrt(src, dst);
    gstore(dst, lpdst);
    return ans;
}

extern "C"
void g521_inv(unsigned char* lpsrc, unsigned char* lpdst)
{
    // Inverse of Curve-Field numbers (p = 2^521-1)
    // NOTE: Not applicable to Field on the curve, but to point-space.
    coord_t src;
    
    gfetch(lpsrc, src);
    ginv(src);
    gstore(src, lpdst);
}

extern "C"
void g521_mul(unsigned char* lpsrc1, unsigned char* lpsrc2, unsigned char *lpdst)
{
    // Multiply two field numbers. Intended for point-space (p = 2^521-1).
    // But perhaps also useful for field numbers on the curve (q = 2^519-big).
    coord_t src1, src2, dst;
    
    gfetch(lpsrc1, src1);
    gfetch(lpsrc2, src2);
    gmul(src1,src2,dst);
    gstore(dst, lpdst);
}

// --- end of ed3363_intf.cpp --- //
