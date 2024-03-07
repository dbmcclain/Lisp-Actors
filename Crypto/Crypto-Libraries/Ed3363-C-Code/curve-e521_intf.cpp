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
static const type64 qbase[NGRP] = {0x1FFFFFFFFFFFFF, 0x1FFFFFFFFFFFFF, 
    0x1FFFFFFFFFFFFF, 0x1FFFFFFFFFFFFF, 0x1FFFFFFFFFFFFF,
    0x1FFFFFFFFFFFFF, 0x1FFFFFFFFFFFFF, 0x1FFFFFFFFFFFFF, 0x000FFFFFFFFFFF};

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

#define KBPW          kBPW
#define KBITS         ((1L << KBPW) - 1)
#define KWRAP         kwrap
#define KHIBPW        kBPWFinal
#define KHIBITS       ((1L << KHIBPW) - 1)
#define KHIWRAP       1
#define lobits(x,msk) (((type64)x) & msk)
#define hibits(x,nsh) (x >> nsh)

// reduce w - Short Coefficient Reduction
static
void scr(coord_t& x)
{
    // After one round of scr(), all elements, x[i], will be positiive values,
    // with the possible exception of x[0]. One more round will force all elements,
    // x[i] to positive values. But we could still be beyond the modular range.
    type64 w;
    
    w = x[9];
    x[9] = lobits(w, KHIBITS);
    
    w = x[0] + KHIWRAP*hibits(w, KHIBPW);
    x[0] = lobits(w, KBITS);
    
    w = x[1] + hibits(w, KBPW);
    x[1] = lobits(w, KBITS);
    
    w = x[2] + hibits(w, KBPW);
    x[2] = lobits(w, KBITS);
    
    w = x[3] + hibits(w, KBPW);
    x[3] = lobits(w, KBITS);
    
    w = x[4] + hibits(w, KBPW);
    x[4] = lobits(w, KBITS);
    
    w = x[5] + hibits(w, KBPW);
    x[5] = lobits(w, KBITS);
    
    w = x[6] + hibits(w, KBPW);
    x[6] = lobits(w, KBITS);
    
    w = x[7] + hibits(w, KBPW);
    x[7] = lobits(w, KBITS);
    
    w = x[8] + hibits(w, KBPW);
    x[8] = lobits(w, KBITS);
    
    w     = x[9] + hibits(w, KBPW);
    x[9]  = lobits(w, KHIBITS);
    x[0] += KHIWRAP * hibits(w, KHIBPW);
}

// multiply w by a constant, w*=i

static
void gmuli(coord_t& x,int i)
{
    type128 w;

#define muli(ix)   ((type128)x[ix])*i
    w = muli(9);
    x[9] = lobits(w,KHIBITS);
    
    w = muli(0) + KHIWRAP*hibits(w, KHIBPW);
    x[0] = lobits(w, KBITS);
    
    w = muli(1) + hibits(w, KBPW);
    x[1] = lobits(w, KBITS);
    
    w = muli(2) + hibits(w, KBPW);
    x[2] = lobits(w, KBITS);
    
    w = muli(3) + hibits(w, KBPW);
    x[3] = lobits(w, KBITS);
    
    w = muli(4) + hibits(w, KBPW);
    x[4] = lobits(w, KBITS);
    
    w = muli(5) + hibits(w, KBPW);
    x[5] = lobits(w, KBITS);
    
    w = muli(6) + hibits(w, KBPW);
    x[6] = lobits(w, KBITS);
    
    w = muli(7) + hibits(w, KBPW);
    x[7] = lobits(w, KBITS);
    
    w = muli(8) + hibits(w, KBPW);
    x[8] = lobits(w, KBITS);
    
    w     = x[9] + hibits(w, KBPW);
    x[9]  = lobits(w, KHIBITS);
    x[0] += KHIWRAP * hibits(w, KHIBPW);
#undef muli
}

// z=x^2
static
void gsqr(coord_t& x,coord_t& z)
{
    type128 w;
    
#define sqr(ix, iy)   ((type128)(x[ix]))*((type128)(x[iy]))
    
    w=2*(sqr(0,9)+sqr(1,8)+sqr(2,7)+sqr(3,6)+sqr(4,5));
    z[9]=lobits(w, KHIBITS);
    
    w=sqr(0,0)+
       KWRAP*(2*(sqr(1,9)+sqr(2,8)+sqr(3,7)+sqr(4,6))+
              sqr(5,5))+
        KHIWRAP*hibits(w, KHIBPW);
    z[0]=lobits(w, KBITS);
    
    w=2*(sqr(0,1)+
          KWRAP*(sqr(2,9)+sqr(3,8)+sqr(4,7)+sqr(5,6)))+
        hibits(w, KBPW);
    z[1]=lobits(w, KBITS);
    
    w=2*sqr(0,2)+
       sqr(1,1)+
       KWRAP*(2*(sqr(3,9)+sqr(4,8)+sqr(5,7))+sqr(6,6))+
       hibits(w, KBPW);
    z[2]=lobits(w, KBITS);
    
    w=2*(sqr(0,3)+sqr(1,2))+
       2*KWRAP*(sqr(4,9)+sqr(5,8)+sqr(6,7))+
       hibits(w, KBPW);
    z[3]=lobits(w, KBITS);
    
    w=2*(sqr(0,4)+sqr(1,3))+
       sqr(2,2)+
       KWRAP*(2*(sqr(5,9)+sqr(6,8))+sqr(7,7))+
       hibits(w, KBPW);
    z[4]=lobits(w, KBITS);
    
    w=2*(sqr(0,5)+sqr(1,4)+sqr(2,3))+
       2*KWRAP*(sqr(6,9)+sqr(7,8))+
       hibits(w, KBPW);
    z[5]=lobits(w, KBITS);
    
    w=2*(sqr(0,6)+sqr(1,5)+sqr(2,4))+
       sqr(3,3)+
       KWRAP*(2*sqr(7,9)+sqr(8,8))+
       hibits(w, KBPW);
    z[6]=lobits(w, KBITS);
    
    w=2*(sqr(0,7)+sqr(1,6)+sqr(2,5)+sqr(3,4))+
       2*KWRAP*sqr(8,9)+
        hibits(w, KBPW);
    z[7]=lobits(w, KBITS);
    
    w=2*(sqr(0,8)+sqr(1,7)+sqr(2,6)+sqr(3,5))+
       sqr(4,4)+
       KWRAP*sqr(9,9)+
        hibits(w, KBPW);
    z[8]=lobits(w, KBITS);
    
    w     = z[9] + hibits(w, KBPW);
    z[9]  = lobits(w, KHIBITS);
    z[0] += KHIWRAP * hibits(w, KHIBPW);
#undef sqr
}

static
void gmul(coord_t& x,coord_t& y,coord_t& z)
{
    type128 w;
    
#define prd(ix,iy)  ((type128)x[ix])*y[iy]
    
    w = prd(0,9) + prd(1,8) + prd(2,7) + prd(3,6) + prd(4,5) + prd(5,4) + prd(6,3) + prd(7,2) + prd(8,1) + prd(9,0);
    z[9] = lobits(w, KHIBITS);
    
    w = prd(0,0) + KWRAP*(prd(1,9) + prd(2,8) + prd(3,7) + prd(4,6) + prd(5,5) + prd(6,4) + prd(7,3) + prd(8,2) + prd(9,1)) + KHIWRAP*hibits(w, KHIBPW);
    z[0] = lobits(w, KBITS);
    w = prd(0,1) + prd(1,0) + KWRAP*(prd(2,9) + prd(3,8) + prd(4,7) + prd(5,6) + prd(6,5) + prd(7,4) + prd(8,3) + prd(9,2)) + hibits(w, KBPW);
    z[1] = lobits(w, KBITS);
    w = prd(0,2) + prd(1,1) + prd(2,0) + KWRAP*(prd(3,9) + prd(4,8) + prd(5,7) + prd(6,6) + prd(7,5) + prd(8,4) + prd(9,3)) + hibits(w, KBPW);
    z[2] = lobits(w, KBITS);
    w = prd(0,3) + prd(1,2) + prd(2,1) + prd(3,0) + KWRAP*(prd(4,9) + prd(5,8) + prd(6,7) + prd(7,6) + prd(8,5) + prd(9,4)) + hibits(w, KBPW);
    z[3] = lobits(w, KBITS);
    w = prd(0,4) + prd(1,3) + prd(2,2) + prd(3,1) + prd(4,0) + KWRAP*(prd(5,9) + prd(6,8) + prd(7,7) + prd(8,6) + prd(9,5)) + hibits(w, KBPW);
    z[4] = lobits(w, KBITS);
    w = prd(0,5) + prd(1,4) + prd(2,3) + prd(3,2) + prd(4,1) + prd(5,0) + KWRAP*(prd(6,9) + prd(7,8) + prd(8,7) + prd(9,6)) + hibits(w, KBPW);
    z[5] = lobits(w, KBITS);
    w = prd(0,6) + prd(1,5) + prd(2,4) + prd(3,3) + prd(4,2) + prd(5,1) + prd(6,0) + KWRAP*(prd(7,9) + prd(8,8) + prd(9,7)) + hibits(w, KBPW);
    z[6] = lobits(w, KBITS);
    w = prd(0,7) + prd(1,6) + prd(2,5) + prd(3,4) + prd(4,3) + prd(5,2) + prd(6,1) + prd(7,0) + KWRAP*(prd(8,9) + prd(9,8)) + hibits(w, KBPW);
    z[7] = lobits(w, KBITS);
    w = prd(0,8) + prd(1,7) + prd(2,6) + prd(3,5) + prd(4,4) + prd(5,3) + prd(6,2) + prd(7,1) + prd(8,0) + KWRAP*(prd(9,9)) + hibits(w, KBPW);
    z[8] = lobits(w, KBITS);
    
    w     = z[9] + hibits(w, KBPW);
    z[9]  = lobits(w, KHIBITS);
    z[0] += KHIWRAP * hibits(w, KHIBPW);
#undef prd
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
void gnorm(coord_t &x, coord_t &y) {
    gcopy(x,y);
    scr(y);
    if(y[0] != lobits(y[0], KBPW))
        scr(y);
    if (y[9] == qbase[9] &&
        y[8] == qbase[8] &&
        y[7] == qbase[7] &&
        y[6] == qbase[6] &&
        y[5] == qbase[5] &&
        y[4] == qbase[4] &&
        y[3] == qbase[3] &&
        y[2] == qbase[2] &&
        y[1] == qbase[1] &&
        y[0] >= qbase[0]) {
        // subtract the modular base
        y[0] -= qbase[0];
        y[1] = 0;
        y[2] = 0;
        y[3] = 0;
        y[4] = 0;
        y[5] = 0;
        y[6] = 0;
        y[7] = 0;
        y[8] = 0;
        y[9] = 0;
    }
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
type64 fetch_bits(unsigned char* v, int maxlen, int start, int nbits) {
    // Fetch "nbits" bits starting at bit "start" from byte vector "v" stored
    // in little-endian form, with a maximum vector length of "maxlen" bytes.
    // Return fetched bits in a type64 value.
    //
    // Bytes in vector v are numbered consecutively from 0 toward ascending addresses.
    // Bit positions are designated as 0 for LSB and increasing toward MSB.
    // Bytes have a length of 8 bits.
    //
    // So, e.g., bit position starting at 13 corresponds to bit number 5 of byte number 1.
    
    int start_byte = start >> 3;
    int start_bit  = start & 7;
    int end_byte   = (start + nbits) >> 3;
    int end_bit    = (start + nbits) & 7;
    type64 ans  = 0;
    int bit_pos = start_bit;
    int kbits   = 0;
    for(int pos = start_byte; pos < maxlen; ++pos) {
        int nel;
        if(pos < end_byte)
            nel = 8 - bit_pos;
        else
            nel = end_bit - bit_pos;
        if(nel > 0) {
            int mask = (1 << nel) - 1;
            int bits = v[pos];
            ans |= ((type64)((bits >> bit_pos) & mask)) << kbits;
            kbits += nel;
        }
        if(pos >= end_byte)
            break;
        bit_pos = 0;
    }
    return ans;
}

static
void store_bits(unsigned char* v, int maxlen, int start, int nbits, type64 val) {
    // Store little-endian encoding of value "val" in byte-vector "v",
    // which has maximum length of "maxlen" bytes. We store "nbits" bits of "val"
    // into "v" starting at bit position "start".
    int start_byte = start >> 3;
    int start_bit  = start & 7;
    int end_byte   = (start + nbits) >> 3;
    int end_bit    = (start + nbits) & 7;
    int bit_pos = start_bit;
    int kbits   = 0;
    for(int pos = start_byte; pos < maxlen; ++pos) {
        int nel;
        if(pos < end_byte)
            nel = 8 - bit_pos;
        else
            nel = end_bit - bit_pos;
        if(nel > 0) {
            int mask = (1 << nel) - 1;
            int byte = v[pos];
            int bits = (((int)(val >> kbits)) & mask) << bit_pos;
            byte |= bits;
            v[pos] = byte;
            kbits += nel;
        }
        if(pos >= end_byte)
            break;
        bit_pos = 0;
    }
}

static
void gfetch(unsigned char* v, coord_t& w)
{
    // fetch 51-bit words from consecutively stored 64-bit words in v.
    // Assumes input value is < 2^251
    
    for(int ix = 0; ix < NGRP; ++ix)
        w[ix] = fetch_bits(v, 66, ix * KBPW, KBPW);
}

static
void gstore(coord_t& w, unsigned char* v)
{
    // store 51-bit words into consecutively stored 64-bit words in v.
    // NOTE: Lisp-provided buffers should have a length of 66 bytes.
    // This much accommodates curves up to E-521.
    memset(v, 0, 66);
    for(int ix = 0; ix < NGRP; ++ix)
        store_bits(v, 66, ix * KBPW, KBPW, w[ix]);
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
