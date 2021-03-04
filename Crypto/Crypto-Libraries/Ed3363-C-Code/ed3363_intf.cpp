// ed3363_intf.cpp -- Interface for Lisp to curve Ed3363 support
//
// Shamelessly lifted from MIRACL web pages
// DM/Emotiq  07/18
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
#include "ed3363_intf.h"

#define WINDOW 4
#define PANES  84

using namespace std;

typedef __int128 type128;  /* non-standard type */
typedef int64_t type64;

static const type64 bot56bits = 0xffffffffffffff;
static const int    kCurveD   = 11111;
static const int    kBPW      = 56;

#include <stdint.h>

   __inline__ uint64_t rdtsc() {
   uint32_t lo, hi;
   __asm__ __volatile__ (      // serialize
     "xorl %%eax,%%eax \n        cpuid"
     ::: "%rax", "%rbx", "%rcx", "%rdx");
   
   __asm__ __volatile__ ("rdtsc" : "=a" (lo), "=d" (hi));
   return (uint64_t)hi << 32 | lo;
   }

// w=x+y
static
void gadd(type64 *x,type64 *y,type64 *w)
{
	w[0]=x[0]+y[0];
	w[1]=x[1]+y[1];
	w[2]=x[2]+y[2];
	w[3]=x[3]+y[3];
	w[4]=x[4]+y[4];
	w[5]=x[5]+y[5];
}

// w=x-y
static
void gsub(type64 *x,type64 *y,type64 *w)
{
	w[0]=x[0]-y[0];
	w[1]=x[1]-y[1];
	w[2]=x[2]-y[2];
	w[3]=x[3]-y[3];
	w[4]=x[4]-y[4];
	w[5]=x[5]-y[5];
}

// w-=x
static
void gdec(type64 *x,type64 *w)
{
	w[0]-=x[0];
	w[1]-=x[1];
	w[2]-=x[2];
	w[3]-=x[3];
	w[4]-=x[4];
	w[5]-=x[5];
}

// w=x
static
void gcopy(type64 *x,type64 *w)
{
	w[0]=x[0];
	w[1]=x[1];
	w[2]=x[2];
	w[3]=x[3];
	w[4]=x[4];
	w[5]=x[5];
}

// w*=2
static
void gmul2(type64 *w)
{
	w[0]*=2;
	w[1]*=2;
	w[2]*=2;
	w[3]*=2;
	w[4]*=2;
	w[5]*=2;
}

// w-=2*x
static
void gsb2(type64 *x,type64 *w)
{
	w[0]-=2*x[0];
	w[1]-=2*x[1];
	w[2]-=2*x[2];
	w[3]-=2*x[3];
	w[4]-=2*x[4];
	w[5]-=2*x[5];
}

// reduce w - Short Coefficient Reduction
static
void scr(type64 *w)
{
	type64 t0,t1,t2;
	t0=w[0]&bot56bits;

	t1=w[1]+(w[0]>>kBPW);
	w[1]=t1&bot56bits;

	t2=w[2]+(t1>>kBPW);
	w[2]=t2&bot56bits;

	t1=w[3]+(t2>>kBPW);
	w[3]=t1&bot56bits;

	t2=w[4]+(t1>>kBPW);
	w[4]=t2&bot56bits;

	t1=w[5]+(t2>>kBPW);
	w[5]=t1&bot56bits;

	w[0]=t0+3*(t1>>kBPW);

}

static
void gneg(type64 *x, type64 *y) {
  y[0] = -x[0];
  y[1] = -x[1];
  y[2] = -x[2];
  y[3] = -x[3];
  y[4] = -x[4];
  y[5] = -x[5];
}
   
static
void gnorm(type64 *x, type64 *y) {
  type64 tmp[6];
  gneg(x, tmp);
  scr(tmp);
  gneg(tmp, y);
  scr(y);
  scr(y);
}

static
bool geq(type64 *x, type64 *y) {
  // compare x, y for equality
  type64 z1[6], z2[6];
  gsub(x, y, z1);
  gnorm(z1, z2);
  return (z2[0] == 0
    && z2[1] == 0
    && z2[2] == 0
    && z2[3] == 0
    && z2[4] == 0
    && z2[5] == 0);
}


// multiply w by a constant, w*=i

static
void gmuli(type64 *w,int i)
{
	type128 t;

	t=(type128)w[0]*i;
	w[0]=((type64)t)&bot56bits;

	t=(type128)w[1]*i+(t>>kBPW);
	w[1]=((type64)t)&bot56bits;

	t=(type128)w[2]*i+(t>>kBPW);
	w[2]=((type64)t)&bot56bits;

	t=(type128)w[3]*i+(t>>kBPW);
	w[3]=((type64)t)&bot56bits;

	t=(type128)w[4]*i+(t>>kBPW);
	w[4]=((type64)t)&bot56bits;

	t=(type128)w[5]*i+(t>>kBPW);
	w[5]=((type64)t)&bot56bits;

	w[0]+=3*(t>>kBPW);

}

// z=x^2

static
void gsqr(type64 *x,type64 *z)
{
	type128 t0,t1;

	t0=2*((type128)x[0]*x[5]+(type128)x[1]*x[4]+(type128)x[2]*x[3]);
	z[5]=((type64) t0)&bot56bits;

	t1=(type128)x[0]*x[0]+3*(2*((type128)x[1]*x[5]+(type128)x[2]*x[4])+(type128)x[3]*x[3]+(t0>>kBPW));
	z[0]=((type64) t1)&bot56bits;

	t0=2*((type128)x[0]*x[1]+3*((type128)x[2]*x[5]+(type128)x[3]*x[4]))+(t1>>kBPW);
	z[1]=((type64) t0)&bot56bits;

	t1=(type128)x[0]*(2*x[2])+(type128)x[1]*x[1]+3*((type128)x[3]*(2*x[5])+(type128)x[4]*x[4])+(t0>>kBPW);
	z[2]=((type64) t1)&bot56bits;

	t0=2*((type128)x[0]*x[3]+(type128)x[1]*x[2]+(type128)x[4]*(3*x[5]))+(t1>>kBPW);
	z[3]=((type64) t0)&bot56bits;

	t1=2*((type128)x[0]*x[4]+(type128)x[1]*x[3])+(type128)x[2]*x[2]+(type128)x[5]*(3*x[5])+(t0>>kBPW);
	z[4]=((type64) t1)&bot56bits;

	t0=z[5]+(t1>>kBPW);
	z[5]=((type64) t0)&bot56bits;
	z[0]+=3*(t0>>kBPW);

}


// z=x*y - Granger's method

static
void gmul(type64 *x,type64 *y,type64 *z)
{
	type128 t0,t1,t2;
	type128 a0,a1,a2,a3,a4,a5;
	type128 b0,b1,b2,b3,b4;
	a0=(type128)x[0]*y[0];
	a1=(type128)x[1]*y[1];
	a2=(type128)x[2]*y[2];
	a3=(type128)x[3]*y[3];
	a4=(type128)x[4]*y[4];
	a5=(type128)x[5]*y[5];

	b4=a5+a4;
	b3=b4+a3;
	b2=b3+a2;
	b1=b2+a1;
	b0=b1+a0;

	t0=b0-(type128)(x[0]-x[5])*(y[0]-y[5])-(type128)(x[1]-x[4])*(y[1]-y[4])-(type128)(x[2]-x[3])*(y[2]-y[3]);
	z[5]=((type64) t0)&bot56bits;

	t1=a0+3*(b1-(type128)(x[1]-x[5])*(y[1]-y[5])-(type128)(x[2]-x[4])*(y[2]-y[4])+(t0>>kBPW));
	z[0]=((type64) t1)&bot56bits;

	a0=a0+a1;
	t0=a0-(type128)(x[0]-x[1])*(y[0]-y[1])+3*(b2-(type128)(x[2]-x[5])*(y[2]-y[5])-(type128)(x[3]-x[4])*(y[3]-y[4]))+(t1>>kBPW);
	z[1]=((type64) t0)&bot56bits;
	a0=a0+a2;

	t1=a0-(type128)(x[0]-x[2])*(y[0]-y[2])+3*(b3-(type128)(x[3]-x[5])*(y[3]-y[5]))+(t0>>kBPW);
	z[2]=((type64) t1)&bot56bits;
	
	a0=a0+a3;
	t0=a0-(type128)(x[0]-x[3])*(y[0]-y[3])-(type128)(x[1]-x[2])*(y[1]-y[2])+3*(b4-(type128)(x[4]-x[5])*(y[4]-y[5]))+(t1>>kBPW);
	z[3]=((type64) t0)&bot56bits;

	a0=a0+a4;
	t1=a0-(type128)(x[0]-x[4])*(y[0]-y[4])-(type128)(x[1]-x[3])*(y[1]-y[3])+3*a5+(t0>>kBPW);
	z[4]=((type64) t1)&bot56bits;

	t0=z[5]+(t1>>kBPW);
	z[5]=t0&bot56bits;
	z[0]+=3*(t0>>kBPW);

}

// Inverse x = 1/x = x^(p-2) mod p
//  = "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB"
// (83 F's)

static
void ginv(type64 *x)
{
	int i;
	type64 xc[6],x11[6],w[6],t1[6],t2[6];
	gsqr(x,w);       // =x^2
	gmul(w,x,xc);     // xc=x^3
	gsqr(w,t1);
	gsqr(t1,x11);      
	gmul(x11,x,t2);   // xc=x^9    
	gmul(t2,w,x11);   // x11=x^11
	
	gsqr(x11,w);
	gmul(w,t2,t1);     // t1=x^31

	gcopy(t1,w);
	gsqr(w,t2);
	gsqr(t2,w);
	gsqr(w,t2);
	gsqr(t2,w);
	gsqr(w,t2);
	gmul(t2,t1,x);   // =x^(2^10-1)
	gcopy(x,t2);

	gsqr(t2,w);
	gsqr(w,t2);
	gmul(t2,xc,w);  
	gcopy(w,xc);    // xc=x^(2^12-1)
	
	gsqr(t2,w);
	gsqr(w,t2);
	gsqr(t2,w);
	gsqr(w,t2);
	gsqr(t2,w);
	gsqr(w,t2);
	gsqr(t2,w);
	gsqr(w,t2);

	gmul(t2,x,t1);    // t1=x^(2^20-1)

	gcopy(t1,x);
	for (i=0;i<10;i++)
	{
		gsqr(t1,w);
		gsqr(w,t1);
	}
	gmul(t1,x,t2);   //t2=x^(2^40-1)

	gcopy(t2,x);
	for (i=0;i<20;i++)
	{
		gsqr(t2,w);
		gsqr(w,t2);
	}
	gmul(t2,x,t1);   //t1=x^(2^80-1)

	gcopy(t1,x);
	for (i=0;i<40;i++)
	{
		gsqr(t1,w);
		gsqr(w,t1);
	}
	gmul(t1,x,t2);   //t2=x^(2^160-1)

	gcopy(t2,x);
	for (i=0;i<80;i++)
	{
		gsqr(t2,w);
		gsqr(w,t2);
	}

	gmul(t2,x,t1);   //t1=x^(2^320-1)
	for (i=0;i<6;i++)
	{
		gsqr(t1,w);
		gsqr(w,t1);
	}

	gmul(t1,xc,t2); // t1=x^(2^332-1)
	for (i=0;i<2;i++)
	{
		gsqr(t2,w);
		gsqr(w,t2);
	}
	
	gmul(t2,x11,x); // x=x^(2^336-5)

}

// Point Structure

typedef struct {
type64 x[6];
type64 y[6];
type64 z[6];
type64 t[6];
} ECp;

// P+=P

static
void double_1(ECp *P)
{
	type64 a[6],b[6],e[6],f[6],g[6],h[6];
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
	type64 a[6],b[6],c[6],e[6],f[6],g[6],h[6];
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
	type64 a[6],b[6],c[6],e[6],f[6],g[6],h[6];
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
	type64 a[6],b[6],c[6],d[6],e[6],f[6],g[6],h[6];
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
	type64 a[6],b[6],c[6],d[6],e[6],f[6],g[6],h[6];
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
	for (int i=0;i<=5;i++)
		P->x[i]=P->y[i]=P->z[i]=P->t[i]=0;
	P->y[0]=P->z[0]=1;
}

// Initialise P

static
void init(type64 *x,type64 *y,ECp *P)
{
	for (int i=0;i<=5;i++)
	{
		P->x[i]=x[i];
		P->y[i]=y[i];
		P->z[i]=0;
	}
	P->z[0]=1;
	gmul(x,y,P->t);
}

//P=Q

static
void copy(ECp *Q,ECp *P)
{
	for (int i=0;i<=5;i++)
	{
		P->x[i]=Q->x[i];
		P->y[i]=Q->y[i];
		P->z[i]=Q->z[i];
		P->t[i]=Q->t[i];
	}
}

// P=-Q

static
void neg(ECp *Q,ECp *P)
{
	for (int i=0;i<=5;i++)
	{
		P->x[i]=-Q->x[i]; 
		P->y[i]=Q->y[i];
		P->z[i]=Q->z[i];
		P->t[i]=-Q->t[i]; 
	}
}

/* Make Affine */

static
void norm(ECp *P)
{
	type64 w[6],t[6];
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

	gmuli(W[2].t,kCurveD);
	gmuli(W[3].t,kCurveD);
	gmuli(W[4].t,kCurveD);
	gmuli(W[5].t,kCurveD);
	gmuli(W[6].t,kCurveD);
	gmuli(W[7].t,kCurveD);
	gmuli(W[8].t,kCurveD);
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
  f[0]^=(f[0]^g[0])&b;
  f[1]^=(f[1]^g[1])&b;
  f[2]^=(f[2]^g[2])&b;
  f[3]^=(f[3]^g[3])&b;
  f[4]^=(f[4]^g[4])&b;
  f[5]^=(f[5]^g[5])&b;
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

  cmov(T,&W[0],equal(babs,0));  // conditional move
  cmov(T,&W[1],equal(babs,1));
  cmov(T,&W[2],equal(babs,2));
  cmov(T,&W[3],equal(babs,3));
  cmov(T,&W[4],equal(babs,4));
  cmov(T,&W[5],equal(babs,5));
  cmov(T,&W[6],equal(babs,6));
  cmov(T,&W[7],equal(babs,7));
  cmov(T,&W[8],equal(babs,8)); 

  neg(T,&MT);  // minus t
  cmov(T,&MT,m&1);
}

/* Point Multiplication - exponent is 333 bits */

static
void mul(int *w,ECp *P)
{
	ECp W[1+(1<<(WINDOW-1))],S[2],Q;
	int j,m;
	precomp(P,W);

	copy(&W[w[PANES-1]],P);  
	for (int i=PANES-2;i>=0;i--)
	{
		select(&Q,W,w[i]);
		window(&Q,P);
	}
	norm(P); 
}

#ifdef testing
void output(ECp *P)
{
	cout << "x[0]= " << hex << P->x[0] << endl;
	cout << "x[1]= " << hex << P->x[1] << endl;
	cout << "x[2]= " << hex << P->x[2] << endl;
	cout << "x[3]= " << hex << P->x[3] << endl;
	cout << "x[4]= " << hex << P->x[4] << endl;
	cout << "x[5]= " << hex << P->x[5] << endl;
	cout << endl;

	cout << "y[0]= " << hex << P->y[0] << endl;
	cout << "y[1]= " << hex << P->y[1] << endl;
	cout << "y[2]= " << hex << P->y[2] << endl;
	cout << "y[3]= " << hex << P->y[3] << endl;
	cout << "y[4]= " << hex << P->y[4] << endl;
	cout << "y[5]= " << hex << P->y[5] << endl;
	cout << endl;
}

#define TEST  /* define to multiply by group order */

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
#if WINDOW==4

w[0]= 5; w[1]= -5; w[2]= 2; w[3]= -5; w[4]= -8;
w[5]= -3; w[6]= 5; w[7]= -2; w[8]= -4; w[9]= 3; 
w[10]= -5; w[11]= -4; w[12]= -6; w[13]= -4; w[14]= -7;
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
w[10]= 6; w[11]= -7; w[12]= 0; w[13]= 3; w[14]= -5;
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

/* ======================================================================== */
/* -------------------------------------------------------------------------
The MIT License

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
---------------------------------------------------------------------------- */
// -------------------------------------------------------------
// Lisp Interface

/* Point Multiplication - exponent is 333 bits */

static
void mul_to_proj(int *w,ECp *P)
{
	ECp W[1+(1<<(WINDOW-1))],S[2],Q;
	int j,m;
	precomp(P,W);

	copy(&W[w[PANES-1]],P);  
	for (int i=PANES-2;i>=0;i--)
	{
		select(&Q,W,w[i]);
		window(&Q,P);
	}
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
void gfetch(unsigned char* v, type64 *w)
{
  // fetch 56-bit words from consecutively stored 64-bit words in v.
  // Assumes input value is < 2^336
  
  type64 *pv;
  uint64_t t1, t2;
  
  pv = (type64*)v;
  t1 = pv[0];
  w[0] = t1 & bot56bits;
  t2 = pv[1];
  w[1] = ((t2 <<  8) | (t1 >> 56)) & bot56bits;
  t1 = pv[2];
  w[2] = ((t1 << 16) | (t2 >> 48)) & bot56bits;
  t2 = pv[3];
  w[3] = ((t2 << 24) | (t1 >> 40)) & bot56bits;
  t1 = pv[4];
  w[4] = ((t1 << 32) | (t2 >> 32)) & bot56bits;
  t2 = pv[5];
  w[5] = ((t2 << 40) | (t1 >> 24)) & bot56bits;
}

static
void gstore(type64 *w, unsigned char* v)
{
  // store 56-bit words into consecutively stored 64-bit words in v.
  type64 *pv;
  uint64_t t1,t2;
  type64 wn[6];

  gnorm(w, wn);
  t1 = w[0];
  t2 = w[1];
  pv = (type64*)v;
  pv[0] = t1 | (t2 << 56);
  t1 = w[2];
  pv[1] = (t2 >>  8) | (t1 << 48);
  t2 = w[3];
  pv[2] = (t1 >> 16) | (t2 << 40);
  t1 = w[4];
  pv[3] = (t2 >> 24) | (t1 << 32);
  t2 = w[5];
  pv[4] = (t1 >> 32) | (t2 << 24);
  pv[5] = (t2 >> 40);
}

extern "C"
void Ed3363_affine_mul(unsigned char* ptx,
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
  type64 px[6], py[6];

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
void normz(type64 *px, type64 *py, type64 *pz)
{
  type64 t[6];
  
  ginv(pz);
  gmul(px, pz, t); scr(t); gcopy(t, px);
  gmul(py, pz, t); scr(t); gcopy(t, py);
}

extern "C"
void Ed3363_projective_mul(unsigned char* ptx,
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
  type64 px[6], py[6], pz[6];

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
void init_proj(type64 *x,type64 *y,type64 *z, ECp *P)
{
  for (int i=0;i<=5;i++)
    {
      P->x[i]=x[i];
      P->y[i]=y[i];
      P->z[i]=z[i];
      P->t[i]=0;
    }
}

static
void add_proj(ECp *Q, ECp *P)
{
  // Add Q to P, both in projective (X,Y,Z) coordinates. We don't use T here.
  //
  // careful here... the 3-address code cannot share output with input
  type64 A[6], B[6], C[6], D[6], E[6], F[6], G[6], X3[6], Y3[6], Z3[6];
  
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
void Ed3363_projective_add(unsigned char* lp1x,
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
  type64 px[6], py[6], pz[6], qx[6], qy[6], qz[6];

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
void Ed3363_to_affine(unsigned char* lpx,
		      unsigned char* lpy,
		      unsigned char* lpz)
{
  // Convert incoming projective coordinates to affine form. Return
  // result in-place.
  //
  // lpx, lpy, and lpz are stored as consecutive 64-bit values
  // in little-endian order (assumes Intel conventions)
  //
  
  type64 px[6], py[6], pz[6], t[6];

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

// --- end of ed3363_intf.cpp --- //
