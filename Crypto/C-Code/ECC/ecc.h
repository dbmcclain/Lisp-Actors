// ecc.h -- NIST B571 ECC over F_2^571
// DMcClain/Acudora  11/11
// ---------------------------------------------

#ifndef __ECC571_H__
#define __ECC571_H__

#include <memory.h>
#include <string.h>
#include <stdio.h>

#ifdef _WINDOWS
typedef unsigned __int32  uint;   // int is 32 bits
typedef unsigned __int8   ubyte;
typedef unsigned __int64  ulong;  // long is 64-bits
#else // OSX?
typedef unsigned int  uint;   // int is 32 bits
typedef unsigned char ubyte;
typedef unsigned long ulong;  // long is 64-bits
#endif

// ---------------------------------------------

typedef ubyte gf571_opnd_bytes[(571+7)/8];
typedef ubyte gf128_opnd_bytes[(128+7)/8];

// ---------------------------------------------

class gf128_opnd
{
 public:
  ubyte *opnd;
  
 public:
  gf128_opnd() { opnd = 0; };
  gf128_opnd(ubyte* pnum) { opnd = pnum; };

  gf128_opnd& operator=(const gf128_opnd &op)
    { return gf128_copy(op); }
  
  // gf128_opnd& operator=(const gf128_opnd &op);
  
  friend gf128_opnd& gf128_add(const gf128_opnd &op1, const gf128_opnd &op2, gf128_opnd &opdst);
  friend gf128_opnd& gf128_mul(const gf128_opnd &op1, const gf128_opnd &op2, gf128_opnd &opdst);
  friend gf128_opnd& gf128_inv(const gf128_opnd &op, gf128_opnd &opdst);
  friend gf128_opnd& gf128_div(const gf128_opnd &op1, const gf128_opnd &op2, gf128_opnd &opdst);
  friend gf128_opnd& gf128_oneplus(const gf128_opnd &op, gf128_opnd &opdst);
  friend gf128_opnd& gf128_shiftl(const gf128_opnd &op, int nsh, gf128_opnd &opdst);
  
  bool operator!() const;
  void step();
  void zero();
  void one();
  void two();
  void prim();
  
  bool operator==(const gf128_opnd &op) const;
  bool operator!=(const gf128_opnd &op) const
  { return !operator==(op); }
  
  bool is_one() const;
  
  void first_bit(int &ix, uint &mask) const;
  int  integer_length() const;
  void show();
  gf128_opnd& gf128_copy(const gf128_opnd &opnd);
  gf128_opnd& init_from_string(const char *str);
};

// ---------------------------------------------

class static_gf128_opnd : public gf128_opnd
{
 public:
  gf128_opnd_bytes bytes;

  static_gf128_opnd();
  static_gf128_opnd(const gf128_opnd &op);
  static_gf128_opnd(const ubyte* pnum, uint nb);
  static_gf128_opnd(const char *str);
};

// ---------------------------------------------
// ---------------------------------------------

class gf571_opnd
{
 public:
  ubyte *opnd;
  
 public:
  gf571_opnd() { opnd = 0; };
  gf571_opnd(ubyte* pnum) { opnd = pnum; };

  gf571_opnd& operator=(const gf571_opnd &op)
    { return gf571_copy(op); }
  
  // gf571_opnd& operator=(const gf571_opnd &op);
  
  friend gf571_opnd& gf571_add(const gf571_opnd &op1, const gf571_opnd &op2, gf571_opnd &opdst);
  friend gf571_opnd& gf571_mul(const gf571_opnd &op1, const gf571_opnd &op2, gf571_opnd &opdst);
  friend gf571_opnd& gf571_inv(const gf571_opnd &op, gf571_opnd &opdst);
  friend gf571_opnd& gf571_div(const gf571_opnd &op1, const gf571_opnd &op2, gf571_opnd &opdst);
  friend gf571_opnd& gf571_oneplus(const gf571_opnd &op, gf571_opnd &opdst);
  friend gf571_opnd& gf571_shiftl(const gf571_opnd &op, int nsh, gf571_opnd &opdst);
  
  bool operator!() const;
  void step();
  void zero();
  void one();
  void two();
  void prim();
  
  bool operator==(const gf571_opnd &op) const;

  bool operator!=(const gf571_opnd &op) const
  { return !operator==(op); }
  
  bool is_one() const;
  
  void first_bit(int &ix, uint &mask) const;
  int  integer_length() const;
  void show();
  gf571_opnd& gf571_copy(const gf571_opnd &opnd);
  gf571_opnd& init_from_string(const char *str);
};

// ---------------------------------------------

class static_gf571_opnd : public gf571_opnd
{
 public:
  gf571_opnd_bytes bytes;

  static_gf571_opnd();
  static_gf571_opnd(const gf571_opnd &op);
  static_gf571_opnd(const ubyte* pnum, uint nb);
  static_gf571_opnd(const char *str);
};

// ---------------------------------------------

class ecc571_projective_pt;

class ecc571_affine_pt {
 public:
  gf571_opnd x;
  gf571_opnd y;
  
 public:
  ecc571_affine_pt() {};
  ecc571_affine_pt(ubyte* px, ubyte* py);

  ecc571_affine_pt& operator=(const ecc571_affine_pt &pt)
    { return ecc571_copy(pt); }
  
  friend ecc571_affine_pt& ecc571_add(const ecc571_affine_pt &pt1, const ecc571_affine_pt &pt2, ecc571_affine_pt &ptdst);
  friend ecc571_affine_pt& ecc571_neg(const ecc571_affine_pt &pt, ecc571_affine_pt &ptdst);
  friend ecc571_affine_pt& ecc571_sub(const ecc571_affine_pt &p1, const ecc571_affine_pt &pt2, ecc571_affine_pt &ptdst);
  // friend ecc571_affine_pt& ecc571_mul(const ecc571_affine_pt &pt, const ulong n, ecc571_affine_pt &ptdst);
  friend ecc571_affine_pt& ecc571_mul(const ecc571_affine_pt &pt, const gf571_opnd &n, ecc571_affine_pt &ptdst,
                                      const gf571_opnd &rnd);
  friend ecc571_affine_pt& ecc571_double(const ecc571_affine_pt &pt, ecc571_affine_pt &ptdst);
  
  bool infinite_p() const;
  ecc571_affine_pt& infinite();

  ecc571_affine_pt& ecc571_copy(const ecc571_affine_pt &pt);
  
  void show();
};

// ---------------------------------------------

class static_ecc571_affine_pt : public ecc571_affine_pt
{
 public:
  gf571_opnd_bytes xbytes;
  gf571_opnd_bytes ybytes;
  
 public:
  static_ecc571_affine_pt();
  static_ecc571_affine_pt(const ecc571_affine_pt &pt);
  static_ecc571_affine_pt(const char* str[2]);
};

// --------------------------------------------------------

class ecc571_projective_pt
{
 public:
  gf571_opnd x;
  gf571_opnd y;
  gf571_opnd z;
  
 public:
  
  friend ecc571_projective_pt& ecc571_double(const ecc571_projective_pt &pt1, ecc571_projective_pt &ptdst);
  friend ecc571_projective_pt& ecc571_add(const ecc571_projective_pt &pt1, const ecc571_projective_pt &pt2, ecc571_projective_pt &ptdst);
  friend ecc571_projective_pt& ecc571_mul(const ecc571_projective_pt &pt, const gf571_opnd &n, ecc571_projective_pt &ptdst);
  
  bool infinite_p() const;
  ecc571_projective_pt &infinite();
  
  ecc571_projective_pt &operator=(const ecc571_projective_pt &pt)
    { return ecc571_copy(pt); }
  
  void show();
  ecc571_affine_pt& convert_to_affine(ecc571_affine_pt &ptdst);
  ecc571_projective_pt& ecc571_copy(const ecc571_projective_pt &pt);
};

// ---------------------------------------------

class static_ecc571_projective_pt : public ecc571_projective_pt
{
 public:
  gf571_opnd_bytes xbytes;
  gf571_opnd_bytes ybytes;
  gf571_opnd_bytes zbytes;
  
 public:
  static_ecc571_projective_pt();
  static_ecc571_projective_pt(const ecc571_affine_pt& pt, const gf571_opnd& rnd);
  static_ecc571_projective_pt(const ecc571_projective_pt &pt);
};

// ---------------------------------------------

ecc571_affine_pt& ecc571_mulGen(const gf571_opnd &n, ecc571_affine_pt &ptdst);

#endif // __ECC571_H__
