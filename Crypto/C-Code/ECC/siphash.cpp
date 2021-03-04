// ecc.cpp -- NIST B571 ECC over F_2^571
// DMcClain/Acudora  11/11
// ---------------------------------------------

#include "ecc.h"

typedef ulong UInt64;

typedef struct {
  UInt64 k0,k1;
  UInt64 v0, v1, v2, v3;
  uint   c, d;
  // c = # compression rounds
  // d = # finalization rounds

  void sipRound();
  UInt64 sipHash(UInt64 *wds, int nwds);

  void compression(UInt64 wd);
  UInt64 finalization();
  
} _T_siphash;

inline UInt64 rotl(UInt64 x, int n)
{
  return ((x >> (64 - n)) | (x << n));
}

void _T_siphash::sipRound()
{
  v0 += v1;
  v2 += v3;
  v1 = rotl(v1, 13);
  v3 = rotl(v3, 16);
  v1 ^= v0;
  v3 ^= v2;
  v0 = rotl(v0, 32);
  v2 += v1;
  v0 += v3;
  v1 = rotl(v1, 17);
  v3 = rotl(v3, 21);
  v1 ^= v2;
  v3 ^= v0;
  v2 = rotl(v2, 32);
}

void _T_siphash::compression(UInt64 w)
{
  v3 ^= w;
  for(int ix = c; --ix >= 0;)
    sipRound();
  v0 ^= w;
}

UInt64 _T_siphash::finalization()
{
  v2 ^= 0x00ff;
  for(int ix = d; --ix >= 0;)
    sipRound();
  return (v0 ^ v1 ^ v2 ^  v3);
  }

UInt64 _T_siphash::sipHash(UInt64 *wds, int nwds)
{
    v0 = k0 ^ 0x736f6d6570736575;
    v1 = k1 ^ 0x646f72616e646f6d;
    v2 = k0 ^ 0x6c7967656e657261;
    v3 = k1 ^ 0x7465646279746573;

    for(int ix = 0; ix < nwds; ++ix)
      compression(wds[ix]);
    return finalization();
}

static _T_siphash gSipHash;

// ------------------------------------------------------------
// Library Entry Points
// ------------------------------------------------------------
extern "C" {

  void c_siphash_init(int c, int d,
		      UInt64 v0, UInt64 v1, UInt64 v2, UInt64 v3)
  {
    gSipHash.c = c;
    gSipHash.d = d;
    
    gSipHash.v0 = v0;
    gSipHash.v1 = v1;
    gSipHash.v2 = v2;
    gSipHash.v3 = v3;
  }

  void c_siphash_compression(UInt64 w)
  {
    gSipHash.compression(w);
  }

  UInt64 c_siphash_finalization()
  {
    return gSipHash.finalization();
  }

  UInt64 c_siphash(_T_siphash &state, UInt64 *wds, int nwds)
  {
    return state.sipHash(wds, nwds);
  }
    
};
