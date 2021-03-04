//-------------------------------------------------------------------
// CrescendoVST.cpp (VST 2.3)
// Stereo plugin which applies Left/Right Crescendo Hearing Corrections
// (C) 2005, AurisLabs, Inc. All rights reserved.
// DM/AurisLabs  01/05
//-------------------------------------------------------------------

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "CrescendoVST.hpp"
#include "CrescendoVSTEditor.h"
#include "ecc.h"

#define __TRACING__  0
#include "tracer.cpp"
//-------------------------------------------------------------------

typedef unsigned long  uint32;
typedef unsigned short uint16;
typedef unsigned char  uchar;
typedef uchar uint8;

extern void* hInstance;
extern void Acudora_sha2(uchar* text, 
                  uint32 len, 
                  uchar* digest, 
                  uint32 count);

extern int Acudora_sha2_file( const char *path, uchar output[32]);

extern int Acudora_decrypt_file(const char* filename,
								char* buf, 
								long  len, 
								const unsigned char* key,
								int keylen);

extern void f_get_fmem(void** pbuf, long* plen);

// -----------------------------------------------------

#define N_DIRS        2
#define PATH_LEN	256

char dirs[N_DIRS][PATH_LEN];

#define LOCAL_DIR    0
#define WINDOWS_DIR  1

#define STRINGS_FNAME  0 // scoffs
#define COFFS_FNAME    1 // coffs
#define FCOFFS_FNAME   2 // fcoffs
#define IMG_FNAME      3 // receptor.jpg
#define TXT_FNAME	   4 // VTuningLicense.txt
#define N_FNAMES       5

#define ENC_STRLEN     80
#define ENC_INDEX_LEN  16
#define ENC_TXTLEN     (ENC_STRLEN - ENC_INDEX_LEN)

uchar fnames[N_FNAMES][ENC_STRLEN] =
	{{ 0x11, 0xE1, 0x15, 0x27, 0xDC, 0x32, 0x09, 0x00, // initially "scoffs" against G
	   0x94, 0x7F, 0x00, 0x23, 0x14, 0x62, 0x7B, 0xDC, 
	   0x07, 0xB1, 0xCB, 0x5A, 0x28, 0xC7, 0x8D, 0xD3, 
	   0x53, 0xC4, 0x59, 0x38, 0x5A, 0x4C, 0x51, 0x62, 
	   0xCD, 0xB7, 0x2E, 0x0C, 0xBA, 0x2F, 0x06, 0xAB, 
	   0x78, 0x65, 0x53, 0x9A, 0x66, 0xB5, 0xEA, 0x33, 
	   0x04, 0x9B, 0x82, 0xA2, 0xA2, 0x97, 0xAE, 0x4F, 
	   0xF4, 0x33, 0x0B, 0xFB, 0x32, 0x9A, 0x45, 0xA8, 
	   0x2A, 0x31, 0x2E, 0xDB, 0x29, 0x46, 0x69, 0x33, 
	   0x9F, 0xB5, 0xAA, 0x45, 0x96, 0x63, 0x05, 0x2A }};

#define DLL_HASH       0
#define IMG_HASH       1
#define FCOFF_HASH     2
#define STRINGS_HASH   3
#define TXT_HASH       4
#define N_HASHES       5
#define HASH_LEN       32

uchar hashes[N_HASHES][HASH_LEN];

ecc_affine_pt strings_public_key;

const uint ECC_LEN = sizeof(strings_public_key.x.opnd);

// -----------------------------------------------------

int get_local_dirname(char *buf, int len)
{
	int ret = -1;
	if(0 < GetModuleFileName((HMODULE)hInstance, buf, len))
	{
		char* cp = strrchr(buf, '\\');
		if(!cp)
			cp = strrchr(buf, '/');
		if(cp)
		{
			cp[0] = 0;
			ret = 0;
		}
	}
	return ret;
}

int get_windows_dirname(char *path, int len)
{
	int ret = GetWindowsDirectory(path, len);
	return (ret <= 0);
}

int get_dirs()
{
	return (get_local_dirname(dirs[LOCAL_DIR], sizeof(dirs[LOCAL_DIR])) ||
		    get_windows_dirname(dirs[WINDOWS_DIR], sizeof(dirs[WINDOWS_DIR])));
}

// --------------------------------------------------------------------

int get_dll_key(unsigned char key[HASH_LEN])
{
	char path[PATH_LEN];
	
	return ((0 >= GetModuleFileName((HMODULE)hInstance, path, sizeof(path))) ||
		    Acudora_sha2_file(path, key));
}

void make_path_raw(int dir, char* fname, char *buf)
{
	strcpy(buf, dirs[dir]);
	strcat(buf, "\\");
	strcat(buf, fname);
}

void make_pathname(int dirix, int fnameix, char* buf)
{
	gf_opnd key((ubyte*)fnames[fnameix],ENC_INDEX_LEN);
	ecc_affine_pt prod;

	ecc_mul(strings_public_key, key, prod);
	key.zero();

	gf_opnd ans((ubyte*)fnames[fnameix], ENC_STRLEN);
	gf_add(ans, prod.x, ans);
	prod.infinite();
	
	make_path_raw(dirix, (char*)&ans.opnd[ECC_LEN - ENC_TXTLEN], buf);
	ans.zero();
}

int get_filenames()
{
	int ret = -1;

	if(0 == get_dll_key(hashes[DLL_HASH]))
	{
		char path[PATH_LEN];

		gf_opnd kone;
		kone.one();
		ecc_mulGen(kone, strings_public_key);

		make_pathname(LOCAL_DIR, 0, path);

		char* buf;
		long  len;

		f_get_fmem((void**)&buf, &len);

		gf_opnd key(hashes[DLL_HASH], HASH_LEN);
		ecc_mulGen(key, strings_public_key);

		ret = Acudora_decrypt_file(path, buf, len, strings_public_key.x.opnd, ECC_LEN);
		if(0 == ret)
		{
			memcpy(strings_public_key.x.opnd, buf, ECC_LEN);
			memcpy(strings_public_key.y.opnd, buf+ECC_LEN, ECC_LEN);
			memcpy(fnames, buf+2*ECC_LEN, N_FNAMES*ENC_STRLEN);
			memset(buf,0,len);
		}
	}
	return ret;
}

// --------------------------------------------------------------------

int get_image_key()
{
	char path[PATH_LEN];

	make_pathname(WINDOWS_DIR, IMG_FNAME, path);
	int ret = Acudora_sha2_file(path, hashes[IMG_HASH]);
	memset(path, 0, sizeof(path));
	return ret;
}

int get_fcoffs_key()
{
	char path[PATH_LEN];
	make_pathname(LOCAL_DIR, FCOFFS_FNAME, path);
	int ret = Acudora_sha2_file(path, hashes[FCOFF_HASH]);
	memset(path, 0, sizeof(path));
	return ret;
}

int get_strings_key()
{
	char path[PATH_LEN];
	make_pathname(LOCAL_DIR, STRINGS_FNAME, path);
	int ret = Acudora_sha2_file(path, hashes[STRINGS_HASH]);
	memset(path, 0, sizeof(path));
	return ret;
}

int get_text_key()
{
	char path[PATH_LEN];
	make_pathname(LOCAL_DIR, TXT_FNAME, path);
	int ret = Acudora_sha2_file(path, hashes[TXT_HASH]);
	memset(path, 0, sizeof(path));
	return ret;
}

int get_product_key(ecc_affine_pt &pt)
{
	int ret = -1;
	char path[PATH_LEN];

	make_pathname(LOCAL_DIR, COFFS_FNAME, path);
	FILE *fp = fopen(path, "rb");
	memset(path, 0, sizeof(path));
	if(fp)
	{
		if(ECC_LEN == fread(pt.x.opnd, 1, ECC_LEN, fp) &&
		   ECC_LEN == fread(pt.y.opnd, 1, ECC_LEN, fp))
		   ret = 0;
		fclose(fp);
	}
	return ret;
}

int load_forth_code()
{
	int ret = -1;
	char path[PATH_LEN];
	ecc_affine_pt product_key;
	gf_opnd key;
	char* buf = 0;
	long  len;

	// DebugBreak();
	if(get_dirs())
		goto exit;

	if(get_filenames())
		goto exit;
	
	if(get_image_key())
		goto exit;

	if(get_fcoffs_key())
		goto exit;

	if(get_strings_key())
		goto exit;

	if(get_text_key())
		goto exit;

	if(get_product_key(product_key))
		goto exit;

	make_pathname(LOCAL_DIR, FCOFFS_FNAME, path);

	Acudora_sha2((uchar*)&hashes[0][0], sizeof(hashes), &key.opnd[ECC_LEN - HASH_LEN], 1);
	ecc_mulGen(key, strings_public_key);
	key.zero();
	ecc_add(product_key, strings_public_key, product_key);
	strings_public_key.infinite();

	f_get_fmem((void**)&buf, &len);
	ret = Acudora_decrypt_file(path, buf, len, product_key.x.opnd, ECC_LEN);

exit:
	product_key.infinite();
	strings_public_key.infinite();
	key.zero();
	memset(hashes, 0, sizeof(hashes));
	memset(dirs, 0, sizeof(dirs));
	memset(fnames, 0, sizeof(fnames));
	memset(path, 0, sizeof(path));
	if(buf && ret)
		memset(buf, 0, len);
	return ret;
}

// --------------------------------------------
#if 0
#if 1
void gf256zero(uint8 x[32])
{
	memset(x, 0, 32);
}

void gf256xor(uint8 a[32], uint8 b[32], uint8 c[32])
{
	uint32 *pa = (uint32*)a;
	uint32 *pb = (uint32*)b;
	uint32 *pc = (uint32*)c;
	pc[0] = pa[0] ^ pb[0];
	pc[1] = pa[1] ^ pb[1];
	pc[2] = pa[2] ^ pb[2];
	pc[3] = pa[3] ^ pb[3];
	pc[4] = pa[4] ^ pb[4];
	pc[5] = pa[5] ^ pb[5];
	pc[6] = pa[6] ^ pb[6];
	pc[7] = pa[7] ^ pb[7];
}

void step_lfsr(uint8 x[32])
{
	static uint8 prim[32] = { 0xA5, 0xF8, 0x4D, 0x47,  0xA4, 0xE4, 0x66, 0x04, 
		                      0xC3, 0x24, 0x76, 0xB0,  0x8E, 0xE6, 0xAA, 0x37, 
							  0x73, 0x56, 0x78, 0xFB,  0x5F, 0xDC, 0x4D, 0x90, 
							  0x30, 0xE7, 0xF3, 0xF0,  0x5D, 0xB4, 0xBB, 0x8C };
	int is_odd = (1 & x[31]);
	for(int ix = 32; --ix > 0;)
		x[ix] = ((x[ix] >> 1) | ((x[ix-1] & 1) << 7));
	x[0] >>= 1;
	if(is_odd)
		gf256xor(x,prim,x);
}

void gf256mul(uint8 a[32], uint8 b[32], uint8 c[32])
{
	gf256zero(c);
	for(int ix = 0; ix < 32; ++ix)
	{
		uint8 byte = a[ix];

		for(uint8 mask = 0x80; mask; mask >>= 1)
		{
			if(mask & byte)
				gf256xor(c, b, c);
			step_lfsr(b);
		}
	}
}

void check_key(uint8 k[32])
{
	uint32 *p = (uint32*)k;
	for(;;)
	{
		if(p[0] || p[1] || p[2] || p[3] ||
		   p[4] || p[5] || p[6] || p[7])
		   return;
		uchar dig[32];
		Acudora_sha2((uchar*)k, 32, dig, 1);
		memcpy((uchar*)k, dig, 32);
	}
}

void gf256_keygen(uint8 a[32], uint8 b[32], uint8 c[32])
{
  gf256zero(b);
  gf256zero(c);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,b);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
  gf256xor(a,c,c);  step_lfsr(a);
}

void crypto_mul(uint8 a[32], uint8 b[32], uint8 c[32])
{
	gf256mul(a,b,c);
	gf256zero(a);
	gf256zero(b);
}

void crypto_keygen(uint8 a[32], uint8 b[32], uint8 c[32])
{
	gf256_keygen(a,b,c);
	gf256zero(a);
	gf256zero(b);
}

int load_forth_code()
{
	int ret = -1;
	char path[256];

	// DebugBreak();
	if(0 == get_local_pathname(path, sizeof(path), "fcoffs"))
	{
		FILE *fp = fopen(path, "rb");
		if(fp)
		{
			extern void f_get_fmem(void** pbuf, long* plen);
			void* buf;
			long  len;

			f_get_fmem(&buf, &len);
			fread(buf, 1, len, fp);
			fclose(fp);

			uchar k1[32];
			uchar k2[32];
			uchar k3[32];
			uchar ktmp[32];

			if(get_dll_key(k1) ||
		       get_image_key(k2) ||
			   get_product_key(k3))
			  return ret;

			check_key(k1);
			check_key(k2);
			check_key(k3);

			crypto_mul(k1, k2, ktmp);
			crypto_mul(ktmp, k3, k1);
			crypto_keygen(k1, k2, ktmp);

			ret = Acudora_decrypt_buffer((char*)buf, len, (char*)ktmp);
			gf256zero(ktmp);
		}
	}
	return ret;
}

#else
#if 1
inline void step_lfsr(uint32 &x)
{
  x = ((x >> 1) ^ (-(x & 1) & 0xa6a6a6a6));
}

uint32 gf32mul(uint32 a, uint32 b)
{
	uint32 ans = 0;
	uint32 mask = 0x80000000;
	while(mask)
	{
		if(mask & a)
			ans ^= b;
		step_lfsr(b);
		mask >>= 1;
	}
	return ans;
}

void check_key(uint32 k[8])
{
	for(;;)
	{
		uint32 zf = 0;

		for(int ix = 8; --ix >= 0;)
		{
			if(0 == k[ix])
			{
				zf = 1;
				break;
			}
		}
		if(0 == zf)
			return;

		uchar dig[32];
		Acudora_sha2((uchar*)k, 32, dig, 1);
		memcpy((uchar*)k, dig, 32);
	}
}

void mul_keys(uchar k1[32], uchar k2[32])
{
	uint32 *p1 = (uint32*)k1;
	uint32 *p2 = (uint32*)k2;
	for(int ix = 8; --ix >= 0;)
		p1[ix] = gf32mul(p1[ix],p2[ix]);
}

uint32 gf32mul_DACCFD6C(uint32 a)
{
  uint32 ans = 0;

  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0);
  return ans;
}

uint32 gf32mul_4AE2B644(uint32 a)
{
  uint32 ans = 0;

  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0);
  return ans;
}

uint32 gf32mul_C7B7B38A(uint32 a)
{
  uint32 ans = 0;

  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0);
  return ans;
}

uint32 gf32mul_5AD4BCA8(uint32 a)
{
  uint32 ans = 0;

  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0);
  return ans;
}

uint32 gf32mul_85DDE53D(uint32 a)
{
  uint32 ans = 0;

  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a;
  return ans;
}

uint32 gf32mul_2051ADE7(uint32 a)
{
  uint32 ans = 0;

  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a;
  return ans;
}

uint32 gf32mul_CD9AC2A4(uint32 a)
{
  uint32 ans = 0;

  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0);
  return ans;
}

uint32 gf32mul_DE424CB9(uint32 a)
{
  uint32 ans = 0;

  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a;
  return ans;
}


void gf_keygen(uint32 *keys)
{
  keys[0] = gf32mul_DACCFD6C(keys[0]);
  keys[1] = gf32mul_4AE2B644(keys[1]);
  keys[1] = gf32mul(keys[1],keys[0]);
  keys[2] = gf32mul_C7B7B38A(keys[2]);
  keys[2] = gf32mul(keys[2],keys[1]);
  keys[3] = gf32mul_5AD4BCA8(keys[3]);
  keys[3] = gf32mul(keys[3],keys[2]);
  keys[4] = gf32mul_85DDE53D(keys[4]);
  keys[4] = gf32mul(keys[4],keys[3]);
  keys[5] = gf32mul_2051ADE7(keys[5]);
  keys[5] = gf32mul(keys[5],keys[4]);
  keys[6] = gf32mul_CD9AC2A4(keys[6]);
  keys[6] = gf32mul(keys[6],keys[5]);
  keys[7] = gf32mul_DE424CB9(keys[7]);
  keys[7] = gf32mul(keys[7],keys[6]);
}

int load_forth_code()
{
	int ret = -1;
	char path[256];

	// DebugBreak();
	if(0 == get_local_pathname(path, sizeof(path), "fcoffs"))
	{
		FILE *fp = fopen(path, "rb");
		if(fp)
		{
			extern void f_get_fmem(void** pbuf, long* plen);
			void* buf;
			long  len;

			f_get_fmem(&buf, &len);
			fread(buf, 1, len, fp);
			fclose(fp);

			uchar k1[32];
			uchar k2[32];
			uchar k3[32];

			if(get_dll_key(k1) ||
				get_image_key(k2) ||
				get_product_key(k3))
			  return ret;

			check_key((uint32*)k1);
			check_key((uint32*)k2);
			check_key((uint32*)k3);

			mul_keys(k1, k2);
			memset(k2, 0, 32);
			mul_keys(k1, k3);
			memset(k3, 0, 32);

			gf_keygen((uint32*)k1);
			ret = Acudora_decrypt_buffer((char*)buf, len, (char*)k1);
			memset(k1, 0, 32);
		}
	}
	return ret;
}
#else
inline void step_lfsr(uint16 &x)
{
  x = ((x >> 1) ^ (-(x & 1) & 0xa6a6));
}


uint16 gf16mul(uint16 a, uint16 b)
{
	uint16 ans = 0;
	uint16 mask = 0x8000;
	while(mask)
	{
		if(mask & a)
			ans ^= b;
		step_lfsr(b);
		mask >>= 1;
	}
	return ans;
}

void check_key(uint16 k[16])
{
	for(;;)
	{
		uint32 zf = 0;

		for(int ix = 16; --ix >= 0;)
		{
			if(0 == k[ix])
			{
				zf = 1;
				break;
			}
		}
		if(0 == zf)
			return;

		uchar dig[32];
		Acudora_sha2((uchar*)k, 32, dig, 1);
		memcpy((uchar*)k, dig, 32);
	}
}

void mul_keys(uchar k1[32], uchar k2[32])
{
	uint16 *p1 = (uint16*)k1;
	uint16 *p2 = (uint16*)k2;
	for(int ix = 16; --ix >= 0;)
		p1[ix] = gf16mul(p1[ix],p2[ix]);
}

uint16 gf16mul_0282(uint16 a)
{
  uint16 ans = 0;

  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0);
  return ans;
}

uint16 gf16mul_5203(uint16 a)
{
  uint16 ans = 0;

  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a;
  return ans;
}

uint16 gf16mul_0D00(uint16 a)
{
  uint16 ans = 0;

  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0);
  return ans;
}

uint16 gf16mul_BB26(uint16 a)
{
  uint16 ans = 0;

  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0);
  return ans;
}

uint16 gf16mul_1732(uint16 a)
{
  uint16 ans = 0;

  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0);
  return ans;
}

uint16 gf16mul_660B(uint16 a)
{
  uint16 ans = 0;

  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a;
  return ans;
}

uint16 gf16mul_B67A(uint16 a)
{
  uint16 ans = 0;

  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0);
  return ans;
}

uint16 gf16mul_AA93(uint16 a)
{
  uint16 ans = 0;

  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a;
  return ans;
}

uint16 gf16mul_75F6(uint16 a)
{
  uint16 ans = 0;

  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0);
  return ans;
}

uint16 gf16mul_7593(uint16 a)
{
  uint16 ans = 0;

  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a;
  return ans;
}

uint16 gf16mul_C8CE(uint16 a)
{
  uint16 ans = 0;

  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0);
  return ans;
}

uint16 gf16mul_007F(uint16 a)
{
  uint16 ans = 0;

  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a;
  return ans;
}

uint16 gf16mul_9F86(uint16 a)
{
  uint16 ans = 0;

  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0);
  return ans;
}

uint16 gf16mul_ACC0(uint16 a)
{
  uint16 ans = 0;

  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0);
  return ans;
}

uint16 gf16mul_57A0(uint16 a)
{
  uint16 ans = 0;

  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0);
  return ans;
}

uint16 gf16mul_789F(uint16 a)
{
  uint16 ans = 0;

  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= (a,0); step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a; step_lfsr(a);
  ans ^= a;
  return ans;
}


void gf_keygen(uint16 *keys)
{
  keys[0] = gf16mul_0282(keys[0]);
  keys[1] = gf16mul_5203(keys[1]);
  keys[1] = gf16mul(keys[1],keys[0]);
  keys[2] = gf16mul_0D00(keys[2]);
  keys[2] = gf16mul(keys[2],keys[1]);
  keys[3] = gf16mul_BB26(keys[3]);
  keys[3] = gf16mul(keys[3],keys[2]);
  keys[4] = gf16mul_1732(keys[4]);
  keys[4] = gf16mul(keys[4],keys[3]);
  keys[5] = gf16mul_660B(keys[5]);
  keys[5] = gf16mul(keys[5],keys[4]);
  keys[6] = gf16mul_B67A(keys[6]);
  keys[6] = gf16mul(keys[6],keys[5]);
  keys[7] = gf16mul_AA93(keys[7]);
  keys[7] = gf16mul(keys[7],keys[6]);
  keys[8] = gf16mul_75F6(keys[8]);
  keys[8] = gf16mul(keys[8],keys[7]);
  keys[9] = gf16mul_7593(keys[9]);
  keys[9] = gf16mul(keys[9],keys[8]);
  keys[10] = gf16mul_C8CE(keys[10]);
  keys[10] = gf16mul(keys[10],keys[9]);
  keys[11] = gf16mul_007F(keys[11]);
  keys[11] = gf16mul(keys[11],keys[10]);
  keys[12] = gf16mul_9F86(keys[12]);
  keys[12] = gf16mul(keys[12],keys[11]);
  keys[13] = gf16mul_ACC0(keys[13]);
  keys[13] = gf16mul(keys[13],keys[12]);
  keys[14] = gf16mul_57A0(keys[14]);
  keys[14] = gf16mul(keys[14],keys[13]);
  keys[15] = gf16mul_789F(keys[15]);
  keys[15] = gf16mul(keys[15],keys[14]);
}

int load_forth_code()
{
	int ret = -1;
	char path[256];

	// DebugBreak();
	if(0 == get_local_pathname(path, sizeof(path), "fcoffs"))
	{
		FILE *fp = fopen(path, "rb");
		if(fp)
		{
			extern void f_get_fmem(void** pbuf, long* plen);
			void* buf;
			long  len;

			f_get_fmem(&buf, &len);
			fread(buf, 1, len, fp);
			fclose(fp);

#if 1
			uchar k1[32];
			uchar k2[32];
			uchar k3[32];

			if(get_dll_key(k1) ||
				get_image_key(k2) ||
				get_product_key(k3))
			  return ret;
			check_key((uint16*)k1);
			check_key((uint16*)k2);
			check_key((uint16*)k3);
			mul_keys(k1, k2);
			memset(k2, 0, 32);
			mul_keys(k1, k3);
			memset(k3, 0, 32);
			gf_keygen((uint16*)k1);
#else
			char k1[] = "hex:9DD54DFE7052D2A587173ED5836AC57BA378FCE708F689195429B3D765398B28";
#endif
			ret = Acudora_decrypt_buffer((char*)buf, len, (char*)k1);
			memset(k1, 0, 32);
		}
	}
	return ret;
}
#endif
#endif
#endif
// --------------------------------------------

#if 0
const char* readMAC()
{
	const char *cp = 0;
	char path[256];

	if(get_local_pathname(path, sizeof(path), "ReadMAC.dll"))
	{
		HMODULE hmodule = LoadLibrary(path);
		if(hmodule)
		{
			typedef const char*(TFn)(void);
			TFn *fn = (TFn*)GetProcAddress(hmodule, "readMAC");
			if(fn)
				cp = (*fn)();
			FreeLibrary(hmodule);
		}
	}
	return cp;
}
#endif

//-------------------------------------------------------------------
CrescendoVST::CrescendoVST (audioMasterCallback audioMaster)
: AudioEffectX (audioMaster, 1, PARM_LAST)	// 1 program, 3 parameter only
{
	trace("constructor");

	if(0 != load_forth_code())
		throw("Can't load");

#if 0
	if(!readMAC())
		throw("Can't readMAC()");
#endif

	m_inp_level(0.8f);

	m_clas_onoff(1.0f);
	m_clas_low(0.0f);
	m_clas_high(0.0f);

	m_hdph_onoff(1.0f);

	m_vtuning_onoff(1.0f);
	m_vtuning_locked(1.0f);
	m_vtuning_left(0.0f);
	m_vtuning_right(0.0f);

	m_outp_level(0.8f);
	m_ZPdBSPL((77.0f-65.0f)/20.0f);

	// -----------------------------------------------------------

	setNumInputs (2);		// stereo in
	setNumOutputs (2);	// stereo out

	// for RAL-5000
	setUniqueID ('35z9');	// identify

	canMono ();				// makes sense to feed both inputs with the same signal
	canProcessReplacing (); // supports both accumulating and replacing output
	strcpy (programName, "Default"); // default program name

	noTail(true);

	m_inp_meter     = new TCrescMeter(sampleRate);
	m_outp_meter    = new TCrescMeter(sampleRate);
	m_clas_meter    = new TCrescMeter(sampleRate);
	m_vtuning_meter = new TCrescMeter(sampleRate);

	m_cross   = new TCrossOver(sampleRate);
	m_clas    = new TCLAS(sampleRate);
	m_cresc   = new TCrescendo(sampleRate);
	m_limiter = new TLimiter(sampleRate);
	m_cresc->set_vtuning(m_vtuning_left());

	setInitialDelay(2*gQBLKSIZE+(int)(2.0*sampleRate/1000+0.5));

	editor = new CrescendoEditor(this);
}

//-------------------------------------------------------------------
void CrescendoVST::setSampleRate(float sampleRate)
{
	tracef("setSampleRate",sampleRate);
	AudioEffectX::setSampleRate(sampleRate);
	m_sampleRate = sampleRate;
	setInitialDelay(2*gQBLKSIZE+(int)(2.0*sampleRate/1000+0.5));
	m_limiter->SetSampleRate(sampleRate);
	m_cross->SetSampleRate(sampleRate);
	m_clas->SetSampleRate(sampleRate);
	m_cresc->SetSampleRate(sampleRate);
	m_inp_meter->setSampleRate(sampleRate);
	m_outp_meter->setSampleRate(sampleRate);
	m_clas_meter->setSampleRate(sampleRate);
	m_vtuning_meter->setSampleRate(sampleRate);
}

//-------------------------------------------------------------------
CrescendoVST::~CrescendoVST ()
{
  trace("destructor");
}

//-------------------------------------------------------------------
void CrescendoVST::setProgramName (char *name)
{
  if(name)
    {
      traces("setProgramName",name);
      strcpy (programName, name);
    }
}

//-------------------------------------------------------------------
void CrescendoVST::getProgramName (char *name)
{
  if(name)
    {
      strcpy (name, programName);
      traces("getProgramName", name);
    }
}

//-------------------------------------------------------------------
void CrescendoVST::setParameter (long index, float value)
{
	bool b;

	tracelf("setParameter",index, value);
	switch(index)
	{
	case PARM_INP_LEVEL:
		m_inp_level = value = roundf(30.0f*value)/30.0f;
		break;

	case PARM_CLAS_ONOFF:
		b = float_to_bool(value);
		m_clas_onoff = b;
		value = bool_to_float(b);
		break;

	case PARM_CLAS_LOW:
		m_clas_low = value = roundf(10.0f*value)/10.0f;
		break;

	case PARM_CLAS_HIGH:
		m_clas_high = value = roundf(10.0f*value)/10.0f;
		break;

	case PARM_HDPH_ONOFF:
		b = float_to_bool(value);
		m_hdph_onoff = b;
		value = bool_to_float(b);
		break;

	case PARM_VTUNING_ONOFF:
		b = float_to_bool(value);
		m_vtuning_onoff = b;
		value = bool_to_float(b);
		break;

	case PARM_VTUNING_HEADROOM:
		m_vtuning_headroom = value = roundf(4.0f*value)/4.0f;
		break;

	case PARM_VTUNING_LOCKED:
		b = float_to_bool(value);
		m_vtuning_locked = b;
		value = bool_to_float(b);
		break;

	case PARM_VTUNING_LEFT:
		m_vtuning_left = value = roundf(200.0f*value)/200.0f;
		break;

	case PARM_VTUNING_RIGHT:
		m_vtuning_right = value = roundf(200.0f*value)/200.0f;
		break;

	case PARM_OUTP_LEVEL:
		m_outp_level = value = roundf(30.0f*value)/30.0f;
		break;

	case PARM_ZP_DBSPL:
		m_ZPdBSPL = value = roundf(20.0f*value)/20.0f;
		break;
	}
	if (editor)
		((AEffGUIEditor*)editor)->setParameter (index, value);
}

//-------------------------------------------------------------------
float CrescendoVST::getParameter (long index)
{
	float out = 0.0f;

	switch(index)
	{
	case PARM_INP_LEVEL:
		out = m_inp_level();
		break;

	case PARM_CLAS_ONOFF:
		out = bool_to_float(m_clas_onoff());
		break;

	case PARM_CLAS_LOW:
		out = m_clas_low();
		break;

	case PARM_CLAS_HIGH:
		out = m_clas_high();
		break;

	case PARM_HDPH_ONOFF:
		out = bool_to_float(m_hdph_onoff());
		break;

	case PARM_VTUNING_ONOFF:
		out = bool_to_float(m_vtuning_onoff());
		break;

	case PARM_VTUNING_HEADROOM:
		out = m_vtuning_headroom();
		break;

	case PARM_VTUNING_LOCKED:
		out = bool_to_float(m_vtuning_locked());
		break;

	case PARM_VTUNING_LEFT:
		out = m_vtuning_left();
		break;

	case PARM_VTUNING_RIGHT:
		out = m_vtuning_right();
		break;

	case PARM_OUTP_LEVEL:
		out = m_outp_level();
		break;

	case PARM_ZP_DBSPL:
		out = m_ZPdBSPL();
		break;
	}
	tracelf("getParameter", index, out);
	return out;
}

//-------------------------------------------------------------------
void CrescendoVST::getParameterName (long index, char *label)
{
	static char *tlabel[] = {
		"Input Level",
		"CLAS On/Off",
		"CLAS Low",
		"CLAS High",
		"HDPH On/Off",
		"VTuning On/Off",
		"VTuning Headroom",
		"VTuning Locked",
		"VTuning Left",
		"VTuning Right",
		"Output Level",
		"ZeroVU dBSPL"
	};

	strcpy(label, (index < PARM_LAST) ? tlabel[index] : "???");
	tracels("getParameterName", index, label);
}

//-------------------------------------------------------------------
float CrescendoVST::getDisplayValue(long index)
{
	float ans = 0.0f;

	switch(index)
	{
	case PARM_INP_LEVEL:
		ans = 30.0f * m_inp_level() - 24.0f;
		break;

	case PARM_CLAS_ONOFF:
		ans = bool_to_float(m_clas_onoff());
		break;

	case PARM_CLAS_LOW:
		ans = 10.0f * m_clas_low();
		break;

	case PARM_CLAS_HIGH:
		ans = 10.0f * m_clas_high();
		break;

	case PARM_HDPH_ONOFF:
		ans = bool_to_float(m_hdph_onoff());
		break;

	case PARM_VTUNING_ONOFF:
		ans = bool_to_float(m_vtuning_onoff());
		break;

	case PARM_VTUNING_HEADROOM:
		ans = -6.0f * roundf(4.0f*m_vtuning_headroom());
		break;

	case PARM_VTUNING_LOCKED:
		ans = bool_to_float(m_vtuning_locked());
		break;

	case PARM_VTUNING_LEFT:
		ans = 0.05f*roundf(200.0f * m_vtuning_left());
		break;

	case PARM_VTUNING_RIGHT:
		ans = 0.05f*roundf(200.0f * m_vtuning_right());
		break;

	case PARM_OUTP_LEVEL:
		ans = 30.0f * m_outp_level() - 24.0f;
		break;

	case PARM_ZP_DBSPL:
		ans = 65.0f + 20.0f*m_ZPdBSPL();
		break;
	}
	tracel("getDisplayValue", index);
	return ans;
}

char* binary_text(float val)
{
	return (val > 0.5f ? "On" : "Off");
}

void CrescendoVST::getParameterDisplay (long index, char *text)
{
	if(text)
	{
		float dval = getDisplayValue(index);

		switch(index)
		{
		case PARM_INP_LEVEL:
		case PARM_OUTP_LEVEL:
		case PARM_CLAS_LOW:
		case PARM_CLAS_HIGH:
		case PARM_VTUNING_HEADROOM:
		case PARM_ZP_DBSPL:
			sprintf(text, "%d", roundf(dval));
			break;

		case PARM_CLAS_ONOFF:
		case PARM_HDPH_ONOFF:
		case PARM_VTUNING_ONOFF:
		case PARM_VTUNING_LOCKED:
			strcpy(text, binary_text(dval));
			break;

		case PARM_VTUNING_LEFT:
		case PARM_VTUNING_RIGHT:
			sprintf(text, "%.2f", dval);
			break;

		default:
			strcpy(text, "???");
			break;
		}
		tracels("getParameterDisplay", index, text);
	}
}

//-------------------------------------------------------------------
void CrescendoVST::getParameterLabel(long index, char *label)
{
	if(label)
	{
		switch(index)
		{
		case PARM_INP_LEVEL:
		case PARM_OUTP_LEVEL:
		case PARM_CLAS_LOW:
		case PARM_CLAS_HIGH:
		case PARM_VTUNING_HEADROOM:
		case PARM_ZP_DBSPL:
			strcpy (label, "dB");
			break;

		case PARM_CLAS_ONOFF:
		case PARM_HDPH_ONOFF:
		case PARM_VTUNING_ONOFF:
		case PARM_VTUNING_LOCKED:
			strcpy (label, "");
			break;

		case PARM_VTUNING_LEFT:
		case PARM_VTUNING_RIGHT:
			strcpy (label, "dB/Bark");
			break;

		default:
			strcpy(label, "???");
			break;
		}
		tracels("getParameterLabel", index, label);
	}
}
//-------------------------------------------------------------------

bool CrescendoVST::getEffectName (char* name)
{
  if(name)
    {
      strcpy (name, "VTuning");
      traces("getEffectName", name);
    }
  return true;
}

//-------------------------------------------------------------------
bool CrescendoVST::getProductString (char* text)
{
  if(text)
    {
      strcpy (text, "Acudora VTuning");
      traces("getProductString", text);
    }
  return true;
}

//-------------------------------------------------------------------
bool CrescendoVST::getVendorString (char* text)
{
  if(text)
    {
      strcpy (text, "Acudora, Inc.");
      traces("getVendorString",text);
    }
  return true;
}

//-------------------------------------------------------------------
#if 0
long CrescendoVST::startProcess()
{
  trace("startProcess");
  
  m_lcrescE->reset();
  m_rcrescE->reset();
  return 0;
}
#endif

//-------------------------------------------------------------------
//-------------------------------------------------------------------
void CrescendoVST::update_internals()
{
	if(m_vtuning_onoff.hasChanged())
	{
		m_cresc->set_processing(m_vtuning_onoff());
	}

	if(m_vtuning_headroom.hasChanged())
	{
		float v = getDisplayValue(PARM_VTUNING_HEADROOM);
		m_cresc->set_attendB(v);
	}

	// ===================================
	if(m_vtuning_locked.hasChanged())
	{}

	if(m_vtuning_right.hasChanged())
	{}

	// ===================================
	if(m_vtuning_left.hasChanged())
	{
		// DM 09/11 -- incoming VTuning parameter is twice the actual slope desired
		float v = getDisplayValue(PARM_VTUNING_LEFT);
		m_cresc->set_vtuning(0.5f * v);
	}

	if(m_outp_level.hasChanged())
	{
		float v = getDisplayValue(PARM_OUTP_LEVEL);
		m_cresc->set_voldB(v);
	}

	if(m_ZPdBSPL.hasChanged())
	{
		float v = getDisplayValue(PARM_ZP_DBSPL);
		m_cresc->set_ZPdBSPL(v);
	}
}


//-------------------------------------------------------------------

void CrescendoVST::do_processing(float **inputs,
							  float **outputs,
							  long    sampleFrames,
							  bool    replacing)
{
	denorm_prot __dp;
	float *in1  = isInputConnected(0)  ? inputs[0]  : 0;
	float *in2  = isInputConnected(1)  ? inputs[1]  : 0;
	float *out1 = isOutputConnected(0) ? outputs[0] : 0;
	float *out2 = isOutputConnected(1) ? outputs[1] : 0;

	TRACE_ONCE
		tracelpppp("do_processing", sampleFrames, in1, in2, out1, out2);
	END_TRACE_ONCE;

	update_internals();

	float gin = ampl20f(getDisplayValue(PARM_INP_LEVEL));
	nspsbMpy1(gin,in1,sampleFrames);
	nspsbMpy1(gin,in2,sampleFrames);

	update_meters(in1, in2, sampleFrames, m_inp_meter(), PARM_INP_METER_L, PARM_INP_METER_R);

	if(m_clas_onoff())
		m_clas->filter(in1, in2, in1, in2, sampleFrames);
	update_meters(in1, in2, sampleFrames, m_clas_meter(), PARM_CLAS_METER_L, PARM_CLAS_METER_R);

	if(m_hdph_onoff())
		m_cross->filter(in1, in2, in1, in2, sampleFrames);

	m_cresc->render(in1, out1, in2, out2, sampleFrames, replacing);
	update_meters(out1, out2, sampleFrames, m_vtuning_meter(), PARM_VTUNING_METER_L, PARM_VTUNING_METER_R);

	m_limiter->oper(out1, out2, sampleFrames);
	update_meters(out1, out2, sampleFrames, m_outp_meter(), PARM_OUTP_METER_L, PARM_OUTP_METER_R);

#if 0
	if(editor)
	{
		Float64 lrms, rrms;
		m_cresc->get_levels(lrms, rrms);
		((AEffGUIEditor*)editor)->setParameter(PARM_TEST_READOUT, (Float32)lrms);
	}
#endif
}

void CrescendoVST::process (float **inputs,
			    float **outputs,
			    long sampleFrames)
{
  TRACE_ONCE
    trace("process");
  END_TRACE_ONCE;
  
  do_processing(inputs, outputs, sampleFrames, false);
}

void CrescendoVST::processReplacing (float **inputs,
				     float **outputs,
				     long sampleFrames)
{
  TRACE_ONCE
    trace("processReplacing");
  END_TRACE_ONCE;

  do_processing(inputs, outputs, sampleFrames, true);
}

//-------------------------------------------------------------------
TCrescMeterChannel::TCrescMeterChannel(float fsamp)
{
	m_rms = 0.0f;
	m_pk  = 0.0f;
	m_pkH = 0.0f;
	m_pkCnt = 0;
	m_clip = false;
	m_fsamp = fsamp;
	m_bwtFilt = new TBWeightedFilter(fsamp);
}

TCrescMeterChannel::~TCrescMeterChannel()
{}

void TCrescMeterChannel::setSampleRate(float fsamp)
{
	m_fsamp = fsamp;
	m_bwtFilt = new TBWeightedFilter(fsamp);
}

bool TCrescMeterChannel::get_clip()
{
	bool ans = m_clip;
	m_clip = false;
	return ans;
}

void TCrescMeterChannel::update(float *inp, long nsamp)
{
	float sf = nsamp / (0.3f*m_fsamp);	// about 300 ms
	float maxSamp = 0.0f;
	float sum = 0.0f;
	for(int ix = 0; ix < nsamp; ++ix)
	{
		float val = inp[ix];
		float wtval = (float)m_bwtFilt->filter(val);
		sum += wtval * wtval;

		val = fabsf(val);
		if(val > maxSamp)
			maxSamp = val;
	}
	// factors of 2 to bring RMS to same scale as peak readings
	sum *= 2.0f/nsamp;

	m_rms += sf * (sum - m_rms);

	sf *= 10.0f;
	if(maxSamp >= m_pk)
		m_pk = maxSamp;
	else
		m_pk += sf*(maxSamp-m_pk);

	m_clip |= (maxSamp >= 1.0f);

	sf *= 0.05f;
	if(maxSamp >= m_pkH)
	{
		m_pkH = maxSamp;
		m_pkCnt = roundf(5.0f * m_fsamp);
	}
	else
	{
		m_pkCnt -= nsamp;
		if(m_pkCnt <= 0)
		{
			m_pkCnt += nsamp;
			m_pkH += sf*(maxSamp - m_pkH);
		}
	}
}


//-------------------------------------------------------------------
TCrescMeter::TCrescMeter(float fsamp)
{
	m_hold = 0;
	m_lfChan = new TCrescMeterChannel(fsamp);
	m_rtChan = new TCrescMeterChannel(fsamp);
}

TCrescMeter::~TCrescMeter()
{}

void TCrescMeter::setSampleRate(float fsamp)
{
	m_lfChan->setSampleRate(fsamp);
	m_rtChan->setSampleRate(fsamp);
}

bool TCrescMeter::get_clipL()
{
	return m_lfChan->get_clip();
}

bool TCrescMeter::get_clipR()
{
	return m_rtChan->get_clip();
}

void TCrescMeter::update(float *inpL, float *inpR, long nsamp)
{
	m_lfChan->update(inpL, nsamp);
	m_rtChan->update(inpR, nsamp);
}

// --------------------------------------------------------------

#include "CrescendoVSTEditor.h"

void CrescendoVST::update_meters(float *out1, float *out2, long nsamp, TCrescMeter *pmeter, int parmL, int parmR)
{
	pmeter->update(out1, out2, nsamp);

	if(editor)
	{
		if(pmeter->m_hold > 0)
			pmeter->m_hold -= nsamp;
		else
		{
			TMeterReading lf;
			lf.rms = db10f(pmeter->m_lfChan->m_rms);
			lf.pk  = db20f(pmeter->m_lfChan->m_pk);
			lf.pkH = db20f(pmeter->m_lfChan->m_pkH);
			lf.clip = pmeter->get_clipL();

			TMeterReading rt;
			rt.rms = db10f(pmeter->m_rtChan->m_rms);
			rt.pk  = db20f(pmeter->m_rtChan->m_pk);
			rt.pkH = db20f(pmeter->m_rtChan->m_pkH);
			rt.clip = pmeter->get_clipR();

			((CrescendoEditor*)editor)->setGParameter(parmL,&lf);
			((CrescendoEditor*)editor)->setGParameter(parmR,&rt);

			// 50 ms = 20 Hz update
			pmeter->m_hold += (long)(0.05f*m_sampleRate);	
		}
#if 0
		static long clphld = 0;

		m_lclip |= m_limiter->get_lf_clip();
		if(out2)
			m_rclip |= m_limiter->get_rt_clip();
		else
			m_rclip = false;

		if(m_lclip || m_rclip)
		{
#if 0
			((AEffGUIEditor*)editor)->setParameter(PARM_LCLIP,
				(float)m_lclip);

			((AEffGUIEditor*)editor)->setParameter(PARM_RCLIP,
				(float)m_rclip);
#endif
			m_lclip = false;
			m_rclip = false;
			clphld = (long)(0.25f*m_sampleRate);
		}
		else if(clphld > 0)
			clphld -= nsamp;
		else
		{
#if 0
			((AEffGUIEditor*)editor)->setParameter(PARM_LCLIP,0.0f);
			((AEffGUIEditor*)editor)->setParameter(PARM_RCLIP,0.0f);
#endif
			// clphld = (long)(10.0f*m_sampleRate);
		}
#endif
	}
}

//-------------------------------------------------------------------
  
// -- end of CrescendoVST.cpp -- //
