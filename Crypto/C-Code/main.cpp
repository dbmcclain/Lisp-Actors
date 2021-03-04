//
//  main.cpp
//  aescrypt2
//
//  Created by David McClain on 7/20/11.
//  Copyright 2011 The Euterpe Group, LLC. All rights reserved.
//

/*
 *  AES-256 file encryption program
 *
 *  Copyright (C) 2006-2010, Brainspark B.V.
 *
 *  This file is part of PolarSSL (http://www.polarssl.org)
 *  Lead Maintainer: Paul Bakker <polarssl_maintainer at polarssl.org>
 *
 *  All rights reserved.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with this program; if not, write to the Free Software Foundation, Inc.,
 *  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#ifndef _CRT_SECURE_NO_DEPRECATE
#define _CRT_SECURE_NO_DEPRECATE 1
#endif

#if defined(WIN32)
#include <windows.h>
#include <io.h>
#else
#include <sys/types.h>
#include <unistd.h>
#endif

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <ctype.h>

#include "Include/my_types.h"
#include "Include/aesx.h"
#include "Include/twofish.h"
#include "Include/sha2.h"
#include "Include/parse_keytext.h"

#define MODE_UNKNOWN    0
#define MODE_ENCRYPT    1
#define MODE_DECRYPT    2

#define USAGE   \
"\n  ctr-hmac-crypt [ENC|DEC] <input filename> <output filename> <key>\n" \
"\n  example: ctr-hmac-crypt ENC file file.aes hex:E76B2413958B00E193\n" \
"\n"

// --------------------------------------------------------------------------------------

extern void drbg(UInt8 *dst, UInt32 nbytes);
extern void make_nonce(UInt8 *dst, UInt32 nbytes);
extern void kdf(UInt8 *dst, UInt32 nbytes, UInt8 *key, UInt32 keylen);

// -----------------------------------------------
extern int encrypt_file(FILE *fin, FILE *fout, unsigned char* key, int keylen, off_t filesize);
extern int decrypt_file(FILE *fin, FILE *fout, unsigned char* key, int keylen, off_t filesize);

void upcase(char *str)
{
    while(*str)
    {
        if(islower(*str))
            *str = toupper(*str);
        ++str;
    }
}

void must_fread(unsigned char *buf, int nel, FILE *fp)
{
    if(nel != fread(buf, 1, nel, fp))
    {
        fprintf(stderr, "fread(%d bytes) failed", nel);
        throw "must_read";
    }
}

void must_fwrite(unsigned char *buf, int nel, FILE *fp)
{
    if(nel != fwrite(buf, 1, nel, fp))
    {
        fprintf(stderr, "fwrite(%d bytes) failed", nel);
        throw "must_write";
    }
}

void convert_int_to_nbytes(UInt32 val, unsigned char *dst, UInt32 nb)
{
    for(int ix = nb; --ix >= 0; )
    {
        dst[ix] = val & 0x0ff;
        val >>= 8;
    }
}

void show_bytes(unsigned char* buf, UInt32 nel)
{
    for(int ix = 0; ix < nel; ++ix)
        printf("%02x", buf[ix]);
    printf("\n");
}

void show_args(int argc, char **argv)
{
    for(int ix = 0; ix < argc; ++ix)
        printf("arg[%d] = %s\n", ix, argv[ix]);
}

// ------------------------------------------------------------------

int main( int argc, char *argv[] )
{
    int keylen, mode, ret = -1;
    FILE *fin = 0, *fout = 0;
    
    unsigned char key[512];
    
#if defined(WIN32)
    LARGE_INTEGER li_size;
    __int64 filesize, offset;
#else
    off_t filesize;
#endif
    
    /*
     * Parse the command-line arguments.
     */
    if( argc != 5 )
    {
        printf( USAGE );
        
#if defined(WIN32)
        printf( "\n  Press Enter to exit this program.\n" );
        fflush( stdout ); getchar();
#endif
        
        goto exit;
    }
    // show_args(argc, argv);
    
    mode = MODE_UNKNOWN;
    upcase(argv[1]);
    if(0 == strncmp("ENC", argv[1], 3))
        mode = MODE_ENCRYPT;
    else if(0 == strncmp("DEC", argv[1], 3))
        mode = MODE_DECRYPT;
    
    if( mode != MODE_ENCRYPT && mode != MODE_DECRYPT )
    {
        fprintf( stderr, "invalid operation mode\n" );
        goto exit;
    }
    
    if( strcmp( argv[2], argv[3] ) == 0 )
    {
        fprintf( stderr, "input and output filenames must differ\n" );
        goto exit;
    }
    
    if( ( fin = fopen( argv[2], "rb" ) ) == NULL )
    {
        fprintf( stderr, "fopen(%s,rb) failed\n", argv[2] );
        goto exit;
    }
    
    if( ( fout = fopen( argv[3], "wb+" ) ) == NULL )
    {
        fprintf( stderr, "fopen(%s,wb+) failed\n", argv[3] );
        goto exit;
    }
    
    /*
     * Read the secret key and clean the command line.
     */
    keylen = parse_keytext(argv[4],key,sizeof(key));
    memset( argv[4], 0, strlen( argv[4] ) );
    if(keylen < 0)
        goto exit; // already issued error message
    
    
#if defined(WIN32)
    /*
     * Support large files (> 2Gb) on Win32
     */
    li_size.QuadPart = 0;
    li_size.LowPart  =
    SetFilePointer( (HANDLE) _get_osfhandle( _fileno( fin ) ),
                   li_size.LowPart, &li_size.HighPart, FILE_END );
    
    if( li_size.LowPart == 0xFFFFFFFF && GetLastError() != NO_ERROR )
    {
        fprintf( stderr, "SetFilePointer(0,FILE_END) failed\n" );
        goto exit;
    }
    
    filesize = li_size.QuadPart;
#else
    if( ( filesize = lseek( fileno( fin ), 0, SEEK_END ) ) < 0 )
    {
        perror( "lseek" );
        goto exit;
    }
#endif
    
    if( fseek( fin, 0, SEEK_SET ) < 0 )
    {
        fprintf( stderr, "fseek(0,SEEK_SET) failed\n" );
        goto exit;
    }
    
    try {
        if( mode == MODE_ENCRYPT )
            ret = encrypt_file(fin, fout, key, keylen, filesize);
        else
            ret = decrypt_file(fin, fout, key, keylen, filesize);
    }
    catch(...)
    {}
    
exit:
    
    if(fin)
        fclose(fin);
    if(fout)
        fclose(fout);
    
    exit(ret);
}

// ---------------------------------------------------------------------------

class ctr3_hmac_crypto
{
    aesx_context  aesx_ctx;
    TWOFISH_DATA  twofish_ctx1;
    TWOFISH_DATA  twofish_ctx2;
    sha2_context  sha_ctx;
    UInt32        m_ctr;
    unsigned char        m_nonce[16];
    static unsigned char sig[16];
    
    void encrypt_decrypt_in_place(unsigned char* buf, UInt32 buflen);
    void clear_contexts();
    
public:
    ctr3_hmac_crypto();
    virtual ~ctr3_hmac_crypto();
    
    void init(unsigned char* key, UInt32 keylen, unsigned char* salt, unsigned char* nonce);
    
    void encrypt_in_place(unsigned char* buf, UInt32 buflen);
    void decrypt_in_place(unsigned char* buf, UInt32 buflen);
    
    void finish_encrypt(unsigned char* buf); // produce encrypted 32-byte digest
    void finish_decrypt(unsigned char* buf); // produce encrypted 32-byte digest
    
    unsigned char *signature()
    { return sig; }
};

unsigned char ctr3_hmac_crypto::sig[16] = {
    // {82272E0A-B0C1-11E1-9C55-C82A14446EA7}
    130, 39,  46, 10,  176, 193,  17, 225,
    156, 85, 200, 42,   20,  68, 110, 167 };

ctr3_hmac_crypto::ctr3_hmac_crypto()
{}

ctr3_hmac_crypto::~ctr3_hmac_crypto()
{
    clear_contexts();
}

void ctr3_hmac_crypto::clear_contexts()
{
    memset(&twofish_ctx1, 0, sizeof(twofish_ctx1));
    memset(&twofish_ctx2, 0, sizeof(twofish_ctx2));
    memset(&aesx_ctx, 0, sizeof(aesx_ctx));
    memset(&sha_ctx, 0, sizeof(sha_ctx));
}

void ctr3_hmac_crypto::init(unsigned char* key, UInt32 keylen, unsigned char* salt, unsigned char* nonce)
{
    unsigned char kbits[128];
    unsigned char *pkey = (unsigned char*)malloc(keylen + 32);
    
    memcpy(m_nonce, nonce, 16);
    m_ctr = 0;

    memcpy(pkey,           key,   keylen);
    memcpy(pkey+keylen,    nonce, 16);
    memcpy(pkey+keylen+16, salt,  16);
    kdf(kbits, 128, pkey, keylen + 32);
    memset(pkey, 0, keylen+32);
    free(pkey);
    
    clear_contexts();    
    Twofish_set_key(&twofish_ctx1, (UInt32*)&kbits[ 0], 256);
    aesx_setkey_enc( &aesx_ctx,    &kbits[32],          256);
    Twofish_set_key(&twofish_ctx2, (UInt32*)&kbits[64], 256);
    sha2_hmac_starts( &sha_ctx,    &kbits[96],           32, 0 );
    
    sha2_hmac_update( &sha_ctx, sig,   16 );
    sha2_hmac_update( &sha_ctx, salt,  16 );
    sha2_hmac_update( &sha_ctx, nonce, 16 );
}

void ctr3_hmac_crypto::encrypt_decrypt_in_place(unsigned char* buf, UInt32 buflen)
{
    // encrypt 16 or fewer bytes in place
    unsigned char tmpv[16];
    
    memcpy(tmpv, m_nonce, 12);
    convert_int_to_nbytes(++m_ctr, tmpv+12, 4);
    
    Twofish_encrypt(&twofish_ctx1, (UInt32*)tmpv, (UInt32*)tmpv);
    aesx_crypt_ecb(&aesx_ctx, AESX_ENCRYPT, tmpv, tmpv);
    Twofish_encrypt(&twofish_ctx2, (UInt32*)tmpv, (UInt32*)tmpv);
    
    for(UInt32 i = 0; i < buflen; ++i)
        buf[i] ^= tmpv[i];
}

void ctr3_hmac_crypto::encrypt_in_place(unsigned char* buf, UInt32 buflen)
{
    encrypt_decrypt_in_place(buf, buflen);
    sha2_hmac_update( &sha_ctx, buf, buflen );
}

void ctr3_hmac_crypto::decrypt_in_place(unsigned char* buf, UInt32 buflen)
{
    // encrypt 16 or fewer bytes in place
    sha2_hmac_update( &sha_ctx, buf, buflen );
    encrypt_decrypt_in_place(buf, buflen);
}

void ctr3_hmac_crypto::finish_encrypt(unsigned char* buf)
{
    // buf should be 32 bytes
    sha2_hmac_finish( &sha_ctx, buf);
    // encrypt_decrypt_in_place(buf, 16);
    // encrypt_decrypt_in_place(buf+16, 16);
}

void ctr3_hmac_crypto::finish_decrypt(unsigned char* buf)
{
    // buf should be 32 bytes
    unsigned char digest[32];
    
    finish_encrypt(digest);
    for(int i = 0; i < 32; ++i)
        if(buf[i] != digest[i])
        {
            fprintf(stderr, "HMAC failure\n");
            throw("HMAC failure");
        }
}

// -----------------------------------------------------------

int encrypt_file(FILE *fin, FILE *fout, unsigned char* key, int keylen, off_t filesize)
{
    unsigned char buffer[32];
    unsigned char salt[16];
    unsigned char nonce[16];
    
    ctr3_hmac_crypto crypto;
    
    int ret = -1;
    off_t offset;
    
    drbg(salt, 16);
    make_nonce(nonce, 16);
    
    crypto.init(key, keylen, salt, nonce);
    
    must_fwrite(crypto.signature(), 16, fout);
    must_fwrite(salt, 16, fout);
    must_fwrite(nonce, 16, fout);
    
    /*
     * Encrypt and write the ciphertext.
     */
    for( offset = 0; offset < filesize; offset += 16 )
    {
#if defined(WIN32)
        __int64 remaining = filesize - offset;
#else
        off_t remaining = filesize - offset;
#endif
        int n = ( remaining > 16 ) ? 16 : (int)remaining;
        
        must_fread(buffer, n, fin);
        crypto.encrypt_in_place(buffer, n);
        must_fwrite(buffer, n, fout);
    }
    
    /*
     * Finally write the HMAC.
     */
    crypto.finish_encrypt(buffer);
    must_fwrite(buffer, 32, fout);
    ret = 0;
    
    memset( buffer, 0, sizeof( buffer ) );
    memset( salt,   0, sizeof(salt));
    memset(nonce,   0, sizeof(nonce));
    return( ret );
}

// --------------------------------------------------------------------------------------

int decrypt_file(FILE *fin, FILE *fout, unsigned char *key, int keylen, off_t filesize)
{
    unsigned char buffer[32];
    unsigned char salt[16];
    unsigned char nonce[16];
    
    ctr3_hmac_crypto crypto;
    
    int ret = -1;
    off_t offset;
    
    if(filesize < (3*16+32))
    {
        fprintf(stderr, "File too short\n");
        throw("File too short");
    }
    
    must_fread(buffer, 16, fin);
    unsigned char* sig = crypto.signature();
    for(int i = 0; i < 16; ++i)
        if(buffer[i] != sig[i])
        {
            fprintf(stderr, "Not a ctr3_hmac_crypt file\n");
            throw("Not a ctr3_hmac_crypt_file");
        }
    
    must_fread(salt, 16, fin);
    must_fread(nonce, 16, fin);
    crypto.init(key, keylen, salt, nonce);
    
    for( offset = (3*16+32); offset < filesize; offset += 16 )
    {
#if defined(WIN32)
        __int64 remaining = filesize - offset;
#else
        off_t remaining = filesize - offset;
#endif
        int n = ( remaining > 16 ) ? 16 : (int)remaining;
        
        must_fread(buffer, n, fin);
        crypto.decrypt_in_place(buffer, n);
        must_fwrite(buffer, n, fout);
    }
    
    /*
     * Finally write the HMAC.
     */
    must_fread(buffer, 32, fin);
    crypto.finish_decrypt(buffer);
    ret = 0;
    
    memset( buffer, 0, sizeof( buffer ) );
    memset( salt,   0, sizeof(salt));
    memset(nonce,   0, sizeof(nonce));
    return( ret );
}

// --------------------------------------------------------------------------------------

void kdf(UInt8 *dst, UInt32 nbytes, UInt8 *key, UInt32 keylen)
{
    unsigned char digest[32];
    UInt32 ctr = 0;
    sha2_context sha_ctx;
    
    memset(digest, 0, 32);
    for(int ix = 0; ix < nbytes; ix += 32)
    {
        int nb = 32;
        if(nb > nbytes - ix)
            nb = nbytes - ix;
        
        ++ctr;
        unsigned char ctrbyts[4];
        convert_int_to_nbytes(ctr, ctrbyts, 4);
        
        for(int jx = 0; jx < 8192; ++jx)
        {
            sha2_starts(&sha_ctx, 0);
            sha2_update(&sha_ctx, ctrbyts, 4);
            sha2_update(&sha_ctx, digest, 32);
            sha2_update(&sha_ctx, key, keylen);
            sha2_finish(&sha_ctx, digest);
        }
        memcpy(dst + ix, digest, nb);
    }
}


void drbg(UInt8 *dst, UInt32 nbytes)
{
    // this is very lame too... but good enough for our purposes
    FILE *fp = fopen("/dev/urandom", "rb");
    if(0 == fp)
        throw("Failed to open /dev/urandom");
    must_fread(dst, nbytes, fp);
    fclose(fp);
}

#include <sys/time.h>
void make_nonce(UInt8 *dst, UInt32 nbytes)
{
    struct timeval tv;
    
    // this is very lame...
    // also -- we asked for nbytes but we are sending back only 8 bytes
    // for our purposes we know we need 16 bytes anyway...
    memset(dst, 0, nbytes);
    gettimeofday(&tv, 0);
    convert_int_to_nbytes((UInt32)tv.tv_sec, dst, 4);
    convert_int_to_nbytes((UInt32)tv.tv_usec, dst+4, 4);
}

