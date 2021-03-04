//
//  decrypt.cpp
//  CrescendoAU
//
//  Created by David McClain on 8/4/11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#include "decrypt.h"

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

#include "aes.h"
#include "sha2.h"
#include "parse_keytext.h"

int Acudora_sha2_file( const char *path, unsigned char output[32])
{
    return sha2_file(path, output, 0);
}

void Acudora_sha2(unsigned char* text, 
                  UInt32           len, 
                  unsigned char* digest, 
                  UInt32           count)
{
    sha2_context sha_ctx;
    
    memset(digest,0,32);
    for(int ix = 0; ix < count; ++ix)
    {
        sha2_starts( &sha_ctx, 0 );
        sha2_update( &sha_ctx, digest, 32 );
        sha2_update( &sha_ctx, text, (int)len );
        sha2_finish( &sha_ctx, digest );        
    }
    memset( &sha_ctx, 0, sizeof( sha2_context ) );
}

void Acudora_digest_to_text(char *buf, const unsigned char* digest)
{
    for(int ix = 0; ix < 32; ++ix)
    {
        sprintf(buf, "%02x", digest[ix]);
        buf += 2;
    }
}

int Acudora_decrypt_buffer(char *buf, long len, const char *keytext, int keylen)
{
    int ret = -1, i, n;
    int lastn;
    int inp, outp;

    unsigned char IV[16];
    unsigned char key[512];
    unsigned char digest[32];
    unsigned char buffer[1024];
    
    aes_context aes_ctx;
    sha2_context sha_ctx;
    
    
    /*
     * Read the secret key and clean the command line.
     */
#if 1
	memcpy(key, keytext, keylen);
#else
    keylen = parse_keytext(keytext, key, sizeof(key));
    if(keylen < 0)
    {
        // can't open key file
        goto exit;
    }
#endif
    
    // mode DECRYPT
    {
        unsigned char tmp[16];
        
        /*
         *  The encrypted file must be structured as follows:
         *
         *        00 .. 15              Initialization Vector
         *        16 .. 31              AES Encrypted Block #1
         *           ..
         *      N*16 .. (N+1)*16 - 1    AES Encrypted Block #N
         *  (N+1)*16 .. (N+1)*16 + 32   HMAC-SHA-256(ciphertext)
         */
        
        if( len < 48 )
        {
            // file too short
            goto exit;
        }
        
        if( ( len & 0x0F ) != 0 )
        {
            // file size not multiple of 16
            goto exit;
        }
        
        
        /*
         * Subtract the IV + HMAC length.
         */
        inp = 0;
		outp = 0;

        /*
         * Read the IV and original filesize modulo 16.
         */
		memcpy(IV, buf, 16);
		inp += 16;
        lastn = IV[15] & 0x0F;
        
        /*
         * Hash the IV and the secret key together 8192 times
         * using the result to setup the AES context and HMAC.
         */
        memset( digest, 0,  32 );
        memcpy( digest, IV, 16 );
        
        for( i = 0; i < 8192; i++ )
        {
            sha2_starts( &sha_ctx, 0 );
            sha2_update( &sha_ctx, digest, 32 );
            sha2_update( &sha_ctx, key, keylen );
            sha2_finish( &sha_ctx, digest );
        }

		memset( key, 0, sizeof( key ) );
        aes_setkey_dec( &aes_ctx, digest, 256 );
        sha2_hmac_starts( &sha_ctx, digest, 32, 0 );
                
        /*
         * Decrypt and write the plaintext.
         */
        for(int offset = 16; offset < len-32; offset += 16 )
        {
			memcpy(buffer, buf+inp, 16);
			inp += 16;

            memcpy( tmp, buffer, 16 );
            
            aes_crypt_ecb( &aes_ctx, AES_DECRYPT, buffer, buffer );
                        
            for( i = 0; i < 16; i++ )
                buffer[i] = (unsigned char)( buffer[i] ^ IV[i] );
            
            sha2_hmac_update( &sha_ctx, buffer, 16 );
            
            n = ( lastn > 0 && offset == len - 16 ) ? lastn : 16;
            
			memcpy(buf + outp, buffer, n);
			outp += n;
            memcpy( IV, tmp, 16 );
        }
        
        /*
         * Verify the message authentication code.
         */
        sha2_hmac_finish( &sha_ctx, digest );
        
        if( memcmp( digest, buf+inp, 32 ) != 0 )
        {
            // check failed. wrong key.
            goto exit;
        }
    }
    
    ret = 0;
    
exit:
    
    memset( buffer, 0, sizeof( buffer ) );
    memset( digest, 0, sizeof( digest ) );
    
    memset( &aes_ctx, 0, sizeof(  aes_context ) );
    memset( &sha_ctx, 0, sizeof( sha2_context ) );
    
    return( ret );
}


