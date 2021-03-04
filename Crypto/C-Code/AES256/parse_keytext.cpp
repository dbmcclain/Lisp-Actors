//
//  parse_keytext.cpp
//  CrescendoAU
//
//  Created by David McClain on 7/30/11.
//  Copyright 2011 Acudora, Inc. All rights reserved.
//

#include <stdio.h>
#include <memory.h>
#include <string.h>
#include "parse_keytext.h"

int parse_keytext(const char* keytext, unsigned char* keybuf, int buflen)
{
    int keylen;
    
    if(0 == memcmp(keytext, "file:", 5))
    {
        FILE *fkey;
        
        if( ( fkey = fopen( keytext+5, "rb" ) ) != NULL )
        {
            keylen = (int)fread( keybuf, 1, buflen, fkey );
            fclose( fkey );
        }
        else
        {
            fprintf(stderr,"Can't open keyfile: %s",keytext+5);
            keylen = -1;
        }
    }
    else if( memcmp( keytext, "hex:", 4 ) == 0 )
    {
        char* p = (char*)&keytext[4];
        keylen = 0;
        int n;
        
        while( sscanf( p, "%02X", &n ) > 0 &&
              keylen < (int) buflen )
        {
            keybuf[keylen++] = (unsigned char) n;
            p += 2;
        }
    }
    else
    {
        keylen = (int)strlen( keytext );
        
        if( keylen > (int) buflen )
            keylen = (int) buflen;
        
        memcpy( keybuf, keytext, keylen );
    }
    return keylen;
}
