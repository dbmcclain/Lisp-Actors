//
//  decrypt.h
//  CrescendoAU
//
//  Created by David McClain on 8/4/11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#ifndef CrescendoAU_decrypt_h
#define CrescendoAU_decrypt_h

#include "my_types.h"

extern void Acudora_sha2(unsigned char* text, 
                         UInt32           len, 
                         unsigned char* digest, 
                         UInt32           count);

extern int  Acudora_sha2_file(const char    *path, 
                              unsigned char output[32]);

extern int  Acudora_decrypt_buffer(char *buf, 
								   long len, 
								   const char *keytext); 

extern void Acudora_digest_to_text(char*                buf, 
                                   const unsigned char* digest);



#endif
