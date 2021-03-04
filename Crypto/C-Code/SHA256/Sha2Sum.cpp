// Sha2Sum.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"

extern "C" int sha2_file( const char *path, unsigned char output[32], int is224 );

#define USAGE   \
"\n  sha2sum <input filename>\n" \
"\n  example: sha2sum file\n" \
"\n"

void Acudora_digest_to_text(char *buf, const unsigned char* digest)
{
    for(int ix = 0; ix < 32; ++ix)
    {
        sprintf(buf, "%02x", digest[ix]);
        buf += 2;
    }
}

int main(int argc, char* argv[])
{
	int ret = -1;

	if(argc < 2)
    {
        printf( USAGE );
        
#if defined(WIN32)
        printf( "\n  Press Enter to exit this program.\n" );
        fflush( stdout ); getchar();
#endif
        
        goto exit;
    }

	unsigned char dig[32];
	ret = sha2_file(argv[1], dig, 0);
	if(0 == ret)
	{
		char buf[256];
		Acudora_digest_to_text(buf, dig);
		printf(buf);
	}

exit:
	return ret;
}

