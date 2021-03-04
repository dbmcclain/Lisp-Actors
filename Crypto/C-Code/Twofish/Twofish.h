
#ifndef TWOFISH_H
#define TWOFISH_H

// ** Thread-safe implementation
// ** 128bit block size
// ** 256bit key

#include "Twofish_data.h"

extern	void Twofish_set_key(TWOFISH_DATA *pTfd,const UInt32 *in_key, const UInt32 key_len);
extern	void Twofish_encrypt(const TWOFISH_DATA *pTfd,const UInt32 *in_blk, UInt32 *out_blk);
extern	void Twofish_decrypt(const TWOFISH_DATA *pTfd,const UInt32 *in_blk, UInt32 *out_blk);

#endif
