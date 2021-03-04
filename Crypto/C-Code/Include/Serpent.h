
#ifndef SERPENT_H
#define SERPENT_H

// ** Thread-safe implementation
// ** 128bit block size
// ** 256bit key

extern	void Serpent_set_key(DWORD *l_key,const DWORD *in_key, const DWORD key_len);
extern	void Serpent_encrypt(const DWORD *l_key,const DWORD *in_blk, DWORD *out_blk);
extern	void Serpent_decrypt(const DWORD *l_key,const DWORD *in_blk, DWORD *out_blk);

#endif
