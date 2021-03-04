
#ifndef TWOFISH_DATA_H
#define TWOFISH_DATA_H

// ** Thread-safe implementation
// ** 128bit block size
// ** 256bit key

#define Q_TABLES
#define M_TABLE
#define MK_TABLE
#define ONE_STEP

typedef struct {
	UInt32	k_len;
	UInt32	l_key[40];
	UInt32	s_key[4];

	#ifdef  Q_TABLES
	UInt32	qt_gen;
	UInt8	q_tab[2][256];
	#endif

	#ifdef  M_TABLE
	UInt32	mt_gen;
	UInt32	m_tab[4][256];
	#endif

	#ifdef  MK_TABLE
	#ifdef  ONE_STEP
	UInt32	mk_tab[4][256];
	#else
	UInt8	sb[4][256];
	#endif
	#endif
} TWOFISH_DATA;

#endif
