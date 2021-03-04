GCC=g++ -I$./include -O2 -mfpmath=sse -msse

all: crypto

crypto: main.cpp \
	aes256/sha2.c \
        include/sha2.h \
	include/aesx.h \
	aes256/aesx.c \
        include/config.h \
        include/padlock.h \
        aes256/padlock.c \
	include/my_types.h \
	include/twofish.h \
	twofish/twofish.cpp \
        include/ecc.h \
        ecc/eccl_le.cpp
	$(GCC) -o cctr-hmac-crypt \
		main.cpp \
		aes256/parse_keytext.cpp \
		aes256/sha2.c \
		aes256/aesx.c \
		aes256/padlock.c \
                ecc/eccl_le.cpp \
	        twofish/twofish.cpp \
		-lmx -lm -lstdc++

