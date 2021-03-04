// my_types.h -- unified type descriptors
// DM/Acudora  07/11
// ----------------------------------------------

#ifndef __MY_TYPES_H__
#define __MY_TYPES_H__

typedef unsigned char                   UInt8;
typedef signed char                     SInt8;
typedef unsigned short                  UInt16;
typedef signed short                    SInt16;

#if __LP64__
typedef unsigned int                    UInt32;
typedef signed int                      SInt32;
typedef long long                       SInt64;
typedef unsigned long long              UInt64;
#else
typedef unsigned long                   UInt32;
typedef signed long                     SInt32;
#endif

typedef float               Float32;
typedef double              Float64;


#endif // __MY_TYPES_H__

// -- end of my_types.h -- //
