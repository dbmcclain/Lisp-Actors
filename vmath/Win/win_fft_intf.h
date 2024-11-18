// fft_intf.h -- This is the DLL interface to FFTX
//
// DM/MCFA  12/01
// ----------------------------------------------------

extern "C" {
  __declspec(dllexport) void GetVersionString(char *buf, long nbuf);
  __declspec(dllexport) long d2zfftf(long nx, long ny, double *src, double *dst);
  __declspec(dllexport) long z2dfftf(long nx, long ny, double *src, double *dst);
  __declspec(dllexport) long z2zfftf(long nx, long ny, double *src, double *dst, long direction);
};

// -- end of fft_intf.h -- //
