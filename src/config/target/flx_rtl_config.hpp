#ifndef __FLX_RTL_CONFIG_HPP__
#define __FLX_RTL_CONFIG_HPP__
#include "flx_rtl_config.h"

#include <stdint.h>
// get variant index code and pointer from packed variant rep
#define FLX_VP(x) ((void*)((uintptr_t)(x) & ~(uintptr_t)0x03))
#define FLX_VI(x) ((int)((uintptr_t)(x) & (uintptr_t)0x03))

// make a packed variant rep from index code and pointer
#define FLX_VR(i,p) ((void*)((uintptr_t)(p)|(uintptr_t)(i)))


// get variant index code and pointer from nullptr variant rep
#define FLX_VNP(x) (x)
#define FLX_VNI(x) ((int)(x!=0))

// make a nullptr variant rep from index code and pointer
#define FLX_VNR(i,p) (p)


#endif

