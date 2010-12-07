#ifndef __FLX_GC_CONFIG_H__
#define __FLX_GC_CONFIG_H__
#include "flx_rtl_config.hpp"
#ifdef BUILD_FLX_GC
#define GC_EXTERN FLX_EXPORT
#else
#define GC_EXTERN FLX_IMPORT
#endif
#endif


