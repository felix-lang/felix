#ifndef __FLX_TRE_CONFIG_H__
#define __FLX_TRE_CONFIG_H__
#include "flx_rtl_config.h"
#ifdef BUILD_TRE
#define TRE_EXTERN FLX_EXPORT
#else
#define TRE_EXTERN FLX_IMPORT
#endif
#endif

