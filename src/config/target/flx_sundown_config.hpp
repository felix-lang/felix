#ifndef __FLX_SUNDOWN_CONFIG_H__
#define __FLX_SUNDOWN_CONFIG_H__
#include "flx_rtl_config.hpp"
#ifdef BUILD_SUNDOWN
#define SUNDOWN_EXTERN FLX_EXPORT
#else
#define SUNDOWN_EXTERN FLX_IMPORT
#endif
#endif

