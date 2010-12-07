#ifndef __FLX_GLOB_CONFIG_H__
#define __FLX_GLOB_CONFIG_H__
#include "flx_rtl_config.hpp"
#ifdef BUILD_GLOB
#define GLOB_EXTERN FLX_EXPORT
#else
#define GLOB_EXTERN FLX_IMPORT
#endif
#endif
