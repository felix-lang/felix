#ifndef __FLX_EXCEPTIONS_CONFIG_H__
#define __FLX_EXCEPTIONS_CONFIG_H__
#include "flx_rtl_config.hpp"
#ifdef BUILD_FLX_EXCEPTIONS
#define FLX_EXCEPTIONS_EXTERN FLX_EXPORT
#else
#define FLX_EXCEPTIONS_EXTERN FLX_IMPORT
#endif
#endif
