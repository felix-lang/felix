#ifndef __FLX_PTHREAD_CONFIG_H__
#define __FLX_PTHREAD_CONFIG_H__
#include "flx_rtl_config.hpp"
#ifdef BUILD_PTHREAD
#define PTHREAD_EXTERN FLX_EXPORT
#else
#define PTHREAD_EXTERN FLX_IMPORT
#endif
#endif
