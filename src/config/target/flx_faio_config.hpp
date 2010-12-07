#ifndef __FLX_FAIO_CONFIG_H__
#define __FLX_FAIO_CONFIG_H__
#include "flx_rtl_config.hpp"
#ifdef BUILD_FAIO
#define FAIO_EXTERN FLX_EXPORT
#else
#define FAIO_EXTERN FLX_IMPORT
#endif
#endif
