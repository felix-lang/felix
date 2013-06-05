#ifndef __FLX_MANYMOUSE_CONFIG_H__
#define __FLX_MANYMOUSE_CONFIG_H__
#include "flx_rtl_config.hpp"
#ifdef BUILD_FLX_MANYMOUSE
#define FLX_MANYMOUSE_EXTERN FLX_EXPORT
#else
#define FLX_MANYMOUSE_EXTERN FLX_IMPORT
#endif
#endif
