#ifndef __FLX_SQLITE3_CONFIG_H__
#define __FLX_SQLITE3_CONFIG_H__
#include "flx_rtl_config.hpp"
#ifdef BUILD_SQLITE3
#define SQLITE3_EXTERN FLX_EXPORT
#else
#define SQLITE3_EXTERN FLX_IMPORT
#endif
#endif
#define SQLITE_API SQLITE3_EXTERN
