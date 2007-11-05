#line 2236 "./lpsrc/flx_rtl.pak"
#ifndef __FLX_EH_H__
#define __FLX_EH_H__
#include "flx_rtl_config.hpp"
#include "flx_exceptions.hpp"

namespace flx { namespace rtl {
int RTL_EXTERN std_exception_handler (std::exception *e);
int RTL_EXTERN flx_exception_handler (flx::rtl::flx_exception_t *e);
}}

#endif

