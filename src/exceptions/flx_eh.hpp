#ifndef __FLX_EH_H__
#define __FLX_EH_H__
#include "flx_rtl_config.hpp"
#include "flx_exceptions.hpp"

namespace flx { namespace rtl {
int FLX_EXCEPTIONS_EXTERN std_exception_handler (::std::exception const *e);
int FLX_EXCEPTIONS_EXTERN flx_exception_handler (::flx::rtl::flx_exception_t const *e);
}}

#endif
