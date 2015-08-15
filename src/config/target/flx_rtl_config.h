#ifndef __FLX_RTL_CONFIG_H__
#define __FLX_RTL_CONFIG_H__

#include "flx_rtl_config_params.hpp"
#include <setjmp.h>

#if FLX_HAVE_GNU_BUILTIN_EXPECT
#define FLX_UNLIKELY(x) __builtin_expect(long(x),0)
#define FLX_LIKELY(x) __builtin_expect(long(x),1)
#else
#define FLX_UNLIKELY(x) x
#define FLX_LIKELY(x) x
#endif


#define FLX_SAVE_REGS \
  jmp_buf reg_save_on_stack; \
  setjmp (reg_save_on_stack)

//
#if FLX_HAVE_CGOTO && FLX_HAVE_ASM_LABELS
#define FLX_CGOTO 1
#else
#define FLX_CGOTO 0
#endif

#if FLX_WIN32 && !defined(_WIN32_WINNT)
#define _WIN32_WINNT 0x0600 // Require Windows NT5 (2K, XP, 2K3)
#endif

#if FLX_WIN32 && !defined(WINVER)
#define WINVER 0x0600 // Require Windows NT5 (2K, XP, 2K3)
#endif

#if FLX_WIN32
// vs windows.h just LOVES to include winsock version 1 headers by default.
// that's bad for everyone, so quit it.
#define _WINSOCKAPI_

// windows.h defines min/max macros, which can cause all sorts of confusion.
#ifndef NOMINMAX
#define NOMINMAX
#endif
#endif


#if !defined(FLX_STATIC_LINK) && FLX_WIN32
#define FLX_EXPORT __declspec(dllexport)
#define FLX_IMPORT __declspec(dllimport)
#else
#define FLX_EXPORT __attribute__((visibility("default")))
#define FLX_IMPORT __attribute__((visibility("default"))) 
#endif


#ifdef BUILD_RTL
#define RTL_EXTERN FLX_EXPORT
#else
#define RTL_EXTERN FLX_IMPORT
#endif

#if FLX_MACOSX && !FLX_HAVE_DLOPEN
#define FLX_MACOSX_NODLCOMPAT 1
#else
#define FLX_MACOSX_NODLCOMPAT 0
#endif

#if FLX_HAVE_GNU
#define FLX_ALWAYS_INLINE __attribute__ ((always_inline))
#define FLX_NOINLINE __attribute__ ((noinline))
#define FLX_CONST __attribute__ ((const))
#define FLX_PURE __attribute__ ((pure))
#define FLX_GXX_PARSER_HACK (void)0,
#define FLX_UNUSED __attribute__((unused))
#else
#define FLX_ALWAYS_INLINE
#define FLX_NOINLINE
#define FLX_CONST
#define FLX_PURE
#define FLX_GXX_PARSER_HACK
#define FLX_UNUSED
#endif

#endif

