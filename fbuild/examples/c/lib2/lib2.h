#ifndef __LIB2_H__
#define __LIB2_H__

#if defined(_WIN32) && !defined(STATIC_LINK)
#  if defined(BUILD_LIB2)
#    define LIB2_EXTERN __declspec(dllexport)
#  else
#    define LIB2_EXTERN __declspec(dllimport)
#  endif
#else
#  define LIB2_EXTERN
#endif

LIB2_EXTERN int fred2();

#endif
