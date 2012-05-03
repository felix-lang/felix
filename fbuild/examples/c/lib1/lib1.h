#ifndef __LIB1_H__
#define __LIB1_H__

#if defined(_WIN32) && !defined(STATIC_LINK)
#  if defined(BUILD_LIB1)
#    define LIB1_EXTERN __declspec(dllexport)
#  else
#    define LIB1_EXTERN __declspec(dllimport)
#  endif
#else
#  define LIB1_EXTERN
#endif

LIB1_EXTERN int fred1();

#endif
