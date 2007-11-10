#ifndef JUDY_EXTERN
#if defined(_WIN32) && !defined(FLX_STATIC_LINK)
#ifdef BUILD_JUDY
#define JUDY_EXTERN __declspec(dllexport)
#else
#define JUDY_EXTERN __declspec(dllimport)
#endif
#else
#define JUDY_EXTERN
#endif
#endif

/* here JU_WIN <=> MSVC CL build */
#ifdef _MSC_VER
#define JU_WIN
#endif


#ifdef JU_64BIT
#include "Judy1Tables64.c"
#else
#include "Judy1Tables32.c"
#endif


