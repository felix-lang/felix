#ifndef __FLX_COMPILER_SUPPORT_HEADERS_H__
#define __FLX_COMPILER_SUPPORT_HEADERS_H__
#include "flx_rtl_config.hpp"
#if defined(FLX_PTF_STATIC_STRUCT) && defined(FLX_PTF_STATIC_PTR)
#error defined(FLX_PTF_STATIC_STRUCT) && defined(FLX_PTF_STATIC_PTR)
#endif

#if defined(FLX_PTF_STATIC_STRUCT)
#define PTF ptf.
#define FLX_POINTER_TO_THREAD_FRAME (&ptf)
#elif defined(FLX_PTF_STATIC_POINTER)
#define PTF ptf->
#define FLX_POINTER_TO_THREAD_FRAME ptf
#else
#define PTF ptf->
#define FLX_POINTER_TO_THREAD_FRAME ptf
#endif

// for declarations in header file
#if defined(FLX_PTF_STATIC_STRUCT)
#define FLX_FMEM_DECL
#define FLX_FPAR_DECL_ONLY
#define FLX_FPAR_DECL
#define FLX_APAR_DECL_ONLY
#define FLX_APAR_DECL
#define FLX_DCL_THREAD_FRAME extern thread_frame_t ptf;
#elif defined(FLX_PTF_STATIC_POINTER)
#define FLX_FMEM_DECL
#define FLX_FPAR_DECL_ONLY
#define FLX_FPAR_DECL
#define FLX_APAR_DECL_ONLY
#define FLX_APAR_DECL
#define FLX_DCL_THREAD_FRAME extern thread_frame_t *ptf;
#else
#define FLX_FMEM_DECL thread_frame_t *ptf;
#define FLX_FPAR_DECL_ONLY thread_frame_t *_ptf
#define FLX_FPAR_DECL thread_frame_t *_ptf,
#define FLX_APAR_DECL_ONLY thread_frame_t *ptf
#define FLX_APAR_DECL thread_frame_t *ptf,
#define FLX_DCL_THREAD_FRAME
#endif

#if FLX_CGOTO
  #define FLX_PC_DECL void *pc;
#else
  #define FLX_PC_DECL int pc;
#endif

#endif
