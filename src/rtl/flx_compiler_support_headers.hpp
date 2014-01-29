#ifndef __FLX_COMPILER_SUPPORT_HEADERS_H__
#define __FLX_COMPILER_SUPPORT_HEADERS_H__
#include "flx_rtl_config.hpp"
#if defined(FLX_PTF_STATIC_STRUCT) || defined(FLX_PTF_STATIC_PTR)
#error "FLX_PTF_STATIC_STRUCT and FLX_PTF_STATIC_PTR no longer supported"
#endif

#define PTF ptf->
#define FLX_POINTER_TO_THREAD_FRAME ptf

// for declarations in header file
#define FLX_FMEM_DECL thread_frame_t *ptf;
#define FLX_FPAR_DECL_ONLY thread_frame_t *_ptf
#define FLX_FPAR_DECL thread_frame_t *_ptf,
#define FLX_APAR_DECL_ONLY thread_frame_t *ptf
#define FLX_APAR_DECL thread_frame_t *ptf,
#define FLX_DCL_THREAD_FRAME

#if FLX_CGOTO
  #define FLX_LOCAL_LABEL_VARIABLE_TYPE void*
  #define FLX_PC_DECL void *pc;
#else
  #define FLX_PC_DECL int pc;
  #define FLX_LOCAL_LABEL_VARIABLE_TYPE int
#endif

#define t typename
#define t2 t,t
#define t3 t,t,t
#define t4 t,t,t,t
#define p template <
#define s > struct
template <typename, int> struct _fix; // fixpoint
template <t,t> struct _ft;            // function
template <t,t> struct _cft;           // cfunction
template <t,int> struct _at;          // array
template <t> struct _pt;              // procedure
  p t2 s _tt2;                        // tuples
  p t3 s _tt3;
  p t4 s _tt4;
  p t,t4 s _tt5;
  p t2,t4 s _tt6;
  p t3,t4 s _tt7;
#undef t
#undef t2
#undef t3
#undef t4
#undef p
#undef s
#endif
