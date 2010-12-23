#ifndef __FLX_COMPILER_SUPPORT_BODIES_H__
#define __FLX_COMPILER_SUPPORT_BODIES_H__
#include "flx_compiler_support_headers.hpp"


#define FLX_SAVE_REGS \
  jmp_buf reg_save_on_stack; \
  setjmp (reg_save_on_stack)


#define FLX_EXEC_FAILURE(f,op,what) \
  throw ::flx::rtl::flx_exec_failure_t (f,op,what)

#define FLX_HALT(f,sl,sc,el,ec,s) \
  throw ::flx::rtl::flx_halt_t (::flx::rtl::flx_range_srcref_t(f,sl,sc,el,ec),__FILE__,__LINE__,s)

// note call should be trace(&v,...) however that requires
// compiler support to make a trace record for each tracepoint
// so we use NULL for now

#ifdef FLX_ENABLE_TRACE
#define FLX_TRACE(v,f,sl,sc,el,ec,s) \
  ::flx::rtl::flx_trace (NULL,::flx::rtl::flx_range_srcref_t(f,sl,sc,el,ec),__FILE__,__LINE__,s)
#else
#define FLX_TRACE(v,f,sl,sc,el,ec,s)
#endif

#define FLX_MATCH_FAILURE(f,sl,sc,el,ec) \
  throw ::flx::rtl::flx_match_failure_t (::flx::rtl::flx_range_srcref_t(f,sl,sc,el,ec),__FILE__,__LINE__)

#define FLX_ASSERT_FAILURE(f,sl,sc,el,ec) \
  throw ::flx::rtl::flx_assert_failure_t (::flx::rtl::flx_range_srcref_t(f,sl,sc,el,ec),__FILE__,__LINE__)

#define FLX_ASSERT2_FAILURE(f,sl,sc,el,ec,f2,sl2,sc2,el2,ec2) \
  throw ::flx::rtl::flx_assert2_failure_t (\
    ::flx::rtl::flx_range_srcref_t(f,sl,sc,el,ec),\
    ::flx::rtl::flx_range_srcref_t(f2,sl2,sc2,el2,sc2),\
    __FILE__,__LINE__)

#define FLX_RANGE_FAILURE(f,sl,sc,el,ec) \
  throw ::flx::rtl::flx_range_failure_t (::flx::rtl::flx_range_srcref_t(f,sl,sc,el,ec),__FILE__,__LINE__)

// for generated code in body file
#define INIT_PC pc=0;
    ///< interior program counter

#if FLX_CGOTO
  #define FLX_START_SWITCH if(pc)goto *pc;
  #define FLX_SET_PC(x) pc=&&case_##x;
  #define FLX_CASE_LABEL(x) case_##x:;
  #define FLX_DECLARE_LABEL(n,i,x) \
    extern void f##i##_##n##_##x(void) __asm__("l"#i"_"#n"_"#x);
  #define FLX_LABEL(n,i,x) x:\
    __asm__(".global l"#i"_"#n"_"#x);\
    __asm__("l"#i"_"#n"_"#x":");\
    __asm__(""::"g"(&&x));
  #define FLX_FARTARGET(n,i,x) (void*)&f##i##_##n##_##x
  #define FLX_END_SWITCH
#else
  #define FLX_START_SWITCH switch(pc){case 0:;
  #define FLX_SET_PC(x) pc=x;
  #define FLX_CASE_LABEL(x) case x:;
  #define FLX_DECLARE_LABEL(n,i,x)
  #define FLX_LABEL(n,i,x) case n: x:;
  #define FLX_FARTARGET(n,i,x) n
  #define FLX_END_SWITCH default: throw ::flx::rtl::flx_switch_failure_t(); }
#endif

#define FLX_RETURN \
{ \
  con_t *tmp = _caller; \
  _caller = 0; \
  return tmp; \
}

#define FLX_NEWP(x) new(*PTF gcp,x##_ptr_map,true)x

#define FLX_FINALISER(x) \
static void x##_finaliser(::flx::gc::generic::collector_t *, void *p){\
  ((x*)p)->~x();\
}

#if FLX_USE_REGPARM3 && FLX_HAVE_GNU_X86
#define FLX_REGPARM __attribute__((regparm(3)))
#else
#define FLX_REGPARM
#endif

#if defined(FLX_PTF_STATIC_STRUCT)
#define FLX_FMEM_INIT_ONLY
#define FLX_FMEM_INIT :
#define FLX_FPAR_PASS_ONLY
#define FLX_FPAR_PASS
#define FLX_APAR_PASS_ONLY
#define FLX_APAR_PASS
#define _PTF _ptf.
#define _PTFV
#define FLX_PASS_PTF 0
#define FLX_EAT_PTF(x)
#define FLX_DEF_THREAD_FRAME thread_frame_t ptf;
#elif defined(FLX_PTF_STATIC_POINTER)
#define FLX_FMEM_INIT_ONLY
#define FLX_FMEM_INIT :
#define FLX_FPAR_PASS_ONLY
#define FLX_FPAR_PASS
#define FLX_APAR_PASS_ONLY
#define FLX_APAR_PASS
#define _PTF _ptf->
#define _PTFV
#define FLX_PASS_PTF 0
#define FLX_EAT_PTF(x)
#define FLX_DEF_THREAD_FRAME thread_frame_t *ptf=0;
#else
#define FLX_FMEM_INIT_ONLY : ptf(_ptf)
#define FLX_FMEM_INIT : ptf(_ptf),
#define FLX_FPAR_PASS_ONLY ptf
#define FLX_FPAR_PASS ptf,
#define FLX_APAR_PASS_ONLY _ptf
#define FLX_APAR_PASS _ptf,
#define _PTF _ptf->
#define _PTFV _ptf
#define FLX_PASS_PTF 1
#define FLX_EAT_PTF(x) x
#define FLX_DEF_THREAD_FRAME
#endif

#if defined(FLX_PTF_STATIC_STRUCT)
#define FLX_FRAME_WRAPPERS(mname) \
extern "C" thread_frame_t *create_thread_frame(\
  ::flx::gc::generic::gc_profile_t *gcp\
) {\
  ptf.gcp = gcp;\
  return &ptf;\
}
#elif defined(FLX_PTF_STATIC_POINTER)
#define FLX_FRAME_WRAPPERS(mname) \
extern "C" thread_frame_t *create_thread_frame(\
  ::flx::gc::generic::gc_profile_t *gcp\
) {\
  mname::thread_frame_t *p = new(*gcp,mname::thread_frame_t_ptr_map,false) mname::thread_frame_t();\
  p->gcp = gcp;\
  ptf = p;\
  return p;\
}
#else
#define FLX_FRAME_WRAPPERS(mname) \
extern "C" FLX_EXPORT mname::thread_frame_t *create_thread_frame(\
  ::flx::gc::generic::gc_profile_t *gcp\
) {\
  mname::thread_frame_t *p = new(*gcp,mname::thread_frame_t_ptr_map,false) mname::thread_frame_t();\
  p->gcp = gcp;\
  return p;\
}
#endif

#if defined(FLX_PTF_STATIC_STRUCT)
#define FLX_START_WRAPPER(mname,x)\
extern "C" ::flx::rtl::con_t *flx_start(\
  mname::thread_frame_t *ptf,\
  int argc,\
  char **argv,\
  FILE *stdin_,\
  FILE *stdout_,\
  FILE *stderr_\
) {\
  ptf->argc = argc;\
  ptf->argv = argv;\
  ptf->flx_stdin = stdin_;\
  ptf->flx_stdout = stdout_;\
  ptf->flx_stderr = stderr_;\
  return (new(*ptf->gcp,mname::x##_ptr_map,false) \
    mname::x()) ->call(0);\
}
#elif defined(FLX_PTF_STATIC_POINTER)
#define FLX_START_WRAPPER(mname,x)\
extern "C" ::flx::rtl::con_t *flx_start(\
  mname::thread_frame_t *ptf,\
  int argc,\
  char **argv,\
  FILE *stdin_,\
  FILE *stdout_,\
  FILE *stderr_\
) {\
  ptf->argc = argc;\
  ptf->argv = argv;\
  ptf->flx_stdin = stdin_;\
  ptf->flx_stdout = stdout_;\
  ptf->flx_stderr = stderr_;\
  return (new(*ptf->gcp,mname::x##_ptr_map,false) \
    mname::x()) ->call(0);\
}
#else
#define FLX_START_WRAPPER(mname,x)\
extern "C" FLX_EXPORT ::flx::rtl::con_t *flx_start(\
  mname::thread_frame_t *ptf,\
  int argc,\
  char **argv,\
  FILE *stdin_,\
  FILE *stdout_,\
  FILE *stderr_\
) {\
  ptf->argc = argc;\
  ptf->argv = argv;\
  ptf->flx_stdin = stdin_;\
  ptf->flx_stdout = stdout_;\
  ptf->flx_stderr = stderr_;\
  return (new(*ptf->gcp,mname::x##_ptr_map,false) \
    mname::x(ptf)) ->call(0);\
}
#endif

#if defined(FLX_PTF_STATIC_STRUCT)
#define FLX_STACK_START_WRAPPER(mname,x)\
extern "C" ::flx::rtl::con_t *flx_start(\
  mname::thread_frame_t *ptf,\
  int argc,\
  char **argv,\
  FILE *stdin_,\
  FILE *stdout_,\
  FILE *stderr_\
) {\
  ptf->argc = argc;\
  ptf->argv = argv;\
  ptf->flx_stdin = stdin_;\
  ptf->flx_stdout = stdout_;\
  ptf->flx_stderr = stderr_;\
  mname::x().stack_call();\
  return 0;\
}
#elif defined(FLX_PTF_STATIC_POINTER)
#define FLX_STACK_START_WRAPPER(mname,x)\
extern "C" ::flx::rtl::con_t *flx_start(\
  mname::thread_frame_t *ptf,\
  int argc,\
  char **argv,\
  FILE *stdin_,\
  FILE *stdout_,\
  FILE *stderr_\
) {\
  ptf->argc = argc;\
  ptf->argv = argv;\
  ptf->flx_stdin = stdin_;\
  ptf->flx_stdout = stdout_;\
  ptf->flx_stderr = stderr_;\
  mname::x().stack_call();\
  return 0;\
}
#else
#define FLX_STACK_START_WRAPPER(mname,x)\
extern "C" FLX_EXPORT ::flx::rtl::con_t *flx_start(\
  mname::thread_frame_t *ptf,\
  int argc,\
  char **argv,\
  FILE *stdin_,\
  FILE *stdout_,\
  FILE *stderr_\
) {\
  ptf->argc = argc;\
  ptf->argv = argv;\
  ptf->flx_stdin = stdin_;\
  ptf->flx_stdout = stdout_;\
  ptf->flx_stderr = stderr_;\
  mname::x(ptf).stack_call();\
  return 0;\
}
#endif

#if defined(FLX_PTF_STATIC_STRUCT)
#define FLX_C_START_WRAPPER(mname,x)\
extern "C" ::flx::rtl::con_t *flx_start(\
  mname::thread_frame_t *ptf,\
  int argc,\
  char **argv,\
  FILE *stdin_,\
  FILE *stdout_,\
  FILE *stderr_\
) {\
  ptf->argc = argc;\
  ptf->argv = argv;\
  ptf->flx_stdin = stdin_;\
  ptf->flx_stdout = stdout_;\
  ptf->flx_stderr = stderr_;\
  mname::x();\
  return 0;\
}
#elif defined(FLX_PTF_STATIC_POINTER)
#define FLX_C_START_WRAPPER(mname,x)\
extern "C" ::flx::rtl::con_t *flx_start(\
  mname::thread_frame_t *ptf,\
  int argc,\
  char **argv,\
  FILE *stdin_,\
  FILE *stdout_,\
  FILE *stderr_\
) {\
  ptf->argc = argc;\
  ptf->argv = argv;\
  ptf->flx_stdin = stdin_;\
  ptf->flx_stdout = stdout_;\
  ptf->flx_stderr = stderr_;\
  mname::x();\
  return 0;\
}
#else
#define FLX_C_START_WRAPPER(mname,x)\
extern "C" FLX_EXPORT ::flx::rtl::con_t *flx_start(\
  mname::thread_frame_t *ptf,\
  int argc,\
  char **argv,\
  FILE *stdin_,\
  FILE *stdout_,\
  FILE *stderr_\
) {\
  ptf->argc = argc;\
  ptf->argv = argv;\
  ptf->flx_stdin = stdin_;\
  ptf->flx_stdout = stdout_;\
  ptf->flx_stderr = stderr_;\
  mname::x(ptf);\
  return 0;\
}
#endif

#endif
