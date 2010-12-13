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


// ****************************************************************************
// Quick and dirty header file for use with old Judy.h without JudyHS defs
// May 2004 (dlb) - No copyright or license -- it is free period.

#include <stdint.h>

// ****************************************************************************
// JUDYHSL MACROS:

#define JHSI(PV,    PArray,   PIndex,   Count)                          \
        J_2P(PV, (&(PArray)), PIndex,   Count, JudyHSIns, "JudyHSIns")
#define JHSG(PV,    PArray,   PIndex,   Count)                          \
        (PV) = (Pvoid_t) JudyHSGet(PArray, PIndex, Count)
#define JHSD(Rc,    PArray,   PIndex,   Count)                          \
        J_2I(Rc, (&(PArray)), PIndex, Count, JudyHSDel, "JudyHSDel")
#define JHSFA(Rc,    PArray)                                            \
        J_0I(Rc, (&(PArray)), JudyHSFreeArray, "JudyHSFreeArray")

// ****************************************************************************
// JUDY memory interface to malloc() FUNCTIONS:

extern Word_t JUDY_EXTERN JudyMalloc(Word_t);               // words reqd => words allocd.
extern Word_t JUDY_EXTERN JudyMallocVirtual(Word_t);        // words reqd => words allocd.
extern void JUDY_EXTERN JudyFree(Pvoid_t, Word_t);        // block to free and its size in words.
extern void JUDY_EXTERN JudyFreeVirtual(Pvoid_t, Word_t); // block to free and its size in words.

// ****************************************************************************
// JUDYHS FUNCTIONS:

extern PPvoid_t JUDY_EXTERN JudyHSGet(       Pcvoid_t,  void *, Word_t);
extern PPvoid_t JUDY_EXTERN JudyHSIns(       PPvoid_t,  void *, Word_t, P_JE);
extern int JUDY_EXTERN JudyHSDel(       PPvoid_t,  void *, Word_t, P_JE);
extern Word_t JUDY_EXTERN JudyHSFreeArray( PPvoid_t,                  P_JE);

extern uint32_t JudyHashStr(                void *, Word_t);
