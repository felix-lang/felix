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



// @(#) $Revision: 4.5 $ $Source: /judy/src/JudyCommon/JudyMemUsed.c $
//
// Return number of bytes of memory used to support a Judy1/L array.
// Compile with one of -DJUDY1 or -DJUDYL.

#if (! (defined(JUDY1) || defined(JUDYL)))
#error:  One of -DJUDY1 or -DJUDYL must be specified.
#endif

#ifdef JUDY1
#include "Judy1.h"
#else
#include "JudyL.h"
#endif

#include "JudyPrivate1L.h"

#ifdef JUDY1
FUNCTION Word_t JUDY_EXTERN Judy1MemUsed
#else  // JUDYL
FUNCTION Word_t JUDY_EXTERN JudyLMemUsed
#endif
        (
        Pcvoid_t PArray         // from which to retrieve.
        )
{
        Word_t   Words = 0;

        if (PArray == (Pcvoid_t) NULL) return(0);

        if (JU_LEAFW_POP0(PArray) < cJU_LEAFW_MAXPOP1) // must be a LEAFW
        {
            Pjlw_t Pjlw = P_JLW(PArray);                // first word of leaf.
            Words = JU_LEAFWPOPTOWORDS(Pjlw[0] + 1);    // based on pop1.
        }
        else
        {
            Pjpm_t Pjpm = P_JPM(PArray);
            Words = Pjpm->jpm_TotalMemWords;
        }

        return(Words * sizeof(Word_t));         // convert to bytes.

} // Judy1MemUsed() / JudyLMemUsed()

