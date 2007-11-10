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



// @(#) $Revision: 4.33 $ $Source: /judy/src/JudyCommon/JudyMalloc.c $
// ************************************************************************ //
//                    JUDY - Memory Allocater                             //
//                              -by-                                      //
//                       Douglas L. Baskins                               //
//                        Hewlett Packard                                 //
//                        Fort Collins, Co                                //
//                         (970) 229-2027                                 //
//                                                                        //
// ************************************************************************ //

// JUDY INCLUDE FILES
#include "Judy.h"

// ****************************************************************************
// J U D Y   M A L L O C
//
// Allocate RAM.  This is the single location in Judy code that calls
// malloc(3C).  Note:  JPM accounting occurs at a higher level.

Word_t JudyMalloc(
        Word_t Words)
{
        Word_t Addr;

        Addr = (Word_t) malloc(Words * sizeof(Word_t));
        return(Addr);

} // JudyMalloc()


// ****************************************************************************
// J U D Y   F R E E

void JudyFree(
        void * PWord,
        Word_t Words)
{
        (void) Words;
        free(PWord);

} // JudyFree()


// ****************************************************************************
// J U D Y   M A L L O C
//
// Higher-level "wrapper" for allocating objects that need not be in RAM,
// although at this time they are in fact only in RAM.  Later we hope that some
// entire subtrees (at a JPM or branch) can be "virtual", so their allocations
// and frees should go through this level.

Word_t JudyMallocVirtual(
        Word_t Words)
{
        return(JudyMalloc(Words));

} // JudyMallocVirtual()


// ****************************************************************************
// J U D Y   F R E E

void JudyFreeVirtual(
        void * PWord,
        Word_t Words)
{
        JudyFree(PWord, Words);

} // JudyFreeVirtual()

