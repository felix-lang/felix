// @(#) From generation tool: $Revision: 4.37 $ $Source: /judy/src/JudyCommon/JudyTables.c $
//

// 32 bit tables
#define JUDY1
#include "Judy1.h"

#ifndef JU_64BIT
// Leave the malloc() sizes readable in the binary (via strings(1)):
const char * Judy1MallocSizes = "Judy1MallocSizes = 3, 5, 7, 11, 15, 23, 32, 47, 64, Leaf1 = 20";


//	object uses 64 words
//	cJU_BITSPERSUBEXPB = 32
const uint8_t
j__1_BranchBJPPopToWords[cJU_BITSPERSUBEXPB + 1] =
{
	 0,
	 3,  5,  7, 11, 11, 15, 15, 23, 
	23, 23, 23, 32, 32, 32, 32, 32, 
	47, 47, 47, 47, 47, 47, 47, 64, 
	64, 64, 64, 64, 64, 64, 64, 64
};

//	object uses 5 words
//	cJ1_LEAF1_MAXPOP1 = 20
const uint8_t
j__1_Leaf1PopToWords[cJ1_LEAF1_MAXPOP1 + 1] =
{
	 0,
	 3,  3,  3,  3,  3,  3,  3,  3, 
	 3,  3,  3,  3,  5,  5,  5,  5, 
	 5,  5,  5,  5
};

//	object uses 32 words
//	cJ1_LEAF2_MAXPOP1 = 64
const uint8_t
j__1_Leaf2PopToWords[cJ1_LEAF2_MAXPOP1 + 1] =
{
	 0,
	 3,  3,  3,  3,  3,  3,  5,  5, 
	 5,  5,  7,  7,  7,  7, 11, 11, 
	11, 11, 11, 11, 11, 11, 15, 15, 
	15, 15, 15, 15, 15, 15, 23, 23, 
	23, 23, 23, 23, 23, 23, 23, 23, 
	23, 23, 23, 23, 23, 23, 32, 32, 
	32, 32, 32, 32, 32, 32, 32, 32, 
	32, 32, 32, 32, 32, 32, 32, 32
};

//	object uses 32 words
//	cJ1_LEAF3_MAXPOP1 = 42
const uint8_t
j__1_Leaf3PopToWords[cJ1_LEAF3_MAXPOP1 + 1] =
{
	 0,
	 3,  3,  3,  3,  5,  5,  7,  7, 
	 7, 11, 11, 11, 11, 11, 15, 15, 
	15, 15, 15, 15, 23, 23, 23, 23, 
	23, 23, 23, 23, 23, 23, 32, 32, 
	32, 32, 32, 32, 32, 32, 32, 32, 
	32, 32
};

//	object uses 32 words
//	cJ1_LEAFW_MAXPOP1 = 31
const uint8_t
j__1_LeafWPopToWords[cJ1_LEAFW_MAXPOP1 + 1] =
{
	 0,
	 3,  3,  5,  5,  7,  7, 11, 11, 
	11, 11, 15, 15, 15, 15, 23, 23, 
	23, 23, 23, 23, 23, 23, 32, 32, 
	32, 32, 32, 32, 32, 32, 32
};
#endif /* JU_64BIT check */
