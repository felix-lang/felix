// judytest.cpp : Defines the entry point for the console application.
//
#include <stdio.h>
#include "judy.h"
#include <stdlib.h>
void insertlots(void **pja, JError_t *pjerr)
{
	uintptr_t key;
	for (auto i = 0; i < 10000; ++i)
	{
		key = (uintptr_t)rand();
		Word_t *ptr_dataslot = (Word_t*)(void*)JudyLIns(pja, key, pjerr);
		*ptr_dataslot = key;
	}
}
void findsome(void *ja, JError_t *pjerr)
{
	int miss = 0;
	int hit = 0;
	uintptr_t key;
	uintptr_t data;
	for (auto i = 0; i < 10000; ++i)
	{
		key = (uintptr_t)rand();
		void **result = JudyLGet(ja, key, pjerr);
		if (result == NULL) ++miss;
		else ++hit;
	}
	printf("Had %d hits and %d misses\n", hit, miss);
}
int main()
{
	printf("Judy Test Starts, randmax = %d\n", RAND_MAX);
	JError_t jerr;
	void *ja = NULL;
	insertlots(&ja,&jerr);
	findsome(ja, &jerr);
	printf("Judy Test Ends\n");
    return 0;
}

