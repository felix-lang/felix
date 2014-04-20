#include "flx_judy_scanner.hpp"
#include <Judy.h>

namespace flx { namespace gc { namespace generic {

void *Judy1_scanner(collector_t *collector, gc_shape_t const *shape, void *pp, unsigned long dyncount, int reclimit)
{
  void *p = *(void**)pp;
  //printf("Scanning judy1 array %p->%p\n", pp, p);
  JError_t je;
  Word_t key = 0;
  int res = Judy1First(p, &key, &je);
  while(res) {
    //printf("Judy1 scanning p=%p\n",key); 
    collector->register_pointer((void*)key,reclimit);
    res = Judy1Next(p,&key, &je);
  }
  return 0;
}

void *JudyL_scanner(collector_t *collector, gc_shape_t const *shape, void *pp, unsigned long dyncount, int reclimit)
{
  void *p = *(void**)pp;
  //printf("Scanning judyL array %p->%p\n", pp, p);
  JError_t je;
  Word_t key = 0;
  Word_t *pval = 0;
  pval = (Word_t*)JudyLFirst(p, &key, &je);
  while(pval) {
    //printf("JudyL scanning p=%p\n",key); 
    collector->register_pointer((void*)key,reclimit);
    //printf("JudyL scanning p=%p\n",key); 
    collector->register_pointer((void*)*pval,reclimit);
    pval = (Word_t*)JudyLNext(p, &key, &je);
  }
  return 0;
}

void *JudySL_scanner(collector_t *collector, gc_shape_t const *shape, void *pp, unsigned long dyncount, int reclimit)
{
  void *p = *(void**)pp;
//  printf("Scanning judySL array %p->%p\n", pp, p);
  JError_t je;
  unsigned char *key = (unsigned char*)::std::malloc(10000); // HACK
  *key = 0;
  Word_t *pval = 0;
  pval = (Word_t*)JudySLFirst(p, key, &je);
  while(pval) {
    //printf("JudyL scanning p=%s, v=%p\n",key,*pval); 
    collector->register_pointer((void*)*pval,reclimit);
    pval = (Word_t*)JudySLNext(p, key, &je);
  }
  ::std::free(key);
  return 0;
}


}}} // end namespaces

