#include "flx_judy_scanner.hpp"
#include <Judy.h>

namespace flx { namespace gc { namespace generic {

void *Judy1_scanner(collector_t *collector, gc_shape_t *shape, void *p, unsigned long dyncount, int reclimit)
{
  JError_t je;
  Word_t key = 0;
  int res = Judy1First(p, &key, &je);
  while(res) {
    collector->register_pointer((void*)key,reclimit);
    res = Judy1Next(p,&key, &je);
  }
  return 0;
}

}}} // end namespaces

