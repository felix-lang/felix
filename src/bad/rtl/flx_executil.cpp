#line 3295 "./lpsrc/flx_rtl.pak"
#include "flx_executil.hpp"
namespace flx { namespace rtl { namespace executil {
void run(flx::rtl::con_t *p)
{
  while(p)
  {
    try { p=p->resume(); }
    catch (flx::rtl::con_t *x) { p = x; }
  }
}

}}}

