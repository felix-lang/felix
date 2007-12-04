#include <stdio.h>
#include "flx_rtl.hpp"
#include "flx_collector.hpp"
#include "flx_dynlink.hpp"

using namespace std;
using namespace flx;

int main(int argc, char** argv)
{
  rtl::flx_dynlink_t library;
  rtl::flx_libinit_t instance;
  library.link(argc>1?argv[1]:"<static>");
  gc::collector::malloc_free allocator;
  gc::collector::flx_collector_t collector(&allocator, NULL);
  gc::generic::gc_profile_t *gcp = new flx::gc::generic::gc_profile_t(
    false,
    false,
    false,
    0,
    -1,
    -1,
    0.2,
    true,
    &collector
  );
  instance.create(&library, gcp,library.main_sym,argc,argv,stdin,stdout,stderr);
  rtl::con_t *top = instance.start_proc;
  while( top ) top = top->resume();
  return 0;
}
