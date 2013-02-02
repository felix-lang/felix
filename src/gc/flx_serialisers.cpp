#include "flx_serialisers.hpp"
#include <string>
namespace flx { namespace gc { namespace generic {
::std::string string_encoder (void *p)
{
  return *(::std::string*)p;
}
}}}

