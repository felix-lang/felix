#include "flx_serialisers.hpp"
#include <string>
namespace flx { namespace gc { namespace generic {

// This is an encoder for a primitive string.
::std::string string_encoder (void *p)
{
  return *(::std::string*)p;
}

// This is NOT an encoder. It's a utility wrapper which
// takes a variable length string and returns another
// string prefixed by the length.
//
// This function is applied to all user defined encoders,
// to get a length managed serialisation.
::std::string string_blit (::std::string const &s) 
{
  size_t n = s.size();
  ::std::string b = blit (&n, sizeof(size_t));
  b+=s;
  return b;
}

// This is a utility for encoding a pod of size n.
// We don't need a length because it is statically known.
::std::string blit (void *p, size_t n) {
  return ::std::string((char*)p,n);
}

size_t string_decoder (void *p, char const *s, size_t i)
{
   size_t n;
   memcpy (&n,s + i,sizeof(size_t));
   new (p) ::std::string(s+i+sizeof(size_t), n);
   return i + sizeof(size_t) + n;
}

size_t unblit (void *p, size_t n, char const *s, size_t i)
{
  memcpy (p,s+i,n);
  return i + n;
}

}}}

