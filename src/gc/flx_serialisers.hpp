#ifndef __FLX_SERIALISERS_HPP__
#define __FLX_SERIALISERS_HPP__

#include "flx_gc.hpp"
namespace flx { namespace gc { namespace generic {
GC_EXTERN encoder_t string_encoder;
GC_EXTERN decoder_t string_decoder;

GC_EXTERN ::std::string blit (void *, size_t);
GC_EXTERN size_t unblit (void *, size_t, char*, size_t);

GC_EXTERN ::std::string string_blit (::std::string const&);

template<class T> 
::std::string tblit(void *p) 
{
  return blit (p, sizeof(T));
}

template<class T> 
size_t tunblit(void *p, char *s, size_t i) 
{
  return unblit (p, sizeof(T), s, i);
}


}}}

#endif

