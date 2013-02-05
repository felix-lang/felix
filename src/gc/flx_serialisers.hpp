#include "flx_gc.hpp"
namespace flx { namespace gc { namespace generic {
GC_EXTERN encoder_t string_encoder;
GC_EXTERN decoder_t string_decoder;

GC_EXTERN ::std::string blit (void *, size_t);
GC_EXTERN size_t unblit (void *, size_t, char const*, size_t);

GC_EXTERN ::std::string string_blit (::std::string const&);
}}}

