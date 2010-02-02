#include "flx_i18n.hpp"
namespace flx { namespace rtl { namespace i18n {
  std::string utf8(unsigned long i)
  {
    char s[7];
    if (i < 0x80UL )
    {
      s[0]= i;
      s[1]= 0;
    }
    else if (i < 0x800UL )
    {
      s[0]=0xC0u | (i >> 6ul)  & 0x1Fu;
      s[1]=0x80u | i           & 0x3Fu;
      s[2]=0;
    }
    else if (i < 0x10000UL )
    {
      s[0]=0xE0u | (i >> 12ul) & 0xFu;
      s[1]=0x80u | (i >> 6ul)  & 0x3Fu;
      s[2]=0x80u | i           & 0x3F;
      s[3]=0;
    }
    else if (i < 0x200000UL )
    {
      s[0]=0xF0u | (i >> 18ul) & 0x7u;
      s[1]=0x80u | (i >> 12ul) & 0x3Fu;
      s[2]=0x80u | (i >> 6ul)  & 0x3Fu;
      s[3]=0x80u | i           & 0x3F;
      s[4]=0;
    }
    else if (i < 0x4000000UL )
    {
      s[0]=0xF8u | (i >> 24ul) & 0x3u;
      s[1]=0x80u | (i >> 18ul) & 0x3Fu;
      s[2]=0x80u | (i >> 12ul) & 0x3Fu;
      s[3]=0x80u | (i >> 6ul)  & 0x3Fu;
      s[4]=0x80u | i           & 0x3Fu;
      s[5]=0;
    }
    else
    {
      s[0]=0xFCu | (i >> 30ul) & 0x1u;
      s[1]=0x80u | (i >> 24ul) & 0x3Fu;
      s[2]=0x80u | (i >> 18ul) & 0x3Fu;
      s[3]=0x80u | (i >> 12ul) & 0x3Fu;
      s[4]=0x80u | (i >> 6ul)  & 0x3Fu;
      s[5]=0x80u | i           & 0x3Fu;
      s[6]=0;
    }
    return s;
  }
}}}
