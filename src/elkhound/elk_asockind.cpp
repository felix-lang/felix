// asockind.cc            see license.txt for copyright and terms of use
// code for asockind.h

#include "elk_asockind.h"
#include "sm_xassert.h"

sm_string toString(AssocKind k)
{
  static char const * const arr[NUM_ASSOC_KINDS] = {
    "AK_LEFT", "AK_RIGHT", "AK_NONASSOC"
  };
  xassert((unsigned)k < NUM_ASSOC_KINDS);
  return sm_string(arr[k]);
}


