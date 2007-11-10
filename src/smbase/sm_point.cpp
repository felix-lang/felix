// point.cc            see license.txt for copyright and terms of use
// code for point.h

#include "sm_point.h"
#include "sm_str.h"

sm_stringBuilder& operator<< (sm_stringBuilder &sb, point const &pt)
{
  return sb << "(" << pt.x << ", " << pt.y << ")";
}

sm_stringBuilder& operator<< (sm_stringBuilder &sb, fpoint const &pt)
{
  return sb << "(" << pt.x << ", " << pt.y << ")";
}
