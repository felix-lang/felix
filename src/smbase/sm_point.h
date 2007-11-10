// point.h            see license.txt for copyright and terms of use
// 2-dimensional point
// derived from Melee's prmtvs2.hpp

#ifndef __POINT_H
#define __POINT_H

#include "sm_typ.h"

// point defined over arbitrary underlying types
template <class num>
class TPoint {
public:
  num x, y;

public:
  TPoint() {}
  TPoint(num nx, num ny) { set(nx,ny); }
  TPoint(TPoint<num> const &obj) : x(obj.x), y(obj.y) {}

  TPoint const& operator = (TPoint<num> const &obj)
    { x=obj.x; y=obj.y; return *this; }

  void set(num nx, num ny) { x=nx; y=ny; }
  void get(num *gx, num *gy) const { *gx=x; *gy=y; }
  bool zero() const { return x==0 && y==0; }
  bool gtez() const { return x>=0 && y>=0; }

  TPoint<num> operator - () const
    { return TPoint<num>(-x, -y); }
  TPoint<num> absval() const
    { return TPoint<num>(x<0? -x : x, y<0? -y : y); }
      // don't call abs() because that requires stdlib.h
      // also, don't call the function "abs" because abs() is usually
      //   implemented as a macro

  TPoint<num> operator + (TPoint<num> const &obj) const
    { return TPoint<num>(x+obj.x, y+obj.y); }
  TPoint<num> operator - (TPoint<num> const &obj) const
    { return TPoint<num>(x-obj.x, y-obj.y); }
  num dotprod(TPoint<num> const &obj) const
    { return x*obj.x + y*obj.y; }

  // for the arithmetic functions of the form point <op> num, <op> num is
  // applied to x and y independently

  TPoint<num> operator * (num factor) const
    { return TPoint<num>(x*factor, y*factor); }
  TPoint<num> operator / (num factor) const
    { return TPoint<num>(x/factor, y/factor); }

  TPoint<num> operator * (TPoint<num> const &factor) const
    { return TPoint<num>(x*factor.x, y*factor.y); }
  TPoint<num> operator / (TPoint<num> const &factor) const
    { return TPoint<num>(x/factor.x, y/factor.y); }

  TPoint<num> operator += (TPoint<num> const &obj)
    { x+=obj.x; y+=obj.y; return *this; }
  TPoint<num> operator -= (TPoint<num> const &obj)
    { x-=obj.x; y-=obj.y; return *this; }
  TPoint<num> operator *= (num factor)
    { x*=factor; y*=factor; return *this; }
  TPoint<num> operator /= (num factor)
    { x/=factor; y/=factor; return *this; }

  bool operator == (TPoint<num> const &obj) const
    { return x==obj.x && y==obj.y; }
  bool operator != (TPoint<num> const &obj) const
    { return ! operator==(obj); }

  // use with care; note that each relation requires the relation to hold for
  // *both* x and y for it to be true (this is different than, for example, the
  // way STL does relations between pairs)
  bool operator > (TPoint<num> const &obj) const
    { return x>obj.x && y>obj.y; }
  bool operator >= (TPoint<num> const &obj) const
    { return x>=obj.x && y>=obj.y; }
  bool operator < (TPoint<num> const &obj) const
    { return x<obj.x && y<obj.y; }
  bool operator <= (TPoint<num> const &obj) const
    { return x<=obj.x && y<=obj.y; }
};


// common incarnations
typedef TPoint<int> point;
typedef TPoint<double> fpoint;


// and we can then define sm_stringBuilder output ops for them
class ELK_EXTERN sm_stringBuilder;
sm_stringBuilder& operator<< (sm_stringBuilder &sb, point const &pt);
sm_stringBuilder& operator<< (sm_stringBuilder &sb, fpoint const &pt);


// iterate: 0,0    1,0    2,0    ... x-1,0    and then
//          0,1    1,1    2,1    ... x-1,1    and then
//           .                          .
//           .                          .     (each line in succession)
//           .                          .
//          0,y-1  1,y-1  2,y-1  ... x-1,y-1  done
// can 'break' out of the loop at any time

#define FOREACH_point(size, var)   \
  if ((size).x > 0)                \
    for(point var(0,0); var.y < (size).y; ++var.x == (size).x && (var.x=0,var.y++))


#endif // ___POINT_H

