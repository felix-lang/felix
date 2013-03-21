#ifndef __FLX_META_H__
#define __FLX_META_H__
// GENERATED during config
// taken from BOOST

#include <cstddef>

// convert an rvalue to an lvalue
template<typename T>
T const &lvalue(T const &x)
{
  return x;
}

// this reinterpret cast works with rvalues too
template<typename T, typename U>
T &reinterpret(U const &x) {
  return reinterpret_cast<T&>(const_cast<U&>(x));
}

template<typename T> void destroy(T *p){ p->T::~T(); }

#endif

