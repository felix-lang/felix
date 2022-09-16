#include <iostream>

// compact linear types are constructed with just two combinators:
// Products and Sums of 0 or more arguments.

using Nat = unsigned long;

template<class T> 
constexpr Nat size(T) { return T::size(); }

template<class ...Args> 
struct Product;

template<class ...Args> 
struct Sum;

// Nullary cases
template<>
struct Product<> {
  static constexpr Nat size() { return 1; }
};

using Unit = Product<>;

template<>
struct Sum<> {
  static constexpr Nat size() { return 0; }
};

using Void = Sum<>;

// Unary cases

template<class T> // requires T to be a compact linear type
struct Product<T> {
  static constexpr Nat size() { return T::size(); }
  Nat const rep;
  constexpr Product(T rep_) : rep(rep_.rep) {}
};

template<class T> // requires T to be a compact linear type
struct Sum<T> {
  static constexpr Nat size() { return T::size(); }
  Nat const rep;
  constexpr Sum(T rep_) : rep(rep_.rep) {}
};

// Recursion

template<class N, class ...Args>
struct Product<N, Args...> {
  static constexpr Nat size() { return N::size() * Product<Args...>::size(); }
  Nat const rep;
  constexpr Product(N head, auto ...tail) : 
    rep(head.rep * Product<Args...>::size() + Product<Args...>(tail...).rep) 
  {}
};

template<class N, class ...Args>
struct Sum<N, Args...> {
  static constexpr Nat size() { return N::size() + Sum<Args...>::size(); }
  Nat const rep;
  constexpr Sum(N head, Sum<Args> ...tail) : 
    rep(head.rep * Sum<Args...>::size() + Sum<Args...>(tail...).rep) 
  {}
};

//====================================================
// Convenience constructors
// Note this ONLY works because C++ templates are broken
// What we actually want is a function constructing a sum of units

template<int N>
struct Enum {
  static constexpr Nat size() { return N; }
  Nat const rep;
  constexpr Enum(int n) : rep (n) {}
};

//====================================================
// Convenience notation: this is WRONG for run time, but is RIGHT for compile time
template<class ...T>
constexpr Product<T...> operator * (T ...x) { return Product<T...>(x...); }

template<class ...T>
constexpr Sum<T...> operator + (T ...x) { return Sum <T...>(x...); }
 
//====================================================
// Projections
// TODO!


int main() {
  constexpr auto one = Enum<3>(1);
  constexpr auto two = Enum<4>(2);
  constexpr auto three = Enum<5>(3);

 
  // 1,2,3 of type 3 * 4 * 5 should have rep
  // 1 * 20 + 2 * 5 + 3 = 33
  constexpr auto const n= Product<Enum<3>,Enum<4>,Enum<5>>  (one,two,three);
  auto xxx = one * two * three;

  double x[n.size()]; // prove its a constant

  ::std::cout << "Size=" << n.size() << ", Rep=" << n.rep << ::std::endl;
  ::std::cout << "Size=" << xxx.size() << ", Rep=" << xxx.rep << ::std::endl;
}
