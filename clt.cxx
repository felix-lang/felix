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
// Indexing is from 0 on the left (big endian)

namespace helper {
  // Computations with parameter packs
  template<int j, class ...T>
  struct pack_divisor;

  // recurive case
  template<int j, class H, class ...T>
  struct pack_divisor<j, H, T...> {
    static constexpr Nat d() { 
      if (j == 0) return Product<T...>::size();
      else return pack_divisor<j - 1, T...>::d();
    }
  };

  // terminator case
  template<int j>
  struct pack_divisor<j> {
    static constexpr Nat d() { return 1; }
  };

}

// The divisor needed to compute projection j of a product
template<int j, class ...T>
struct product_divisor;

template<int j, class ...T>
struct product_divisor<j, Product<T...>> {
  static constexpr Nat dd() { return helper::pack_divisor<j,T...>::d(); }
};

namespace helper {
  // Computations with parameter packs
  template<int j, class ...T>
  struct pack_modulus;

  // recursive case
  template<int j, class H, class ...T>
  struct pack_modulus<j, H, T...> {
    static constexpr Nat m() { 
      if (j == 0) return Product<H>::size();
      else return pack_modulus<j - 1, T...>::m();
    }
  };

  // error terminator case 
  template<int j>
  struct pack_modulus<j> {
    static constexpr Nat m() { return -1; }
  };
}


// The modulus to compute projection j of a product
template<int j, class ...T>
struct product_modulus;

template<int j, class ...T>
struct product_modulus<j, Product<T...>> {
  static constexpr Nat mm() { return helper::pack_modulus<j,T...>::m(); }
};

// Projection
template<int j, class T>
struct projection;

template<int j, class ...T>
struct projection<j, Product<T...>> {
  using P = Product<T...>;

  static constexpr Nat prj (P x) { 
    return x.rep /
      helper::pack_divisor<j,T...>::d() %
      helper::pack_modulus<j,T...>::m()
    ; 
  }
};



int main() {
  constexpr auto one = Enum<3>(1);
  constexpr auto two = Enum<4>(2);
  constexpr auto three = Enum<5>(3);
  using CLT345 = Product<Enum<3>,Enum<4>,Enum<5>>;


 
  // 1,2,3 of type 3 * 4 * 5 should have rep
  // 1 * 20 + 2 * 5 + 3 = 33
  constexpr auto const t345_v123= CLT345 (one,two,three);

  double x[t345_v123.size()]; // prove its a constant

  ::std::cout << "Size=" << t345_v123.size() << ", Rep=" << t345_v123.rep << ::std::endl;

  // divisor for index 0 should be 20
  ::std::cout << "Divisor index 0=" << product_divisor<0, CLT345>::dd() << ::std::endl;
  ::std::cout << "Divisor index 1=" << product_divisor<1, CLT345>::dd() << ::std::endl;
  ::std::cout << "Divisor index 2=" << product_divisor<2, CLT345>::dd() << ::std::endl;

   // modulus for index 0 should be 3
  ::std::cout << "Modulus index 0=" << product_modulus<0, CLT345>::mm() << ::std::endl;
  ::std::cout << "Modulus index 1=" << product_modulus<1, CLT345>::mm() << ::std::endl;
  ::std::cout << "Modulus index 2=" << product_modulus<2, CLT345>::mm() << ::std::endl;

  // projection for index 0 should be 1
  ::std::cout << "Applied projection index 0=" << projection<0, CLT345>::prj (t345_v123) << ::std::endl;
  ::std::cout << "Applied projection index 1=" << projection<1, CLT345>::prj (t345_v123) << ::std::endl;
  ::std::cout << "Applied projection index 2=" << projection<2, CLT345>::prj (t345_v123) << ::std::endl;
 
}
