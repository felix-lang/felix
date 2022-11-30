#include <cstdint>
#include <iostream>
#include <tuple>

using Nat = ::std::uint64_t;
constexpr Nat maxrep = Nat(uint32_t(-1));

template<Nat n> requires (n <= maxrep)
struct N {
  Nat rep;  // probably should be const ..

  // default constructor
  N() : rep(0) {}

  // constructor
  N(uint64_t x) : rep(x%n) {}

  // standard value semantics
  N(N const&) = default;
  N(N const&&) = default;
  N& operator=(N const&) = default;
  N& operator=(N const&&) = default;

  // number of values
  static constexpr Nat size() { return n; }

  // operations, note modulo performed by constructor
  constexpr N add(N x) const { return rep + x.rep; }
  constexpr N neg() const { return n - rep; }
  constexpr N sub(N x) const { return rep - x.rep + n; }
  constexpr N mul(N x) const { return rep * x.rep; }
  constexpr N quot(N x) const { return rep / x.rep; }
  constexpr N mod(N x) const { return rep % x.rep; }

  // comparisons
  constexpr bool eq( N x) const { return x.rep == rep; }
  constexpr bool lt( N x) const { return x.rep < rep; }
  constexpr bool le( N x) const { return x.rep <= rep; }
  constexpr bool ge( N x) const { return x.rep >= rep; }
  constexpr bool gt( N x) const { return x.rep > rep; }
  constexpr bool ne( N x) const { return x.rep != rep; }

  // iterators
  constexpr N succ () const { return rep + 1; }
  constexpr N pred () const { return rep + n - 1; }

  // oputput
  friend constexpr ::std::ostream &
    operator<<(::std::ostream &o, N x) { return o << x.rep << ":" << n; }
};

// functional forms: operators
template<Nat n> requires (n <= maxrep)
constexpr N<n> operator + (N<n> x, N<n> y) { return x.add(y); }

template<Nat n> requires (n <= maxrep)
constexpr N<n> operator - (N<n> x, N<n> y) { return x.sub(y); }

template<Nat n> requires (n <= maxrep) 
constexpr N<n> operator * (N<n> x, N<n> y) { return x.mul(y); }

template<Nat n> requires (n <= maxrep) 
constexpr N<n> operator / (N<n> x, N<n> y) { return x.quot(y); }

template<Nat n> requires (n <= maxrep) 
constexpr N<n> operator % (N<n> x, N<n> y) { return x.mod(y); }

template<Nat n> requires (n <= maxrep) 
constexpr N<n> operator - (N<n> x) { return x.neg(); }

// functional forms: comparisons
template<Nat n> requires (n <= maxrep) 
constexpr bool operator == (N<n> x, N<n> y) { return x.eq(y); }

template<Nat n> requires (n <= maxrep) 
constexpr bool operator != (N<n> x, N<n> y) { return x.ne(y); }

template<Nat n> requires (n <= maxrep) 
constexpr bool operator <  (N<n> x, N<n> y) { return x.lt(y); }

template<Nat n> requires (n <= maxrep) 
constexpr bool operator <= (N<n> x, N<n> y) { return x.le(y); }

template<Nat n> requires (n <= maxrep) 
constexpr bool operator >= (N<n> x, N<n> y) { return x.ge(y); }

template<Nat n> requires (n <= maxrep) 
constexpr bool operator >  (N<n> x, N<n> y) { return x.gt(y); }

// functional forms: iterators
template<Nat n> requires (n <= maxrep) 
constexpr N<n> succ (N<n> x) { return x.succ(); }

template<Nat n> requires (n <= maxrep) 
constexpr N<n> pred (N<n> x) { return x.pred(); }

//---------------------------------------
// products
template<class ...Args> 
struct Product;

namespace helper {
  // Computations with parameter packs
  template<Nat j, class ...T>
  struct pack_divisor;

  // recurive case
  template<Nat j, class H, class ...T>
  struct pack_divisor<j, H, T...> {
    static constexpr Nat d() { 
      if (j == 0) return Product<T...>::size();
      else return pack_divisor<j - 1, T...>::d();
    }
  };

  // terminator case
  template<Nat j>
  struct pack_divisor<j> {
    static constexpr Nat d() { return 1; }
  };

  // Computations with parameter packs
  template<Nat j, class ...T>
  struct pack_modulus;

  // recursive case
  template<Nat j, class H, class ...T>
  struct pack_modulus<j, H, T...> {
    static constexpr Nat m() { 
      if (j == 0) return H::size();
      else return pack_modulus<j - 1, T...>::m();
    }
  };

  // error terminator case  (division by zero)
  template<Nat j>
  struct pack_modulus<j> {
    static constexpr Nat m() { return 0; }
  };

  template <Nat j, class ...T>
  struct pack_prj;

  // recursive case
  template<Nat j, class H, class ...T> 
  struct pack_prj <j,H,T...> {
    static Nat prj( Nat x) { 
      if (j == 0) return x / Product<T...>::size() % Product<H>::size();
      else return pack_prj<j - 1, T...>::prj(x);
    }
  };

  template<Nat j, class T>
  struct pack_prj<j,T> {
    static Nat prj(Nat x) { return x % T::size();} 
  };

  // addition
  template <class ...T>
  struct add {
    static Nat fold(Nat, Nat, Nat);
  };

  template<class H>
  struct add<H> {
    static Nat fold(Nat acc, Nat x, Nat y) { 
      return acc + 
        (
          (x % H::size()) + 
          (y % H::size())
        ) % H::size () 
      ;
    }
  };

  template<class H, class ...T>
  struct add<H,T...> {
    static Nat fold(Nat acc, Nat x, Nat y) { 
      return acc + 
        (
          (x / Product<T...>::size() % H::size()) + 
          (y / Product<T...>::size() % H::size())
        ) % H::size() * Product<T...>::size() +
        add<T...>::fold(acc, x,y);
    }
  };

  // output
  template <class ...T>
  struct out {
    static ::std::ostream& put(::std::ostream&, Nat x);
  };

  template<class H>
  struct out<H> {
    static ::std::ostream& put(::std::ostream &o, Nat x) { 
      return o << H(x % H::size());
    }
  };

  template<class H, class ...T>
  struct out<H,T...> {
    static ::std::ostream& put(::std::ostream& o, Nat x) { 
      o << H(x / Product<T...>::size() % H::size()) << ", ";
      return out<T...>::put (o,x);
    }
  };
}

// Product

// Nullary case: type with one value, namely 0 so no representation is required
using Unit = Product<>;

template<>
struct Product<> {
  static constexpr Nat size() { return 1; }
  constexpr Product() {}
  Product add(Product x) const { return Product(); } 
  friend ::std::ostream& operator << (::std::ostream& o, Product x) { 
    return o << "{}";
  }
};


// Unary case

template<class T> // requires T to be a compact linear type
struct Product<T> {
  static constexpr Nat size() { return T::size(); }
  Nat const rep;
  constexpr Product(T rep_) : rep(rep_.rep) {}
  T add(T x) const { return (rep + x.rep) % T::size() ; } 
  friend ::std::ostream& operator << (::std::ostream& o, Product x) { 
    return o << "{" << x << "}";
  }
};

// Recursion

template<class H, class ...T>
struct Product<H, T...> {
  static constexpr Nat size() { return H::size() * Product<T...>::size(); }
  Nat const rep; // should be private
  Product(Nat x) : rep(x) {} // should be private ... 

  constexpr Product(H head, T ...tail) : 
    rep((head.rep % H::size())* Product<T...>::size() + Product<T...>(tail...).rep) 
  {}

  Product add(Product x) { return helper::add<H,T...>::fold(0,rep, x.rep); }

  friend ::std::ostream& operator << (::std::ostream& o, Product x) { 
    o << "{";
    helper::out<H,T...>::put(o,x.rep);
    return o << "}";
  }
};

// deduction guide
template<class H, class ...T>
Product(H, T...) -> Product<H,T...>;


// Projection
template<Nat j, class T>
struct projection;

template<Nat j, class ...T>
struct projection<j, Product<T...>> {
  using P = Product<T...>;
  using Dummy = ::std::tuple<T...>;
  using PrjT = typename ::std::tuple_element<j,Dummy>::type;
  static PrjT prj (P x) { return helper::pack_prj<j,T...>::prj(x.rep); }
};


int main() {
  ::std::cout << "Hello world" << ::std::endl;
  N<16> x{9};
  ::std::cout << x << ", " << x + x << ::std::endl;
 
  // product
  N<3> x3_2{2};
  N<2> x2_1{1};
  using P32 = Product<N<3>,N<2>>;

  P32 x32_21{x3_2,x2_1};
  ::std::cout << "x32_21="<< x32_21 << ::std::endl;

  // apply projections
  N<3> c0 = projection<0,P32>::prj(x32_21);
  N<2> c1 = projection<1,P32>::prj(x32_21);

  // print components
  ::std::cout << "x32_21.prj0="<< c0 << ::std::endl;
  ::std::cout << "x32_21:prj1="<< c1 << ::std::endl;

  // addition
  ::std::cout << "x32_21+x32_21=" << (x32_21.add(x32_21)) << ::std::endl;
  ::std::cout << x32_21 << " + " << x32_21 <<" = " << (x32_21.add(x32_21)) << ::std::endl;

  // OK lets get messy!!!

 auto messy = Product/*<P32, P32>*/ {x32_21, x32_21};
 ::std::cout << messy << ::std::endl;
 ::std::cout << messy.add(messy) << ::std::endl;
 
}
