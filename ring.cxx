#include <cstdint>
#include <iostream>
#include <tuple>
#include <string>

// SYNOPSIS
//
// We define data structures which form finite commuitative rings with 
//
// * addition, negation, subtraction and multiplication as usual
// * integer division and modulus
// * total ordering
// * single machine word representation
//
// We start with N<n> which is a subrange of integers 0..n-1
//
// We then define products (tuples) with arithmetic componentwise
// and lexicographical total ordering and provide projection functions
// to extract components,
//
// and sums (variants) with injection functions to construct them
// decoding a sum can be expensive: a reference linear decoding
// is provided and for small sums constant time decoding can
// be done with an array the size of the sum
//
// Total ordering is defined lexicographically as for products
//
// Arithmetic is defined as if the rings with packed next to each other,
// For example N<n> + N<m> is treated as if it were N<n+m>.
//
// clang bugs!!

// consteval strangely fails calling size() in succ and pred everywhere for no good reason at all
// it works everywhere else
//https://stackoverflow.com/questions/63364918/clang-says-call-to-void-consteval-function-is-not-a-constant-expression
#define consteval constexpr

// default spaceship operator<=> doesn't work in clang 
// so we're forced to hand define all 6 comparisons every time

using Nat = ::std::uint64_t;
constexpr Nat maxrep = Nat(uint32_t(-1));

// These templates map a template with a name X to a call of a method named X
// This is required to pass the operation as a template template argument
// so as to avoid any run time overhead in higher order polymorphic functions

namespace helper {
  template<class T>
  struct add { static T op(T x, T y) { return x.add(y); }};
  template<class T>
  struct sub { static T op(T x, T y) { return x.sub(y); }};
  template<class T>
  struct mul { static T op(T x, T y) { return x.mul(y); }};
  template<class T>
  struct div { static T op(T x, T y) { return x.div(y); }};
  template<class T>
  struct mod { static T op(T x, T y) { return x.mod(y); }};
  template<class T>
  struct neg { static T op(T x) { return x.neg(); }};
}

// The most basic rings N<n> are subranges of the natural
// numbers (unsigned integers) from 0 to n-1
// All the operations are purely functional.
// The representation is uint64_t and the value of n is constrained
// to be at most the maximum value of uint32_t to ensure multiplication
// does not overflow
//
// FIXME: N<0> and N<1> are special cases and should be
// handled specially
//
// N0 is the empty range, it has no values, and so no
// operations requiring values are permitted: it has size 0
//
// N1 has only one value, namely 0, and so as an optimisation
// does not require a representation
//
// Addition, subtraction and multiplication are well defined
// However division and modulus necessarily fail for all RHS values
// since the only value is 0 so these should throw a division by zero
// exception!
//
// For all n, division and modulus by zero also fail
// These should throw division by zero exception,
// At present, we rely on the standard division by zero exception to be thrown
//


template<Nat n> requires (n <= maxrep)
class N {
  Nat rep;  // probably should be const ..
public:
  static ::std::string type_name() { return "N<"+::std::to_string(n)+">"; }

  Nat get() const { return rep; }

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
  static consteval Nat size() { return n; }

  // operations, note modulo performed by constructor
  constexpr N add(N x) const { return rep + x.rep; }
  constexpr N neg() const { return n - rep; }
  constexpr N sub(N x) const { return rep - x.rep + n; }
  constexpr N mul(N x) const { return rep * x.rep; }
  constexpr N div (N x) const { return rep / x.rep; }
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

  ::std::string repr() const { return type_name() + "{" + ::std::to_string (get()) + "}"; }
};

template<Nat n> 
struct ring_iterator {
  N<n> v;
  bool ended;

  ring_iterator() : v{0}, ended(false) {}
  ring_iterator(int) : v{0}, ended(true) {}

  N<n> operator*(){ return v; }
  void operator++() { v = v.succ(); ended = v==N<n>{0}; }
  friend int operator <=>(ring_iterator<n>,ring_iterator<n>)=default;

  static ring_iterator begin() { return ring_iterator(); }
  static ring_iterator end() { return ring_iterator(N<n>::size()); }


  struct all {
     all() {}
     ring_iterator begin() { return ring_iterator::begin(); }
     ring_iterator end() { return ring_iterator::end(); }
  };

};
//---------------------------------------
// products
template<class...> 
struct Product;

// Sums
template<class...>
struct Sum;

namespace helper {
  // PROJECTIONS
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

  template<Nat j>
  struct pack_prj<j> {
    static Nat prj(Nat x) { throw "Invalid projection index"; } 
  };

  // INJECTIONS
  template <Nat j, class ...T>
  struct pack_inj;

  // recursive case
  template<Nat j, class H, class ...T> 
  struct pack_inj <j,H,T...> {
    static Nat inj( Nat x) { 
      if (j == 0) return x + Sum<T...>::size();
      else return pack_inj<j - 1, T...>::inj(x);
    }
  };

  template<Nat j>
  struct pack_inj<j> {
    static Nat inj(Nat x) { throw "Invalid injection index"; } 
  };

  template<Nat j, class ...T>
  struct caseno;

  template<Nat j, class H, class ...T>
  struct caseno<j, H, T...> {
    static Nat idx(Nat x) {
      if(x >= Sum<T...>::size()) return j;
      else return caseno<j + 1, T...>::idx(x);
    }
  };

  template<Nat j, class...>
  struct caseno {
    static Nat idx(Nat x) { throw "Impossible caseno runout"; }
  };


  template<Nat j, class ...T>
  struct caseval;

  template<Nat j, class H, class ...T>
  struct caseval<j, H, T...> {
    static Nat val(Nat x) {
      if(x >= Sum<T...>::size()) return x - Sum<T...>::size();
      else return caseval<j + 1, T...>::val(x);
    }
  };

  template<Nat j, class...>
  struct caseval {
    static Nat val(Nat x) { throw "Impossible caseval runout"; }
  };


  //Unary operators 
  template <template<class> class op, class ...T>
  struct unop{
    static Nat mfold(Nat, Nat);
    static Nat afold(Nat, Nat);
  };

  template<template<class> class op>
  struct unop<op> {
    static Nat mfold(Nat acc, Nat x) { return acc; }
    static Nat afold(Nat acc, Nat x) { return acc; }
  };

  template<template<class> class op, class H, class ...T>
  struct unop<op,H,T...> {
    static Nat mfold(Nat acc, Nat x) { 
      Nat LHS = x / Product<T...>::size() % H::size();
      H R = op<H>::op(H{LHS});
      Nat Rscaled = R.get() * Product<T...>::size();
      Nat Nuacc = acc + Rscaled;
      return unop<op,T...>::mfold(Nuacc, x);
    }
    static Nat afold(Nat acc, Nat x) { 
      Nat LHS = x - Sum<T...>::size() % H::size();
      H R = op<H>::op(H{LHS});
      Nat Rscaled = R.get() + Sum<T...>::size();
      Nat Nuacc = acc + Rscaled;
      return unop<op,T...>::afold(Nuacc, x);
    }
  };

  //binary operators 
  template <template<class> class op, class ...T>
  struct binop{
    static Nat mfold(Nat, Nat, Nat);
    static Nat afold(Nat, Nat, Nat);
  };

  template<template<class> class op>
  struct binop<op> {
    static Nat mfold(Nat acc, Nat x, Nat y) { return acc; }
    static Nat afold(Nat acc, Nat x, Nat y) { return acc; }
  };

  template<template<class> class op, class H, class ...T>
  struct binop<op,H,T...> {
    static Nat mfold(Nat acc, Nat x, Nat y) { 
      Nat LHS = x / Product<T...>::size() % H::size();
      Nat RHS = y / Product<T...>::size() % H::size();
      H R = op<H>::op(H{LHS}, H{RHS});
      Nat Rscaled = R.get() * Product<T...>::size();
      Nat Nuacc = acc + Rscaled;
      return binop<op,T...>::mfold(Nuacc, x,y);
    }
    static Nat afold(Nat acc, Nat x, Nat y) { 
      Nat LHS = x - Sum<T...>::size() % H::size();
      Nat RHS = y - Sum<T...>::size() % H::size();
      H R = op<H>::op(H{LHS}, H{RHS});
      Nat Rscaled = R.get() + Sum<T...>::size();
      Nat Nuacc = acc + Rscaled;
      return binop<op,T...>::afold(Nuacc, x,y);
    }

  };

  // output for type name lists
  template <class ...T>
  struct tname;

  template<class H>
  struct tname<H> {
    static ::std::string tnam () { return H::type_name(); }
  };

  template<class H, class ...T>
  struct tname<H,T...> {
    static ::std::string tnam() { return H::type_name()+ ", " + tname<T...>::tnam(); }
  };

  template<class ...>
  struct repr;

   template<class H>
   struct repr<H> {
     static ::std::string prepr(Nat x) { return H{x % H::size()}.repr(); }
   };

   template<class H, class ...T>
   struct repr<H,T...> {
     static ::std::string prepr(Nat x) {
       return  H{x / Product<T...>::size() % H::size()}.repr() + "," + repr<T...>::prepr(x);  }
   };

  template<int cur, class...>
  struct sumarg;

  template<int cur> 
  struct sumarg<cur> { 
    static ::std::string arg(int,Nat) { throw "impossible"; }
  };

  template<int cur, class H, class ...T>
  struct sumarg<cur,H,T...> {
    static ::std::string arg(int req, Nat rep) {
      if(cur == req) return H{rep - Sum<T...>::size()}.repr();
      else return sumarg<cur + 1, T...>::arg(req, rep);
    }
  };

  // output for products
  template <class ...T>
  struct out;

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

// Cartesian Product
// 
// Products of rings are rings with compponent wise arithmetic
//

// Nullary case: type with one value, namely 0 so no representation is required
using Unit = Product<>;

// Unit is isomorphic to N<1>
// So we might think about conversions .. however the behavour is the same.
// 

template<>
struct Product<> {
  static consteval Nat size() { return 1; }
  constexpr Product() {}
  static ::std::string type_name() { return "Product<>"; }

  Nat get() const { return 0; }

  Product add(Product x) const { return Product(); } 
  Product sub(Product x) const { return Product(); } 
  Product mul(Product x) const { return Product(); } 
  Product div(Product x) const { return Product(); } 
  Product mod(Product x) const { return Product(); } 
  Product neg() const { return Product(); } 
  
  Product succ() const { return Product(); }
  Product pred() const { return Product(); }

  friend ::std::ostream& operator << (::std::ostream& o, Product x) { 
    return o << "{}";
  }
};

// Recursion

template<class H, class ...T>
class Product<H, T...> {
  Nat rep; // should be private
public:
  static consteval Nat size() { return H::size() * Product<T...>::size(); }
  static ::std::string type_name() { return "Product<"+helper::tname<H,T...>::tnam() +">"; }

  Nat get() const { return rep; }

  Product(Nat x) : rep(x) {} // should be private ... 

  constexpr Product(H head, T ...tail) : 
    rep((head.get() % H::size())* Product<T...>::size() + Product<T...>(tail...).get()) 
  {}

  Product add(Product x) const { return helper::binop<helper::add,H,T...>::mfold(0,rep, x.rep); }
  Product sub(Product x) const { return helper::binop<helper::sub,H,T...>::mfold(0,rep, x.rep); }
  Product mul(Product x) const { return helper::binop<helper::mul,H,T...>::mfold(0,rep, x.rep); }
  Product div(Product x) const { return helper::binop<helper::div,H,T...>::mfold(0,rep, x.rep); }
  Product mod(Product x) const { return helper::binop<helper::mod,H,T...>::mfold(0,rep, x.rep); }
  Product neg() const { return helper::unop<helper::neg,H,T...>::mfold(0,rep); }

  Product succ() const { return Product((rep + 1) % size()); }
  Product pred() const { return Product((rep + size() - 1) % size()); }

  // comparisons
  constexpr bool eq(Product x) const { return x.rep == rep; }
  constexpr bool lt(Product x) const { return x.rep < rep; }
  constexpr bool le(Product x) const { return x.rep <= rep; }
  constexpr bool ge(Product x) const { return x.rep >= rep; }
  constexpr bool gt(Product x) const { return x.rep > rep; }
  constexpr bool ne(Product x) const { return x.rep != rep; }


  friend ::std::ostream& operator << (::std::ostream& o, Product x) { 
    o << "{";
    helper::out<H,T...>::put(o,x.rep);
    return o << "}";
  }
  ::std::string repr() const { return "Product{" + helper::repr<H,T...>::prepr(rep) + "}"; }
};


template<>
struct Sum<> {
  static consteval Nat size() { return 0; }
  constexpr Sum() {}
};

using Void = Sum<>;

template<class H, class ...T>
class Sum<H, T...> {
  Nat rep; // should be private
public:
  static consteval Nat size() { return H::size() + Sum<T...>::size(); }
  static ::std::string type_name() { return "Sum<"+helper::tname<H,T...>::tnam() +">"; }

  Nat get() const { return rep; }

  Sum(Nat x) : rep(x) {} // should be private ... 

  Nat caseno() const { return helper::caseno<0,H,T...>::idx(rep); }
  Nat caseval() const { return helper::caseval<0,H,T...>::val(rep); }
  
  Sum add(Sum x) const { return helper::binop<helper::add,H,T...>::afold(0,rep, x.rep); }
  Sum sub(Sum x) const { return helper::binop<helper::sub,H,T...>::afold(0,rep, x.rep); }
  Sum mul(Sum x) const { return helper::binop<helper::mul,H,T...>::afold(0,rep, x.rep); }
  Sum div(Sum x) const { return helper::binop<helper::div,H,T...>::afold(0,rep, x.rep); }
  Sum mod(Sum x) const { return helper::binop<helper::mod,H,T...>::afold(0,rep, x.rep); }
  Sum neg() const { return helper::unop<helper::neg,H,T...>::afold(0,rep); }

  Sum succ() const { Nat x = size(); return Sum((rep + 1) % x); }
  Sum pred() const { return Sum((rep + size() - 1) % size()); }

  // comparisons
  constexpr bool eq(Sum x) const { return x.rep == rep; }
  constexpr bool lt(Sum x) const { return x.rep < rep; }
  constexpr bool le(Sum x) const { return x.rep <= rep; }
  constexpr bool ge(Sum x) const { return x.rep >= rep; }
  constexpr bool gt(Sum x) const { return x.rep > rep; }
  constexpr bool ne(Sum x) const { return x.rep != rep; }


  friend ::std::ostream& operator << (::std::ostream& o, Sum x) { 
    o << "<";
    helper::out<H,T...>::put(o,x.rep);
    return o << ">";
  }

  // NOTE: it's impossible to write the code to show the correctly typed argument
  // because that needs a *runtime* switch over the caseno.
  // we CAN show the representation though.. there's a constructor for it
  // but although it's public at the moment, it should be private
  ::std::string repr() const { return "injection<" +
    ::std::to_string (caseno()) + ", " + type_name() + 
    ">::inj{"+helper::sumarg<0,H,T...>::arg(caseno(), rep)+"}";
  }
};


// deduction guide
template<class H, class ...T>
Product(H, T...) -> Product<H,T...>;

// Projection
template<Nat j, class ...T>
struct projection;

template<Nat j, class ...T>
struct projection<j, Product<T...>> {
  using P = Product<T...>;
  using Dummy = ::std::tuple<T...>;
  using PrjT = typename ::std::tuple_element<j,Dummy>::type;
  static auto prj (P x) -> PrjT { return helper::pack_prj<j,T...>::prj(x.get()); }
};

// Injection 
template<Nat j, class ...T>
struct injection;

template<Nat j, class ...T>
struct injection<j, Sum<T...>> {
  using S = Sum<T...>;
  using Dummy = ::std::tuple<T...>;
  using InjT = typename ::std::tuple_element<j,Dummy>::type;
  static auto inj (InjT x) -> S { return helper::pack_inj<j,T...>::inj(x.get()); }
};
 
 
// ========================================================================= 
// ATOMIC 

// functional forms: operators
template<Nat n> requires (n <= maxrep)
constexpr N<n> operator + (N<n> x, N<n> y) { return x.add(y); }

template<Nat n> requires (n <= maxrep)
constexpr N<n> operator - (N<n> x, N<n> y) { return x.sub(y); }

template<Nat n> requires (n <= maxrep) 
constexpr N<n> operator * (N<n> x, N<n> y) { return x.mul(y); }

template<Nat n> requires (n <= maxrep) 
constexpr N<n> operator / (N<n> x, N<n> y) { return x.div (y); }

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


// ========================================================================= 
// PRODUCTS

// functional forms: operators
template<class ...Args>
constexpr Product<Args...> operator + (Product<Args...> x, Product<Args...> y) { return x.add(y); }

template<class ...Args>
constexpr Product<Args...> operator - (Product<Args...> x, Product<Args...> y) { return x.sub(y); }

template<class ...Args>
constexpr Product<Args...> operator * (Product<Args...> x, Product<Args...> y) { return x.mul(y); }

template<class ...Args>
constexpr Product<Args...> operator / (Product<Args...> x, Product<Args...> y) { return x.div (y); }

template<class ...Args>
constexpr Product<Args...> operator % (Product<Args...> x, Product<Args...> y) { return x.mod(y); }

template<class ...Args>
constexpr Product<Args...> operator - (Product<Args...> x) { return x.neg(); }

// functional forms: comparisons
template<class ...Args>
constexpr bool operator == (Product<Args...> x, Product<Args...> y) { return x.eq(y); }

template<class ...Args>
constexpr bool operator != (Product<Args...> x, Product<Args...> y) { return x.ne(y); }

template<class ...Args>
constexpr bool operator <  (Product<Args...> x, Product<Args...> y) { return x.lt(y); }

template<class ...Args>
constexpr bool operator <= (Product<Args...> x, Product<Args...> y) { return x.le(y); }

template<class ...Args>
constexpr bool operator >= (Product<Args...> x, Product<Args...> y) { return x.ge(y); }

template<class ...Args>
constexpr bool operator >  (Product<Args...> x, Product<Args...> y) { return x.gt(y); }

// functional forms: iterators
template<class ...Args>
constexpr Product<Args...> succ (Product<Args...> x) { return x.succ(); }

template<class ...Args>
constexpr Product<Args...> pred (Product<Args...> x) { return x.pred(); }


// ========================================================================= 
// SUMS  
 
// functional forms: operators
template<class ...Args>
constexpr Sum<Args...> operator + (Sum<Args...> x, Sum<Args...> y) { return x.add(y); }

template<class ...Args>
constexpr Sum<Args...> operator - (Sum<Args...> x, Sum<Args...> y) { return x.sub(y); }

template<class ...Args>
constexpr Sum<Args...> operator * (Sum<Args...> x, Sum<Args...> y) { return x.mul(y); }

template<class ...Args>
constexpr Sum<Args...> operator / (Sum<Args...> x, Sum<Args...> y) { return x.div (y); }

template<class ...Args>
constexpr Sum<Args...> operator % (Sum<Args...> x, Sum<Args...> y) { return x.mod(y); }

template<class ...Args>
constexpr Sum<Args...> operator - (Sum<Args...> x) { return x.neg(); }

// functional forms: comparisons
template<class ...Args>
constexpr bool operator == (Sum<Args...> x, Sum<Args...> y) { return x.eq(y); }

template<class ...Args>
constexpr bool operator != (Sum<Args...> x, Sum<Args...> y) { return x.ne(y); }

template<class ...Args>
constexpr bool operator <  (Sum<Args...> x, Sum<Args...> y) { return x.lt(y); }

template<class ...Args>
constexpr bool operator <= (Sum<Args...> x, Sum<Args...> y) { return x.le(y); }

template<class ...Args>
constexpr bool operator >= (Sum<Args...> x, Sum<Args...> y) { return x.ge(y); }

template<class ...Args>
constexpr bool operator >  (Sum<Args...> x, Sum<Args...> y) { return x.gt(y); }

// functional forms: iterators
template<class ...Args>
constexpr Sum<Args...> succ (Sum<Args...> x) { return x.succ(); }

template<class ...Args>
constexpr Sum<Args...> pred (Sum<Args...> x) { return x.pred(); }
// ========================================================================= 

int main() {
  ::std::cout << "Hello world" << ::std::endl;
  N<16> x{9};
  ::std::cout << "x Type name = " << x.type_name() << ::std::endl; 
  ::std::cout << "x repr = " << x.repr() << ::std::endl; 
  ::std::cout << x << ", " << x + x << ::std::endl;

  // product
  N<3> x3_2{2};
  N<2> x2_1{1};
  using P32 = Product<N<3>,N<2>>;

  P32 x32_21{x3_2,x2_1};
  ::std::cout << "x32_21="<< x32_21 << ::std::endl;
  ::std::cout << "x32_21::type_name="<< x32_21.type_name() << ::std::endl;
  ::std::cout << "x32_21::repr ="<< x32_21.repr() << ::std::endl;

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
  ::std::cout <<"x=  "<< messy << ::std::endl;
  ::std::cout <<"-x= "<< messy.neg() << ::std::endl;
  ::std::cout <<"x+x="<< messy.add(messy) << ::std::endl;
  ::std::cout <<"x-x="<< messy.sub(messy) << ::std::endl;
  ::std::cout <<"x*x="<< messy.mul(messy) << ::std::endl;
  ::std::cout <<"x/x="<< messy.div(messy) << ::std::endl;
  ::std::cout <<"x%x="<< messy.mod(messy) << ::std::endl;

  ::std::cout <<"-x= "<< (-messy) << ::std::endl;
  ::std::cout <<"x+x="<< (messy + messy) << ::std::endl;
  ::std::cout <<"x-x="<< (messy - messy) << ::std::endl;
  ::std::cout <<"x*x="<< (messy * messy) << ::std::endl;
  ::std::cout <<"x/x="<< (messy / messy) << ::std::endl;
  ::std::cout <<"x%x="<< (messy % messy) << ::std::endl;


  ::std::cout <<"x==x="<< (messy == messy) << ::std::endl;
  ::std::cout <<"x!=x="<< (messy != messy) << ::std::endl;
  ::std::cout <<"x<x="<< (messy < messy) << ::std::endl;
  ::std::cout <<"x<=x="<< (messy <= messy) << ::std::endl;
  ::std::cout <<"x>x="<< (messy > messy) << ::std::endl;
  ::std::cout <<"x>=x="<< (messy >= messy) << ::std::endl;

  // unary case
  auto unitary = Product { N<3>{2} };
  auto xx = projection<0, Product<N<3>>>::prj (unitary);
  ::std::cout << xx << ::std::endl;
  ::std::cout << unitary << ::std::endl;
  ::std::cout << unitary.add(unitary) << ::std::endl;

  // nullary case 
  auto nullary =Product{}; 
  ::std::cout << nullary << ::std::endl;
  ::std::cout << nullary.add(nullary) << ::std::endl;

  for (auto i = ring_iterator<5>::begin(); i != ring_iterator<5>::end(); ++i)
    ::std::cout << *i << ::std::endl
  ;

  for (auto i : ring_iterator<5>::all())
    ::std::cout << i << ::std::endl
  ;

  using S32 = Sum<N<3>,N<2>>;
  auto c1_1 = injection<1,S32>::inj(1); // case 1, value 1:2, rep = 1
  auto c0_1 = injection<0,S32>::inj(1); // case 0, value 1:3, rep = 3
  ::std::cout << "c1_1 repr = " << c1_1.repr() << ::std::endl;
  
  ::std::cout << "c1_1.rep=" << c1_1.get() << ", caseno = " << c1_1.caseno() << ", caseval=" << c1_1.caseval() << ::std::endl;
  ::std::cout << "c0_1.rep=" << c0_1.get() << ", caseno = " << c0_1.caseno() <<  ", caseval=" << c0_1.caseval() << ::std::endl;

  // nested
  using S32_32 = Sum<S32, S32>;
  auto c111 = injection<1, S32_32>::inj(c1_1);
  auto c011 = injection<0, S32_32>::inj(c1_1);
  ::std::cout << "c111.rep=" << c111.get() << ", caseno = " << c111.caseno() << ", caseval=" << c111.caseval() << ::std::endl;
  ::std::cout << "c011.rep=" << c011.get() << ", caseno = " << c011.caseno() << ", caseval=" << c011.caseval() << ::std::endl;
  ::std::cout << "c111 repr = " << c111.repr() << ::std::endl;
  ::std::cout << "c011 repr = " << c011.repr() << ::std::endl;

}
