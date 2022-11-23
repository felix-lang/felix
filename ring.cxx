#include <cstdint>
#include <iostream>

using Nat = ::std::uint64_t;
constexpr Nat maxrep = Nat(uint32_t(-1));

template<Nat n> requires (n <= maxrep)
class N {
  Nat rep;
public:
  // default constructor
  N() : rep(0) {}

  // constructor
  N(uint64_t x) : rep(x%n) {}

  // standard value semantics
  N(N const&) = default;
  N(N const&&) = default;
  N& (N const&) = default;
  N& (N const&&) = default;

  // number of values
  constexpr Nat size()const { return n; }

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
    operator<<(::std::ostream &o, N x) { return o << x.rep; }
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

template<Nat n> `requires (n <= maxrep) 
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



int main() {
  ::std::cout << "Hello world" << ::std::endl;
  N<16> x{9};
  ::std::cout << x << ", " << x + x << ::std::endl;
}
