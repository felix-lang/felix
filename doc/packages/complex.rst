Package: src/packages/complex.fdoc


===================
Approximate Complex
===================

=========== ================================
key         file                             
=========== ================================
complex.flx share/lib/std/scalar/complex.flx 
=========== ================================



.. index:: Fcomplex(class)
.. index:: Dcomplex(class)
.. index:: Lcomplex(class)
.. index:: real(fun)
.. index:: imag(fun)
.. index:: abs(fun)
.. index:: arg(fun)
.. index:: neg(fun)
.. index:: zero(fun)
.. index:: one(fun)
.. index:: sin(fun)
.. index:: cos(fun)
.. index:: tan(fun)
.. index:: asin(fun)
.. index:: acos(fun)
.. index:: atan(fun)
.. index:: sinh(fun)
.. index:: cosh(fun)
.. index:: tanh(fun)
.. index:: asinh(fun)
.. index:: acosh(fun)
.. index:: atanh(fun)
.. index:: exp(fun)
.. index:: log(fun)
.. index:: pow(fun)
.. code-block:: felix

  //[complex.flx]
  open class Fcomplex
  {
    ctor[t in reals] fcomplex : t * t = "::std::complex<float>($1,$2)";
    ctor[t in reals] fcomplex : t = "::std::complex<float>($1,0)";
    instance Str[fcomplex] {
      fun str (z:fcomplex) => str(real z) + "+" + str(imag z)+"i";
    }
  }
  
  open class Dcomplex
  {
    ctor[t in reals] dcomplex : t * t = "::std::complex<double>($1,$2)";
    ctor[t in reals] dcomplex : t = "::std::complex<double>($1,0)";
    instance Str[dcomplex] {
      fun str (z:dcomplex) => str(real z) + "+" + str(imag z)+"i";
    }
  }
  
  open class Lcomplex
  {
    ctor[t in reals] lcomplex : t * t = "::std::complex<long double>($1,$2)";
    ctor[t in reals] lcomplex : t = "::std::complex<long double>($1,0)";
    instance Str[lcomplex] {
      fun str (z:lcomplex) => str(real z) + "+" + str(imag z)+"i";
    }
  }
  
  instance[t in floats] Complex[complex[t],t] {
    fun real : complex[t] -> t = "real($1)";
    fun imag : complex[t] -> t = "imag($1)";
    fun abs: complex[t] -> t = "abs($1)";
    fun arg : complex[t] -> t = "arg($1)";
    fun neg : complex[t] -> complex[t] = "-$1";
    fun + : complex[t] * complex[t] -> complex[t] = "$1+$2";
    fun - : complex[t] * complex[t] -> complex[t] = "$1-$2";
    fun * : complex[t] * complex[t] -> complex[t] = "$1*$2";
    fun / : complex[t] * complex[t] -> complex[t] = "$1/$2";
    fun + : complex[t] * t -> complex[t] = "$1+$2";
    fun - : complex[t] * t -> complex[t] = "$1-$2";
    fun * : complex[t] * t -> complex[t] = "$1*$2";
    fun / : complex[t] * t -> complex[t] = "$1/$2";
    fun + : t * complex[t] -> complex[t] = "$1+$2";
    fun - : t * complex[t] -> complex[t] = "$1-$2";
    fun * : t * complex[t] -> complex[t] = "$1*$2";
    fun / : t * complex[t] -> complex[t] = "$1/$2";
    fun zero: 1 -> complex[t] = "::std::complex<?1>(0.0)";
    fun one: 1 -> complex[t] = "::std::complex<?1>(1.0)";
  }
  
  instance[t in (floats  \cup  complexes)] Trig[t] {
    requires Cxx_headers::cmath;
    fun sin: t -> t = "::std::sin($1)";
    fun cos: t -> t = "::std::cos($1)";
    fun tan: t -> t = "::std::tan($1)";
    fun asin: t -> t = "::std::asin($1)";
    fun acos: t -> t = "::std::acos($1)";
    fun atan: t -> t = "::std::atan($1)";
    fun sinh: t -> t = "::std::sinh($1)";
    fun cosh: t -> t = "::std::cosh($1)";
    fun tanh: t -> t = "::std::tanh($1)";
    fun asinh: t -> t = "::std::asinh($1)";
    fun acosh: t -> t = "::std::acosh($1)";
    fun atanh: t -> t = "::std::atanh($1)";
    fun exp: t -> t = "::std::exp($1)";
    fun log: t -> t = "::std::log($1)";
    fun pow: t * t -> t = "::std::pow($1,$2)";
  }
  
  
  typedef complex[t in floats] = typematch t with
    | float => fcomplex
    | double => dcomplex
    | ldouble => lcomplex
    endmatch
  ;
  
Complex Constructors.
---------------------


.. code-block:: felix

  //[complex.flx]
  ctor complex[float] (x:float, y:float) => fcomplex(x,y);
  ctor complex[double] (x:double, y:double) => dcomplex(x,y);
  ctor complex[ldouble] (x:ldouble, y:ldouble) => lcomplex(x,y);
  
  ctor complex[float] (x:float) => fcomplex(x,0.0f);
  ctor complex[double] (x:double) => dcomplex(x,0.0);
  ctor complex[ldouble] (x:ldouble) => lcomplex(x,0.0l);
  
  typedef polar[t in floats] = complex[t];
  ctor[t in floats] polar[t] : t * t = "::std::polar($1,$2)";
  
  open Complex[fcomplex, float];
  open Complex[dcomplex, double];
  open Complex[lcomplex, ldouble];
  
  
  
