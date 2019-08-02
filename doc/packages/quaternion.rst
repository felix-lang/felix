Package: src/packages/quaternion.fdoc


=======================
Approximate Quaternions
=======================

============== ===================================
key            file                                
============== ===================================
quaternion.flx share/lib/std/scalar/quaternion.flx 
============== ===================================


Quaternions
===========


.. index:: Quaternion(class)
.. index:: quaternion(type)
.. index:: quaternion(ctor)
.. index:: r(fun)
.. index:: i(fun)
.. index:: j(fun)
.. index:: k(fun)
.. index:: q(ctor)
.. index:: conj(fun)
.. index:: norm(fun)
.. index:: reciprocal(fun)
.. code-block:: felix

  //[quaternion.flx]
  
  class Quaternion
  {
    type quaternion = new double ^ 4;
    ctor quaternion (x:double^4) => _make_quaternion x;
    private typedef q = quaternion;
    fun r(x:q)=> (_repr_ x) . 0;
    fun i(x:q)=> (_repr_ x) . 1;
    fun j(x:q)=> (_repr_ x) . 2;
    fun k(x:q)=> (_repr_ x) . 3;
  
    ctor q (x:double) => quaternion (x,0.0,0.0,0.0);
  
    fun + (a:q,b:q):q =>
      quaternion (a.r+ b.r, a.i + b.i, a.j + b.j, a.k+b.k)
    ;
  
    fun * (a:q, b:q):q =>
      quaternion (
        a.r * b.r - a.i * b.i - a.j * b.j - a.k * b.k,
        a.r * b.i + a.i * b.r + a.j * b.k - a.k * b.j,
        a.r * b.j - a.i * b.k + a.j * b.r - a.k * b.i,
        a.r * b.k + a.i * b.j - a.j * b.i + a.k * b.r
      )
    ;
  
    fun conj (a:q):q => quaternion (a.r, -a.i, -a.j, -a.k);
    fun norm (a:q):double => sqrt (a.r * a.r + a.i * a.i + a.j * a.j +a.k * a.k);
  
    fun * (a:q, b: double):q => quaternion (a.r * b, a.i * b, a.j * b, a.k * b);
    fun * (a: double, b:q):q => a * b;
  
    fun reciprocal (a:q):q => let n = norm a in conj a * (1.0/ (n * n));
  
    // add more later, generalise scalar type
    // Later, GET RID of complex and quaternions
    // by introducing typeclasses for arbitrary R-modules
  }
  
  
