.. index:: 
  pair: type; abstract

Abstract Types
==============

Felix provides a class based mechanism for defining an abstract type
represented by another type. Here is an except from the definition
of `Darray`:

.. code-block:: felix

  class Darray[T] {

    private struct darray_ctl[T]
    {
      a: varray[T];
      resize: size * size --> size;
    }

    type darray[T] = new &darray_ctl[T];

    proc do_resize[T] (pd: darray[T], new_size: size)
    {
      var old = (_repr_ pd)*.a;
      (_repr_ pd).a <- varray[T] (new_size, (len old), (fun(i:size)=>old.i));
    }


    ctor[T] darray[T] () => 
      _make_darray[T]$ new darray_ctl[T](varray[T] 20uz , dflt_resize);

    ...
  }

.. index:: _repr_, _make_LHS
An abstract type is defined in a class by its representation as a new type
based on an old one by using the `type LHS = new RHS;` construction.
The LHS type is the abstract type and the RHS is its representation.
In this case the RHS is `darray_ctl` which is marked `private` to stop
it escaping from the class.

The special name `_make_LHS` is used to define a constructor for the new
type.

The special name `_repr_` is used to map the LHS type to the RHS type so it
can be operated on to implement the functions which the user want to export.

The representation of the LHS cannot be accessed outside the class.
In particular the `_make_LHS` and `_repr_` functions are private and can
only be used inside the class. Thus, the implementation of LHS by RHS is hidden
outside the class, in fact it is hidden inside the class as well, except by
using the `_make_LHS` and `_repr_` functions.










