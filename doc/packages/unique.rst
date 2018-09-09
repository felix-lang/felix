Package: src/packages/unique.fdoc


=========================
General Unique Facilities
=========================

========== ================================
key        file                             
========== ================================
unique.flx share/lib/std/control/unique.flx 
========== ================================

General Facilities
==================


.. index:: Unique(class)
.. index:: box(fun)
.. index:: unbox(fun)
.. index:: kill(proc)
.. index:: def(type)
.. index:: peek(fun)
.. code-block:: felix

  //[unique.flx]
  open class Unique 
  {
    // box up a value as a unique thing
    fun box[T] : T -> _uniq T = "($t)";
  
    // unsafely unpack the unique box
    fun unbox[T] : _uniq T -> T = "($t)";
  
    // kill a live unique value
    proc kill[T] : uniq T = ";";
  
    // functor for typing
    typedef fun uniq (T:TYPE):TYPE => _uniq T;
  
    // peek inside the box without changing livenes state
    fun peek[T] : &<(uniq T) -> T = "*($t)";
  
    // string representions
    instance[T] Repr[uniq T] {
      fun repr(var x:uniq T) => "uniq " + (C_hack::cast[T] x).str;
    }
  
    instance[T] Str[uniq T] {
      fun str(var x:uniq T) => "uniq " + (C_hack::cast[T] x).str;
    }
  
    instance[T with Repr[T]] Repr[&<(uniq T)] {
      fun repr(var x:&<(uniq T)) => "uniq " + x.peek.repr;
    }
  
    instance[T with Str[T]] Str[&<(uniq T)] {
      fun str(var x:&<(uniq T)) => x.peek.str;
    }
  }
  
  
  
