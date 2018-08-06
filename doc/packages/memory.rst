Package: src/packages/memory.fdoc


=================
Memory Operations
=================

=========== ================================
key         file                             
=========== ================================
memory.flx  share/lib/std/scalar/memory.flx  
address.flx share/lib/std/scalar/address.flx 
=========== ================================


Raw Address
===========


.. index:: Address
.. code-block:: felix

  //[address.flx]
  
  //$ Core operations on addresses.
  open class Address {
    //$ Construct from Felix object pointer.
    ctor[T] address: &T = "(void*)$1";
  
    //$ Construct from possibly NULL pointer.
    ctor[T] address: cptr[T] = "(void*)$1"; //@
  
    //$ Construct from possibly array element pointer.
    ctor[T] address: +T = "(void*)$1";
  
    //$ Construct from C function
    ctor[D,C] address: D --> C = "(void*)$1";
  
  
    //$ Check is an address is NULL.
    fun isNULL: address -> bool = "(0==$1)";
  
    //$ Define NULL address.
    const NULL : address = "NULL";
  
    instance Eq[address] {
      fun == : address * address -> bool = "$1==$2";
    }
    instance Tord[address] {
      fun < : address * address -> bool = "::std::less<void*>()($1,$2)";
    }
  
    const addrstrfmt : +char = '"%" PRIxPTR' requires C99_headers::inttypes_h;
    const addrreprfmt : +char = '"0x%" PRIxPTR' requires C99_headers::inttypes_h;
  
    instance Str[address] {
      fun str (t:address) : string => vsprintf (addrstrfmt, C_hack::cast[uintptr] t);
    }
    instance Repr[address] {
      fun repr (t:address) : string => vsprintf (addrreprfmt, C_hack::cast[uintptr] t);
    }
  
  
    instance Str[byte] {
      fun str (t:byte) : string => vsprintf (c"%02x", C_hack::cast[uint] t);
    }
  
    instance Repr[byte] {
      fun repr (t:byte) : string => vsprintf (c"0x%02x", t);
    }
  
  
    fun + : address * !ints -> address = "(void*)((char*)$1+$2)";
    fun - : address * !ints -> address = "(void*)((char*)$1-$2)";
    fun - : address * address -> ptrdiff = "(char*)$1-(char*)$2";
  }
  
  open Eq[byte];
  open Tord[address];


