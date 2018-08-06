Package: src/packages/libstruct.fdoc


=================
Library structure
=================

========== ====================
key        file                 
========== ====================
std.flx    share/lib/std.flx    
========== ====================
=============== =============================
key             file                          
=============== =============================
linux.flx       share/lib/std/linux/linux.flx 
linux_smaps.flx share/lib/std/linux/smaps.flx 
=============== =============================
============== =================================
key            file                              
============== =================================
cstdlib.flx    share/lib/std/c/cstdlib.flx       
c_hack.flx     share/lib/std/c/c_hack.flx        
platindep.flxh share/lib/std/plat/platindep.flxh 
============== =================================


Library structure
=================

Specifies some of the automatic inclusion files.
Others are located in other packages. 

Also throw in some "don't know where to put them" files.

Std.flx
=======

The top level library module.

.. code-block:: felix
  //[std.flx]
  header '#include "flx_rtl_config.hpp"';
  include "std/__init__";

Default includes for Standard library.
======================================


.. code-block:: text
  
  open String;
  
  // ISO C++99 standard header tags
  include "std/c/__init__";
  
  // core type classes
  include "std/algebra/__init__";
  
  // base scalar types 
  include "std/scalar/__init__";
  
  // utility
  include "std/debug";
  
  // control
  include "std/control/__init__";
  include "std/pthread/__init__";
  include "std/program/__init__";
  
  //memory management
  include "std/gc";
  
  // I/O
  include "std/io/__init__";
  include "std/time";
  
  // codecs
  include "std/codec/__init__";
  
  // base data types
  include "std/datatype/__init__";
  include "std/strings/__init__";
  
  // regexes
  include "std/regex/__init__";
  
  // database REMOVED
  //include "std/db/__init__";
  
  // Version
  include "std/version";
  
  // Platform support (implementation exposure)
  include "std/osx/__init__";
  include "std/posix/__init__";
  include "std/win32/__init__";
  
  // Felix (implementation exposure)
  include "std/felix/__init__";
  


C stuff
=======

Structure of C sublibrary.

.. code-block:: text
  
  include "std/c/c_headers";
  include "std/c/cxx_headers";
  include "std/c/cptr";
  include "std/c/cstdlib";
  include "std/c/carray";
  include "std/c/c_hack";
  include "std/c/shared_ptr";
  


Data types
==========

Structure of datatype library.

.. code-block:: text
  
  // special
  include "std/datatype/typing";
  include "std/datatype/functional";
  include "std/datatype/special";
  include "std/datatype/unitsum";
  
  // base data types
  include "std/datatype/tuple";
  include "std/datatype/option";
  include "std/datatype/slice";
  include "std/datatype/list";
  include "std/datatype/assoc_list";
  include "std/datatype/stream";
  //include "std/datatype/sexpr";
  //include "std/datatype/lsexpr";
  //include "std/datatype/ralist";
  
  // arrays
  include "std/datatype/array_class";
  include "std/datatype/array";
  include "std/datatype/varray";
  include "std/datatype/darray";
  //include "std/datatype/sarray";
  //include "std/datatype/bsarray";
  include "std/datatype/judy";
  include "std/datatype/sort";
  
  // dictionaries
  include "std/datatype/strdict";
  
  // tree
  //include "std/datatype/avl";


Posix
=====

Structure of Posix support library.

.. code-block:: text
  
  struct Posix {};
  include "std/posix/posix_headers";
  include "std/posix/errno";
  include "std/posix/signal";
  include "std/posix/time";
  include "std/posix/filestat";
  include "std/posix/directory";
  include "std/posix/filesystem";
  include "std/posix/process";
  include "std/posix/shell";
  include "std/posix/faio_posix";
  include "std/posix/mmap";


Win32
=====

Structure of Win32 library.

.. code-block:: text
  
  struct Win32 {};
  
  // windows services
  include "std/win32/shell";
  include "std/win32/filestat";
  include "std/win32/directory";
  include "std/win32/process";
  include "std/win32/filesystem";
  include "std/win32/time";
  include "std/win32/signal";
  include "std/win32/faio_win32";
  include "std/win32/win32_headers";


Platform independent Computation enforcement
============================================

Using --import=std/plat/platindep.flxh on flxg command
fails to set any of the usual platform macros like
FLX_LINUX, FLX_POSIX, FLX_WIN32. Instead it sets
the macro  :code:`PLAT_INDEP`. This should bug out any
compilations requiring platform specific macros.


.. code-block:: text
  // Platform independent compilation enforced by
  // failing to set any platform macros.
  macro val PLAT_INDEP = 1;


C hackery
=========

Hackery for mapping between Felix and C/C++.

.. index:: C_hack
.. code-block:: felix
  //[c_hack.flx]
  
  //$ This class provides access to raw C/C++ encodings.
  //$ Incorrect typing is likely to pass by Felix and
  //$ be trapped by the C/C++ compiler. Incorrect management
  //$ of storage can lead to corruption. The use of the
  //$ C_hack class is necessary for interfacing.
  class C_hack
  {
    //$ C void type. Incomplete, can't be instantiated.
    incomplete type void_t = "void";
  
    //$ Standard variable argument list pointer type.
    type va_list = "va_list";
  
    //$ GCC specific valist thingo: it will
    //$ be optimised away if not used (eg on MSVC).
    type __builtin_va_list = '__builtin_va_list';
  
    //$ Throw away result of a function call:
    //$ only useful for C functions that are mainly
    //$ called for side effects.
    proc ignore[t]:t = "(void)$t;";
  
    //$ C style cast.
    fun cast[dst,src]: src->dst = '(?1)($t/*cast*/)' is cast;
  
    //$ C++ static cast.
    fun static_cast[dst,src]: src->dst = 'static_cast<?1>($t)' is postfix;
  
    //$ C++ dynamic cast.
    fun dynamic_cast[dst,src]: src->dst = 'dynamic_cast<?1>($t)' is postfix;
  
    //$ C++ const cast.
    fun const_cast[dst,src]: src->dst = 'const_cast<?1>($t)' is postfix;
  
    //$ C++ reinterpret cast.
    fun reinterpret_cast[dst,src]: src->dst = 'reinterpret_cast<?1>($t)' is postfix;
  
    //$ Felix reinterpret cast.
    //$ More powerful than C++ reinterpret cast.
    //$ Allows casting an rvalue to an lvalue.
    fun reinterpret[dst,src]: src->dst = 'reinterpret<?1>($t)' is postfix;
  
    const sizeof[t]:size = 'sizeof(?1)';
  
    //$ Special NULL check for Felix pointers.
    //$ Should never succeed.
    fun isNULL[t]: &t -> bool = "(0==$1)";
  
    //$ Special NULL check for carray.
    //$ Should never succeed.
    fun isNULL[t]: +t -> bool = "(0==$1)";
  
    //$ Polymorphic null pointer constant
    //$ Values of this type should not exist.
    //$ This value is provided for checking.
    const null[t]:&t = "(?1*)NULL";
  
    //$ C++ default value for a type T.
    //$ Workaround for g++ 3.2.2 parsing bug,
    //$ it can parse T() as a default ctor call,
    //$ but screws up on (T())
    fun dflt[t]:1->t = "dflt<?1>()" requires header
      "template<class T> T dflt() { return T(); }";
  
    //$ Invoke C++ destructor
    proc destroy[T] : &T = "::destroy($1);/*C_hack*/"; // from flx_compiler_support_bodies
  }
  
  


C stdlib Rand
=============

Just to get random functions.

.. index:: Cstdlib
.. code-block:: felix
  //[cstdlib.flx]
  
  open class Cstdlib
  {
    requires Cxx_headers::cstdlib;
    const RAND_MAX:long;
  
    //$ C89 Standard C library seed random number generator.
    proc srand: uint = '::std::srand($1);';
  
    //$ C89 Standard C library random number generator.
    //$ Known to be not very good. Try not to use it!
    fun rand: unit -> int = '::std::rand()';
  }
  
  


OSX platform tag
================


.. code-block:: text
  
  struct Osx{};
  include "std/posix/__init__";


Linux specifics
===============


.. code-block:: felix
  //[linux.flx]
  
  module Linux {
    header '#include "plat_linux.hpp"';
    requires package "plat_linux";
    fun get_cpu_nr: 1 -> int;
  }


Linux smap
==========

Parses and totals proc/PID/smaps memory stats. 

