General lookup
==============

By default Felix looks up symbols in nested scopes, 
starting with all symbols in the current scope
and proceeding through its containing scope outwards
until the outermost scope is reached.

Symbols are visible in the whole of a scope,
both before and after their introduction.

A symbol lookup may properly find either a single
non-function symbol, which is final, or a set 
of function symbols.

If the kind of symbol being sought is a function 
symbol, overload resolution is performed on 
the set of function signatures found in a scope.
If a best match is found, that is final.
If no match is found the search continues in 
the next outermost scope.

All other cases are in error.

Classes
-------

`Syntax <http://felix-lang.org/share/lib/grammar/namespaces.flxh>`_

The top level Felix module can contain submodules 
which are specified by a non-polymorphic class
statement:

.. code-block:: felix

   class classname { ... }

The effect is to produce a qualified name to be used
outside the class:

.. code-block:: felix

   class classname { proc f () {} }
   classname::f (); 
   
Classes may be nested.

A class may contain private definitions:

.. code-block:: felix

   class X {
     private var a = 1;
   }
   // X::a will fail, since a is private to the class X

A private definition is visible within the scope
of the class but not outside it.

A class must be specified within a single file.

Classes are not extensible, a definition of a class
with the same name in the same scope is not permitted.

The body of a class forms a nested scope. Within
a class all symbols defined in the class are visible,
along with all those visible in the enclosing context.

The reserved name ``root`` may be used as a prefix
for the top level module:

.. code-block:: felix

   var x = 1;
   class A { var x = root::x; }


Lookup control directives
-------------------------


Open directive
--------------

The simple ``open`` directive may be used to make the symbols
defined in a class visible in the scope containing the ``open`` directive.

.. code-block:: felix
   
   class X { var x = 1; }
   open X;
   println$ x;

Names made visible by an open directive
live in a weak scope under the current scope.
Names in the weak scope may be hidden by definitions
in the current scope without error.

.. code-block:: felix
   
   class X { var x = 1; }
   open X;
   var x = 2;
   println$ x; // prints 2

The open directive is not transitive.
The names it makes visible are only visible
in the scope in which the open directive is written.

Inherit directive
-----------------

The inherit directive allows all of the public symbols
of a class to be included in another scope as if they
were defined in that scope. This means such names
inherited into a class can be accessed by qualification
with the inheriting class name, and will be visible
if that class is opened. 

Inheriting is transtitive.

If a name is inherited it will clash with a local definition.

.. code-block:: felix

   class A { var a = 1; }
   class B { inherit A; }
   println$ B::a;


Rename directive
----------------

This directive is can be used to inherit a single
symbol into a scope, possibly with a new name,
and also to add an alias for a name in the current
scope.

When applied to a function name all functions with
that name are renamed.

.. code-block:: felix
    
   class A { 
     var a = 1; 
     proc f() {} 
     proc f(x:int) {} 
   }
   
   class B { 
     rename a = A::a;
     rename fun f = A::f;
   }

The new name injected by a rename may be polymorphic:

.. code-block:: felix

   class A { proc f[T] () {} }
   class B { rename g[T] = A::f[T]; } 

Use directive
-------------

This is a short form of the rename directive:

.. code-block:: felix
   
   class A { var a = 1; }
   class B { use A::a; use b = A::a; }

It cannot be applied to functions. The first
form is equivalent to

.. code-block:: felix
   
   use a = A::a;

Unlike the rename directive the new name cannot be polymorphic
and is limited to a simple identifier.

Export directives
-----------------

The ``export`` directives make the exported symbol a root
of the symbol graph. 

The functional export forces it to be place in the
generated code as an ``extern "C"`` symbol with the
given name:

.. code-block:: felix
   
   export fun f of (int) as "myf";
   export cfun f of (int) as "myf";
   export proc f of (int) as "myf";
   export cproc f of (int) as "myf";

Functions are exported by generating a wrapper around
the Felix function. If the function is exported as ``fun``
or ``proc`` the C function generated requires a pointer
to the thread frame as the first argument,
if the ``cfun`` or ``cproc`` forms are used, the wrapper
will not require the thread frame. 

In the latter case, the Felix function must not
require the thread frame.

A type may also be exported:

.. code-block:: felix
   
   export type ( mystruct ) as "MyStruct";

This causes a C typedef to be emitted making 
the name ``MyStruct`` an alias to the Felix type.
This is useful because Felix types can have unpredictable
mangled names.

The word ``export`` optionally followed by a string
may also be used as a prefix for any Felix function,
generator, or procedure definition. If the string
is omitted is taken as the symbol name. The effect
is the same as if an export statement has been written.

