Program structure
=================

A Felix program consists of a nominated root parse unit and
the transitive closure of units with respect to inclusion.

The behaviour of this system consists of the action of the
initialisation code in each unit, performed in sequence
within a given unit, with the order of action between
units unspecified.

Parse Unit
----------

A parse unit is a file augmented by prefixing specified import
files to the front. These consist of a suite of grammar files
defining the syntax and other files defining macros.

By convention import files have the extension ``.flxh``_.

With this augmentation all parse units in a program
are independently parsed to produce an list of statements
represented as abstract syntax, denoted an AST (even
though it is a list of trees, not a single tree).

AST
---

The program consists of the concatenation of the ASTs
of each parse unit, resulting in a single AST, which
is then translated to a C++ translation unit by the
compiler.

Grammar
=======

The Felix grammar is part of the library.
It is notionally prefixed to each file to be processed
prior to any import files to specify the syntax
with which the file is to be parsed and translated to
an AST.

The grammar uses an augmented BNF like syntax
with parse actions specified in R5RS Scheme.

The resulting S-expressions are translated to
an intermediate form and then into an internal
AST structure.

The parser is a scannerless GLR parser with significant
extensions.

Grammar syntax
--------------

Not written yet. Browse the 
`grammar directory <http://felix-lang.org/share/lib/grammar>`_
for examples.

Modules
=======

Every Felix program is encapsulated in a module with
the name being a mangle of the basename of the root unit.
The mangling replaces characters in the filename with
other characters so that the module name is a valid
ISO C identifier.

Special procedure ``flx_main``_
------------------------------

A program module may contain at most one top level
procedure named ``flx_main``_. After initialisation 
code suspends or terminates, this procedure is invoked
if it exists. It is the analogue of ``main``_ in C++
however it is rarely used: side-effects of the
root units initialisation code are typically used instead.

Libraries
---------

In Felix a library is a root unit together with its
transitive closure with respect to inclusion,
which does not contain a top level ``flx_main``_.

A program unit can be augmented by a set of libraries
which are then considered as if included, but without
an include directive being present.


Lexicology
==========

All Felix files are considered to be UTF-8 encoded Unicode.

Felix uses a scannerless parser, there are no keywords.

Identifiers
-----------

A plain identifier starts with a letter or underscore,
then consists of a sequence of letters, digits, apostrophy, has no more
than one apostrophy or dash in a row, except at the end no dash is
allowed, and any number of apostrophies.

.. code:: felix
   
   Ab_cd1  a' b-x

Identifies starting with underscore are reserved for the implementation.

A letter may be any Unicode character designated for use in an identifier
by the ISO C++ standard. In practice, all high bit set octets are allowed.

A TeX identifier starts with a slosh and is followed by a sequence
of letters. 

Integer Literals
----------------


`Reference <http://felix-lang.org/share/lib/grammar/grammar_int_lexer.flxh>`_

An plain integer literal consists of a sequence of digits,
optionally separated by underscores. Each separating
underscore must be between digits.

A prefixed integer literal is a plain integer literal
or a plain integer literal prefixed by a radix specifier.
The radix specifier is a zero followed by one of
the letters ``bBoOdDxX``_ for binary, octal, decimal or hex.

An underscore is permitted after the prefix.

The radix is the one specified by the prefix or decimal
by default.

The digits of an integer consist of those permitted
by the radix: ``01`` for binary, ``01234567``_
for octal, ``0123456789`` for decimal, ``0123456789abcdefABCDEF``_
for hex.

Note there are no negative integer literals.

A type suffix may be added to the end of a prefixed
integer to designate a literal of a particular integer type,
it has the form of an upper or lower case letter or pair of
letters usually combined with a prefix or suffix ``u`` or ``U``_
to designate an unsigned variant of the type. 

Signed integers are expected to be two's complement with one
more negative value that positive value. Bitwise and,
or, exclusive or, and complement operations do not apply
with signed types.

The effect of overflow on signed types is unspecified.

Unsigned types use the standard representation. 
Bitwise operations may be applied to unsigned types.
Basic arithmetic operations on unsigned types are
all well defined as the result of the operation
mathematically modulo the maximum value of the type
plus one.

The maximum value of an unsigned type is one less than
two raised to the power of the number of bits in the type.
The number of bits is 8, 16, 32, or 64 or 128 for all unsigned types.

Note that integers starting with 0 are decimal not octal as in C.

A table
of suffices and the types they signify follows in lower case.

====== ==================================================================================
Suffix  Type      C type              Description
====== ==================================================================================
i8      int8      int8_t              8 bit signed integer
i16     int16     int16_t             16 bit signed integer
i32     int32     int32_t             32 bit signed integer
i64     int64     int64_t             64 bit signed integer

u8      uint8     uint8_t             8 bit unsigned integer
u16     uint16    uint16_t            16 bit unsigned integer
u32     uint32    uint32_t            32 bit unsigned integer
u64     uint64    uint64_t            64 bit unsigned integer

t       tiny      signed char         C++ signed char used an integer
s       short     short               C short
i       int       int                 C int
l       long      long                C long
ll v    vlong     long long           very long: C long long


ut tu   utiny     unsigned char       unsigned tiny: C++ unsigned char used as an integer
us su   ushort    unsigned short      C unsigned short
u       uint      unsigned            C unsigned int
ul lu   ulong     unsigned long       C unsigned long
ull uv  uvlong    unsigned long long  C unsigned longlong

uz zu   size      size_t              array size
j       intmax    intmax_t            largest integer type
uj ju   uintmax   uintmax_t           largest unsigned integer type
p       intptr    intptr_t            pointer considered as an integer
up pu   uintptr   uintptr_t           pointer considered as an unsigned integer
d       ptrdiff   ptrdiff_t           signed distance between pointers 
ud      uptrdiff  uptrdiff_t          unsigned distance between pointers
====== ==================================================================================

Note that all these types are distinct unlike C and C++.
The types designated are not the complete set of available
integer like types since not all have literal representations.

Note the suffices do not entirely agree with C.

Floating point Literals
-----------------------

`Reference <http://felix-lang.org/share/lib/grammar/grammar_float_lexer.flxh>`_

Floating point literals follow ISO C89, except that underscores
are allowed between digits, and a a digit is required both before
and after the decimal point if it is present.

The mantissa may be decimal, or hex, a hex mantissa uses a
leading 0x or 0X prefix optionally followed by an underscore.

The exponent may designate a power of 10 using E or e,
or a power of 2, using P or p.

A suffix may be F,f,D,d, L or l, designating floating type,
double precision floating type, or long double precision floating 
type.

.. code:: felix
   
   123.4
   123_456.78
   12.6E-5L
   0xAf.bE6f
   12.7p35


String literals
---------------


`Reference <http://felix-lang.org/share/lib/grammar/grammar_string_lexer.flxh>`_

Generaly we follow Python here.
Felix allows strings to be delimited by: 
single quotes ',
double quotes ",
triped single quotes ''' or
tripled double quotes """.

The single quote forms must be on a single line.

The triple quoted forms may span lines, and include embedded newline
characters.

These forms all allows embedded escape codes.

Raw strings
^^^^^^^^^^^

A prefix "r" or "R" on a double quoted string
or triple double quoted string suppresses escape processing,

this is called a raw string literal.
NOTE: single quoted string cannot be used!

Null terminated strings
^^^^^^^^^^^^^^^^^^^^^^^

A prefix of "c" or "C" specifies a C NTBS (Nul terminated
byte string) be generated instead of a C++ string.
Such a string has type +char rather than string.

Perl interpolation strings
^^^^^^^^^^^^^^^^^^^^^^^^^^

A literal prefixed by "q" or "Q" is a Perl interpolation
string. Such strings are actually functions.
Each occurrence of $(varname) in the string is replaced
at run time by the value "str varname". The type of the
variable must provide an overload of "str" which returns
a C++ string for this to work.

C format strings
^^^^^^^^^^^^^^^^

A literal prefixed by a "f" or "F" is a C format string.

Such strings are actually functions.

The string contains code such as "%d" or other supported
C format specifiers. 

Variable field width specifiers "*" are not permitted. 

The additional format specification %S
is supported and requires a Felix string argument.

Such functions accept a tuple of values like this:

.. code:: felix
   
   f"%d-%S" (42, "Hello")

If ``vsnprintf``_ is available on the local platform it is used
to provide an implementation which cannot overrun.
If it is not, ``vsprintf``_ is used instead with a 1000 character
buffer.

The argument types and code types are fully checked for type safety.

Special identifiers
^^^^^^^^^^^^^^^^^^^

The special literal with a "n" or "N" prefix is a way to encode
an arbitrary sequence of characters as an identifer in a context
where the parser might interpret it otherwise.
It can be used, for example, to define special characters as functions.
For example:

.. code:: felix
   
   typedef fun n"@" (T:TYPE) : TYPE => cptr[T];

Include Directive
-----------------

An include directive has the syntax:

.. code:: felix
   
   include "filename";

where the filename is a Unix relative filename,
may not have an extension, and may not begin with or 
contain ``..``_ (two dots).

If the filename begins with ``./``_ then the balance of the name
is relative, a sibling of the including file, otherwise the name
is searched for on an include path. 

In either case, a search succeeds when it finds a file with
the appropriate base path in the search directory with
extension ``.flx`` or ``.fdoc``_. If both files exist the
most recently changed one is used. If the time stamps are
the same the choice is unspecified.

Macro processing
================

`Syntax <http://felix-lang.org/share/lib/grammar/macros.flxh>`_

`Semnantics <http://felix-lang.org/share/src/compiler/flx_desugar/flx_macro.ml>`_

Macro val
---------

The macro val statement is used to specify an identifier should
be replaced by the defining expression wherever it occurs in an
expression, type expression, or pattern.

.. code:: felix
   
   macro val WIN32 = true;
   macro val hitchhiker;
   macro val a,b,c = 1,2,3;


Macro for
---------

This statement allows a list of statements to be repeated
with a sequence of replacements.

.. code:: felix
   
forall name in 1,2,3 do
  println$ name;
done
@

Constant folding and conditional compilation
--------------------------------------------

`Reference <http://felix-lang.org/share/src/compiler/flx_desugar/flx_constfld.ml>`_

Felix provides two core kinds of constant folding:
folding of arithmetic, boolean, and string values, and 
deletion of code, either statements or expressions,
which would become unreachable due to certain
value of conditionals.

Basic operations on integer literals, namely 
addition, subtraction, negation, multiplication,
division, and remainder are folded.

Strings are concatenated.

Boolean and, or, exclusive or, and negation,
are evaluated.

False branches of if/then/else/endif expression
and match expressions are eliminated.

False branches of if/do/elif/else/done 
are also eliminated.

By this mechanism of constant folding and
elimination, Felix provides conditional
compilation without the need for special
constructions.


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
=======

`Syntax <http://felix-lang.org/share/lib/grammar/namespaces.flxh>`_

The top level Felix module can contain submodules 
which are specified by a non-polymorphic class
statement:

.. code:: felix

   class classname { ... }

The effect is to produce a qualified name to be used
outside the class:

.. code:: felix

   class classname { proc f () {} }
   classname::f (); 
   
Classes may be nested.

A class may contain private definitions:

.. code:: felix

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

The reserved name ``root``_ may be used as a prefix
for the top level module:

.. code:: felix

   var x = 1;
   class A { var x = root::x; }

Lookup control directives
=========================


Open directive
--------------

The simple ``open``_ directive may be used to make the symbols
defined in a class visible in the scope containing the ``open``_ directive.

.. code:: felix
   
   class X { var x = 1; }
   open X;
   println$ x;

Names made visible by an open directive
live in a weak scope under the current scope.
Names in the weak scope may be hidden by definitions
in the current scope without error.

.. code:: felix
   
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

.. code:: felix

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

.. code:: felix
    
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

.. code:: felix

   class A { proc f[T] () {} }
   class B { rename g[T] = A::f[T]; } 

Use directive
-------------

This is a short form of the rename directive:

.. code:: felix
   
   class A { var a = 1; }
   class B { use A::a; use b = A::a; }

It cannot be applied to functions. The first
form is equivalent to

.. code:: felix
   
   use a = A::a;

Unlike the rename directive the new name cannot be polymorphic
and is limited to a simple identifier.

Export directives
-----------------

The ``export``_ directives make the exported symbol a root
of the symbol graph. 

The functional export and forces it to be place in the
generated code as an ``extern "C"``_ symbol with the
given name:

.. code:: felix
   
   export fun f of (int) as "myf";
   export cfun f of (int) as "myf";
   export proc f of (int) as "myf";
   export cproc f of (int) as "myf";

Functions are exported by generating a wrapper around
the Felix function. If the function is exported as ``fun``_
or ``proc``_ the C function generated requires a pointer
to the thread frame as the first argument,
if the ``cfun`` or ``cproc``_ forms are used, the wrapper
will not require the thread frame. 

In the latter case, the Felix function must not
require the thread frame.

A type may also be exported:

.. code:: felix
   
   export type ( mystruct ) as "MyStruct";

This causes a C typedef to be emitted making 
the name ``MyStruct``_ an alias to the Felix type.
This is useful because Felix types can have unpredictable
mangled names.

The word ``export``_ optionally followed by a string
may also be used as a prefix for any Felix function,
generator, or procedure definition. If the string
is omitted is taken as the symbol name. The effect
is the same as if an export statement has been written.


Variable Definitions
====================

`Syntax <http://felix-lang.org/share/lib/grammar/variables.flxh>`_

A definition is a statement which defines a name, but does
no cause any observable behavior, or, a class statement, or, 
a var or val statement. The latter two exceptions define a name
but may also have associated behaviour.

The ``var``_ statement
---------------------

The ``var``_ statement is used to introduce a variable name
and potential executable behaviour. It has one of three 
basic forms:

.. code:: felix
   
   var x : int = 1;
   var y : int;
   var z = 1;

The first form specifies the type and an initialising
expression which must be of the specified type.

The second form specifies a variable of the given type
without an explicit initialiser, however the variable
will be initialised anyhow with the default contructor
for the underlying C++ type, although that constructor
may be trivial.

The third form does not specify the type, it will be deduced
from the initialiser.

If the initialiser has observable behaviour it will be observed
if at all, when control passes through the variable statement.

If the variable introduced by the ``var``_ statement is not used,
the variable and its initaliser will be elided and any observable
behaviour will be lost.

To be used means to have its address taken in a used expression,
to occur in a used expression. A used expression is one which
initialises a used variable, or, is an argument to function
or generator in a used expression, or an argument to a procedure
through which control passes. 

In other words, the variable is used if the behaviour of
the program appears to depend on its value or its address.

The library procedure ``C_hack::ignore``_ ensures the compiler
believes a variable is used:

.. code:: felix
   
   var x = expr;
   C_hack::ignore x;

so that any side effects of @{expr} will be seen.
In general the argument to any primitive function, generator
or procedure will be considered used if its containing 
entity is also considered used. In general this means there
is a possible execution path from a root procedure of the
program.

A variable may have its address taken:

.. code:: felix
   
   var x = 1;
   var px = &x;

it may be assigned a new value directly or indirectly:

.. code:: felix

   x = 2;
   px <- 3;
   *px = 4;

A variable is said to name an object, not a value.
This basically means it is associated with the address of a typed
storage location.

Multiple variables
^^^^^^^^^^^^^^^^^^

Multipls variables can be defined at once:

.. code:: felix
   
   var m = 1,2;
   var a,b = 1,2;
   var c,d = m;

With this syntax, no type annotation may be given.

The ``val``_ statement.
----------------------

A ``val``_ statement defines a name for an expression.

.. code:: felix
   
   val x : int = 1;
   val z = 1;

The value associated with a ``val``_ symbol may be computed
at any time between its definition and its use, and may
differ between uses, if the initialising expression depends
on variable state, such as a variable or call to a generator.

It is not an error to create such a dependence since either
the value may, in fact, not change, or the change may
not be significant.

Nevertheless the user must be warned to take care
with the indeterminate evaluation time and use
a ``var``_ when there is any doubt.

Since a ``val``_ simply names an expression, it is associated
with a value not an object and cannot be addressed
or assigned to. However this does NOT mean its value cannot
change:

.. code:: felix
   
   for var i in 0 upto 9 do
     val x = i;
     println$ x;
   done

In this example, x isn't mutable but it does take on
all the values 0 to 9 in succession. This is just a 
most obvious case: a less obvious one:

.. code:: felix
   
   var i = 0;
   val x = i;
   println$ x;
   ++i;
   println$ x;

which is clearly just an expansion of the the first two
iteration of the previously given for loop. However in
this case there is no assurance ``x`` will change after ``i``_
is incremented because the compiler is free to replace
any ``val`` definition with a ``var``_ definition.

Multiple values
^^^^^^^^^^^^^^^

Multiple values can be defined at once:

.. code:: felix
   
   val m = 1,2;
   val a,b = 1,2;
   val c,d = m;

With this syntax, no type annotation may be given.



Functions
=========

`Syntax <http://felix-lang.org/share/lib/grammar/functions.flxh>`_

A felix function definition takes one of three basic forms:

.. code:: felix
   
   fun f (x:int) = { var y = x + x; return y + 1; }
   fun g (x:int) => x + x + 1;
   fun h : int -> int = | ?x => x + x + 1;

The first form is the most general, the body 
of the function contains executable statements
and the result is returned by a return statement.

The second form is equivalent to a function in the first
form whose body returns the RHS expression.

The third form specifies the function type then the
body of a pattern match. It is equivalent to

.. code:: felix
   
   fun h (a:int) = { return match a with | ?x => x + x + 1 endmatch; }

The first two forms also allow the return type to be
specified:

.. code:: felix
   
   fun f (x:int) : int = { var y = x + x; return y + 1; }
   fun g (x:int) :int => x + x + 1;

Functions may not have side effects.

All these function have a type:

.. code:: felix
   
   D -> C

where D is the domain and C is the codomain: both would
be ``int``_ in the examples.

A function can be applied by the normal forward
notation using juxtaposition or what is whimsically
known as operator whitespace, or in reverse notation
using operator dot:

.. code:: felix

   f x
   x.f

Such applications are equivalent.  Both operators are left
associative. Operator dot binds more
tightly than whitespace so that

.. code:: felix
   
   f x.g    // means
   f (g x)

A special notation is used for application to the unit tuple:

.. code:: felix
   
   #zero // means
   zero ()

The intention is intended to suggest a constant since a pure
function with unit argument must always return the
same value. 

This hash operator binds more tightly than operator dot so

.. code:: felix
   
   #a.b // means
   (#a).b


Pre- and post-conditions
------------------------

A function using one of the first two forms
may have pre-conditions, post-conditions, or both:

.. code:: felix
   
   fun f1 (x:int when x > 0) => x + x + 1;
   fun f2 (x:int) expect result > 1 => x + x + 1;
   fun f3 (x:int when x > 0) expect result > 1 => x + x + 1;

Pre- and pos-conditions are usually treated as boolean assertions
which are checked at run time. The compiler may occasionally be able
to prove a pre- or post-condition must hold and elide it.

The special identifier ``result``_ is used to indicate the return
value of the function.

Higher order functions
----------------------

A function may be written like

.. code:: felix
   
   fun hof (x:int) (y:int) : int = { return x + y; }
   fun hof (x:int) (y:int) => x + y;

These are called higher order functions of arity 2.
They have the type

.. code:: felix
   
   int -> int -> int   // or equivalently
   int -> (int -> int) //since -> is right associative.

They are equivalent to

.. code:: felix
   
   fun hof (x:int) : int -> int = 
   {
     fun inner (y:int) : int => x + y;
     return inner;
  }

that is, a function which returns another function.

Such a function can be applied like

.. code:: felix
   
   hof 1 2 // or equivalently
   (hof 1) 2

since whitespace application is left associative.

Procedures
----------

A function which returns control but no value is called a procedure.
Procedures may have side effects.

.. code:: felix
   
   fun show (x:int) : 0 = { println x; }
   proc show (x:int) { println x; }
   proc show (x:int) => println x;

The second form is a more convenient notation.
The type 0 is also called ``void``_ and denotes
a type with no values.

A procedure may return with a simple return statement:

.. code:: felix
   
   proc show (x:int) { println x; return; }

however one is assumed at the end of the procedure
body .

Procedures can also have pre- and post-conditions.

A procedure may be called like an application,
however it must be a whole statement since
expressions of type void may not occur interior
to an expression.

.. code:: felix
   
   show 1;
   1.show;

If a procedure accepts the unit argument, it may be elided:

.. code:: felix
   
   proc f () =>  show 1;
   f; // equivalent to
   f ();

Generators
----------

TBD

Types
=====

`Syntax <http://felix-lang.org/share/lib/grammar/type_decls.flxh>`_

Tuples
------

Tuple types are well known: a tuple is just a Cartesian Product
with components identified by position, starting at 0. 
The n-ary type combinator is infix ``*``_ and the n-ary value
constructor is infix ``,``_:

.. code:: felix
   
   val tup : int * string * double = 1, "Hello", 4.2;

The 0-ary tuple type is denoted ``1`` or ``unit``_
with sole value ``()``_:

.. code:: felix
   
   val u : unit = ();

There 1-array tuple of type ``T`` component value ``v``_ is identified
with the type ``T`` and has value ``v``_.

The individual components of a tuple may be accessed by a projection
function. Felix uses an integer literal to denote this function.

.. code:: felix
   
   var x = 1,"Hello";
   assert 0 x == 1; assert x.0 == 1;
   assert 1 x == "Hello"; assert x.1 == "Hello";

[There should be a way to name this function without application to
a tuple!]

A pointer to a tuple is also in itself a tuple, namely the
tuple of pointers to the individual components. This means
if a tuple is addressable, so are the components.

.. code:: felix
   
   var x = 1, "Hello";
   val px = &x;
   val pi = px.0; pi <-42;
   val ps = px.1; ps <-"World";
   assert x.0 == 42;
   assert x.1 == "World";

In particular note:

.. code:: felix
   
   var x = 1, "Hello";
   &x.0 <- 42;

because the precedences make the grouping ``(&x).0``_.

You cannot take the address of a tuple component because
a projection of a value is a value.

Assignment to components of tuples stored in variables is supported
but only to one level, for general access you must take a pointer
and use the store-at-addres operator ``<-``_.

Records
-------

A record is similar to a tuple except the components are 
named and considered unordered.


Structs
-------

TBD
Sums
----

TBD

union
^^^^^

TBD

enum
----

TBD

variant
-------

TBD

Array
-----

TBD

typedef
-------

TBD

typedef fun
^^^^^^^^^^^

TBD

typematch
---------

TBD

type sets
---------

TBD

Abstract types
--------------

TBD

Expressions
===========

`Syntax <http://felix-lang.org/share/lib/grammar/expressions.flxh>`_

TBD

Executable statements
=====================

Assignment
----------

`Syntax <http://felix-lang.org/share/lib/grammar/assignment.flxh>`_

The ``goto``_ statement and label prefix
---------------------------------------

Felix statements may be prefixed by a label
to which control may be transfered by a @{goto}
statement:

.. code:: felix
   
   alabel:>
     dosomething;
     goto alabel;

The label must be visible from the goto statement.

There are two kinds of gotos. A local goto is a jump
to a label in the same scope as the goto statement.

A non-local goto is a jump to any other visible label.

Non-local transfers of control may cross procedure
boundaries. They may not cross function or generator 
boundaries.

The procedure or function containing the label 
must be active at the time of the control transfer.

A non-local goto may be wrapped in a procedure closure
and passed to a procedure from which the goto target
is not visible.

.. code:: felix
   
   proc doit (err: 1 -> 0) { e; }
   
   proc outer () {
     proc handler () { goto error; }
     doit (handler);
     return;
   
     error:> println$ error;
   }

This is a valid way to handle errors.
the code is correct because ``outer``_ is active
at the time that ``handler``_ performs the
control transfer.

halt
^^^^

Stops the program with a diagnostic.

.. code:: felix
   
   halt "Program complete";

try/catch/entry
^^^^^^^^^^^^^^^

The try/catch construction may only be user to wrap
calls to C++ primitives, so as to catch exceptions.

.. code:: felix
   
   proc mythrow 1 = "throw 0;";
   try
      mythrow;
   catch (x:int) =>
      println$ "Caughht integer " + x.str;
   endtry

goto-indirect/label_address
^^^^^^^^^^^^^^^^^^^^^^^^^^^

The ``label-address``_ operator captures the address
of code at a nominated label. 

The address has type ``LABEL``_ and can be stored in a variable.

Provided the activation record of the procedure containing
the label remains live, a subsequent ``goto-indirect``_ can
be used to jump to that location.

.. code:: felix
   
   proc demo (selector:int) {
     var pos : LABEL = 
       if selector == 1 
       then label_address lab1
       else label_address lab2
       endif
     ;
     goto-indirect selector;
   lab1:>
     println$ "Lab1"; return;
   lab2:>
     println$ "Lab2"; return;
   }

Exchange of control
^^^^^^^^^^^^^^^^^^^

TBD

match/endmatch
--------------

TBD

if/goto
-------

The conditional goto is an abbreviation for 
the more verbose conditional:

.. code:: felix
   
   if c goto lab; // equivalent to
   if c do goto lab; done

if/return
^^^^^^^^^

The conditional return is an abbreviation for
the more verbose conditional:

.. code:: felix
   
   if c return; // equivalent to
   if c do return; done

if/call
^^^^^^^

The conditional call is an abbreviation for
the more verbose conditional:

.. code:: felix
   
   if c call f x; // equivalent to
   if c do call f x; done


if/do/elif/else/done
--------------------

The procedural conditional branch is used to select
a control path based on a boolean expression.

The ``else`` and ``elif``_ clauses are optional.

.. code:: felix

   if c1 do 
     stmt1;
     stmt2;
   elif c2 do
     stmt3;
     stmt4;
   else
     stmt5;
     stmt6;
   done

The ``elif``_ clause saves writing a nested conditional.
The above is equivalent to:

.. code:: felix
   
   if c1 do 
     stmt1;
     stmt2;
   else 
     if c2 do
       stmt3;
       stmt4;
     else
       stmt5;
       stmt6;
     done
   done

One or more statements may be givn in the selected control path.

A simple conditional is an abbreviation for a statement match:

.. code:: felix
   
   if c do stmt1; stmt2; else stmt3; stmt4; done
   // is equivalent to
   match c with
   | true => stmt1; stmt2; 
   | false => stmt3; stmt4;
   endmatch;

call
----

The ``call``_ statement is used to invoke a procedure.

.. code:: felix
   
   proc p(x:int) { println$ x; }
   call p 1;

The word ``call``_ may be elided in a simple call:

.. code:: felix
   
p 1;

If the argument is of unit type; that is, it is the
empty tuple, then the tuple may also be elided in
a simple call:

.. code:: felix
   
   proc f() { println$ "Hi"; }
   call f (); // is equivalent to
   f(); // is equivalent to
   f;

procedure return
----------------

The procedural return is used to return control
from a procedure to its caller.

A return is not required at the end of a procedure
where control would otherwise appear to drop through,
a return is assumed:

.. code:: felix
   
   proc f() { println$ 1; }
   // equivalent to
   proc f() { println$ 1; return; }

return from
^^^^^^^^^^^

The return from statement allows control to be
returned from an enclosing procedure, provided that
procedure is active.

.. code:: felix
   
   proc outer () {
     proc inner () {
        println$ "Inner";
        return from outer;
     }
     inner;
     println$ "Never executed";
   }

jump 
^^^^

The procedural jump is an abbreviation for 
the more verbose sequence:

.. code:: felix
   
   jump procedure arg; // is equivalent to
   call procedure arg;
   return;

function return
---------------

The functional return statement returns a value from
a function.

.. code:: felix
   
   fun f () : int = {
     return 1;
   }

Control may not fall through the end of a function.

yield
^^^^^

The yield statement returns a value from a generator
whilst retaining the current location so that execution
may be resumed at the point after the yield.

For this to work a closure of the generator must be stored
in a variable which is subsequently applied.

.. code:: felix
   
   gen counter () = { 
     var x = 0;
   next_integer:>
     yield x;
     ++x;
     goto next_integer;
   }
   
   var counter1 = counter;
   var zero = counter1 ();
   var one = counter1 ();
   println$ zero, one;


spawn_fthread
-------------

`Reference <http://felix-lang.org/share/lib/std/control/fibres.flx>`_

The ``spawn_fthread``_ library function invokes the corresponding
service call to schedule the initial continuation of a procedure 
taking a unit argument as an fthread (fibre). 

The spawned fthread begins executing immediately.
If coutrol returns before yielding by a synchronous
channel operation, the action is equivalent to calling
the procedure.

Otherwise the spawned fthread is suspended when the first
write, or the first unmatched read operation occurs.


read/write/broadcast schannel
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

`Reference <http://felix-lang.org/share/lib/std/control/schannels.flx>`_

spawn_pthread
-------------

`Reference <http://felix-lang.org/share/lib/std/control/pthread.flx>`_

read/write pchannel
^^^^^^^^^^^^^^^^^^^

`Reference <http://felix-lang.org/share/lib/std/control/pchannels.flx>`_

exchange
^^^^^^^^


loops
=====

`Reference <http://felix-lang.org/share/lib/grammar/loops.flxh>`_

Felix has some low level and high level loop constructions.

The low level for, while, and repeat loops are equivalent
to loops implemented with gotos.

The bodies of do loops do not constitute a scope,
therefore any symbol defined in such a body is also visible
in the surrounding code.

Low level loops may be labelled with a loop label
which is used to allow break, continue, and redo
statements to exit from any containing loop.

.. code:: felix
   
   outer:for var i in 0 upto 9 do
      inner: for var j in 0 upto 9 do
        println$ i,j;
        if i == j do break inner; done
        if i * j > 60 do break outer; done
      done
   done


redo
----

The redo statement causes control to jump to the start
of the specified loop without incrementing the control variable.

break
-----

The break statement causes control to jump past the end of
the specified loop, terminating iteration.

continue
--------

The continue statement causes the control variable to
be incremented and tests and the next iteration commenced
or the loop terminated.

for/in/upto/downto/do/done
--------------------------

A basic loop with an inclusive range.

.. code:: felix
   
   // up
   for var ti:int in 0 upto 9 do println$ ti; done
   for var i in 0 upto 9 do println$ i; done
   for i in  0 upto 9 do println$ i; done
   
   // down
   for var tj:int in 9 downto 0 do println$ j; done
   for var j in 9 downto 0 do println$ j; done
   for j in  0 upto 9 do println$ j; done

The start and end expressions must be of the same type.

If the control variable is defined in the loop with a type
annotation, that type must agree with the control variable.

The type must support comparison with the equality operator ``==``_
the less than or equals operator ``<=``_ and increment with 
the pre increment procedure ``++``_.

For loops over unsigned types cannot handle the empty case.
For loops over signed types cannot span the whole range of the type.

The loop logic takes care to ensure the control variable is not
incremented (resp. decremented) past the end (resp.start) value.

while/do/done
-------------

The while loop executes the body repeatedly whilst the control
condition is true at the start of the loop body.

.. code:: felix
   
   var i = 0;
   while i < 10 do println$ i; ++i; done

until loop
----------

The until loop executes the loop body repeatedly
until the control condition is false at the start of the loop,
it is equivalent o a while loop with a negated condition.

.. code:: felix
   
   var i = 0;
   until i == 9 do println$ i; ++i; done

for/match/done
--------------

TBD

loop
----

TBD

Assertions
----------

`Reference <http://felix-lang.org/share/lib/grammar/assertions.flxh>`_

assert
------

Ad hoc assertion throws an assertion exception if its argument
is false. 

.. code:: felix
   
   assert x > 0;

axiom
^^^^^

An axiom is a relationship between functions, typically
polymorphic, which is required to hold.

.. code:: felix
   
   axiom squares (x:double) => x * x >= 0;
   class addition[T]
   {
     virtual add : T * T -> T;
     virtual == : T * T -> bool;
   
     axiom assoc (x:T, y:T, z:T) : 
       add (add (x,y),z) == add (x, add (y,z))
     ;
   }

In a class, an axiom is a specification constraining
implementations of virtual function in instances.

Axioms are restricted to first order logic, that is, they
may be polymorphic, but the universal quantification implied
is always at the head.

Existential quantification can be provided in a constructive
logic by actually constructing the requisite variable.

Second order logic, with quantifiers internal to the 
logic term, are not supported.

lemma
^^^^^

A lemma is similar to an axiom, except that is it
easily derivable from axioms; in particular,
a reasonable automatic theorem prover should
be able to derived it.

theorem
^^^^^^^

A theorem is similar to a lemma, except that it is 
too hard to expect an automatic theorem prover
to be able to derive it without hints or assistance.

There is currently no standard way to prove such hints.

reduce
^^^^^^

A reduce statement specifies a term reduction and is logically
equivalent to an axiom, lemma, or theorem, however it acts
as an instruction to the compiler to attempt to actually 
apply the axiom.

The compiler may apply the axiom, but it may miss opportunities
for application.

The set of reductions must be coherent and terminal, 
that is, after a finite number of reductions the final
term must be unique and irreducible. 

Application of reduction is extremely expensive and they
should be used lightly.

.. code:: felix
   
   reduce revrev[T] (x: list[T]) : rev (rev x) => x;



invariant
^^^^^^^^^

An invariant is an assertion which must hold on the state variables
of an object, at the point after construction of the state
is completed by the constructor function and just before the
record of method closures is returned, and, at the start and
end of every method invocation.

The invariant need not hold during execution of a method.

Felix inserts the a check on the invariant into the constructor function
and into the post conditions of every procedure or generator
method.

.. code:: felix
   
   object f(var x:int, var y:int) =
   {
      invariant y >= 0;
      method proc set_y (newy: int) => y = newy;
   }


code
----

The code statement inserts C++ code literally into the current
Felix code.

The code must be one or more C++ statements.

.. code:: felix
   
   code 'cout << "hello";';

noreturn code
^^^^^^^^^^^^^

Similar to code, however noreturn code never returns.

.. code:: felix
   
   noreturn code "throw 1;";

Service call
------------

The service call statement calls the Felix system kernel
to perform a specified operation.

It is equivalent to an OS kernel call.

The available operations include:

.. code:: felix
   
     union svc_req_t =
     /*0*/ | svc_yield
     /*1*/ | svc_get_fthread         of &fthread    // CHANGED LAYOUT
     /*2*/ | svc_read                of address
     /*3*/ | svc_general             of &address    // CHANGED LAYOUT
     /*4*/ | svc_reserved1
     /*5*/ | svc_spawn_pthread       of fthread
     /*6*/ | svc_spawn_detached      of fthread
     /*7*/ | svc_sread               of _schannel * &gcaddress
     /*8*/ | svc_swrite              of _schannel * &gcaddress
     /*9*/ | svc_kill                of fthread
     /*10*/ | svc_reserved2
     /*11*/ | svc_multi_swrite       of _schannel * &gcaddress 
     /*12*/ | svc_schedule_detached  of fthread
     ;

These operations are typically related to coroutine or thread scheduling.
However ``svc_general``_ is an unspecified operation, which is typically
used to invoke the asynchronous I/O subsystem.

Service calls can only be issued from flat code, that is,
from procedures, since they call the system by returning
control, the system must reside exactly one return address
up the machine stack at the point a service call is executed.

with/do/done
------------

The with/do/done statement is use to define temporary variables
which are accessible only in the do/done body of the statement.

It is the statement equivalent of the let expression.

.. code:: felix
   
   var x = 1;
   with var x = 2; do println$ x; done
   assert x == 1;

do/done
-------

The do/done statement has no semantics and merely acts as a
way to make a sequence of statements appear as a single
statement to the parser.

Jumps into do/done groups are therefore allowed, and
any labels defined in a do/done group are visible in
the enclosing context.

Any variables, functions, or other symbols defined in a do/done
group are visible in the enclosing context.

.. code:: felix
   
   do something; done

begin/end
---------

The begin/end statement creates an anonymous procedure
and then calls it. It therefore appears as a single statement
to the parser, but it simulates a block as would be used in C.
It is exactly equivalent to a brace enclosed procedure called
by a terminating semi-colon.

.. code:: felix
   
   begin
     var x = 1;
   end
   // equivalent to
   {
     var x = 1;
   };


C bindings
==========

Felix is specifically designed to provide almost seamless integration
with C and C++.

In particular, Felix and C++ can share types and functions,
typically without executable glue.

However Felix has a stronger and stricter type system than C++
and a much better syntax, so binding specifications which lift
C++ entities into Felix typically require some static glue.

Type bindings
-------------

In general, Felix requires all primitive types to be first class,
that is, they must be default initialisable, copy constructible,
assignable, and destructible. Assignment to a default initialised
variable must have the same semantics as copy construction.

It is recommended C++ objects provide move constructors as
Felix generated code uses pass by value extensively.

The Felix type system does not support C++ references in general,
you should use pointers instead. 

However, there is a special lvalue annotation for C++ functions
returning lvalues that allows them to appear on the LHS of
an assignment. Only primitives can be marked lvalue.

The Felix type system does not support either const or volatile.
This has no impact when passing arguments to C++ functions.
However it may be necessary to cast a pointer returned from
a primitive function in order for the generated code to type check.



Expression bindings
-------------------

TBD

Function bindings
-----------------

TBD

Floating insertions
-------------------

TBD

Package requirements
--------------------

TBD

Domain Specific Sublanguages
============================

Regexps
-------

`Syntax <http://felix-lang.org/share/lib/grammar/regexps.flxh>`_

`Combinators <http://felix-lang.org/share/lib/std/strings/regdef.flx>`_

`Google Re2 Binding <http://felix-lang.org/share/lib/std/strings/re2.flx>`_

Pipelines
---------

Synchronouse pipelines
^^^^^^^^^^^^^^^^^^^^^^

`Library <http://felix-lang.org/share/lib/std/control/spipe.flx>`_

Asynchronouse pipelines
^^^^^^^^^^^^^^^^^^^^^^^

`Library <http://felix-lang.org/share/lib/std/control/ppipe.flx>`_

Json
^^^^

TBD

Sqlite3
^^^^^^^

TBD



