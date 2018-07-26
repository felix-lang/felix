Language Reference Manual
=========================

This is the Felix Language Reference manual, it is intended primarily
to document the common language interface presented to the programmer.
It is not complete or precise because the grammar and features
the user would normally call a language are actually defined in
user space, in the library.
 
This chapter briefly explains some of the central concepts of Felix.

Ease of use
^^^^^^^^^^^

Let's start with a simple script:
 
.. code-block:: felix
   
   println$ "Hello World!";

To run it you just say:

.. code-block:: bash 
   
   flx hello.flx

It's pretty simple. Felix runs programs like Python does, you run the 
source code directly.

Performance
^^^^^^^^^^^

Behind the scenes, Felix translates the program
into C++, compiles the program, and runs it. Felix programs run *fast*.
Here's a silly comparison for Ackermann's function, `acl(3,13)`:

=============  ======  ===========
Compiler       Ack     Takfp
=============  ======  ===========
Felix/clang    3.71    6.23
Clang/C++      3.95    6.29
Felix/gcc      2.34    6.60
Gcc/C++        2.25    6.25
Ocaml          2.93    8.41
=============  ======  ===========

Felix motto is *hyperlight* performance which means we aim
to run programs *faster than C*. 

The compiler is fast too. Felix is designed so all files
can be independently parsed. The `flx` tool does automatic
dependency checking and caches temporary results.

No File system pollution
^^^^^^^^^^^^^^^^^^^^^^^^

All the generated files
are cached in the .felix/cache subdirectory of your $HOME directory.
Felix can run script files in read-only directories.

Portability
^^^^^^^^^^^

Felix lets you write both portable and non-portable programs.
It generates ISO compliant C++ and the run time library (RTL) 
is written in ISO C++. 

Portability is obtain by a combination of conditional compilation
in the Felix libraries and in the RTL and reliance on the ISO C++ Standard
Library. Most library functions
in the Felix library are designed to work the same way
on all platforms. Well written script should run on Linux, OSX
and Windows without modification.

By using a configuration database, platform and compiler switches
are avoided: Felix knows how to run your C++ compiler and
how to link third party libraries.

Even the `flx` tool has switches specifically designed to work
on all platforms, for example `flx --static -c -od . hello` will compile
and link your program and put it in the current directory,
even though the executable is named `hello.exe` on Windows.


Domain Specific Sublanguage Support
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Whilst C++ allows you to overload operators so you can
design a syntax suitable to your application domain,
Felix goes a step further and allows you to specify
grammar extensions. Indeed, almost the entire Felix
language is specified by parsing rules in user space,
in the library.

Constructs based on theory
^^^^^^^^^^^^^^^^^^^^^^^^^^

The core constructions in Felix are modelled on 
mathematical theory: set theory, type theory,
category theory.

First class lexically scoped functions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Functions, procedures and coroutines in Felix are
all first class. Function and procedure values can
be constructed and capture their environment to
form a closure.

It is important to note variables *including* `val`s are
part of a stack frame object which may be allocated
on the heap and the capture is via a pointer to the
whole frame. This means when a closure is executed,
the value of the captured variable is the value
current *at the time the closure executes* and not
the value at the time of capture.

For example:

.. code-block:: felix

  var x = 1;
  var g: (1->0)^3;
  noinline proc f (y:int) () { println$ y; };
  for i in 1..3 do
    &g.(i - 1) <- f x; // capture value x in y
    ++x;
  done
  for i in 1..3 do
    g.(i - 1) (); // prints 1,2,3
  done

Without the `noinline` Felix is too smart and the variable
y is inlined to the mainline, so there is only one copy,
when you run the script, it prints 4,4,4. If you just change
the parameter to `var y:int` to force eager evaluation,
it prints 3,3,3.

Capture by address is not a design fault, it is in fact
the only option. Just consider:

.. code-block:: felix

  var x = 1;
  fun getx() => x;
  ++x;
  println$ getx();

You would be surprised if this printed 1! You expect
the function to report the current value of x.


