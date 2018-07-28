Language Reference Manual
=========================

This is the Felix Language Reference manual, it is intended primarily
to document the common language interface presented to the programmer.
It is not complete or precise because the grammar and features
the user would normally call a language are actually defined in
user space, in the library.
 
This chapter briefly explains some of the central concepts of Felix.

Simplicity, Performance.
------------------------

Felix is, first and foremost, dedicated to obtaining run time performance.

Felix motto is *hyperlight* performance which means we aim
to run programs *faster than C*. 

Let's start with a simple script:
 
.. code-block:: felix
     
  fun ack(x:int,y:int):int =>
    if x == 0 then y + 1
    elif y == 0 then ack(x - 1, 1)
    else ack(x - 1, ack(x, y - 1))
    endif
  ;

  do
    val n = 13;
    var v = ack(3,n);
    println$ f"Ack(3,%d): %d" (n, v);
  done

  fun Tak (x:double, y:double, z:double): double =>
    if (y >= x) then z
    else Tak(Tak(x - 1.0,y,z), Tak(y - 1.0,z,x), Tak(z - 1.0,x,y))
    endif
  ;

  do 
    val n = 12.0;
    var v = Tak(n*3.0, n*2.0, n*1.0);
    println$ f"%.2f" v;
  done

To run it you just say:

.. code-block:: bash 
   
   flx test.flx

It's pretty simple. Felix runs programs like Python does, you run the 
source code directly.

All the generated files
are cached in the .felix/cache subdirectory of your $HOME directory.
Felix can run script files in read-only directories.

Felix translates the code into C++, compiles the C++, and runs it. 
Felix programs run *fast*. Felix itself implements high level optimisations
beyond the scope of traditional compilers, then passes the generated
code to your system C++ compiler which in turn implements low level
optimisations. 

Here's a silly comparison for Ackermann's function and Takfp,
times in seconds:

=============  ======  ===========
Compiler       Ack     Takfp
=============  ======  ===========
Felix/clang    3.71    6.23
Clang/C++      3.95    6.29
Felix/gcc      2.34    6.60
Gcc/C++        2.25    6.25
Ocaml          2.93    8.41
=============  ======  ===========

