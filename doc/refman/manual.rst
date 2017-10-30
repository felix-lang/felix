Language Reference Manual
=========================

This is the :math:`\alpha^2` Felix Language Reference manual, it is intended primarily
to document the common language interface presented to the programmer.
It is not complete or precise because the grammar and features
the user would normally call a language are actually defined in
user space, in the library.
 
Basics: Hello 
*************


Running a Program
^^^^^^^^^^^^^^^^^

Let's start with a simple script:
 
.. code-block:: felix
   
   println$ "Hello World!";

To run it you just say:

.. code-block:: bash 
   
   flx hello.flx

It's pretty simple. Felix runs programs like Python does, you run the 
source code directly. Behind the scenes, Felix translates the program
into C++, compiles the program, and runs it. All the generated files
are cached in the .felix/cache subdirectory of your $HOME directory.


