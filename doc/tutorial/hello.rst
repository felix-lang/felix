Hello
=====

This is the Felix tutorial

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
are cached in the .felix/cache subdirectory of your $HOME directory
on Unix like systems, and $USERPROFILE on Windows.


