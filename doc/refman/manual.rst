Manual
======

This is the Felix manual, which is aiming at being the authorative guide to using
Fbuild in your projects. Note that this is still a work-in-progress!

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


