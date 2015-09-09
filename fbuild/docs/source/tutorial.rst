Tutorial
========

This is a simple tutorial to get you up on your feet with using Fbuild.

Basics
******

Let's start with a simple app. We'll be writing a "Hello, world!" program in C and building it using Fbuild. Here's our C code:

.. code-block:: c
   
   #include <stdio.h>
   
   int main() {
       puts("Hello, world!");
       return 0;
   }

And here's the Fbuild build script:

.. code-block:: python
   
   from fbuild.builders.c import guess_static
   
   def build(ctx):
       builder = guess_static(ctx)
       builder.build_exe('hello', ['hello.c'])

It's pretty simple. First, ``guess_static`` is imported. That function returns a new builder for building C programs (we'll get to builders in a moment). Then we define a ``build`` function that takes an object of type ``fbuild.context.Context`` (``ctx``). That object is kind of like the "build engine." The next line calls ``guess_static``, and the line after calls the ``build_exe`` method. It can take several keyword arguments, but the only two positional ones are the output file (``hello``) and a list of source files (``['hello.c']``). Let's run it::

   ryan@DevPC-LX:/media/ryan/stuff/fbuild/playground/doc$ fbuild
   determining platform     : {'posix', 'linux'}
   looking for clang        : ok /home/ryan/stuff/Downloads/clang+llvm-3.6.0-x86_64-linux-gnu/bin/clang
   checking clang           : ok
   looking for ar           : ok /usr/bin/ar
   looking for ranlib       : ok /usr/bin/ranlib
   checking if clang can make objects : ok
   checking if clang can make libraries : ok
   checking if clang can make exes      : ok
   checking if clang can link lib to exe : ok
    * clang                              : hello.c -> build/obj/hello/hello.o
    * clang                              : build/obj/hello/hello.o -> build/hello
   ryan@DevPC-LX:/media/ryan/stuff/fbuild/playground/doc$ 

That just:

- Detected our C compiler and the associated utilities (``ar`` and ``ranlib``).
- Tested it.
- Built our program.

Builders
********

In Fbuild, a ``builder`` is an object that...builds stuff. In our last example, the builder could build C executables and libraries. Most of Fbuild revolves around builders. Pretty much all of them are located within ``fbuild.builders``.
