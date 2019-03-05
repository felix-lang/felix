Plugins
=======

One of the primary uses of objects is to implement *plugins*. 
A plugin is a *dynamic load library* or DLL, also called a shared
library on Unix platforms.

Felix can load DLLs at run time. However plugins are special DLLs
with a specific structure.

Loading Plugins
+++++++++++++++

To load a plugin named "ob_implementation" we do this:

.. code-block:: felix

  var joe =  
     Dynlink::load-plugin-func1 
       [ob_t,string] 
       ( dll-name="ob_implementation", setup-str="")
  ;

The `load-plugin-func1` loads a plugin with DLL basename "ob_implementation", initialising
global memory by calling a function `setup_ob_implementation` and passing it the empty string "",
then it calls the entry-point function `ob_implementation` which accepts a value of
type `string` and returns a value of type `ob_t`.

The DLL will be searched for on the standard OS search path, given by the
environment variables `LD_LIBRARY_PATH` on Linux, `DYLD_LIBRRY_PATH` on MacOSX,
and `PATH` on Windows. The standard extension for each platform is appended,
that will be `.so` on Linux, `.dylib` on MacOSX, and `.dll` on Windows.

The load operation uses a wrapper around the OS load function: `dlopen` on
Linux and MacOSX, and `LoadLibrary` on Windows.

The system then uses the symbol finding function to find `setup`.
It uses `dlsym` on Linux and MacOSX and `LoadAddress` on Windows.
It must be a C function, not a C++ function, and it must accept a C++
string as an argument and return an int.

Then the system find the entry point, which must be a C function
which accepts a value of type string, and returns a value of type
`ob_t`. The return type and parameter type of the function are given
in square brackets in the call to `load-plugin-func1`.

The important thing here is that the type `ob_t` as to be known to both
the plugin code as well as the client code. Therefore it is usual to
put the interface specification in a separate file and include it in
both places, in just the same manner as header files in C and C++.
There is no type checking done on loading, so it's important if the
interface changes to rebuild both the plugin and the client.

Because plugins typically provide a lot of functions, not just one,
we typically provide just one function, and object factory, which
will return a set of function packed into a record when called.



