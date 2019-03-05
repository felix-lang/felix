Building a Plugin
=================

Here are specific instructions to build and use a plugin.

Interface File
--------------

First we have a file `ob_interface.flx` in the current directory:

.. code-block:: felix

  // ob_interface.flx
  interface ob_t  {
    name: 1->string;
    underage: 1->bool;
    setage: int -> 0;
  };

Implementation File
-------------------

Now we implement the plugin in `ob_implementation.flx` in
the current directory:

.. code-block:: felix

  // ob_implementation.flx
  include "./ob_interface";

  object ob (x:string) implements ob_t {
    var age = 0;
    method fun name () => x;
    method fun underage () => age < 21;
    method proc setage (y:int) { age = y; }
  };

  fun setup(x:string) {
    println$ "Setup string `" + x + "`";
    return 0;
  }

  export fun setup of string as "ob_implementation_setup";
  export fun ob of string as "ob_implementation";

Here the `include` directive is loading the interface relative
to the including file. Put both in the current directory for
convenience!

The `export fun` directive generates an `extern "C"` wrapper
for a Felix function, giving it the quoted linker name.
Because of overloading you have to specify the function domain
type to pick the right function (even if there's only one).

Client File
-----------

Our plugin client is the file `ob_client.flx` in the current directory:

.. code-block:: felix

  // ob_client.flx
  include "./ob_interface";

  var ob =  
    Dynlink::load-plugin-func1 
      [ob_t,string] 
      ( dll-name="ob_implementation", setup-str="", entry-point="ob")
  ;
  var joe = ob "joe";
  println$ "name " + joe.name(); 
   
This will load the plugin, initialise it using the setup function,
and then run the primary entry point.

Building the plugin
-------------------

The plugin is built on all platforms using the command line:

.. code-block:: bash

  flx -c -od . ob_implementation.flx

The `-c` tells Felix to compile and link the file but not run it.
The `-od .` tells Felix to put the linker output in the current directory.
The file will have the basename `ob_implementation` on all platforms,
and will have an extension appropriate to a DLL on your platform.

Running the client
------------------

To run the client on OSX I just did this:

.. code-block:: bash

  flx ob_client.flx

That's it! You may need to set the environment variable that controls the DLL
search path. So on OSX:

.. code-block:: bash

  DYLD_LIBRARY_PATH=. flx ob_client.flx

On Linux:

.. code-block:: bash

  LD_LIBRARY_PATH=. flx ob_client.flx

and on Windows:

.. code-block:: bash

  PATH=. flx ob_client.flx

Result
------

The result should be this output on your console:

.. code-block:: text

  Setup string ``
  name joe

Debugging
---------

If you set the environment variable `FLX_SHELL_ECHO=1` then all calls the 
system shell will be echoed to the console. In addition, calls to the
system dynamic loader to load a plugin will be reported.




