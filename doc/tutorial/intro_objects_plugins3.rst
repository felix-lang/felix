Static Linked Plugins
=====================

Building an Executable
----------------------

Instead of just running `ob_client` as script, we can build an executable.
The command works on all platforms:

.. code-block:: bash

  flx --static -c -od . ob_client.flx

This will put an executable `ob_client` in the current directory on
Linux or OSX, or `ob_client.exe` on Windows.

You can run the executable:

.. code-block:: bash

  ./ob_client


and it will work as before, loading the plugin dynamically. Hopefully.
But the executable is not self-contained so cannot be easily shipped,
the plugin binary has to be shipped too, and put on the dynamic linker
search path.

Static Prelinking Plugins
-------------------------

There is a solution! When Felix tries to load a plugin by its text name,
it first looks in a special data structure called the *prelink respository*
to see if it is pre-loaded.
If not, it tries an actual OS level library load. If we want to avoid
this, we can statically link the plugin with the program, and then
load the appropriate information into the pre-link repository.

To do this, we have to first create an object file for the plugin,
instead of a DLL.

.. code-block:: bash

  flx --static -c -od . --nolink ob_implementation.flx

This puts the object file into the current directory.
It will be called `ob_implementation_static.o` on MacOSX or Linux,
or `ob_implementation_static.obj` on Windows.

Now we have to compile our program as an object file too:

.. code-block:: bash

  flx --static -c -od . --nolink ob_client.flx

Now we need a special file called a *static loader thunk*:

.. code-block:: felix

  // ob.flx
  open Dynlink;

  static-link-plugin ob_implementation;

  static-link-symbol ob_client_create_thread_frame in plugin ob_client;
  static-link-symbol ob_client_flx_start in plugin ob_client;

  val linstance =  Dynlink::prepare_lib("ob_client");
  C_hack::ignore(linstance);

This is actually our mainline now! We need to open the class `Dynlink`
to find the functions used for storing stuff in the registry.

Next, we specify the name of our plugin.

We also need to specify the names of two symbols used to run our
program. `ob_client_create_thread_frame` allocates a 
global storage object.  `ob_client_flx_start` runs the initialisation
procedure for that object: this is actually what you previously,
and incorrectly, thought of as your program. 

What?? Yes, that's right. Felix doesn't do programs, only libraries.
What you thought was your program is actually the side-effects of the 
initialisation procedure for a library.

Finally, we create an instance of the library `ob_client` with
Dylink's function `prepare_lib`. This creates the thread frame
object and initialises it (yep, that's your "program" running).

Since that's all we want to do we just ignore the library handle
and we're finished.

When the `ob_client` code runs, it tries to load the plugin
`ob_implementation`. But it finds it in the registry, along
with the stanard symbols a plugin has. The `static-link-plugin`
statement generates code that updates the repository.

Here's how you link the program:

.. code-block:: bash

  flx --static -c -od . \
    ob_implementation_static.o \
    ob_client_static.o \
    ob.flx

Notice that unfortunately you have to give the platform dependent name of the
object files. Notice also Felix adds the suffix `_static` to object files compiled
for static linkage.  Object files compiled for dynamic linkage get the suffix
`_dynamic` instead. On some platforms these are the same, but not Linux.
Dynamic link objects are compiled with `-fPIC` for position independent code.
Static link files are not. For the x86_64 processor, leaving out -fPIC
generates much faster function calls.

The final program can be run like:

.. code-block:: bash

  ./ob




