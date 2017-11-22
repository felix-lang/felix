===================
Installation Guide
===================

This is the Felix installation guide.

Linux: Ubuntu
=============

Install Pre-requisites
----------------------

The first step is to install the pre-requisites.
The following list may not be complete. From 
command line or other tool install packages:

.. code-block:: bash

   sudo apt-get install binutils
   sudo apt-get install make
   sudo apt-get install git
   sudo apt-get install g++
   sudo apt-get install python3
   sudo apt-get install ocaml-native-compilers

These are optional components which provide graphics for
the Felix GUI using Simple Direct Media Layer, version 2:

.. code-block:: bash

   sudo apt-get install libsdl2-dev
   sudo apt-get install libsdl2-ttf-dev
   sudo apt-get install libsdl2-image-dev
   sudo apt-get install libsdl2-gfx-dev
 
There are many other optional packages for which there
is some level of support or interest. These include,
without their apt-name at the momen (sorry, please help!):

* gmp: gnu big number library
* gmp++: C++ wrapper for gmp
* gsl: gnu scientific library
* botan: Crypto library

Clone Felix
-----------

You first need to configure git, please see git docs.
Then for a user without write access to the Felix repository,
make a workspace directory *in which* the Felix repository
directory will be created and `cd` into it. Now grab a copy of Felix:

.. code-block:: bash

    git clone https://github.com/felix-lang/felix.git


Build Felix
-----------

Now you have to go into the repository clone and build Felix:

.. code-block:: bash

    cd felix
    . buildscript/linuxsetup.sh
    make

What happens is that a build system called *fbuild* which is a Python
program, will build a bootstrap version of Felix first.

Then, the bootstrap Felix will be used to build Felix again,
this time using Felix own build tools, which are written
in Felix.

Finally, a four test suites run. The first is a small number
of bad tests that are supposed to all fail, ignore it.

Then the main regression test suite runs. Most of these tests
should pass, or there's a bug, but as long as most pass, don't
worry (at least, not yet!)

Then some tutorial examples are run as tests. Again, most
should pass but don't worry about one or two fails.

Finally, some optional tests run, which exercise optional
packages. Most of these are graphics and GUI tests and
they WILL fail on Ubuntu. We will fix that in a moment!

Patch SDL2 configuration
------------------------

Felix comes with a pre-built configuration for SDL that
assumed you compiled and built it yourself and installed
it in `/usr/local`, however if you use `apt-get` to install
the components as instructed in this document, it will
be installed in `/usr/` instead. So we have to fix this
with a patch.

You need to edit these files now, using your favourite editor:

.. code-block:: text

    build/release/host/config/sdl2.fpc
    build/release/host/config/sdl2_image.fpc
    build/release/host/config/sdl2_ttf.fpc

and replace `/usr/local` with `/usr`.

Now you should be able to run the GUI tests:

.. code-block:: bash

    make tutopt-check

Preserve the SDL2 Patches
-------------------------

The changes you made above to the Felix configuration
will be lost next time you upgrade Felix. To fix this
problem do this:

.. code-block:: bash

    mkdir -p $HOME/.felix/config
    cp build/release/host/config/sdl2*.fpc $HOME/.felix/config

Next time you build Felix, it will first clobber the changed
files in `build/release/config` and then clobber those changed
files with the ones from `$HOME/.felix/config`, thereby
preserving your modification.


Installation
------------

Felix does *not* have to be installed to work.
I recommend you do *not* install it, at least not yet.
Installation is for enterprise users, rather than personal users.

The reason is that upgrades are frequent: Felix is typically
upgraded every day. Rebuilding Felix is easy, but it is a pain
reinstalling it all the time, it is better initially to run it
in place. But here is how you would install it:

.. code-block:: bash

    sudo mkdir -p /usr/local/lib
    sudo mkdir -p /usr/local/bin
    sudo make install

This will put most of the Felix system in `/usr/local/lib/felix/felix-version`
where `felix-version` is the version of Felix you're installing. You can
install many versions of Felix all at once.

The install process *also* puts the `flx` command into `/usr/local/bin`.
This will overwrite any previous `flx`. For the installed Felix to work
at all you will need to setup the `PATH` variable:

.. code-block:: bash

    export PATH=/usr/local/bin:$PATH

The best place to do this is in your `$HOME/.profile`, if it is not
set already.

For full plugin and dynamic library support, you will also need to
set `LD_LIBRARY_PATH`. Normally, `flx` sets this for you, but if you want
to run Felix built executables directly as standalone programs, *and*
you want to link to Felix shared libraries, including plugins, 
then the system linker has to find the libraries so you will also need this:

.. code-block:: bash

    export LD_LIBRARY_PATH=/usr/local/lib/felix/felix-latest/host/lib/rtl:$LD_LIBRARY_PATH

Felix does not put its shared libraries in the usual place, directly in 
a `/usr/lib` or `/usr/local/lib` directory. This is deliberate.
You need to be able to delete a Felix version, or all of Felix easily,
and for this reason *almost everything* lives under master directory
`/usr/local/lib/felix` and subdirectory `felix-latest`, the main
exception being the `flx` program, which is copied to `/usr/local/bin`.

Running in Place
----------------

This option is prefered over installing Felix at the moment,
although it is a little tricker to set up, it makes it very
much easier to upgrade Felix.

After you have built Felix, you can use it in place, without
installing it. First you need to do this: make sure you
are still in the Felix directory, be very careful to use the
correct quotation marks as indicated below!!

.. code-block:: bash

    echo `export PATH=/usr/local/bin:$PATH` >> $HOME/.profile
    mkdir -p $HOME/.felix/config
    echo "FLX_INSTALL_DIR: $PWD/build/release" >$HOME/.felix/config/felix.fpc

The `flx` executable looks to see if the file `$HOME/.felix/config/felix.fpc` exists,
and if it does, it will set the variables seen in that file. You can also set the
environment variable FLX_INSTALL_DIR in the Linux environment by adding this command
to your `$HOME/.profile`:
 
.. code-block:: bash

    export FLX_INSTALL_DIR=$PWD/build/release

where $PWD has to be replaced by the absolute path of the repository,
and then the command above should be put into your $HOME/.profile.
I personally use the first method.

Test it
-------

This should work now:

.. code-block:: bash

    flx hello.flx



