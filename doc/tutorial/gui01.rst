==================================
Getting Started With the Felix GUI
==================================

Felix comes with a library to create platform
independent graphical user interfaces. It uses
the Simple Direct Media Layer, version 2, SDL2
system with add-ons to do this.

SDL2 runs on Linux, OSX, Windows, iOS and Android
and is designed for implementation of portable games.

Installation
============

For the Felix GUI to work, *development versions* of
the following components must be installed:

.. code-block:: text

    SDL2
    SDL2_image
    SDL2_ttf
    SDL2_gfx

On Linux using apt package manager do this:

.. code-block:: text

    sudo apt-get install libsdl2-dev
    sudo apt-get install libsdl2_image-dev
    sudo apt-get install libsdl2_ttf-dev
    sudo apt-get install libsdl2_gfx-dev

Felix already contains database entries for these
packages, but at the time of writing this tutorial,
the libraries are expected to be in `/usr/local`
which is where you would put them if you built them yourself.

However Debian filesystem layout standards used by Linux OS
such as Ubuntu that use `apt` package manager put components
in `/usr/` instead. So unfortunately you will have to modify the
database by hand by editing these files

.. code-block:: text

    build/release/host/config/sdl2.fpc
    build/release/host/config/sdl2_image.fpc
    build/release/host/config/sdl2_ttf.fpc

replacing `/usr/local` with just `/usr`. To preserved these modifications
across upgrades, you should also copy the files:

.. code-block:: bash

    cp build/release/host/config/sdl2.fpc $HOME/.felix/config
    cp build/release/host/config/sdl2_image.fpc $HOME/.felix/config 
    cp build/release/host/config/sdl2_ttf.fpc $HOME/.felix/config 

I hope this problem can be reduced in future versions, but it is
likely to be an issue for some time because most developers will have
a libraries installed in both places, depending on whether they're
using a package manager to install them, or building from source.

To check your installation works, do this:

.. code-block:: bash

    make tutopt-check

and a series of tests will run which use the Felix GUI and SDL2.

