================
Platform Concept
================

Felix uses a powerful abstract platform concept which is designed
to support both personal use and enterprise level operation.

To understand how this works, we need first to understand the
notion of a `platform`, and the platforms involved with Felix.

A platform is basically a specific computer with specific
environment configured. Roughly, a Windows box is one platform,
a Mac is another, and Linux is a third.

However this is overly simplistic, because for example on
Linux you can compile C++ with GNU g++ but you could also use
clang. For C code, these produce the different files with
the same interfaces, but this is definitely not the case
for C++. Even though they share the same ABI, the library
templates are different and compiled code using C++ standard
library components is not compatible.

Platforms
=========

When you use Felix there are four significant platforms.
These are often the same, but they need not be.

The build platform
------------------

The `build` platform is the platform used to build
Felix itself. If you build from source that's your computer,
but it may be that one day there is a Debian package for
Felix, and in that case the Debian autobuilder is the
build platform.

Similarly, if you can get the GitHub posted tarball of
Felix to actually work, the build platform for that system
is the Travis Continuous Integration Server.

The host platform
-----------------

The `host` platform is the system on which you write
Felix programs and cause the Felix compiler `flxg`
to be run.

This is usually your computer, the one you are sitting
in front of when you edit and compile. However, there
are complicated systems where this is not enough of
a description. For example Windows 10 can run Ubuntu,
and you can write programs for Windows on that Ubuntu.

The target platform
-------------------

The `target` platform is the one where the C++ compiler
runs. It is usually the same as the host platform, but it
need not be.

You can run Ubuntu on Windows 10, and you can run `flxg`
under Ubuntu to produce C++ files, which are then
compiled by MSVC++ using the CMD.EXE shell with an
environment set up for MSVC++.

The run platform
----------------

The `run` platform is the machine where your generated
binary code actually runs.

Commonly on a Mac, the host platform is your Mac,
the target is the same Mac, but the run platform is
actually iOS because you're building an iPhone app.

In this case, your target C++ compiler will be
clang configured to generate code for the run platform,
which is an ARM processor, even though the Mac is an x86_64
processor.

Cross-Cross Compilation Model
=============================

What this means is that Felix is actually a cross-cross-compiler,
not merely a cross-compiler!

This means generating platform dependent code requires two
steps of platform adaption. In the first phase the host
Felix system may generate C++ code specific to the downstream
build tools.

In the second phase, a C++ cross compiler may compile that
code to binaries, specific to the downstream run platform.

For most users, the build platform, host platform, target platform
and run platform will all be the same.


Felix target directories
========================

Felix represents the platform model primarily by allowings
the user to create a number of distinct target directories.

After building Felix, in the build directory, there 
are two files:

.. code-block:: text

    build/release/share
    build/release/host

The directory named `host` is the default target. It represents the
platform chain for which the host, target, and run platformn are
all the same and use the defaults set up during the build processes.

On Linux, for example, this will normally be that you are
generating C++ code for Unix, using the `g++` compiler tool chain
compile it, and targetting 64 bit Linux.

If you want to also be able to use the clang tool chain,
you would make a new target directory:

.. code-block:: text

    build/release/clang

and use it like this:

.. code-block:: bash

     flx --target=clang hello.flx

This will use the clang toolchain instead of g++,
and it will link against libraries built with clang,
instead of those built by g++.

In the `GNUmakefile` there are two ready made make targets
for making Felix targets for the iPhone simulator and the
iPhone. These look like:

.. code-block:: make

    iphonesimulator:
      # prepare directory
      flx_build_prep --target-dir=build/release --target-bin=iphonesimulator --source-dir=build/release \
              --source-bin=host --clean-target-bin-dir --copy-compiler --copy-pkg-db \
              --copy-config-headers --toolchain=toolchain_iphonesimulator --debug
      rm -rf build/rtl-tmp
      # build rtl
      DYLD_LIBRARY_PATH=build/release/host/lib/rtl flx_build_rtl \
        --target-dir=build/release --target-bin=iphonesimulator --static --noexes


    iphoneos:
      # prepare directory
      flx_build_prep --target-dir=build/release --target-bin=iphoneos --source-dir=build/release \
              --source-bin=host --clean-target-bin-dir --copy-compiler --copy-pkg-db \
              --copy-config-headers --toolchain=toolchain_iphoneos --debug
      rm -rf build/rtl-tmp
      # build rtl
      DYLD_LIBRARY_PATH=build/release/host/lib/rtl flx_build_rtl \
        --target-dir=build/release --target-bin=iphoneos --static --noexes

These are advanced uses of the Felix build tools which create the extra 
targets

.. code-block:: text

    build/release/iphonesimulator
    build/release/iphoneos

which can be used on a Mac to build code for the respective run platforms like

.. code-block:: text
  
    flx --target=iphonesimulator filename.flx
    flx --target=iphone filename.flx

These use clang for building the code, with options set for it to
cross-compile to the relevant target.

Our purpose here is not meant to explain how to create a new target,
merely to show that the Felix architecture is designed to support
code generation for multiple targets.

In an enterprise install, the system administrator of a large network
would create and install targets for each kind of developer, and put
all of them up on a server, so the developers could pick the target
they need to use.















