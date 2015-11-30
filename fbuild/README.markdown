Fbuild Build System v0.2
========================

[![AppVeyor](https://ci.appveyor.com/api/projects/status/github/felix-lang/fbuild?svg=true)](https://ci.appveyor.com/project/erickt/fbuild)
[![Travis](https://travis-ci.org/felix-lang/fbuild.svg?branch=master)](https://travis-ci.org/felix-lang/fbuild)

Overview
--------

Fbuild is a build system designed for configuring and compiling both small and
large projects. It has extensive feature list:

 * Linux, Apple, and Windows support
 * Supports compiling C, C++, OCaml, Java, Scala, GHC, and Felix
 * Extensive configuration detection for the C and C++ standard library, posix,
   and more
 * Parallel compilation
 * Cross compiling
 * Digest-based tracking of file changes
 * Full [Python](http://docs.python.org/py3k) scriptability
 * Easy to extend
 * Pretty output
 * ... and it is quite fast, too

**WARNING:** MinGW is currently NOT supported. It *should* be, but things quickly went haywire. You can try it still, but note that *building shared libraries does not work*. Using Visual C++ works as expected.

Downloading and Installation
----------------------------

Fbuild is hosted and developed on
[Github](http://github.com/felix-lang/fbuild). It requires [Python
3](http://docs.python.org/py3k). To download the current Fbuild version, you
visit the Fbuild website. Or, if you have access to the `curl` program, you can
just run this command:

    $ curl -L https://github.com/felix-lang/fbuild/tarball/v0.2 | tar -zx

To download the development version, run this [git](http://git-scm.com)
command:

    $ git clone https://github.com/felix-lang/fbuild.git

if your firewall blocks port 9418 (but you still have access to the https://
protocol).

To install, run:

    $ python3 setup.py install

Introduction
------------

**NOTE:** There are some incomplete docs at [RTD](http://fbuild.readthedocs.org/en/latest/).

Fbuild has extensive support for advanced build systems, but this doesn't
complicate simple projects. Here is the classic "Hello World" example, written
in a C file named `helloworld.c`:

    #include <stdio.h>
    int main() {
        printf("hello world!\n");
        return 0;
    }

Along side of it, we will create a Python 3 file named `fbuildroot.py`, which
will drive the compilation. We'll step through each line in a moment:

    import fbuild.builders.c

    def build(ctx):
        builder = fbuild.builders.c.guess_static(ctx)
        exe = builder.build_exe('helloworld', ['helloworld.c'])

        ctx.logger.log(' * running ' + exe)
        ctx.execute([exe])

As you can see, it's a pretty compact Python script. To compile it, if you
installed Fbuild, simply run `fbuild` in the same directory as the
`fbuildroot.py`. Otherwise, you can run `fbuild-light` out of the Fbuild
distribution, which doesn't require installation. This is what you'll see:

    $ fbuild  # or $FBUILD_DIR/fbuild-light
    determining platform     : {'bsd', 'darwin', 'macosx', 'posix'}
    looking for program gcc   : ok /usr/bin/gcc
    checking gcc              : ok
    looking for program ar    : ok /usr/bin/ar
    looking for program ranlib : ok /usr/bin/ranlib
    checking if gcc can make objects : ok
    checking if gcc can make libraries : ok
    checking if gcc can make exes      : ok
    checking if gcc can link lib to exe : ok
     * gcc                              : helloworld.c -> build/helloworld.o
     * gcc                              : build/helloworld.o -> build/helloworld
     * running build/helloworld
    hello world!

Broken down, Fbuild:

 * automatically determined that I'm on a Apple machine
 * configured gcc for our C builder
 * tested that the C builder worked
 * compiled our code
 * and finally, ran the program

Now lets go through the `fbuildroot.py`:

    ...
    def main(ctx):
        ...

Fbuild is written as a Python library with the `fbuild` script as a simple
driver in order start up and stop the build. The default entry point is a
function called `build`. If you run `fbuild` without any arguments or with
`fbuild build`, it will call this function. You can also create your own entry
points with the `fbuild.target` module, which is described further on.

    import fbuild.builders.c
    ...
    builder = fbuild.builders.c.guess_static(ctx)
    ...

Next we have to make a C builder. Since pretty much each platform has it's own
C compiler, Fbuild provides a mechanism to guess the platform's preferred one.
`fbuild.builders.c.guess_static` creates a C builder that is capable of
creating static libraries. You can also use `fbuild.builders.c.guess_shared`
if you want to create dynamically loadable libraries. In this case though,
we're just creating an executable, so it doesn't matter which function we use.

    exe = builder.build_exe('helloworld', ['helloworld.c'])

Now that we've got a builder, we can compile our binary. The first argument is
the name of the executable, and the second is a list of input sources. There are
a large amount of other options available. The most important are:

| Argument       | Description                                              |
| -------------- | -------------------------------------------------------- |
| includes       | a list of directories to add to the include search path  |
| libs           | a list of libraries to link in                           |
| libpaths       | a list of directories to add to the library search path  |
| external\_libs | a list of libraries Fbuild should not track modification |
| macros         | a list of string macros                                  |
| debug          | a boolean to enable debug builds                         |
| optimize       | a boolean to enable optimized builds                     |
| cflags         | a list of arbitrary arguments to pass to the compiler    |
| lflags         | a list of arbitrary arguments to pass to the linker      |

These arguments can also be passed to `fbuild.builders.c.guess_static` if you
want them to apply to everything built by the builder.

    ctx.logger.log(' * running ' + exe)
    ctx.execute([exe])

Finally, we'll execute the command we just compiled. We use `ctx.logger.log` so
that the message will be logged into the Fbuild log file, normally found in
`build/fbuild.log`. To run the command we use `ctx.execute`, which is a wrapper
around the `subprocess` module that also writes the command's output to the log
file.

In comparison, this is an equivalent `Makefile`, written to use gcc:

    exe = helloworld
    srcs = helloworld.c
    objs = $(srcs:%.c=%.o)

    all: helloworld
        ./helloworld

    helloworld: $(objs)
        gcc -o $@ $<

    %.o: %.c
        gcc -c -o $@ $<

As you can see, Fbuild's driver script is about half the size and does much
more.

Unit Tests
----------

To run the unit tests:

    $ cd tests
    $ python3 run_tests.py

Help
----

If you run into any problems, don't hesitate to ask a question on the [fbuild
mailing list](http://groups.google.com/group/fbuild).
