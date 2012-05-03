Introducing Fbuild
==================

I'd like to introduce yet another build system, this one called `Fbuild`.  It's
a different way to build things.  As opposed to pretty much every other build
system, `Fbuild` is designed as just a caching library for [Python
3](http://docs.python.org/py3k).  It takes advantage of Python's evaluation
scheme to implicitly describe the build dependency tree.  In order to evaluate
the build, we simply evaluate python functions and cache their results.  This
turns out to be an elegant and easy way to describe and program build systems.

On top of this alternative design, `Fbuild` comes out of the box with some
pretty advanced features:

 * Linux, Apple and Windows support
 * C, C++, OCaml, Java, Scala, Bison, and Felix builders
 * Multilevel namespaces for builders
 * Simple creation of new builders
 * Extensive configuration system for c90, c99, most of posix, and other
   libraries
 * On-demand configuration
 * Simultaneous building
 * Detects file changes via file digests
 * Pretty output
 * Very speedy

Here's a quick example, in a file called `fbuildroot.py`:

{{{
:::python
import fbuild.builders.c

def build():
    static = fbuild.builders.c.guess_static()
    static.build_exe('exe', ['a.c', 'b.c', 'c.c'])
}}}

When run with as `fbuild`, assuming all those files exist, produces:

{{{
determining platform     : {'bsd', 'darwin', 'macosx', 'posix'}
looking for program gcc  : ok /usr/bin/gcc
checking /usr/bin/gcc    : ok
checking /usr/bin/gcc with -g : ok
checking /usr/bin/gcc with -O2 : ok
checking /usr/bin/gcc with -c  : ok
looking for program ar         : ok /usr/bin/ar
looking for program ranlib     : ok /usr/bin/ranlib
checking if /usr/bin/gcc -c can make objects: ok
checking if /usr/bin/gcc -c can make libraries: ok
checking if /usr/bin/gcc -c can make exes: ok
checking if /usr/bin/gcc -c can link lib to exe: ok
 * /usr/bin/gcc -MM                     : c.c
 * /usr/bin/gcc -MM                     : b.c
 * /usr/bin/gcc -MM                     : a.c
 * /usr/bin/gcc                         : c.c -> build/c.o
 * /usr/bin/gcc                         : b.c -> build/b.o
 * /usr/bin/gcc                         : a.c -> build/a.o
 * /usr/bin/gcc                         : build/a.o build/b.o build/c.o -> build/exe
}}}

You may notice a couple interesting things about the output:

 1. `Fbuild` configures the c static builder for you and tests that it works.
 2. Much of the output is hidden. `Fbuild` does this because the output can get
    overwhelming for large projects.  It can be very easy to miss warnings and
    messages.  `Fbuild` hides this in a log file, normally called
    `build/fbuild.log`.
 3. The generated files are stored in a `build` directory.  This is a
    user-customizable directory that can be used to support different build
    targets, such as a `debug` and a `release` build.
 4. `Fbuild` automatically discovers and tracks and sorts the dependencies
    between each c file.

To develop the last idea further, `Fbuild` was designed to work with globbed
paths if you don't want to bother writing out each filename.  I'll show this by
rewriting the last example using the `fbuild.path.Path` class.  This is a
convenience class that implements many useful path functions, as well as
supports replacing the Unix-style '/' separator with the native path separator.
Lets combine that with showing off what happens if we tweak what we want built:

{{{
:::python
import fbuild.builders.c
import fbuild.path

def build():
    static = fbuild.builders.c.guess_static()
    lib = static.build_lib('lib', fbuild.path.Path.glob('*.c'))
    static.build_exe('exe', [], libs=[lib])
}}}

This will produce:

{{{
 * /usr/bin/ar           : build/a.o build/b.o build/c.o -> build/liblib.a
 * /usr/bin/ranlib       : build/liblib.a
 * /usr/bin/gcc          :  -> build/exe
}}}

As you can see, most of the work was skipped.  `Fbuild` detected that none of
the source files changed, so it didn't need to recompile them.  Since the glob
passed all the source files to `static.build_lib` they were all linked together
in a library.  Finally, since the arguments changed for `static.build_exe`,
`build/exe` was recompiled.

Now that you've got a taste for `Fbuild`, lets go through some of the reasoning
behind its design.  As I mentioned previously, every build system I've found is
based off of a multi-phase declarative tree evaluation.  Here's the list of the
system I looked at:

 * [Ant](http://ant.apache.org/)
 * [Automake](http://www.gnu.org/software/automake/)
 * [Boost Build](http://www.boost.org/doc/tools/build/index.html)
 * [Cmake](http://www.cmake.org/)
 * [Make](http://www.gnu.org/software/make/)
 * [Ocamlbuild](http://gallium.inria.fr/~pouillar/)
 * [Omake](http://omake.metaprl.org/index.html)
 * [Qmake](http://doc.trolltech.com/4.2/qmake-manual.html)
 * [Rake](http://rake.rubyforge.org/)
 * [SCons](http://www.scons.org/)
 * [Waf](http://code.google.com/p/waf/)

By "declarative tree evaluation", I mean that the order of evaluation is
explicitly encoded in a dependency tree.  Each node has a function that takes
one or more inputs and returns one or more outputs. By "multi-phased", I
consider building the tree as one phase, walking the dependencies as another,
and evaluating the nodes as yet another.  This contrasts with `Fbuild`, which I
consider to be a single-phase procedural build system.  As python evaluates
each cached function, the dependency implicitly gets evaluated.  This means
that it's possible to dynamically modify the build on demand:

Here's a stupid example of something that's difficult with `Make`:

{{{
:::python
import random
import fbuild.builders.c

def build():
    static = fbuild.builders.c.guess_static()
    static.build('exe1', ['exe1.c'])

    # Exit the build early if we roll a '6'
    if random.randint(1, 6) == 6: return

    static.build('exe2', ['exe2.c'])
}}}

Because we don't have to build up the tree first before we evaluate it, we
don't have to shoehorn weird nodes into the graph to perform an action between
one node and another.

For a more realistic example of how this is useful, consider a common problem
with compilers.  The compiler is normally written in one language, then the
compiler is used to compile the standard library.  This essentially requires
dynamically creating new nodes for the tree, and can result in some pretty
scary build scripts.  `Fbuild` makes it easy though:

{{{
:::python
import fbuild.builders.c

def build():
    static = fbuild.builders.c.guess_static()
    exe = static.build_exe('exe', [...])
    my_static = fbuild.builders.c.guess_static(exe)
    my_static.build_lib('lib', [...])
}}}

First, we configure the system C builder.  Next, we reconfigure another C
builder with the one we just created.  Finally, we use that builder to compile
our standard library.  As far as I can tell, most other build systems require
some painful contortions to support this, and I suspect most projects cheat by
ignoring the dependency between the standard library and the compiler.
`Fbuild` inherently supports this style since we don't have to work around the
graph.

Some systems allow for dynamically specifying dependency nodes in their script,
such as in `SCons`.  As far as I can tell, this is the simplest node you can
create:

{{{
:::python
def action(target, source, env):
    pass
env = Environment(BUILDERS={'A': Builder(action=Action(action))})
env.A('foo.out', 'foo.in')
}}}

First, you define a simple function that converts a source file into a target
file.  Next, you create an environment where you update the builders with your
custom builder.  Finally, you use the environment to perform your action.  It's
not actually that bad.  Here's the equivalent in `Fbuild`:

{{{
:::python
import fbuild.db

@fbuild.db.caches
def action(target:fbuild.db.DST, source:fbuild.dst.SRC):
    pass

def build():
    action('foo.out', 'foo.in')
}}}

It's actually a little longer.  Part of that is that `SCons` imports all of the
common code into the global namespace.  This can save some lines,  but it also
forces a lot of `SCons` into the same namespace.  This forces each node to be
uniquely named, and is more likely to run into conflicts.  Anyway, for `Fbuild`
we need to do a couple extra things to get the to cache.  First, we mark that
the function is cached using the `fbuild.db.caches` decorator.  There's also
`fbuild.db.cachemethod` for decorating methods, and
`fbuild.db.PersistentObject` parent class for caching object creation.  We also
need to tell `Fbuild` if the arguments are source or destination files.  We do
this using python 3.0's annotation support. This tells the database to check to
see if source files were modified or if the destination file was removed.  If
either of these occurred, the function is rerun.

As the builders get nontrivial and more complex, `Fbuild` becomes a much
simpler. For instance, consider `SCons`'s [Yacc builder]
(http://scons.tigris.org/source/browse/*checkout*/scons/trunk/src/engine/SCons/Tool/yacc.py?content-type=text%2Fplain).
There are three main things it needs to do.  First, `SCons` has to set up the
environment variables for `yacc`. Next, they register `yacc` with the C
builder.  Finally, they have a function that uses the values from the
environment and actually evaluates yacc.

`Fbuild`'s current bison builder is much more straight forward:

{{{
:::python
import fbuild
import fbuild.builders
import fbuild.db
from fbuild.path import Path

class Bison(fbuild.db.PersistentObject):
    def __init__(self, exe=None, flags=[], *, suffix='.c'):
        self.exe = fbuild.builders.find_program([exe or 'bison'])
        self.flags = flags
        self.suffix = suffix

    def __call__(self, src:fbuild.db.SRC, dst=None, *,
            suffix=None,
            verbose=False,
            name_prefix=None,
            defines=False,
            flags=[],
            buildroot=None) -> fbuild.db.DST:
        buildroot = buildroot or fbuild.buildroot
        suffix = suffix or self.suffix
        dst = Path.addroot(dst or src, buildroot).replaceext(suffix)
        dst.parent.makedirs()

        cmd = [self.exe]

        if verbose:
            cmd.append('-v')

        if name_prefix is not None:
            cmd.extend(('-p', name_prefix))

        if defines:
            cmd.append('-d')

        cmd.extend(self.flags)
        cmd.extend(flags)
        cmd.extend(('-o', dst))
        cmd.append(src)

        fbuild.execute(cmd, self.exe, '%s -> %s' % (src, dst), color='yellow')

        return dst
}}}

We do some simple configuration to set up the executable and flags.  Then we
define a function to evaluate the executable.  Since we're able to wrap a
callable object, we don't have to be aware of anything else happening in the
tree.  We also don't need to pollute the environment with extra values, when
they're only needed in this builder.

It's easy to extend this paradigm to configuration.  Say you want to check if a
header exists.  It's simple to write this:

{{{
:::python
import fbuild.builders.c
import fbuild.db

@fbuild.db.caches
def check_stdlib_h(builder):
    return builder.header_exists('stdlib_h')

def build():
    static = fbuild.builders.c.guess_static()

    if check_stdlib_h(builder):
        ...
    else:
        ...
}}}

It's even easier using `fbuild.config` though.  That package replicates much of
the functionality in `autoconf` to provide a convenient lazy configuration
system:

{{{
:::python
import fbuild.builders.c
import fbuild.config.c.c90

def build():
    static = fbuild.buildesr.c.guess_static()
    stdlib_h = fbuild.config.c.c90.stdlib_h(static)
    if stdlib_h.header:
        ...
    else:
        ...

    if stdlib_h.atof.return_type == 'float':
        ...
    else:
        ...
}}}

There are already hundreds of tests defined in `fbuild.config`.  It's a new
system, but it's already proven to be pretty useful.

So, that's `Fbuild`.  Hopefully you'll find it interesting.  It's still a very
young project, and the interfaces aren't stable.  But if you still want to
check it out, you can find the source
[here](http://git.felix-lang.org/?p=fbuild.git;a=summary). If you want to try
it out, you'll need python 3.0 and git installed.  You can check it out with:

    git clone http://git.felix-lang.org/r/fbuild.git

There are a handful of examples in the examples directory. These can be run
either with `examples/examples.py`, or run directly as in:

{{{
cd examples/c
../../fbuild-light
}}}

Where `fbuild-light` is just a simple wrapper around the `fbuild` executable.
It's used so that you can run `fbuild` without it being installed.

If you have any questions, feel free to ask on the [fbuild mailing
list](http://groups.google.com/group/fbuild), or reply to the comments.
