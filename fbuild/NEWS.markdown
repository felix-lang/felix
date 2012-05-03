I'm pleased to announce FBuild version 0.2. It's been way too long since the
last FBuild release, but a lot of features have been added over the past year.
Overall, the core concept of FBuild is still the same, but there have been
some structural changes in response to some suggestions I've had.

The most obvious change is that there is now a context object that you must
pass around in order to interact with cacheable functions. Now, to write a
simple builder, you must do:

    import fbuild.builders.c

    def build(ctx):
        shared = fbuild.builders.c.guess_shared(ctx)
        shared.build_exe('foo', ['foo.c', 'bar.c'])

Next is the support for command line targets. This allows you to easily call a
specifically marked function, as in:

    import fbuild.builders.ocaml
    def build(ctx):
        ocamlc = fbuild.builders.ocaml.Ocamlc(ctx)
        ocamlc.build_exe('foo', ['foo.ml'])

    import fbuild.target
    @fbuild.target.register(name='run-test', help='run the foo test')
    def run_test(ctx):
        # make sure 'foo' is built
        build(ctx)
        ctx.execute([ctx.buildroot / 'foo', 'world'])
        ctx.execute([ctx.buildroot / 'foo', 'fbuild'])

Which can be run by:

    % fbuild run-test
    looking for program ocamlc.opt : ok /opt/local/bin/ocamlc.opt
    looking for program ocamldep.opt : ok /opt/local/bin/ocamldep.opt
    determining platform             : {'bsd', 'darwin', 'macosx', 'posix'}
    checking if ocamlc.opt can make objects : ok
    checking if ocamlc.opt can make libraries: ok
    checking if ocamlc.opt can make exes    : ok
    checking if ocamlc.opt can link lib to exe: ok
     * ocamldep.opt                         : foo.ml
     * ocamlc.opt                           : build/foo.ml -> build/foo.cmo
     * ocamlc.opt                           : build/foo.cmo -> build/foo
    Hello world
    Hello fbuild!

One nice feature of the targets is that they are automatically integrated into
the "--help" option:

    % fbuild -h
    Usage: fbuild-light [options] target1 [target2 target3 ...]

    ...

    Targets:
      build
      run-test	run the foo test

Next up is another removal of magic, this time in `fbuild.config.c`. Now, if
you want to define a config test that requires a header, you must explicitly
define it yourself, as in:

    import fbuild.config.c as c
    class assert_h(c.Test):
        header = c.header_test('assert.h')

Finally, the last major addition was support for Linux's LD\_LIBRARY\_PATH and
OS X's DYLD\_LIBRARY\_PATH for shared libraries. Now it's much easier to
execute test code that depends on a shared library. To use it, just do:

    ctx.execute(cmd, runtime_libpaths=['lib1', 'lib2'])

Beyond that, we added builders for ghc, avr-gcc, gcc/iOS, O'Caml Findlib,
and O'Caml Batteries. For configuration support, we now can check for SDL,
LLVM, GNU Readline, GNU GMP, Google Test Framework, and OpenSSL. And, of
course, lots of miscellaneous bug fixes. For a more detailed breakdown, please
look at our git history.

Oh and one last thing. FBuild-0.2's build/ directory is incompatible with
FBuild-0.1's build/ directory, so you'll have to remove the old directory
before you can compile.

Please let us know if you run into any problems or need any help. You can:

 * File a bug at http://github.com/felix-lang/fbuild/issues
 * Contact our mailing list at http://groups.google.com/group/fbuild
 * Or find us on IRC on freenode.net in #felix
