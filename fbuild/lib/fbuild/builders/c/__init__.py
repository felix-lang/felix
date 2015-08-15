import abc
from functools import partial
from itertools import chain

import fbuild
import fbuild.db
import fbuild.temp
import fbuild.builders
import fbuild.builders.platform
import fbuild.functools
from fbuild.path import Path

# ------------------------------------------------------------------------------

class MissingHeader(fbuild.ConfigFailed):
    def __init__(self, filename=None):
        self.filename = filename

    def __str__(self):
        if self.filename is None:
            return 'missing header'
        else:
            return 'missing header %r' % self.filename

# ------------------------------------------------------------------------------

class Builder(fbuild.builders.AbstractCompilerBuilder):
    def __init__(self, *args, flags=(), cross_compiler=False, **kwargs):
        self.flags = tuple(flags)

        # If we're a cross compiler, don't try to execute tests as they'll
        # probably fail.
        self.cross_compiler = cross_compiler

        super().__init__(*args, **kwargs)

        # ----------------------------------------------------------------------
        # Check the builder to make sure it works.

        self.ctx.logger.check('checking if %s can make objects' % self)
        try:
            with self.tempfile_compile('int main() { return 0; }'):
                self.ctx.logger.passed()
        except fbuild.ExecutionError as e:
            raise fbuild.ConfigFailed('compiler failed: %s' % e)

        self.ctx.logger.check('checking if %s can make libraries' % self)
        try:
            with self.tempfile_link_lib('int foo() { return 5; }'):
                self.ctx.logger.passed()
        except fbuild.ExecutionError as e:
            raise fbuild.ConfigFailed('lib linker failed: %s' % e)

        self.ctx.logger.check('checking if %s can make exes' % self)
        try:
            if not self.cross_compiler:
                self.tempfile_run('int main() { return 0; }')
            else:
                with self.tempfile_link_exe('int main() { return 0; }'):
                    pass
        except fbuild.ExecutionError as e:
            raise fbuild.ConfigFailed('exe linker failed: %s' % e)
        else:
            self.ctx.logger.passed()

        self.ctx.logger.check('checking if %s can link lib to exe' % self)
        with fbuild.temp.tempdir() as dirname:
            src_lib = dirname / 'templib' + self.src_suffix
            with open(src_lib, 'w') as f:
                print('''
                    #ifdef __cplusplus
                    extern "C" {
                    #endif
                    #if defined _WIN32 || defined __CYGWIN__
                    __declspec(dllexport)
                    #else
                    __attribute__((visibility("default")))
                    #endif
                    int foo() { return 5; }
                    #ifdef __cplusplus
                    }
                    #endif
                ''', file=f)

            src_exe = dirname / 'tempexe' + self.src_suffix
            with open(src_exe, 'w') as f:
                print('''
                    #include <stdio.h>
                    #ifdef __cplusplus
                    extern "C" {
                    #endif
                    extern int foo();
                    #ifdef __cplusplus
                    }
                    #endif
                    int main(int argc, char** argv) {
                        printf("%d", foo());
                        return 0;
                    }''', file=f)

            obj = self.uncached_compile(src_lib, quieter=1)
            lib = self.uncached_link_lib(dirname / 'templib', [obj],
                    quieter=1)
            obj = self.uncached_compile(src_exe, quieter=1)
            exe = self.uncached_link_exe(dirname / 'tempexe', [obj],
                    libs=[lib],
                    quieter=1)

            if not self.cross_compiler:
                try:
                    stdout, stderr = self.run([exe], quieter=1)
                except fbuild.ExecutionError:
                    raise fbuild.ConfigFailed('failed to link lib to exe')
                else:
                    if stdout != b'5':
                        raise fbuild.ConfigFailed('failed to link lib to exe')
                    self.ctx.logger.passed()

    # --------------------------------------------------------------------------

    def build_lib(self, dst, srcs, *args, **kwargs):
        """Compile all of the passed in L{srcs} in parallel, then link them
        into a library."""
        return self._build_link(self.link_lib, dst, srcs, *args, **kwargs)

    def build_exe(self, dst, srcs, *args, **kwargs):
        """Compile all of the passed in L{srcs} in parallel, then link them
        into an executable."""
        return self._build_link(self.link_exe, dst, srcs, *args, **kwargs)

    def _build_link(self, function, dst, srcs, *,
            objs=[],
            includes=[],
            macros=[],
            warnings=[],
            cflags=[],
            ckwargs={},
            libs=[],
            external_libs=[],
            lflags=[],
            lkwargs={}):
        """Actually compile and link the sources."""
        objs = objs + self.build_objects(srcs,
            includes=includes,
            macros=macros,
            warnings=warnings,
            flags=cflags,
            **ckwargs)

        return function(dst, objs,
            libs=libs,
            external_libs=external_libs,
            flags=lflags,
            **lkwargs)

    # -------------------------------------------------------------------------

    def run(self, cmd, *args, runtime_libpaths=[], **kwargs):
        """Executes a c executable."""
        exe = cmd[0]
        return self.ctx.execute(cmd, *args,
            runtime_libpaths=runtime_libpaths + [l.parent for l in exe.libs],
            **kwargs)


    def tempfile_run(self, *args,
            quieter=1,
            ckwargs={},
            lkwargs={},
            **kwargs):
        """Overload tempfile_run to add the library search path."""
        with self.tempfile_link_exe(*args,
                quieter=quieter,
                ckwargs=ckwargs,
                **lkwargs) as exe:
            return self.run([exe], quieter=quieter, **kwargs)

    # -------------------------------------------------------------------------

    def check_statement(self, name, statement, *,
            msg=None, headers=[], **kwargs):
        code = '''
            %s;
            int main() {
                %s
                return 0;
            }
        ''' % ('\n'.join('#include <%s>' % h for h in headers), statement)

        self.ctx.logger.check(msg or 'checking %r' % name)
        if self.try_compile(code, **kwargs):
            self.ctx.logger.passed()
            return True
        else:
            self.ctx.logger.failed()
            return False

    def check_statements(self, *items, msg='checking %r', **kwargs):
        results = set()
        for name, statement in items:
            if self.check_statement(name, statement, msg=msg % name, **kwargs):
                results.add(name)

        return results

    # -------------------------------------------------------------------------

    def check_header_exists(self, header, **kwargs):
        return self.check_statement(header, '',
            msg='checking if header %r exists' % header,
            headers=[header],
            **kwargs)

    def check_functions_exist(self, *args, **kwargs):
        return self.check_statements(*args,
            msg='checking if function %r exists', **kwargs)

    def check_macros_exist(self, *macros, **kwargs):
        code = '''
            #ifndef %s
            #error %s
            #endif
        '''

        return self.check_statements(
            *((m, code % (m, m)) for m in macros),
            msg='checking if macros %r exists', **kwargs)

    def check_types_exist(self, *types, **kwargs):
        items = []
        for name in types:
            try:
                name, statement = name
            except ValueError:
                name, statement = name, '%s t;' % name
            items.append((name, statement))

        return self.check_statements(*items,
            msg='checking if type %r exists', **kwargs)

# ------------------------------------------------------------------------------

class Library(Path):
    """Wrapper around a library path that carries extra metadata about what
    was used to compile the library."""

    def __new__(cls, *args,
            libpaths=(),
            libs=(),
            external_libs=(),
            **kwargs):
        self = super().__new__(cls, *args, **kwargs)

        self.libpaths = tuple(libpaths)
        self.libs = tuple(libs)
        self.external_libs = tuple(external_libs)

        return self

    def __repr__(self):
        return 'Library({}{}{}{})'.format(
            super().__repr__(),
            ', libpaths={}'.format(self.libpaths) if self.libpaths else '',
            ', libs={}'.format(self.libs) if self.libs else '',
            ', external_libs={}'.format(self.external_libs)
                if self.external_libs else '')

    def __eq__(self, other):
        if self is other:
            return True

        # Check the types as well because Path doesn't require that for
        # equality.
        return isinstance(other, self.__class__) and \
            super().__eq__(other) and \
            self.libpaths == other.libpaths and \
            self.libs == other.libs and \
            self.external_libs == other.external_libs

    def __hash__(self):
        return hash((
            super().__hash__(),
            self.libpaths,
            self.libs,
            self.external_libs))

# ------------------------------------------------------------------------------

class Executable(Path):
    """Wrapper around an executable path that carries extra metadata about what
    was used to link the executable."""

    def __new__(cls, *args, libs=(), **kwargs):
        self = super().__new__(cls, *args, **kwargs)

        self.libs = tuple(libs)

        return self

    def __repr__(self):
        return 'Executable({0}{1})'.format(
            super().__repr__(),
            ', libs={0}'.format(self.libs) if self.libs else '')

    def __eq__(self, other):
        if self is other:
            return True

        # Check the types as well because Path doesn't require that for
        # equality.
        return isinstance(other, self.__class__) and \
            super().__eq__(other) and \
            self.libs == other.libs

    def __hash__(self):
        return hash((
            super().__hash__(),
            self.libs))

# ------------------------------------------------------------------------------

def _guess_builder(name, functions, ctx, *args,
        platform=None,
        platform_options=[],
        **kwargs):
    if platform is None:
        platform = fbuild.builders.platform.guess_platform(ctx, platform)

    for subplatform, function in functions:
        if subplatform <= platform:
            new_kwargs = kwargs.copy()

            for p, kw in platform_options:
                if p <= subplatform:
                    new_kwargs.update(kw)

            # Try to use this compiler. If it doesn't work, skip this compiler
            # and try another one.
            try:
                x = fbuild.functools.call(function, ctx, *args, **new_kwargs)
                return x
            except fbuild.ConfigFailed:
                pass

    raise fbuild.ConfigFailed('cannot find a %s builder for %s' %
        (name, platform))

def guess_static(*args, **kwargs):
    """L{static} tries to guess the static system c compiler according to the
    platform. It accepts a I{platform} keyword that overrides the system's
    platform. This can be used to use a non-default compiler. Any extra
    arguments and keywords are passed to the compiler's configuration
    functions."""

    return _guess_builder('c static', (
        ({'avr', 'gcc'}, 'fbuild.builders.c.gcc.avr.static'),
        ({'iphone', 'simulator'},
            'fbuild.builders.c.gcc.iphone.static_simulator'),
        ({'iphone'}, 'fbuild.builders.c.gcc.iphone.static'),
        ({'darwin'}, 'fbuild.builders.c.clang.darwin.static'),
        ({'darwin'}, 'fbuild.builders.c.gcc.darwin.static'),
        ({'posix'}, 'fbuild.builders.c.clang.static'),
        ({'posix'}, 'fbuild.builders.c.gcc.static'),
        ({'windows'}, 'fbuild.builders.c.msvc.static'),
    ), *args, **kwargs)

def guess_shared(*args, **kwargs):
    """L{shared} tries to guess the shared system c compiler according to the
    platform. It accepts a I{platform} keyword that overrides the system's
    platform. This can be used to use a non-default compiler. Any extra
    arguments and keywords are passed to the compiler's configuration
    functions."""

    return _guess_builder('c shared', (
        ({'avr', 'gcc'}, 'fbuild.builders.c.gcc.avr.shared'),
        ({'iphone', 'simulator'},
            'fbuild.builders.c.gcc.iphone.shared_simulator'),
        ({'iphone'}, 'fbuild.builders.c.gcc.iphone.shared'),
        ({'darwin'}, 'fbuild.builders.c.clang.darwin.shared'),
        ({'darwin'}, 'fbuild.builders.c.gcc.darwin.shared'),
        ({'posix'}, 'fbuild.builders.c.clang.shared'),
        ({'posix'}, 'fbuild.builders.c.gcc.shared'),
        ({'windows'}, 'fbuild.builders.c.msvc.shared'),
    ), *args, **kwargs)
