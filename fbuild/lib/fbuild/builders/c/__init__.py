import abc, copy
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
                    #elif defined __GNUC__
                    __attribute__ ((visibility ("default")))
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
            ldlibs=[],
            lkwargs={},
            include_source_dirs=True):
        """Actually compile and link the sources."""
        objs = objs + self.build_objects(srcs,
            includes=includes,
            macros=macros,
            warnings=warnings,
            flags=cflags,
            include_source_dirs=include_source_dirs,
            buildroot = self.ctx.buildroot / 'obj' / dst,
            **ckwargs)

        return function(dst, objs,
            libs=libs,
            external_libs=external_libs,
            ldlibs=ldlibs,
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

def _guess_builder(name, compilers, functions, ctx, *args,
        platform=None,
        platform_extra=set(),
        platform_options=[],
        exe=None,
        **kwargs):
    if platform is None:
        platform = fbuild.builders.platform.guess_platform(ctx, platform)
    if not platform_extra & compilers:
        if exe is not None:
            tp = identify_compiler(ctx, exe)
            if tp is None:
                raise fbuild.ConfigFailed('cannot identify exe given for ' +\
                                          name)
            platform_extra |= tp
        else:
            platform_extra |= compilers
    platform |= platform_extra

    full_compilers = compilers
    if 'windows' not in platform:
        full_compilers |= {'windows'}

    for subplatform, function in functions:
        # XXX: this is slightly a hack to make sure:
        # a) Clang can actually be detected
        # b) Any compilers explicitly listed in platform_extra will have #1
        #  priority
        if subplatform - (compilers & platform_extra) <= platform:
            new_kwargs = copy.deepcopy(kwargs)

            for p, kw in platform_options:
                if (p - subplatform & full_compilers) <= subplatform:
                    for k, v in kw.items():
                        if k[-1] in '+-':
                            func = k[-1]
                            k = k[:-1]
                            try:
                                curval = new_kwargs[k]
                            except:
                                if isinstance(v, str):
                                    curval = ''
                                elif isinstance(v, list):
                                    curval = []
                                elif isinstance(v, tuple):
                                    curval = ()
                            if func == '+':
                                curval += v
                            elif func == '-':
                                lst = list(curval)
                                for x in v:
                                    lst.pop(lst.index(x))
                                if isinstance(curval, str):
                                    curval = ''.join(lst)
                                else:
                                    curval = type(curval)(lst)
                            new_kwargs[k] = curval
                        else:
                            new_kwargs[k] = v

            # Try to use this compiler. If it doesn't work, skip this compiler
            # and try another one.

            try:
                return fbuild.functools.call(function, ctx, exe, *args, **new_kwargs)
            except fbuild.ConfigFailed:
                pass

    raise fbuild.ConfigFailed('cannot find a %s builder for %s' %
        (name, platform))

@fbuild.db.caches
def identify_compiler(ctx, exe):
    res = None
    ctx.logger.check('identifying exe %s' % exe)
    # take a cue from the name
    if exe == 'clang' or exe == 'clang++':
        res = {'clang', 'clang++'}
    elif exe == 'gcc' or exe == 'g++' or exe == 'colorgcc':
        res = {'gcc', 'g++'}
    elif exe == 'cl' or exe == 'cl.exe':
        res = {'windows'}
    else:
        try:
            out, err = ctx.execute((exe, '--version'), quieter=1)
        except fbuild.ExecutionError:
            try:
                # is it MSVC?
                out, err = ctx.execute((exe,), quieter=1)
            except fbuild.ExecutionError:
                pass
            else:
                if b'Microsoft' in out or b'Microsoft' in err:
                    res = {'windows'}
        else:
            ccmap = {'Free Software Foundation': {'gcc', 'g++'},
                     'clang': {'clang', 'clang++'}}
            for cc in ccmap:
                if bytes(cc, encoding='ascii') in out:
                    res = ccmap[cc]
    if res is None:
        ctx.logger.failed()
    else:
        ctx.logger.passed(res)
    return res

def guess_static(*args, **kwargs):
    """L{static} tries to guess the static system c compiler according to the
    platform. It accepts a I{platform} keyword that overrides the system's
    platform and a I{platform_extra} keyword that is joined to the system's
    platform. This can be used to use a non-default compiler. Any extra
    arguments and keywords are passed to the compiler's configuration
    functions."""

    return _guess_builder('c static', {'gcc', 'clang'}, (
        ({'windows'}, 'fbuild.builders.c.msvc.static'),
        ({'avr', 'gcc'}, 'fbuild.builders.c.gcc.avr.static'),
        ({'iphone', 'simulator', 'gcc'},
            'fbuild.builders.c.gcc.iphone.static_simulator'),
        ({'iphone', 'gcc'}, 'fbuild.builders.c.gcc.iphone.static'),
        ({'darwin', 'clang'}, 'fbuild.builders.c.clang.darwin.static'),
        ({'darwin', 'gcc'}, 'fbuild.builders.c.gcc.darwin.static'),
        ({'clang'}, 'fbuild.builders.c.clang.static'),
        ({'gcc'}, 'fbuild.builders.c.gcc.static'),
    ), *args, **kwargs)

def guess_shared(*args, **kwargs):
    """L{shared} tries to guess the shared system c compiler according to the
    platform. It accepts a I{platform} keyword that overrides the system's
    platform. This can be used to use a non-default compiler. Any extra
    arguments and keywords are passed to the compiler's configuration
    functions."""

    return _guess_builder('c shared', {'gcc', 'clang'}, (
        ({'windows'}, 'fbuild.builders.c.msvc.shared'),
        ({'avr', 'gcc'}, 'fbuild.builders.c.gcc.avr.shared'),
        ({'iphone', 'simulator', 'gcc'},
            'fbuild.builders.c.gcc.iphone.shared_simulator'),
        ({'iphone', 'gcc'}, 'fbuild.builders.c.gcc.iphone.shared'),
        ({'darwin', 'clang'}, 'fbuild.builders.c.clang.darwin.shared'),
        ({'darwin', 'gcc'}, 'fbuild.builders.c.gcc.darwin.shared'),
        ({'clang'}, 'fbuild.builders.c.clang.shared'),
        ({'gcc'}, 'fbuild.builders.c.gcc.shared'),
    ), *args, **kwargs)
