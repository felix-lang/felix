import io
import re
from itertools import chain

import fbuild
import fbuild.builders
import fbuild.builders.c
import fbuild.builders.platform
import fbuild.db
import fbuild.record
from fbuild.path import Path
from fbuild.temp import tempfile

# ------------------------------------------------------------------------------

class Ar(fbuild.db.PersistentObject):
    def __init__(self, ctx, exe='ar', *,
            platform=None,
            prefix=None,
            suffix=None,
            flags=('-rcs',),
            libpaths=(),
            libs=(),
            external_libs=(),
            ranlib='ranlib',
            ranlib_flags=()):
        super().__init__(ctx)

        self.exe = fbuild.builders.find_program(ctx, [exe])
        try:
            self.ranlib = fbuild.builders.find_program(ctx, [ranlib])
        except fbuild.ConfigFailed:
            self.ranlib = None

        self.prefix = prefix or \
            fbuild.builders.platform.static_lib_prefix(platform)
        self.suffix = suffix or \
            fbuild.builders.platform.static_lib_suffix(platform)
        self.libpaths = tuple(libpaths)
        self.libs = tuple(libs)
        self.external_libs = tuple(external_libs)
        self.flags = tuple(flags)
        self.ranlib_flags = tuple(ranlib_flags)

    @fbuild.db.cachemethod
    def __call__(self, dst, srcs:fbuild.db.SRCS, *,
            libs:fbuild.db.SRCS=(),
            external_libs=(),
            flags=(),
            ranlib_flags=(),
            prefix=None,
            suffix=None,
            buildroot=None,
            **kwargs) -> fbuild.db.DST:
        buildroot = buildroot or self.ctx.buildroot
        #libs = set(libs)
        #libs.update(self.libs)
        #libs = sorted(libs)

        #assert srcs or libs, 'no sources passed into ar'
        assert srcs, 'no sources passed into ar'

        prefix = prefix or self.prefix
        suffix = suffix or self.suffix
        dst = Path(dst).addroot(buildroot)
        dst = dst.parent / prefix + dst.name + suffix
        dst.parent.makedirs()

        srcs = list(Path.globall(srcs))

        cmd = [self.exe]
        cmd.extend(self.flags)
        cmd.extend(flags)
        cmd.append(dst)
        cmd.extend(srcs)
        #cmd.extend(libs)
        #cmd.extend(self.external_libs)
        #cmd.extend(external_libs)

        self.ctx.execute(cmd,
            msg1=str(self),
            msg2='%s -> %s' % (' '.join(srcs), dst),
            color='link',
            **kwargs)

        if self.ranlib is not None:
            cmd = [self.ranlib]
            cmd.extend(self.ranlib_flags)
            cmd.extend(ranlib_flags)
            cmd.append(dst)

            self.ctx.execute(cmd,
                msg1=self.ranlib.name,
                msg2=dst,
                color='link',
                **kwargs)

        return dst

    def __str__(self):
        return str(self.exe.name)

# ------------------------------------------------------------------------------

class Gcc(fbuild.db.PersistentObject):
    def __init__(self, ctx, exe, *,
            pre_flags=(),
            flags=(),
            includes=(),
            macros=(),
            warnings=(),
            libpaths=(),
            libs=(),
            external_libs=(),
            debug=None,
            profile=None,
            optimize=None,
            debug_flags=('-g',),
            profile_flags=('-pg',),
            optimize_flags=('-O2',),
            arch=None,
            machine_flags=(),
            requires_version=None,
            requires_at_least_version=None,
            requires_at_most_version=None):
        super().__init__(ctx)

        self.exe = exe
        self.pre_flags = tuple(pre_flags)
        self.flags = tuple(flags)
        self.includes = tuple(includes)
        self.macros = tuple(macros)
        self.warnings = tuple(warnings)
        self.libpaths = tuple(libpaths)
        self.libs = tuple(libs)
        self.external_libs = tuple(external_libs)
        self.debug = debug
        self.profile = profile
        self.optimize = optimize
        self.debug_flags = tuple(debug_flags)
        self.profile_flags = tuple(profile_flags)
        self.optimize_flags = tuple(optimize_flags)
        self.arch = arch
        self.machine_flags = tuple(machine_flags)

        if not self.check_flags(flags):
            raise fbuild.ConfigFailed('%s failed to compile an exe' % self)

        if debug and debug_flags and not self.check_flags(debug_flags):
            raise fbuild.ConfigFailed('%s failed to compile an exe' % self)

        if profile and profile_flags and not self.check_flags(profile_flags):
            raise fbuild.ConfigFailed('%s failed to compile an exe' % self)

        if optimize and optimize_flags and not self.check_flags(optimize_flags):
            raise fbuild.ConfigFailed('%s failed to compile an exe' % self)

        # Make sure we've got a valid version.
        fbuild.builders.check_version(ctx, self, self.version,
            requires_version=requires_version,
            requires_at_least_version=requires_at_least_version,
            requires_at_most_version=requires_at_most_version)

    def __call__(self, srcs, dst=None, *,
            pre_flags=(),
            flags=(),
            includes=(),
            macros=(),
            warnings=(),
            libpaths=(),
            libs=(),
            external_libs=(),
            debug=None,
            profile=None,
            optimize=None,
            arch=None,
            machine_flags=(),
            **kwargs):
        srcs = [Path(src) for src in srcs]

        # Make sure we don't repeat includes
        new_includes = []
        for include in chain(self.includes, includes, (s.parent for s in srcs)):
            if include not in new_includes:
                new_includes.append(include)
        includes = new_includes

        # Make sure we don't repeat flags
        new_flags = []
        for flag in chain(self.flags, flags):
            if flag not in new_flags:
                new_flags.append(flag)
        flags = new_flags

        macros = set(macros)
        macros.update(self.macros)

        warnings = set(warnings)
        warnings.update(self.warnings)

        machine_flags = set(machine_flags)
        machine_flags.update(self.machine_flags)

        # Make sure we don't repeat library paths
        new_libpaths = []
        for libpath in chain(self.libpaths, libpaths):
            if libpath not in new_libpaths:
                new_libpaths.append(libpath)
        libpaths = new_libpaths

        # Make sure we don't repeat external library paths
        new_external_libs = []
        for lib in chain(self.external_libs, external_libs):
            if lib not in new_external_libs:
                new_external_libs.append(lib)
        external_libs = new_external_libs

        # Since the libs could be derived from fbuild.builders.c.Library, we need
        # to extract the extra libs and flags that they need.  Linux needs the
        # libraries listed in a particular order.  Libraries must appear left
        # of their dependencies in order to optimize linking.
        new_libs = []
        def f(lib):
            if lib in new_libs:
                return

            if isinstance(lib, fbuild.builders.c.Library):
                for libpath in lib.libpaths:
                    if libpath not in libpaths:
                        libpaths.append(libpath)

                for l in lib.external_libs:
                    if l not in external_libs:
                        external_libs.append(l)

                # In order to make linux happy, we'll recursively walk the
                # dependencies first, then add the library.
                for l in lib.libs:
                    f(l)

            parent, lib = Path(lib).split()

            if parent not in libpaths:
                libpaths.append(parent)

            lib = lib.name[len('lib'):]
            lib = lib.rsplit('.', 1)[0]

            new_libs.append(lib)

        for lib in chain(self.libs, libs):
            f(lib)

        # Finally, we need to reverse the list so it's in the proper order.
        new_libs.reverse()
        libs = new_libs

        # ----------------------------------------------------------------------

        cmd = [self.exe]
        cmd.extend(self.pre_flags)
        cmd.extend(pre_flags)

        if (debug is None and self.debug) or debug:
            cmd.extend(self.debug_flags)

        if (profile is None and self.profile) or profile:
            cmd.extend(self.profile_flags)

        if (optimize is None and self.optimize) or optimize:
            cmd.extend(self.optimize_flags)

        arch = (arch is None and self.arch) or arch
        if arch:
            cmd.extend(('-arch', arch))

        # make sure that the path is converted into the native path format
        cmd.extend('-I' + Path(i) for i in sorted(includes) if i)
        cmd.extend('-D' + d for d in sorted(macros))
        cmd.extend('-W' + w for w in sorted(warnings))
        cmd.extend('-L' + Path(p) for p in sorted(libpaths) if p)
        cmd.extend('-m' + m for m in sorted(machine_flags) if m)

        if dst is not None:
            cmd.extend(('-o', dst))
            msg2 = '%s -> %s' % (' '.join(chain(srcs, libs)), dst)
        else:
            msg2 = ' '.join(srcs)

        cmd.extend(flags)
        cmd.extend(srcs)

        # Libraries must come last on linux in order to find symbols.
        cmd.extend('-l' + l for l in libs)
        cmd.extend('-l' + l for l in external_libs)

        return self.ctx.execute(cmd, msg2=msg2, **kwargs)

    def version(self):
        """Return the version of the gcc executable."""

        stdout, stderr = self.ctx.execute((self.exe, '--version'), quieter=1)
        return stdout.decode().split('\n')[0].split(' ')[2]

    def check_flags(self, flags):
        if flags:
            self.ctx.logger.check('checking %s with %s' %
                (self, ' '.join(flags)))
        else:
            self.ctx.logger.check('checking %s' % self)

        code = 'int main(int argc, char** argv){return 0;}'

        with tempfile(code, suffix='.c') as src:
            try:
                self([src], flags=flags, quieter=1, cwd=src.parent)
            except fbuild.ExecutionError:
                self.ctx.logger.failed()
                return False

        self.ctx.logger.passed()
        return True

    def __str__(self):
        return str(self.exe.name)

def make_cc(ctx, exe=None, default_exes=['gcc', 'cc'], **kwargs):
    return Gcc(ctx,
        fbuild.builders.find_program(ctx, [exe] if exe else default_exes),
        **kwargs)

# ------------------------------------------------------------------------------

class Compiler(fbuild.db.PersistentObject):
    def __init__(self, ctx, cc, flags, *, suffix):
        super().__init__(ctx)

        self.cc = cc
        self.flags = tuple(flags)
        self.suffix = suffix

        if flags and not cc.check_flags(flags):
            raise fbuild.ConfigFailed('%s does not support %s flags' %
                (cc, flags))

    def __call__(self, src, dst=None, *,
            suffix=None,
            buildroot=None,
            **kwargs):
        buildroot = buildroot or self.ctx.buildroot
        src = Path(src)

        suffix = suffix or self.suffix
        dst = Path(dst or src).addroot(buildroot).replaceext(suffix)
        dst.parent.makedirs()

        stdout, stderr = self.cc([src], dst,
            pre_flags=list(chain(('-c',), self.flags)),
            msg1=str(self),
            color='compile',
            **kwargs)

        return dst, stdout, stderr

    def __str__(self):
        return str(self.cc)

# ------------------------------------------------------------------------------

class Linker(fbuild.db.PersistentObject):
    def __init__(self, ctx, cc, flags=(), *, prefix, suffix):
        super().__init__(ctx)

        self.cc = cc
        self.flags = tuple(flags)
        self.prefix = prefix
        self.suffix = suffix

        if flags and not cc.check_flags(flags):
            raise fbuild.ConfigFailed('%s does not support %s' %
                (cc, ' '.join(flags)))

    def __call__(self, dst, srcs, *,
            prefix=None,
            suffix=None,
            buildroot=None,
            **kwargs):
        prefix = prefix or self.prefix
        suffix = suffix or self.suffix
        buildroot = buildroot or self.ctx.buildroot
        dst = Path(dst).addroot(buildroot)
        dst = dst.parent / prefix + dst.name + suffix
        dst.parent.makedirs()

        self.cc(srcs, dst,
            pre_flags=self.flags,
            msg1=str(self),
            color='link',
            **kwargs)

        return dst

    def __str__(self):
        return str(self.cc)

# ------------------------------------------------------------------------------

class Builder(fbuild.builders.c.Builder):
    def __init__(self, *args,
            compiler,
            lib_linker,
            exe_linker,
            **kwargs):
        self.compiler = compiler
        self.lib_linker = lib_linker
        self.exe_linker = exe_linker

        # This needs to come last as the parent class tests the builder.
        super().__init__(*args, **kwargs)

    def __str__(self):
        return str(self.compiler)

    # --------------------------------------------------------------------------

    @fbuild.db.cachemethod
    def compile(self, src:fbuild.db.SRC, dst=None, *,
            flags=[],
            **kwargs) -> fbuild.db.DST:
        """Compile a c file and cache the results."""
        # Generate the dependencies while we compile the file.
        with tempfile() as dep:
            obj = self.uncached_compile(src, dst,
                flags=list(chain(('-MMD', '-MF', dep), flags)),
                **kwargs)

            with open(dep, 'rb') as f:
                # Parse the output and return the module dependencies.
                stdout = f.read().replace(b'\\\n', b'')

        # Parse the output and return the module dependencies.
        m = re.match(b'\s*\S+:(?: (.*))?$', stdout)
        if not m:
            raise fbuild.ExecutionError('unable to understand %r' % stdout)

        s = m.group(1)
        if s is not None:
            deps = s.decode().split()
            self.ctx.db.add_external_dependencies_to_call(srcs=deps)

        return obj

    def uncached_compile(self, *args, **kwargs):
        """Compile a c file without caching the results.  This is needed when
        compiling temporary files."""
        obj, stdout, stderr = self.compiler(*args, **kwargs)
        return obj

    def uncached_link_lib(self, *args, **kwargs):
        """Link compiled c files into a library without caching the results.
        This is needed when linking temporary files."""
        lib = self.lib_linker(*args, **kwargs)
        return fbuild.builders.c.Library(lib,
            libpaths=kwargs.get('libpaths', []),
            libs=kwargs.get('libs', []),
            external_libs=kwargs.get('external_libs', []))

    def uncached_link_exe(self, *args, **kwargs):
        """Link compiled c files into am executable without caching the
        results.  This is needed when linking temporary files."""
        exe = self.exe_linker(*args, **kwargs)
        return fbuild.builders.c.Executable(exe,
            libs=kwargs.get('libs', []))

    # --------------------------------------------------------------------------

    def __repr__(self):
        return '%s(compiler=%r, lib_linker=%r, exe_linker=%r)' % (
            self.__class__.__name__,
            self.compiler,
            self.lib_linker,
            self.exe_linker)

# ------------------------------------------------------------------------------

def static(ctx, exe=None, *args,
        make_cc=make_cc,
        make_compiler=Compiler,
        make_lib_linker=Ar,
        make_exe_linker=Linker,
        platform=None,
        flags=(),
        compile_flags=(),
        ar=None,
        libpaths=(),
        libs=(),
        link_flags=(),
        exe_link_flags=(),
        src_suffix='.c',
        obj_suffix=None,
        lib_prefix=None,
        lib_suffix=None,
        exe_suffix=None,
        cross_compiler=False,
        **kwargs):
    cc = make_cc(ctx, exe, libpaths=libpaths, libs=libs, **kwargs)

    # Allow the user to overload the file extensions.
    if obj_suffix is None:
        obj_suffix = fbuild.builders.platform.static_obj_suffix(ctx, platform)

    if lib_prefix is None:
        lib_prefix = fbuild.builders.platform.static_lib_prefix(ctx, platform)

    if lib_suffix is None:
        lib_suffix = fbuild.builders.platform.static_lib_suffix(ctx, platform)

    if exe_suffix is None:
        exe_suffix = fbuild.builders.platform.exe_suffix(ctx, platform)

    return Builder(ctx,
        compiler=make_compiler(ctx, cc,
            flags=list(chain(flags, compile_flags)),
            suffix=obj_suffix),
        lib_linker=make_lib_linker(ctx,
            libs=libs,
            libpaths=libpaths,
            prefix=lib_prefix,
            suffix=lib_suffix),
        exe_linker=make_exe_linker(ctx, cc,
            flags=list(chain(flags, link_flags, exe_link_flags)),
            prefix='',
            suffix=exe_suffix),
        src_suffix=src_suffix,
        flags=flags,
        cross_compiler=cross_compiler)

# ------------------------------------------------------------------------------

def shared(ctx, exe=None, *args,
        make_cc=make_cc,
        make_compiler=Compiler,
        make_lib_linker=Linker,
        make_exe_linker=Linker,
        platform=None,
        flags=(),
        compile_flags=('-fPIC',),
        libpaths=(),
        libs=(),
        link_flags=(),
        lib_link_flags=('-fPIC', '-shared'),
        exe_link_flags=(),
        src_suffix='.c',
        obj_suffix=None,
        lib_prefix=None,
        lib_suffix=None,
        exe_suffix=None,
        cross_compiler=False,
        **kwargs):
    cc = make_cc(ctx, exe, libpaths=libpaths, libs=libs, **kwargs)

    # Allow the user to overload the file extensions.
    if obj_suffix is None:
        obj_suffix = fbuild.builders.platform.shared_obj_suffix(ctx, platform)

    if lib_prefix is None:
        lib_prefix = fbuild.builders.platform.shared_lib_prefix(ctx, platform)

    if lib_suffix is None:
        lib_suffix = fbuild.builders.platform.shared_lib_suffix(ctx, platform)

    if exe_suffix is None:
        exe_suffix = fbuild.builders.platform.exe_suffix(ctx, platform)

    return Builder(ctx,
        compiler=make_compiler(ctx, cc,
            flags=list(chain(flags, compile_flags)),
            suffix=obj_suffix),
        lib_linker=make_lib_linker(ctx, cc,
            flags=list(chain(flags, link_flags, lib_link_flags)),
            prefix=lib_prefix,
            suffix=lib_suffix),
        exe_linker=make_exe_linker(ctx, cc,
            flags=list(chain(flags, link_flags, exe_link_flags)),
            prefix='',
            suffix=exe_suffix),
        src_suffix=src_suffix,
        flags=flags,
        cross_compiler=cross_compiler)
