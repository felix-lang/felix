import fbuild
import fbuild.packages
from fbuild.path import Path
from fbuild.record import Record

# -----------------------------------------------------------------------------

class Builder:
    def __init__(self, flxg, cxx,
            flx_run_exe,
            flx_arun_exe,
            flx_run_lib,
            flx_arun_lib):
        self.flxg = flxg
        self.cxx = cxx
        self.flx_run_exe  = flx_run_exe
        self.flx_arun_exe = flx_arun_exe
        self.flx_run_lib  = flx_run_lib
        self.flx_arun_lib = flx_arun_lib

    def _run_flxg(self, src, *,
            includes=[],
            imports=[],
            flags=[],
            include_std=True,
            preparse=False,
            buildroot=fbuild.buildroot,
            **kwargs):
        src = Path(src)
        # first, copy the src file into the buildroot
        src_buildroot = src.replace_root(buildroot)

        if src != src_buildroot:
            src_buildroot.parent.make_dirs()
            src.copy(src_buildroot)
            src = src_buildroot

        dst = src.replace_root(buildroot)
        dst.parent.make_dirs()

        if preparse:
            dst = dst.replace_ext('.par')
        else:
            dst = dst.replace_ext('.cpp')

        cmd = [self.flxg]

        if preparse:
            cmd.append('-c')

        includes = set(includes)
        includes.add(src.parent)
        includes.add(dst.parent)

        imports = set(imports)

        if include_std:
            imports.add('flx.flxh')

        cmd.extend('-I' + i for i in sorted(includes) if Path.exists(i))
        cmd.extend('--import=' + i for i in sorted(imports))
        cmd.append('--output_dir=' + dst.parent)
        cmd.extend(flags)

        if include_std:
            cmd.append('std')

        if src.ext == '.flx':
            cmd.append(src.replace_ext(''))
        else:
            cmd.append(src)

        fbuild.execute(cmd, self.flxg.name, '%s -> %s' % (src, dst),
                color='yellow', **kwargs)

        return dst

    def preparse(self, *args, **kwargs):
        return self._run_flxg(*args, preparse=True, **kwargs)

    def compile(self, *args, **kwargs):
        return self._run_flxg(*args, **kwargs)

    def _link(self, linker, dst, src, *,
            includes=[],
            macros=[],
            cflags=[],
            libs=[],
            lflags=[],
            buildroot=fbuild.buildroot):
        obj = self.cxx.compile(src,
            includes=includes,
            macros=macros,
            buildroot=buildroot,
            flags=cflags)

        return linker(dst, [obj],
            libs=libs,
            flags=lflags,
            buildroot=buildroot)

    def link_exe(self, *args, async=True, macros=[], libs=[], **kwargs):
        macros = macros + ['FLX_STATIC_LINK']
        libs = libs + [self.flx_arun_lib if async else self.flx_run_lib]

        return self._link(self.cxx.link_exe, *args,
            macros=macros,
            libs=libs,
            **kwargs)

    def link_lib(self, *args, **kwargs):
        return self._link(self.cxx.link_lib, *args, **kwargs)

    def run_lib(self, src, *args, async=True, **kwargs):
        if async:
            cmd = [self.flx_arun_exe]
        else:
            cmd = [self.flx_run_exe]

        cmd.append(src)

        return fbuild.execute(cmd, *args, **kwargs)

# -----------------------------------------------------------------------------

def build(flxg, cxx, drivers):
    return fbuild.packages.BuilderWrapper(Builder, [
        flxg,
        cxx,
        drivers.flx_run_exe,
        drivers.flx_arun_exe,
        drivers.flx_run_lib,
        drivers.flx_arun_lib,
    ])

def build_flx_pkgconfig(flx, phase):
    return Executable(fbuild.buildroot / 'bin/flx_pkgconfig',
        'src/flx_pkgconfig/flx_pkgconfig.flx',
        config=flx,
        includes=[fbuild.buildroot / 'lib'],
        cxx_includes=['src/flx_pkgconfig', fbuild.buildroot / 'lib/rtl'],
        cxx_libs=[
            fbuild.env.run('buildsystem.flx_pthread.build_runtime', phase),
            fbuild.env.run('buildsystem.flx_rtl.build_runtime', phase),
            fbuild.env.run('buildsystem.flx_gc.build_runtime', phase),
            fbuild.env.run('buildsystem.judy.build_runtime', phase),
            fbuild.env.run('buildsystem.flx_exceptions.build_runtime', phase),
        ],
        cxx_cflags=['-Wno-invalid-offsetof'],
    )

# -----------------------------------------------------------------------------

class _Compiler(fbuild.packages.SimplePackage):
    @property
    def config(self):
        return super().config.build()

class Par(_Compiler):
    def command(self, *args, **kwargs):
        return self.config.compile(*args, preparse=True, **kwargs)

class Module(_Compiler):
    def command(self, *args, **kwargs):
        return self.config.compile(*args, **kwargs)

class _Linker(fbuild.packages.OneToOnePackage):
    def __init__(self, dst, src, *,
            async=True,
            includes=[],
            flags=[],
            cxx_includes=[],
            cxx_libs=[],
            cxx_cflags=[],
            cxx_lflags=[],
            **kwargs):
        super().__init__(dst, src, **kwargs)

        self.async = async
        self.includes = includes
        self.flags = flags
        self.cxx_includes = cxx_includes
        self.cxx_libs = cxx_libs
        self.cxx_cflags = cxx_cflags
        self.cxx_lflags = cxx_lflags

    @property
    def config(self):
        return super().config.build()

    def dependencies(self):
        return [self.src] + [lib for lib in self.cxx_libs
            if isinstance(lib, fbuild.packages.Package)]

    def run(self):
        cxx_libs = [fbuild.packages.build(lib) for lib in self.cxx_libs]

        obj = self.config.compile(fbuild.packages.build(self.src),
            includes=self.includes,
            flags=self.flags)

        return self.command(self.dst, obj,
            async=self.async,
            includes=self.cxx_includes,
            libs=cxx_libs,
            cflags=self.cxx_cflags,
            lflags=self.cxx_lflags,
            **self.kwargs)

class Executable(_Linker):
    def dependencies(self):
        if self.async:
            lib = self.config.flx_arun_lib
        else:
            lib = self.config.flx_run_lib

        return super().dependencies() + [lib]

    def command(self, *args, **kwargs):
        return self.config.link_exe(*args, **kwargs)

class Library(_Linker):
    def command(self, *args, **kwargs):
        return self.config.link_lib(*args, **kwargs)

# -----------------------------------------------------------------------------

class Test(fbuild.packages.SimplePackage):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

        self.expect = self.target.replace_ext('.expect')

    def dependencies(self):
        return (self.target, self.expect)

    def run(self):
        from fbuild.packages.felix import Felix

        exe = Felix(self.target, config=self.config).build()

        fbuild.logger.check('checking ' + self.target)
        stdout, stderr = self.config.run(exe, quieter=1)

        if not self.expect.exists():
            fbuild.logger.passed()
        else:
            with open(self.expect, 'rb') as f:
                s = f.read()
                if stdout == s and stderr == b'':
                    fbuild.logger.passed()
                else:
                    fbuild.logger.failed()
                    raise fbuild.ConfigFailed('%s does not equal %s' % (stdout, s))

    def __repr__(self):
        return '%s(%s)' % (self.__class__.__name__, self.target)

# -----------------------------------------------------------------------------

def copy_flxs_to_lib(builder, srcs):
    return [fbuild.packages.Copy(fbuild.buildroot / 'lib' / src.name, src)
        for src in srcs]
