import fbuild
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
        dst = src.replace_root(buildroot)

        if preparse:
            dst = dst.replace_ext('.par')
        else:
            dst = dst.replace_ext('.cpp')

        if not dst.is_dirty(src):
            return dst

        if src != src_buildroot:
            src_buildroot.parent.make_dirs()
            src.copy(src_buildroot)
            src = src_buildroot

        dst.parent.make_dirs()

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

    def _link(self, linker, src, dst=None, *,
            includes=[],
            macros=[],
            cflags=[],
            libs=[],
            lflags=[],
            buildroot=fbuild.buildroot):
        src = Path(src)

        if dst is None:
            dst = src.replace_ext('')
        dst.replace_root(src)

        if not dst.is_dirty(src):
            return dst

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

    # -------------------------------------------------------------------------

    def run_lib(self, src, *args, async=True, **kwargs):
        if async:
            cmd = [self.flx_arun_exe]
        else:
            cmd = [self.flx_run_exe]

        cmd.append(src)

        return fbuild.execute(cmd, *args, **kwargs)

    # -------------------------------------------------------------------------

    def _build_link(self, function, src, dst=None, *,
            async=True,
            includes=[],
            flags=[],
            cxx_includes=[],
            cxx_cflags=[],
            cxx_libs=[],
            cxx_lflags=[]):
        obj = self.compile(src, includes=includes, flags=flags)

        return function(obj, dst,
            async=async,
            includes=cxx_includes,
            libs=cxx_libs,
            cflags=cxx_cflags,
            lflags=cxx_lflags,
        )

    def build_lib(self, *args, **kwargs):
        return self._build_link(self.link_lib, *args, **kwargs)

    def build_exe(self, *args, **kwargs):
        return self._build_link(self.link_exe, *args, **kwargs)

# -----------------------------------------------------------------------------

def build(flxg, cxx, drivers):
    return Builder(
        flxg,
        cxx,
        drivers.flx_run_exe,
        drivers.flx_arun_exe,
        drivers.flx_run_lib,
        drivers.flx_arun_lib,
    )

def build_flx_pkgconfig(flx, phase):
    exe = flx.build_exe(
        dst=fbuild.buildroot / 'bin/flx_pkgconfig',
        src='src/flx_pkgconfig/flx_pkgconfig.flx',
        includes=[fbuild.buildroot / 'lib'],
        cxx_includes=['src/flx_pkgconfig', fbuild.buildroot / 'lib/rtl'],
        cxx_libs=[
            fbuild.env.run('buildsystem.flx_pthread.build_runtime', phase).shared,
            fbuild.env.run('buildsystem.flx_rtl.build_runtime', phase).shared,
            fbuild.env.run('buildsystem.flx_gc.build_runtime', phase).shared,
            fbuild.env.run('buildsystem.judy.build_runtime', phase).shared,
            fbuild.env.run('buildsystem.flx_exceptions.build_runtime', phase).shared,
        ],
        cxx_cflags=['-Wno-invalid-offsetof'],
    )

    # make sure flx_pkgconfig runs
    fbuild.logger.check('checking ' + exe)
    try:
        fbuild.execute([exe], quieter=1)
    except fbuild.ExecutionError:
        fbuild.logger.failed()
        raise
    else:
        fbuild.logger.passed()

    return exe

# -----------------------------------------------------------------------------

def test_flx(felix, src):
    exe = felix.compile(src)
    dst = exe + '.stdout'

    if dst.is_dirty(src):
        fbuild.logger.check('checking ' + src)
        stdout, stderr = felix.run(exe, quieter=1)

        with open(dst, 'wb') as f:
            f.write(stdout)

        expect = src.replace_ext('.expect')
        if not expect.exists():
            fbuild.logger.log('no .expect', color='cyan')
        else:
            with open(expect, 'rb') as f:
                s = f.read()

            if stdout == s and stderr == b'':
                fbuild.logger.passed()
            else:
                fbuild.logger.failed()
                raise fbuild.ConfigFailed('%s does not equal %s' % (stdout, s))
