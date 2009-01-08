import difflib

import fbuild
import fbuild.db
from fbuild.functools import call
from fbuild.path import Path
from fbuild.record import Record

# ------------------------------------------------------------------------------

class Builder(fbuild.db.PersistentObject):
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

    @fbuild.db.cachemethod
    def _run_flxg(self, src:fbuild.db.SRC, *,
            includes=[],
            imports=[],
            flags=[],
            include_std=True,
            preparse=False,
            buildroot=fbuild.buildroot,
            **kwargs) -> fbuild.db.DST:
        src = Path(src)
        # first, copy the src file into the buildroot
        src_buildroot = src.addroot(buildroot)
        dst = src.addroot(buildroot)

        if preparse:
            dst = dst.replaceext('.par')
        else:
            dst = dst.replaceext('.cpp')

        if src != src_buildroot:
            src_buildroot.parent.makedirs()
            src.copy(src_buildroot)
            src = src_buildroot

        dst.parent.makedirs()

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
            cmd.append(src.replaceext(''))
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
            dst = src.replaceext('')
        dst.addroot(src)

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

    # --------------------------------------------------------------------------

    def run_lib(self, src, *args, async=True, **kwargs):
        if async:
            cmd = [self.flx_arun_exe]
        else:
            cmd = [self.flx_run_exe]

        cmd.append(src)

        return fbuild.execute(cmd, *args, **kwargs)

    # --------------------------------------------------------------------------

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

# ------------------------------------------------------------------------------

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
            call('buildsystem.flx_pthread.build_runtime', phase).shared,
            call('buildsystem.flx_rtl.build_runtime', phase).shared,
            call('buildsystem.flx_gc.build_runtime', phase).shared,
            call('buildsystem.judy.build_runtime', phase).shared,
            call('buildsystem.flx_exceptions.build_runtime', phase).shared,
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

# ------------------------------------------------------------------------------

def test_flx(felix, src):
    for static in False, True:
        try:
            exe = felix.compile(src, static=static)
        except fbuild.ExecutionError as e:
            fbuild.logger.log(e, verbose=1)
            if e.stdout:
                fbuild.logger.log(e.stdout.decode().strip(), verbose=1)
            if e.stderr:
                fbuild.logger.log(e.stderr.decode().strip(), verbose=1)
            continue

        expect = src.replaceext('.expect')

        check_flx(felix,
            exe=exe,
            dst=exe + '.stdout',
            expect=expect if expect.exists() else None,
            static=static)

@fbuild.db.caches
def check_flx(felix,
        exe:fbuild.db.SRC,
        dst:fbuild.db.DST,
        expect:fbuild.db.OPTIONAL_SRC,
        static):
    fbuild.logger.check('checking ' + exe)
    try:
        stdout, stderr = felix.run(exe, static=static, quieter=1)
    except fbuild.ExecutionError as e:
        fbuild.logger.failed()

        fbuild.logger.log(e, verbose=1)
        if e.stdout:
            fbuild.logger.log(e.stdout.decode().strip(), verbose=1)
        if e.stderr:
            fbuild.logger.log(e.stderr.decode().strip(), verbose=1)
        return False

    with open(dst, 'wb') as f:
        f.write(stdout)

    if expect is None:
        fbuild.logger.log('no .expect', color='cyan')
        return True
    else:
        with open(expect, 'rb') as f:
            s = f.read()

        if stdout == s:
            fbuild.logger.passed()
            return True
        else:
            fbuild.logger.failed('failed: output does not match')
            for line in difflib.ndiff(
                    stdout.decode().split('\n'),
                    s.decode().split('\n')):
                print(line)
            Path.remove(dst)
            return False
