import difflib
from itertools import chain

import fbuild
import fbuild.db
from fbuild.functools import call
from fbuild.path import Path

# ------------------------------------------------------------------------------

class Builder(fbuild.db.PersistentObject):
    def __init__(self, ctx, flxg, cxx,
            flx_run_exe,
            flx_arun_exe,
            flx_run_lib,
            flx_arun_lib):
        super().__init__(ctx)

        self.flxg = flxg
        self.cxx = cxx
        self.flx_run_exe  = flx_run_exe
        self.flx_arun_exe = flx_arun_exe
        self.flx_run_lib  = flx_run_lib
        self.flx_arun_lib = flx_arun_lib

    @fbuild.db.cachemethod
    def _run_flxg(self, src:fbuild.db.SRC, *,
            includes=[],
            syntaxes=[],
            imports=[],
            flags=[],
            include_std=True,
            preparse=False,
            buildroot=None,
            **kwargs) -> fbuild.db.DST:
        buildroot = buildroot or self.ctx.buildroot

        src = Path(src)
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

        imports = list(imports)
        syntaxes = list(syntaxes)
        if include_std:
            imports.insert(0, 'plat/flx.flxh')
            imports.insert(0, 'nugram.flxh')
            syntaxes.insert(0, 'nugram.flxh')

        cmd.extend('-I' + i for i in sorted(includes) if Path.exists(i))
        cmd.extend('--import=' + i for i in imports)
        cmd.extend('--syntax=' + i for i in syntaxes)
        cmd.append('--output_dir=' + dst.parent)
        cmd.extend(flags)

        if include_std:
            cmd.append('std')

        if src.ext == '.flx':
            cmd.append(src.replaceext(''))
        else:
            cmd.append(src)

        self.ctx.execute(cmd, self.flxg.name, '%s -> %s' % (src, dst),
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
            objects=[],
            buildroot=None):
        buildroot = buildroot or self.ctx.buildroot

        src = Path(src)

        if dst is None:
            dst = src.replaceext('')
        dst = Path(dst).addroot(buildroot)

        obj = self.cxx.compile(src,
            includes=includes,
            macros=macros,
            buildroot=buildroot,
            flags=cflags)

        return linker(dst, list(chain(objects, [obj])),
            libs=libs,
            flags=lflags,
            buildroot=buildroot)

    def link_exe(self, *args, async=True, macros=[], objects=[], **kwargs):
        macros = macros + ['FLX_STATIC_LINK']
        objs = objects + [self.flx_arun_lib if async else self.flx_run_lib]

        return self._link(self.cxx.link_exe, *args,
            macros=macros,
            objects=objs,
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

        return self.ctx.execute(cmd, *args, **kwargs)

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

        # run flx_pkgconfig to generate the include files, normally done
        # by flx command line harness but we're probably building it here

        flx_pkgconfig = Path("bin/flx_pkgconfig")
        flx_pkgconfig = flx_pkgconfig.addroot(self.ctx.buildroot)
        config = Path("config")
        config = config.addroot(self.ctx.buildroot)
        cmd = [flx_pkgconfig, "--path+="+config, "--field=includes", "@"+src[:-4]+".resh"]
        stdout, stderr =self.ctx.execute(cmd)
        output = stdout.decode()[:-1] # strip trailing newline
        includes = output.split(' ')
        files = ["#include "+i+"\n" for i in includes]
        fn = src[:-4]+".includes"
        f = open(fn,"w")
        for file in files: f.write(file)
        f.close()

        return function(obj, dst,
            async=async,
            includes=cxx_includes,
            libs=cxx_libs,
            cflags=cxx_cflags,
            lflags=cxx_lflags,
        )

    def _build_flx_pkgconfig_link(self, function, src, dst=None, *,
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

    def build_flx_pkgconfig_exe(self, *args, **kwargs):
        return self._build_flx_pkgconfig_link(self.link_exe, *args, **kwargs)

# ------------------------------------------------------------------------------

def build(ctx, flxg, cxx, drivers):
    return Builder(
        ctx,
        flxg,
        cxx,
        drivers.flx_run_exe,
        drivers.flx_arun_exe,
        drivers.flx_run_lib,
        drivers.flx_arun_lib,
    )

def build_flx_pkgconfig(phase):
    return phase.flx.build_flx_pkgconfig_exe(
        dst='bin/flx_pkgconfig',
        src='src/flx_pkgconfig/flx_pkgconfig.flx',
        includes=[phase.ctx.buildroot / 'lib'],
        cxx_includes=['src/flx_pkgconfig', phase.ctx.buildroot / 'lib/rtl'],
        cxx_libs=[call('buildsystem.flx_rtl.build_runtime', phase).static],
    )


def build_flx(phase):
    return phase.flx.build_exe(
        dst='bin/flx',
        src=Path('src/flx/flx.flx').addroot(phase.ctx.buildroot),
        includes=[phase.ctx.buildroot / 'lib'],
        cxx_includes=['tools', phase.ctx.buildroot / 'lib/rtl'],
        cxx_libs=[call('buildsystem.flx_rtl.build_runtime', phase).static],
    )

# ------------------------------------------------------------------------------

def test_flx(phase, src, *args, **kwargs):
    src = Path(src)

    passed = True
    for static in False, True:
        try:
            exe = phase.felix.compile(src, static=static)
        except fbuild.ExecutionError as e:
            phase.ctx.logger.log(e, verbose=1)
            if e.stdout:
                phase.ctx.logger.log(e.stdout.decode().strip(), verbose=1)
            if e.stderr:
                phase.ctx.logger.log(e.stderr.decode().strip(), verbose=1)
            passed = False
            continue

        if static:
            dst = exe + '.static.stdout'
        else:
            dst = exe + '.shared.stdout'

        expect = src.replaceext('.expect')

        passed &= check_flx(phase.ctx, phase.felix, *args,
            exe=exe,
            dst=dst,
            expect=expect if expect.exists() else None,
            static=static,
            **kwargs)

    return passed

@fbuild.db.caches
def check_flx(ctx, felix,
        exe:fbuild.db.SRC,
        dst:fbuild.db.DST,
        expect:fbuild.db.OPTIONAL_SRC,
        static,
        env={}):
    ctx.logger.check('checking ' + exe)
    try:
        stdout, stderr = felix.run(exe,
            env=dict(env, TEST_DATA_DIR=Path('test/test-data')),
            static=static,
            timeout=60,
            quieter=1)
    except fbuild.ExecutionError as e:
        if isinstance(e, fbuild.ExecutionTimedOut):
            ctx.logger.failed('failed: timed out')
        else:
            ctx.logger.failed()

        ctx.logger.log(e, verbose=1)
        if e.stdout:
            ctx.logger.log(e.stdout.decode().strip(), verbose=1)
        if e.stderr:
            ctx.logger.log(e.stderr.decode().strip(), verbose=1)
        return False

    with open(dst, 'wb') as f:
        f.write(stdout)

    if expect is None:
        ctx.logger.log('no .expect', color='cyan')
        return True
    else:
        stdout = stdout.replace(b'\r\n', b'\n').replace(b'\r', b'\n')

        with open(expect, 'rb') as f:
            s = f.read().replace(b'\r\n', b'\n').replace(b'\r', b'\n')

        if stdout == s:
            ctx.logger.passed()
            return True
        else:
            ctx.logger.failed('failed: output does not match')
            for line in difflib.ndiff(
                    stdout.decode().split('\n'),
                    s.decode().split('\n')):
                ctx.logger.log(line)
            dst.remove()
            return False
