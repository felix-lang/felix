import sys
from functools import partial
from itertools import chain

import fbuild
import fbuild.builders
import fbuild.builders.platform
import fbuild.db
from fbuild.path import Path
from fbuild.temp import tempfile

# ------------------------------------------------------------------------------

class Flx(fbuild.db.PersistentObject):
    def __init__(self, ctx, exe, *, includes=[], debug=False, flags=[]):
        super().__init__(ctx)

        # we split exe in case extra arguments were specified in the name
        self.exe = fbuild.builders.find_program(ctx, [exe])
        self.includes = includes
        self.debug = debug
        self.flags = flags

        if not self.check_flags([]):
            raise fbuild.ConfigFailed('%s failed to compile an exe' % self)

    def __call__(self, src, *args,
            includes=[],
            debug=None,
            static=False,
            stdout=None,
            flags=[],
            cwd=None,
            **kwargs):
        cmd = [self.exe]

        if debug is None:
            debug = self.debug

        if debug:
            cmd.append('--debug')

        if static:
            cmd.append('--static')

        if stdout:
            cmd.append('--stdout='+stdout)

        cmd.extend('-I' + i for i in sorted(includes) if Path(i).exists())
        cmd.extend(self.flags)
        cmd.extend(flags)
        cmd.append(src)

        return self.ctx.execute(cmd, *args, **kwargs)

    def check_flags(self, flags=[]):
        if flags:
            self.ctx.logger.check('checking %s with %s' %
                (self, ' '.join(flags)))
        else:
            self.ctx.logger.check('checking %s' % self)

        with tempfile('', suffix='.flx') as src:
            try:
                self(src, flags=flags, quieter=1)
            except fbuild.ExecutionError as e:
                self.ctx.logger.failed()
                if e.stdout:
                    self.ctx.logger.log(e.stdout.decode())
                if e.stderr:
                    self.ctx.logger.log(e.stderr.decode())
                return False

        self.ctx.logger.passed()
        return True

    def __str__(self):
        return ' '.join([self.exe] + self.flags)

# ------------------------------------------------------------------------------

class Felix(fbuild.builders.AbstractCompiler):
    def __init__(self, ctx, exe='flx', *,
            platform=None,
            includes=[],
            static=False,
            debug=False,
            flags=[]):
        super().__init__(ctx, src_suffix='.flx')

        self.flx = Flx(ctx, exe, debug=debug, flags=flags)
        self.exe_suffix = fbuild.builders.platform.exe_suffix(ctx, platform)
        self.lib_suffix = fbuild.builders.platform.shared_lib_suffix(ctx,
            platform)
        self.static = static
        self.includes = includes
        self.flags = flags

    @fbuild.db.cachemethod
    def compile(self, src:fbuild.db.SRC, *args, **kwargs) -> fbuild.db.DST:
        """Compile a felix file and cache the results."""
        return self.uncached_compile(src, *args, **kwargs)

    def uncached_compile(self, src, *,
            static=None,
            includes=[],
            flags=[],
            buildroot=None,
            **kwargs):
        """Compile a felix file without caching the results.  This is needed
        when compiling temporary files."""
        src = Path(src)
        buildroot = buildroot or self.ctx.buildroot
        src_buildroot = src.addroot(buildroot)

        if static is None:
            static = self.static

        if static:
            dst = src_buildroot.replaceext(self.exe_suffix)
        else:
            dst = src_buildroot.replaceext(self.lib_suffix)

        if src != src_buildroot:
            src_buildroot.parent.makedirs()
            src.copy(src_buildroot)
            src = src_buildroot

        includes = set(includes)
        includes.update(self.includes)
        includes.add(src.parent)

        cmd_flags = ['-c']
        cmd_flags.extend(self.flags)
        cmd_flags.extend(flags)

        self.flx(src, self.flx, '%s -> %s' % (src, dst),
            includes=includes,
            static=static,
            flags=cmd_flags,
            color='compile',
            **kwargs)

        return dst

    def build_objects(self, srcs, **kwargs):
        return fbuild.scheduler.map(partial(self.compile, **kwargs), srcs)

    def run(self, src, *args, **kwargs):
        src = src.replaceexts({self.exe_suffix: '', self.lib_suffix: ''})
        return self.flx(src, *args, **kwargs)

    # --------------------------------------------------------------------------

    def tempfile_run(self, code='', *, quieter=1, **kwargs):
        with self.tempfile(code) as src:
            exe = self.uncached_compile(src, quieter=quieter, **ckwargs)
            return self.run(exe, quieter=quieter, **kwargs)
