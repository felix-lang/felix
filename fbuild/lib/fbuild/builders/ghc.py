from functools import partial
from itertools import chain

import fbuild
import fbuild.builders
import fbuild.builders.platform
import fbuild.db
import fbuild.path
import fbuild.temp

# ------------------------------------------------------------------------------

class Ghc(fbuild.db.PersistentObject):
    """Create a ghc driver."""

    def __init__(self, ctx, exe, *, flags=[]):
        super().__init__(ctx)

        self.exe = fbuild.builders.find_program(ctx, [exe if exe else 'ghc'])
        self.flags = flags

        if not self.check_flags(flags):
            raise fbuild.ConfigFailed('%s failed to compile an exe' % self)

    def __call__(self, dst, srcs, *,
            pre_flags=[],
            flags=[],
            odir=None,
            hidir=None,
            **kwargs):
        cmd = [self.exe]

        cmd.extend(pre_flags)
        cmd.extend(('-o', dst))

        if odir is not None:  cmd.extend(('-odir', odir))
        if hidir is not None: cmd.extend(('-hidir', hidir))

        # Make sure we don't repeat flags
        new_flags = []
        for flag in chain(self.flags, flags):
            if flag not in new_flags:
                new_flags.append(flag)
        flags = new_flags

        cmd.extend(flags)
        cmd.extend(srcs)

        return self.ctx.execute(cmd, str(self),
            msg2='%s -> %s' % (' '.join(srcs), dst),
            **kwargs)

    def check_flags(self, flags):
        if flags:
            self.ctx.logger.check('checking %s with %s' % (self, ' '.join(flags)))
        else:
            self.ctx.logger.check('checking %s' % self)

        code = '''
        import System.Exit
        main = exitSuccess
        '''

        with fbuild.temp.tempfile(code, suffix='.hs') as src:
            try:
                self('test', [src], flags=flags, quieter=1, cwd=src.parent)
            except fbuild.ExecutionError:
                self.ctx.logger.failed()
                return False

        self.ctx.logger.passed()
        return True

    def __str__(self):
        return self.exe.name

# ------------------------------------------------------------------------------

class Builder(fbuild.builders.AbstractExeLinker):
    def __init__(self, ctx, *,
            ghc='ghc',
            platform=None,
            **kwargs):
        super().__init__(ctx, src_suffix='.hs')

        self.ghc = Ghc(ctx, ghc, **kwargs)
        self.obj_suffix = fbuild.builders.platform.obj_suffix(ctx, platform)

    # --------------------------------------------------------------------------

    @fbuild.db.cachemethod
    def compile(self, src:fbuild.db.SRC, *args, **kwargs) -> fbuild.db.DST:
        """Compile a haskell file and cache the results."""
        return self.uncached_compile(src, *args, **kwargs)

    def uncached_compile(self, src, dst=None, *args,
            pre_flags=[],
            odir=None,
            hidir=None,
            buildroot=None,
            **kwargs):
        """Compile a haskell file."""

        buildroot = buildroot or self.ctx.buildroot

        src = fbuild.path.Path(src)
        dst = fbuild.path.Path(dst or src).replaceext(self.obj_suffix)
        dst = dst.addroot(buildroot)

        dst.parent.makedirs()

        pre_flags = list(pre_flags)
        pre_flags.append('-c')

        self.ghc(dst, [src],
            pre_flags=pre_flags,
            odir=fbuild.path.Path(odir or dst.parent),
            hidir=fbuild.path.Path(hidir or dst.parent),
            color='compile',
            *args, **kwargs)

        return dst

    # --------------------------------------------------------------------------

    @fbuild.db.cachemethod
    def link_exe(self, dst, srcs:fbuild.db.SRCS, *args,
            **kwargs) -> fbuild.db.DST:
        """Link all the L{srcs} into an executable and cache the result."""
        return self.uncached_link_exe(dst, srcs, *args, **kwargs)

    def uncached_link_exe(self, dst, srcs, *args, buildroot=None, **kwargs):
        """Link all the L{srcs} into an executable."""
        dst = fbuild.path.Path(dst).addroot(buildroot or self.ctx.buildroot)
        dst.parent.makedirs()

        self.ghc(dst, srcs, *args, color='link', **kwargs)

        return dst

    # --------------------------------------------------------------------------

    @fbuild.db.cachemethod
    def build_objects(self, srcs:fbuild.db.SRCS,
            **kwargs) -> fbuild.db.DSTS:
        """Compile all the L{srcs} in parallel."""

        return self.ctx.scheduler.map(partial(self.compile, **kwargs), srcs)

    def build_exe(self, dst, srcs:fbuild.db.SRCS, *args,
            ckwargs={},
            lkwargs={},
            **kwargs) -> fbuild.db.DST:
        """Compile all the L{srcs} and link into an executable."""
        objs = self.build_objects(srcs, **dict(kwargs, **ckwargs))
        return self.link_exe(dst, objs, **dict(kwargs, **lkwargs))
