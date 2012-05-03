import fbuild
import fbuild.builders.java
from fbuild.path import Path

# ------------------------------------------------------------------------------

class AbstractCompiler(fbuild.builders.java.AbstractCompiler):
    def __init__(self, ctx, exe, *args,
            optimize=False,
            optimize_flags=['-optimise'],
            **kwargs):
        self.optimize = optimize
        self.optimize_flags = optimize_flags

        super().__init__(ctx, exe, '.scala', *args, **kwargs)

        if optimize_flags and not self.check_flags(optimize_flags):
            raise fbuild.ConfigFailed('%s failed to compile an exe' % self)

    def _run(self, *args, optimize=None, flags=[], **kwargs):
        """Process the L{srcs} on the arguments."""

        # Add scala's optimization flags to java's flags.
        flags = list(flags)

        if (optimize is None and self.optimize) or optimize:
            flags.extend(self.optimize_flags)

        return super()._run(*args, flags=flags, **kwargs)

# ------------------------------------------------------------------------------

class Scala(AbstractCompiler):
    def __init__(self, ctx, exe='scala', *args, **kwargs):
        super().__init__(ctx, exe, *args, **kwargs)

    def __call__(self, src, *args, flags=[], buildroot=None, **kwargs):
        """Run a scala script."""

        src = Path(src)
        buildroot = buildroot or self.ctx.buildroot
        src_buildroot = src.addroot(buildroot)
        dst = src.replaceext('.jar')

        # We need to copy the src into the buildroot so we don't pollute our
        # tree.
        if src != src_buildroot:
            src_buildroot.parent.makedirs()
            src.copy(src_buildroot)
            src = src_buildroot

        # Always save the compilation results.
        flags = list(flags)
        flags.append('-savecompiled')

        stdout, stderr = self._run([src], *args, flags=flags, **kwargs)
        return dst, stdout, stderr

# ------------------------------------------------------------------------------

class Scalac(AbstractCompiler):
    def __init__(self, ctx, exe='scalac', *args, **kwargs):
        super().__init__(ctx, exe, *args, **kwargs)

    def __call__(self, dst, srcs, *args, buildroot=None, **kwargs):
        """Run a scala script."""

        dst = Path(dst).addroot(buildroot or self.ctx.buildroot)
        dst.makedirs()

        stdout, stderr = self._run(srcs, *args, dst=dst, **kwargs)
        return dst, stdout, stderr

# ------------------------------------------------------------------------------

class Builder(fbuild.builders.java.AbstractBuilder):
    def __init__(self, ctx, *,
            jar='jar',
            java='java',
            scala='scala',
            scalac='scalac',
            **kwargs):
        super().__init__(ctx, jar=jar, java=java, src_suffix='.scala')

        self.scala = Scala(ctx, scala)
        self.scalac = Scalac(ctx, scalac, **kwargs)

    def where(self):
        """Return the scala library directory."""
        return self.scalac.exe.realpath().parent.parent / 'lib'

    def uncached_compile(self, *args, **kwargs):
        return self._run(self.scalac, *args, **kwargs)

    def run_script(self, src, *args, **kwargs):
        """Run a scala script."""
        return self.scala((src,), *args, **kwargs)

    def run_class(self, *args, classpaths=[], **kwargs):
        """Run a scala library."""
        # Automatically add the scala-library.jar to the classpath
        classpaths = list(classpaths)
        classpaths.append(self.where() / 'scala-library.jar')

        return self.java.run_class(*args, classpaths=classpaths, **kwargs)
