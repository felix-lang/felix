import os
import io
import re
from functools import partial
from itertools import chain

import fbuild
import fbuild.builders
import fbuild.db
from fbuild.path import Path
from fbuild.temp import tempfile

# ------------------------------------------------------------------------------

class Jar(fbuild.db.PersistentObject):
    def __init__(self, ctx, exe='jar'):
        super().__init__(ctx)

        self.exe = fbuild.builders.find_program(ctx, [exe])

    # --------------------------------------------------------------------------

    @fbuild.db.cachemethod
    def create(self, dst, srcs:fbuild.db.SRCS, *args,
            manifest:fbuild.db.OPTIONAL_SRC=None,
            **kwargs) -> fbuild.db.DST:
        """Collect all the L{srcs} into a jar and cache the result."""
        return self.uncached_create(dst, srcs, *args,
            manifest=manifest,
            **kwargs)

    def uncached_create(self, dst, srcs, *,
            manifest=None,
            cwd=None,
            buildroot=None,
            **kwargs):
        """Collect all the L{srcs} into a jar."""
        # Unfortunately, we need to adjust the current working directory to
        # where we want the jar files to be relative to. By default we'll at
        # least run jar from the buildroot.

        buildroot = buildroot or self.ctx.buildroot
        cwd = cwd or buildroot
        dst = Path(dst).addroot(buildroot)

        # We need ot make sure jars have sources passed in
        assert srcs, "%s: no sources passed in" % dst

        cmd = [self.exe]

        if manifest is None:
            cmd.append('cf')
        else:
            # Adjust the manifest to the cwd.
            cmd.extend(('cmf', Path(manifest).relpath(cwd)))

        # Adjust the dst and srcs to the cwd
        cmd.append(dst.relpath(cwd))
        cmd.extend(Path(src).relpath(cwd) for src in srcs)

        if manifest is None:
            msg2 = '%s -> %s' % (' '.join(srcs), dst)
        else:
            msg2 = '%s %s -> %s' % (manifest, ' '.join(srcs), dst)

        self.ctx.execute(cmd, self, msg2, cwd=cwd, color='link', **kwargs)

        return dst

    # --------------------------------------------------------------------------

    def __str__(self):
        return self.exe.name

# ------------------------------------------------------------------------------

class Java(fbuild.db.PersistentObject):
    def __init__(self, ctx, exe='java', *, classpaths=()):
        super().__init__(ctx)

        self.exe = fbuild.builders.find_program(ctx, [exe])
        self.classpaths = tuple(classpaths)

    def run_class(self, cls, *args, classpaths=(), **kwargs):
        cmd = [self.exe]

        cmd.extend(('-cp', os.pathsep.join(chain(self.classpaths, classpaths))))
        cmd.append(cls)

        return self.ctx.execute(cmd, *args, **kwargs)

    def run_jar(self, jar, *args, **kwargs):
        cmd = [self.exe]
        cmd.extend(('-jar', jar))

        return self.ctx.execute(cmd, *args, **kwargs)

    def __str__(self):
        return self.exe.name

# ------------------------------------------------------------------------------

class AbstractCompiler(fbuild.db.PersistentObject):
    def __init__(self, ctx, exe, src_suffix, *,
            classpaths=[],
            sourcepaths=[],
            debug=False,
            debug_flags=['-g'],
            target=None,
            flags=[]):
        super().__init__(ctx)

        self.exe = fbuild.builders.find_program(ctx, [exe])
        self.src_suffix = src_suffix
        self.classpaths = classpaths
        self.sourcepaths = sourcepaths
        self.debug = debug
        self.debug_flags = debug_flags
        self.target = target
        self.flags = flags

        if not self.check_flags([]):
            raise fbuild.ConfigFailed('%s failed to compile an exe' % self)

        if debug_flags and not self.check_flags(debug_flags):
            raise fbuild.ConfigFailed('%s failed to compile an exe' % self)

    def _run(self, srcs, *args,
            dst=None,
            classpaths=[],
            sourcepaths=[],
            debug=None,
            optimize=None,
            target=None,
            flags=[],
            **kwargs):
        """Process the L{srcs} on the arguments."""

        assert len(srcs) > 0, "%s: no sources passed in" % dst

        cmd = [self.exe]

        if dst is not None:
            cmd.extend(('-d', dst))

        if (debug is None and self.debug) or debug:
            cmd.extend(self.debug_flags)

        target = self.target if target is None else target
        if target is not None:
            cmd.append('-target:' + str(target))

        for classpath in chain(self.classpaths, classpaths):
            cmd.extend(('-cp', classpath))

        for sourcepath in chain(self.sourcepaths, sourcepaths):
            cmd.extend(('-sourcepath', sourcepath))

        cmd.extend(self.flags)
        cmd.extend(flags)
        cmd.extend(srcs)

        return self.ctx.execute(cmd, *args, **kwargs)

    def check_flags(self, flags=[]):
        """Verify that we can run with these flags."""

        if flags:
            self.ctx.logger.check('checking %s with %s' %
                (self, ' '.join(flags)))
        else:
            self.ctx.logger.check('checking %s' % self)

        with tempfile('', suffix=self.src_suffix) as src:
            try:
                self._run([src], flags=flags, quieter=1)
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
        return ' '.join([self.exe.name] + self.flags)

# ------------------------------------------------------------------------------

class Javac(AbstractCompiler):
    def __init__(self, ctx, exe='javac', *args, **kwargs):
        super().__init__(ctx, exe, '.java', *args, **kwargs)

    def __call__(self, dst, srcs, *args,
            buildroot=None,
            **kwargs):
        """Compile a java src."""

        dst = dst.addroot(buildroot or self.ctx.buildroot)
        dst.makedirs()

        stdout, stderr = self._run(srcs, *args, dst=dst,  **kwargs)
        return dst, stdout, stderr

# ------------------------------------------------------------------------------

class AbstractBuilder(fbuild.builders.AbstractLibLinker):
    def __init__(self, ctx, *, jar='jar', java='java', **kwargs):
        super().__init__(ctx, src_suffix='.class')

        self.jar = fbuild.builders.java.Jar(ctx, jar)
        self.java = fbuild.builders.java.Java(ctx, java)

    # --------------------------------------------------------------------------

    _dep_regex = re.compile(r'\[wrote (?:RegularFileObject\[)?([^\]]+)')

    def _run(self, builder, src, dst=None, *,
            flags=[],
            quieter=0,
            stderr_quieter=0,
            buildroot=None,
            **kwargs):
        """Compile a java file."""

        src = Path(src)
        dst = Path(dst or src.parent).addroot(buildroot or self.ctx.buildroot)

        # Extract the generated files when we compile the file.
        try:
            dst, stdout, stderr = builder(dst, [src],
                flags=list(chain(('-verbose',), flags)),
                quieter=quieter,
                stderr_quieter=1 if stderr_quieter == 0 else stderr_quieter,
                **kwargs)
        except fbuild.ExecutionError as e:
            if quieter == 0 and stderr_quieter == 0:
                # We errored out, but we've hidden the stderr output.
                for line in io.StringIO(e.stderr.decode()):
                    if not line.startswith('['):
                        self.ctx.logger.write(line)
            raise e

        # Parse the output and find what files we generated.
        dsts = []
        for line in io.StringIO(stderr.decode()):
            m = self._dep_regex.match(line)
            if m:
                dsts.append(Path(m.group(1)))
            elif quieter == 0 and stderr_quieter == 0:
                if not line.startswith('['):
                    self.ctx.logger.write(line)

        # Log all the files we found
        self.ctx.logger.check(str(builder), '%s -> %s' % (src, ' '.join(dsts)),
            color='compile')

        return dsts

    # --------------------------------------------------------------------------

    @fbuild.db.cachemethod
    def compile(self, src:fbuild.db.SRC, *args, **kwargs) -> fbuild.db.DSTS:
        """Compile the L{src} file and cache the results."""
        return self.uncached_compile(src, *args, **kwargs)

    def link_lib(self, *args, **kwargs):
        """Link all the L{srcs} into a library and cache the result."""
        return self.jar.create(*args, **kwargs)

    def uncached_link_lib(self, *args, **kwargs):
        """Link all the L{srcs} into a library."""
        return self.jar.uncached_create(*args, **kwargs)

    # --------------------------------------------------------------------------

    @fbuild.db.cachemethod
    def build_objects(self, srcs:fbuild.db.SRCS, **kwargs) -> fbuild.db.DSTS:
        """Compile all the L{srcs} in parallel."""
        dsts = []
        for d in self.ctx.scheduler.map(partial(self.compile, **kwargs), srcs):
            dsts.extend(d)

        return dsts

    def build_lib(self, dst, srcs, *args,
            cwd=None,
            ckwargs={},
            lkwargs={},
            **kwargs):
        """Compile all the L{srcs} and link into a library."""
        objs = self.build_objects(srcs, *args, **dict(ckwargs, **kwargs))
        return self.link_lib(dst, objs, cwd=cwd, **lkwargs)

    # --------------------------------------------------------------------------

    def tempfile_run(self, code='', *, quieter=1, ckwargs={}, **kwargs):
        """Execute a temporary file."""
        with self.tempfile(code) as src:
            exe = self.uncached_compile(src, quieter=quieter, **ckwargs)
            return self.run(exe, quieter=quieter, **kwargs)

    # --------------------------------------------------------------------------

    def run_class(self, *args, **kwargs):
        """Run a class."""
        return self.java.run_class(*args, **kwargs)

    def run_jar(self, *args, **kwargs):
        """Run a jar library."""
        return self.java.run_jar(*args, **kwargs)

# ------------------------------------------------------------------------------

class Builder(AbstractBuilder):
    def __init__(self, ctx, *, jar='jar', java='java', javac='javac', **kwargs):
        super().__init__(ctx, jar=jar, java=java, src_suffix='.java')

        self.javac = fbuild.builders.java.Javac(ctx, javac, **kwargs)

    def uncached_compile(self, *args, **kwargs):
        return self._run(self.javac, *args, **kwargs)
