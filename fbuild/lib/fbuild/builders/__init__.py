import abc
import contextlib
import os
import sys
from functools import partial
from itertools import chain

import fbuild
import fbuild.db
import fbuild.path
import fbuild.temp

# ------------------------------------------------------------------------------

class MissingProgram(fbuild.ConfigFailed):
    def __init__(self, programs=None):
        self.programs = programs

    def __str__(self):
        if self.programs is None:
            return 'cannot find program'
        else:
            return 'cannot find any of the programs %s' % \
                ' '.join(repr(str(p)) for p in self.programs)

# ------------------------------------------------------------------------------

@fbuild.db.caches
def find_program(ctx, names, paths=None, *, quieter=0):
    """L{find_program} is a test that searches the paths for one of the
    programs in I{name}.  If one is found, it is returned.  If not, the next
    name in the list is searched for."""

    if paths is None:
        paths = os.environ['PATH'].split(os.pathsep)

    # If we're running on windows, we need to append '.exe' to the filenames
    # that we're searching for.
    if sys.platform == 'win32':
        new_names = []
        for name in names:
            if \
                    not name.endswith('.exe') or \
                    not name.endswith('.cmd') or \
                    not name.endswith('.bat'):
                new_names.append(name + '.exe')
                new_names.append(name + '.cmd')
                new_names.append(name + '.bat')
            new_names.append(name)
        names = new_names

    for name in names:
        filename = fbuild.path.Path(name)
        ctx.logger.check('looking for ' + filename.name, verbose=quieter)

        if filename.exists() and filename.isfile():
            ctx.logger.passed('ok %s' % filename, verbose=quieter)
            return fbuild.path.Path(name)
        else:
            for path in paths:
                filename = fbuild.path.Path(path, name)
                if filename.exists() and filename.isfile():
                    ctx.logger.passed('ok %s' % filename, verbose=quieter)
                    return fbuild.path.Path(filename)

        ctx.logger.failed(verbose=quieter)

    raise MissingProgram(names)

# ------------------------------------------------------------------------------

def check_version(ctx, builder, version_function, *,
        requires_version=None,
        requires_at_least_version=None,
        requires_at_most_version=None):
    """Helper function to simplify checking the version of a builder."""
    if any(v is not None for v in (
            requires_version,
            requires_at_least_version,
            requires_at_most_version)):
        ctx.logger.check('checking %s version' % builder)

        version_str = version_function()

        # Convert the version into a tuple
        version = []
        for i in version_str.split('.'):
            try:
                version.append(int(i))
            except ValueError:
                # The subversion isn't a number, so just convert it to a
                # string.
                version.append(i)
        version = tuple(version)

        if requires_version is not None and requires_version != version:
            msg = 'version %s required; found %s' % (
                '.'.join(str(i) for i in requires_version), version_str)

            ctx.logger.failed(msg)
            raise fbuild.ConfigFailed(msg)

        if requires_at_least_version is not None and \
                requires_at_least_version > version:
            msg = 'at least version %s required; found %s' % (
                '.'.join(str(i) for i in requires_at_least_version),
                version_str)

            ctx.logger.failed(msg)
            raise fbuild.ConfigFailed(msg)

        if requires_at_most_version is not None and \
                requires_at_most_version < version:
            msg = 'at most version %s required; found %s' % (
                '.'.join(str(i) for i in requires_at_most_version),
                version_str)

            ctx.logger.failed(msg)
            raise fbuild.ConfigFailed(msg)

        ctx.logger.passed(version_str)

# ------------------------------------------------------------------------------

class AbstractCompiler(fbuild.db.PersistentObject):
    def __init__(self, *args, src_suffix, **kwargs):
        super().__init__(*args, **kwargs)

        self.src_suffix = src_suffix

    @fbuild.db.cachemethod
    def compile(self, src:fbuild.db.SRC, *args, **kwargs) -> fbuild.db.DST:
        return self.uncached_compile(src, *args, **kwargs)

    @abc.abstractmethod
    def uncached_compile(self, src, *args, **kwargs):
        pass

    @fbuild.db.cachemethod
    def build_objects(self, srcs:fbuild.db.SRCS, *args, **kwargs) -> \
            fbuild.db.DSTS:
        """Compile all of the passed in L{srcs} in parallel."""
        # When a object has extra external dependencies, such as .c files
        # depending on .h changes, depending on library changes, we need to add
        # the dependencies in build_objects.  Unfortunately, the db doesn't
        # know about these new files and so it can't tell when a function
        # really needs to be rerun.  So, we'll just not cache this function.
        # We need to add extra dependencies to our call.
        objs = []
        src_deps = []
        dst_deps = []
        for o, s, d in self.ctx.scheduler.map(
                partial(self.compile.call, **kwargs),
                srcs):
            objs.append(o)
            src_deps.extend(s)
            dst_deps.extend(d)

        self.ctx.db.add_external_dependencies_to_call(
            srcs=src_deps,
            dsts=dst_deps)

        return objs

    # --------------------------------------------------------------------------

    def tempfile(self, code):
        return fbuild.temp.tempfile(code, self.src_suffix)

    @contextlib.contextmanager
    def tempfile_compile(self, code='', *, quieter=1, **kwargs):
        with self.tempfile(code) as src:
            yield self.uncached_compile(src, quieter=quieter, **kwargs)

    def try_compile(self, *args, **kwargs):
        try:
            with self.tempfile_compile(*args, **kwargs):
                return True
        except fbuild.ExecutionError:
            return False

    def check_compile(self, code, msg, *args, **kwargs):
        self.ctx.logger.check(msg)
        if self.try_compile(code, *args, **kwargs):
            self.ctx.logger.passed()
            return True
        else:
            self.ctx.logger.failed()
            return False

# ------------------------------------------------------------------------------

class AbstractLibLinker(AbstractCompiler):
    @fbuild.db.cachemethod
    def link_lib(self, dst, srcs:fbuild.db.SRCS, *args,
            libs:fbuild.db.SRCS=(),
            **kwargs) -> fbuild.db.DST:
        """Link compiled files into a library and caches the results."""
        return self.uncached_link_lib(dst, srcs, *args, libs=libs, **kwargs)

    @abc.abstractmethod
    def uncached_link_lib(self, *args, **kwargs):
        pass

    def build_lib(self, dst, srcs, *, objs=(), libs=(), ckwargs={}, lkwargs={}):
        """Compile all of the passed in L{srcs} in parallel, then link them
        into a library."""
        objs = tuple(chain(objs, self.build_objects(srcs, **ckwargs)))
        return self.link_lib(dst, objs, libs=libs, **lkwargs)

    # --------------------------------------------------------------------------

    @contextlib.contextmanager
    def tempfile_link_lib(self, code='', *, quieter=1, ckwargs={}, **kwargs):
        with self.tempfile(code) as src:
            dst = src.parent / 'temp'
            obj = self.uncached_compile(src, quieter=quieter, **ckwargs)
            yield self.uncached_link_lib(dst, [obj], quieter=quieter, **kwargs)

    def try_link_lib(self, *args, **kwargs):
        try:
            with self.tempfile_link_lib(*args, **kwargs):
                return True
        except fbuild.ExecutionError:
            return False

    def check_link_lib(self, code, msg, *args, **kwargs):
        self.ctx.logger.check(msg)
        if self.try_link_lib(code, *args, **kwargs):
            self.ctx.logger.passed()
            return True
        else:
            self.ctx.logger.failed()
            return False

# ------------------------------------------------------------------------------

class AbstractRunner(fbuild.db.PersistentObject):
    @abc.abstractmethod
    def tempfile_run(self, *args, **kwargs):
        pass

    def try_run(self, code='', quieter=1, **kwargs):
        try:
            self.tempfile_run(code, quieter=quieter, **kwargs)
        except fbuild.ExecutionError:
            return False
        else:
            return True

    def check_run(self, code, msg, *args, **kwargs):
        self.ctx.logger.check(msg)
        if self.try_run(code, *args, **kwargs):
            self.ctx.logger.passed()
            return True
        else:
            self.ctx.logger.failed()
            return False

# ------------------------------------------------------------------------------

class AbstractExeLinker(AbstractCompiler, AbstractRunner):
    @fbuild.db.cachemethod
    def link_exe(self, dst, srcs:fbuild.db.SRCS, *args,
            libs:fbuild.db.SRCS=(),
            **kwargs) -> fbuild.db.DST:
        """Link compiled files into an executable."""
        return self.uncached_link_exe(dst, srcs, *args, libs=libs, **kwargs)

    @abc.abstractmethod
    def uncached_link_exe(self, *args, **kwargs):
        pass

    def build_exe(self, dst, srcs, *, objs=(), libs=(), ckwargs={}, lkwargs={}):
        """Compile all of the passed in L{srcs} in parallel, then link them
        into an executable."""
        objs = tuple(chain(objs, self.build_objects(srcs, **ckwargs)))
        return self.link_exe(dst, objs, libs=libs, **lkwargs)

    # --------------------------------------------------------------------------

    @contextlib.contextmanager
    def tempfile_link_exe(self, code='', *, quieter=1, ckwargs={}, **kwargs):
        with self.tempfile(code) as src:
            dst = src.parent / 'temp'
            obj = self.uncached_compile(src, quieter=quieter, **ckwargs)
            yield self.uncached_link_exe(dst, [obj], quieter=quieter, **kwargs)

    def try_link_exe(self, *args, **kwargs):
        try:
            with self.tempfile_link_exe(*args, **kwargs):
                return True
        except fbuild.ExecutionError:
            return False

    def check_link_exe(self, code, msg, *args, **kwargs):
        self.ctx.logger.check(msg)
        if self.try_link_exe(code, *args, **kwargs):
            self.ctx.logger.passed()
            return True
        else:
            self.ctx.logger.failed()
            return False

    def tempfile_run(self, *args, quieter=1, ckwargs={}, lkwargs={}, **kwargs):
        with self.tempfile_link_exe(*args,
                quieter=quieter,
                ckwargs=ckwargs,
                **lkwargs) as exe:
            return self.ctx.execute([exe],
                quieter=quieter,
                cwd=exe.parent,
                **kwargs)

# ------------------------------------------------------------------------------

class AbstractCompilerBuilder(AbstractLibLinker, AbstractExeLinker):
    pass
