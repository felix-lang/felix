from itertools import chain

import fbuild.builders
import fbuild.db
import fbuild.path

# ------------------------------------------------------------------------------

class PkgConfig(fbuild.db.PersistentObject):
    def __init__(self, ctx, package, exe=None):
        super().__init__(ctx)

        self.exe = fbuild.builders.find_program(ctx, [exe or 'pkg-config'])
        self.pkg = package

    @fbuild.db.cachemethod
    def __call__(self, cmd, *args, **kwargs):
        stdout, stderr = self.ctx.execute(list(chain((self.exe, self.pkg), cmd)),
            quieter=1)
        return stdout.decode().strip()

    def version(self, *args, **kwargs):
        """Return the version of the pkg-config executable."""
        return self(('--version',), *args, **kwargs)

    def version(self, *args, **kwargs):
        """Return the version of the current package."""
        return self(('--modversion',), *args, **kwargs)

    def cflags(self, components=(), *args, **kwargs):
        """Return C compiler flags for files that use the given package."""
        return self(tuple(chain(('--cflags',), components)), *args, **kwargs).\
            split(' ')

    def libs(self, components=(), *args, **kwargs):
        """Return linker flags needed for linking against the given package."""
        return self(tuple(chain(('--libs',), components)), *args, **kwargs)

    def require_version(self, require_version=None,
        requires_at_least_version=None, requires_at_most_version=None):
        """Require a version of the current module."""
        fbuild.builders.check_version(ctx, self, self.modversion,
            requires_version=requires_version,
            requires_at_least_version=requires_at_least_version,
            requires_at_most_version=requires_at_most_version)

    def __str__(self):
        return self.exe.name
