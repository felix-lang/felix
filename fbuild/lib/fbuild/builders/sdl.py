from itertools import chain

import fbuild.builders
import fbuild.db
import fbuild.path

# ------------------------------------------------------------------------------

class SDLConfig(fbuild.db.PersistentObject):
    def __init__(self, ctx, exe=None, *,
            requires_version=None,
            requires_at_least_version=None,
            requires_at_most_version=None):
        super().__init__(ctx)

        self.exe = fbuild.builders.find_program(ctx, [exe or 'sdl-config'])

        # Make sure we've got a valid version.
        fbuild.builders.check_version(ctx, self, self.version,
            requires_version=requires_version,
            requires_at_least_version=requires_at_least_version,
            requires_at_most_version=requires_at_most_version)

    def __call__(self, cmd, *args,  **kwargs):
        stdout, stderr = self.ctx.execute(list(chain((self.exe,), cmd)),
            quieter=1)
        return stdout.decode().strip()

    def version(self, *args, **kwargs):
        return self(('--version',), *args, **kwargs)

    def prefix(self, *args, **kwargs):
        return fbuild.path.Path(self(('--prefix',), *args, **kwargs))

    def exec_prefix(self, *args, **kwargs):
        return fbuild.path.Path(self(('--exec-prefix',), *args, **kwargs))

    def cflags(self, *args, **kwargs):
        return self(('--cflags',), *args, **kwargs)

    def libs(self, *args, **kwargs):
        return self(('--libs',), *args, **kwargs)

    def static_libs(self, *args, **kwargs):
        return self(('--static-libs',), *args, **kwargs)

    def __str__(self):
        return self.exe.name
