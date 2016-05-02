import re

import fbuild.builders.c.gcc

# ------------------------------------------------------------------------------

class Intel(fbuild.builders.c.gcc.Gcc):
    def version(self, *args, **kwargs):
        """Return the version of the Intel compiler executable."""

        stdout, stderr = self.ctx.execute((self.exe, '--version'), quieter=1)

        m = re.match(r'ic(?:c|pc) \(ICC\) ([\d.]+)', stdout.decode())
        return m.group(1) if m else None

# ------------------------------------------------------------------------------

def make_cc(ctx, exe=None, default_exes=['icc'], **kwargs):
    return Intel(ctx,
        fbuild.builders.find_program(ctx, [exe] if exe else default_exes),
        **kwargs)

# ------------------------------------------------------------------------------

def static(*args, make_cc=make_cc, **kwargs):
    return fbuild.builders.c.gcc.static(*args, make_cc=make_cc, **kwargs)

def shared(*args, make_cc=make_cc, **kwargs):
    return fbuild.builders.c.gcc.shared(*args, make_cc=make_cc, **kwargs)
