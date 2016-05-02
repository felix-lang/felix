import re

import fbuild.builders.c.gcc

# ------------------------------------------------------------------------------

class Clang(fbuild.builders.c.gcc.Gcc):
    def version(self, *args, **kwargs):
        """Return the version of the llvm-config executable."""

        stdout, stderr = self.ctx.execute((self.exe, '--version'), quieter=1)

        m = re.match(
            r'(?:Apple clang .* \(based on LLVM (\S+)\))'
            r'|'
            r'(?:clang version (\S+))', stdout.decode())
        if m:
            if m.group(1):
                return m.group(1)
            else:
                return m.group(2)

        return None

# ------------------------------------------------------------------------------

def make_cc(ctx, exe=None, default_exes=['clang'], **kwargs):
    return Clang(ctx,
        fbuild.builders.find_program(ctx, [exe] if exe else default_exes),
        **kwargs)

# ------------------------------------------------------------------------------

def static(*args, make_cc=make_cc, **kwargs):
    return fbuild.builders.c.gcc.static(*args, make_cc=make_cc, **kwargs)

def shared(*args, make_cc=make_cc, **kwargs):
    return fbuild.builders.c.gcc.shared(*args, make_cc=make_cc, **kwargs)
