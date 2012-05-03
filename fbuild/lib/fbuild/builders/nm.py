import io
import re
from itertools import chain

import fbuild
import fbuild.path
import fbuild.builders
import fbuild.db

# ------------------------------------------------------------------------------

class Nm(fbuild.db.PersistentObject):
    def __init__(self, ctx, exe=None):
        super().__init__(ctx)

        self.exe = fbuild.builders.find_program(ctx, [exe or 'nm'])

    @fbuild.db.cachemethod
    def object_dependencies(self, objs:fbuild.db.SRCS):
        defined_symbols_lookup = {}
        undefined_symbols_lookup = {}

        for obj in objs:
            defined_symbols, undefined_symbols = self._run(obj)
            for symbol in defined_symbols:
                defined_symbols_lookup[symbol] = obj

            undefined_symbols_lookup[obj] = undefined_symbols

        new_objs = []
        def f(obj):
            if obj in new_objs:
                return

            for undefined_symbol in undefined_symbols_lookup[obj]:
                try:
                    o = defined_symbols_lookup[undefined_symbol]
                except KeyError:
                    pass
                else:
                    if obj != o:
                        f(o)
            new_objs.append(obj)

        for obj in objs:
            f(obj)

        return reversed(new_objs)

    def _run(self, obj:fbuild.db.SRC):
        obj = fbuild.path.Path(obj)

        cmd = [self.exe, '-gP']
        cmd.append(obj)

        self.ctx.logger.check(' * %s -gP' % self.exe, obj, color='yellow')
        stdout, stderr = self.ctx.execute(cmd, stdout_quieter=1)

        # Gather the external symbols from the object files.
        defined_symbols = set()
        undefined_symbols = set()

        regex = re.compile(b'^(\S+) ([ABCDGRSTU])')
        for line in io.BytesIO(stdout):
            m = regex.match(line)
            if m:
                # Break up the symbols into defined and undefined symbols.
                symbol = m.group(1)
                if b'U' in m.group(2):
                    undefined_symbols.add(symbol)
                else:
                    defined_symbols.add(symbol)

        return defined_symbols, undefined_symbols

    def __str__(self):
        return str(self.exe.name)
