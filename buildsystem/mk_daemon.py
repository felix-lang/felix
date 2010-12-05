import fbuild
from fbuild.path import Path
from fbuild.record import Record

import buildsystem

# ------------------------------------------------------------------------------

def build(phase):
    path = Path('tools')
    dst = phase.ctx.buildroot / 'bin/mk_daemon'
    src = path / 'mk_daemon.c'
    includes = ['src/tools']
    return Record (
        static = phase.c.static.build_exe(dst, [src], includes=includes)
      )
