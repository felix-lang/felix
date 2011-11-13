import fbuild
from fbuild.path import Path
from fbuild.record import Record

import buildsystem

# ------------------------------------------------------------------------------

def build(phase):
    path = Path('src/tools/timeout-4.11')
    dst = phase.ctx.buildroot / 'bin/timeout'
    srcs = [path / 'errhelp.c', path / 'stderr.c', path / 'timeout.c'] 
    includes = ['src/tools/timeout-4.11']
    return Record (
        static = phase.c.static.build_exe(dst, srcs, includes=includes)
      )
