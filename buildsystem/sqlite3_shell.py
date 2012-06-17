import fbuild
from fbuild.path import Path
from fbuild.record import Record
from fbuild.functools import call

import buildsystem

# ------------------------------------------------------------------------------

def build(phase):
    libs=[call('buildsystem.sqlite3.build_runtime', phase, phase).static]
    path = Path('src/tools')
    dst = phase.ctx.buildroot / 'bin/flx_sqlite3'
    srcs = [path / 'flx_sqlite3.c'] 
    includes = [ phase.ctx.buildroot / 'lib/rtl', phase.ctx.buildroot / 'config/target' ]
    return Record (
        static = phase.c.static.build_exe(dst, srcs, includes=includes, libs=libs)
      )
