import fbuild
from fbuild.path import Path
from fbuild.record import Record
from fbuild.functools import call
from buildsystem.config import config_call

import buildsystem

# ------------------------------------------------------------------------------

def build(phase):
    pthread_h = config_call('fbuild.config.c.posix.pthread_h',
        phase.platform,
        phase.cxx.shared)

    dlfcn_h = config_call('fbuild.config.c.posix.dlfcn_h',
        phase.platform,
        phase.cxx.static,
        phase.cxx.shared)

    flags = []
    libs=[call('buildsystem.sqlite3.build_runtime', phase, phase).static]
    external_libs = []
    path = Path('src/tools')
    dst = phase.ctx.buildroot / 'bin/flx_sqlite3'
    srcs = [path / 'flx_sqlite3.c'] 
    includes = [ phase.ctx.buildroot / 'lib/rtl', phase.ctx.buildroot / 'config/target' ]

    if dlfcn_h.dlopen:
        external_libs.extend ( dlfcn_h.external_libs)

    if pthread_h.pthread_create:
        flags.extend(pthread_h.flags)
        libs.extend(pthread_h.libs)
        external_libs.extend(pthread_h.external_libs)

    return Record (
        static = phase.c.static.build_exe(dst, srcs, 
          includes=includes, 
          cflags=flags, 
          libs=libs, 
          external_libs=external_libs)
      )
