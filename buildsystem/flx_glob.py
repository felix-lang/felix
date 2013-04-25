import fbuild
from fbuild.functools import call
from fbuild.path import Path
from fbuild.record import Record

import buildsystem

# ------------------------------------------------------------------------------

def build_runtime(host_phase, target_phase):
    path = Path('src/unixem')

    buildsystem.copy_hpps_to_rtl(target_phase.ctx,
        path / 'flx_glob.hpp',
    )

    dst = target_phase.ctx.buildroot / 'lib/rtl/flx_glob'
    srcs = []
    includes = [target_phase.ctx.buildroot / 'lib/rtl']
    macros = ['BUILD_GLOB']
    libs = [call('buildsystem.flx_gc.build_runtime', host_phase, target_phase)]

    if 'win32' in target_phase.platform:
        srcs.extend((path / 'flx_glob.cpp', path / 'unixem_util.cpp'))

        return Record(
          static=buildsystem.build_cxx_static_lib(target_phase, dst, srcs,
              includes=includes,
              macros=macros,
              libs=[lib.static for lib in libs]),
          shared=buildsystem.build_cxx_shared_lib(target_phase, dst, srcs,
              includes=includes,
              macros=macros,
              libs=[lib.shared for lib in libs]))
    else:
        return Record()
