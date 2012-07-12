import fbuild
from fbuild.functools import call
from fbuild.path import Path
from fbuild.record import Record
from fbuild.builders.file import copy

import buildsystem

# ------------------------------------------------------------------------------

def build_runtime(phase):
    path = Path ('src/flx_async')
    buildsystem.copy_hpps_to_rtl(phase.ctx,
        path / 'flx_async.hpp',
    )

    dst = 'lib/rtl/flx_async'
    suffix = '.so'
    srcs = [copy(ctx=phase.ctx, src=f, dst=phase.ctx.buildroot / f) for f in ['src/flx_async/flx_async.cpp']]
    includes = [
        phase.ctx.buildroot / 'config/target',
        phase.ctx.buildroot / 'lib/rtl'
    ]
    macros = ['BUILD_ASYNC']
    libs = [
        call('buildsystem.flx_pthread.build_runtime', phase),
    ]

    return Record(
        static=buildsystem.build_cxx_static_lib(phase, dst, srcs,
            includes=includes,
            macros=macros,
            libs=[lib.static for lib in libs]),
        shared=buildsystem.build_cxx_shared_lib(phase, dst, srcs,
            includes=includes,
            macros=macros,
            libs=[lib.shared for lib in libs]))
