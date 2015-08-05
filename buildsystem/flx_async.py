import fbuild
from fbuild.functools import call
from fbuild.path import Path
from fbuild.record import Record
from fbuild.builders.file import copy

import buildsystem

# ------------------------------------------------------------------------------

def build_runtime(host_phase,target_phase):
    path = Path ('src/flx_async')
    #buildsystem.copy_hpps_to_rtl(target_phase.ctx,
    #    path / 'flx_async.hpp',
    #)

    dst = 'host/lib/rtl/flx_async'
    suffix = '.so'
    srcs = [copy(ctx=target_phase.ctx, src=f, dst=target_phase.ctx.buildroot / f) for f in ['src/flx_async/flx_async.cpp']]
    includes = [
        target_phase.ctx.buildroot / 'host/lib/rtl',
        target_phase.ctx.buildroot / 'share/lib/rtl'
    ]
    macros = ['BUILD_ASYNC']
    libs = [
        call('buildsystem.flx_pthread.build_runtime', target_phase),
        call('buildsystem.flx_gc.build_runtime', host_phase,target_phase),
    ]

    return Record(
        static=buildsystem.build_cxx_static_lib(target_phase, dst, srcs,
            includes=includes,
            macros=macros,
            libs=[lib.static for lib in libs]),
        shared=buildsystem.build_cxx_shared_lib(target_phase, dst, srcs,
            includes=includes,
            macros=macros,
            libs=[lib.shared for lib in libs]))
