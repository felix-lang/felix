import fbuild
from fbuild.functools import call
from fbuild.record import Record

import buildsystem

# ------------------------------------------------------------------------------

def build_runtime(phase):
    dst = 'lib/rtl/flx_async'
    suffix = '.so'
    srcs = ['src/flx_async/flx_async.cpp']
    includes = [
        phase.ctx.buildroot / 'config/target',
        'src/exceptions',
        'src/demux',
        'src/faio',
        'src/gc',
        'src/pthread',
        'src/rtl',
    ]
    macros = ['BUILD_ASYNC']
    libs = [
        call('buildsystem.flx_pthread.build_runtime', phase),
        call('buildsystem.faio.build_runtime', phase),
        call('buildsystem.demux.build_runtime', phase),
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
