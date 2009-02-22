import fbuild
from fbuild.functools import call
from fbuild.record import Record

# ------------------------------------------------------------------------------

def build_runtime(phase):
    dst = fbuild.buildroot / 'lib/rtl/flx_async'
    suffix = '.so'
    srcs = ['src/flx_async/flx_async.cpp']
    includes = [
        fbuild.buildroot / 'config/target',
        'src/exceptions',
        'src/demux',
        'src/faio',
        'src/gc',
        'src/pthread',
    ]
    macros = ['BUILD_ASYNC']
    libs = [
        call('buildsystem.flx_pthread.build_runtime', phase),
        call('buildsystem.faio.build_runtime', phase),
        call('buildsystem.demux.build_runtime', phase),
    ]

    return Record(
        static=phase.cxx.static.build_lib(dst + '_static', srcs,
            includes=includes,
            macros=macros + ['FLX_STATIC_LINK'],
            libs=[lib.static for lib in libs]),
        shared=phase.cxx.shared.build_lib(dst + '_dynamic', srcs,
            includes=includes,
            macros=macros,
            libs=[lib.shared for lib in libs]))
