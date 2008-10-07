import fbuild
from fbuild.path import Path
from fbuild.record import Record

import buildsystem

# -----------------------------------------------------------------------------

def build_runtime(phase):
    path = Path('src/gc')

    buildsystem.copy_hpps_to_rtl(
        fbuild.buildroot / 'config/target/flx_gc_config.hpp',
        path / 'flx_gc.hpp',
        path / 'flx_collector.hpp',
        path / 'flx_gc_private.hpp',
        path / 'flx_ts_collector.hpp',
    )

    dst = fbuild.buildroot / 'lib/rtl/flx_gc'
    srcs = [path / '*.cpp']
    includes = [
        fbuild.buildroot / 'config/target',
        'src/rtl',
        'src/pthread',
        'src/exceptions',
        'src/judy',
    ]
    macros = ['BUILD_GC']
    libs = [
        fbuild.env.run('buildsystem.judy.build_runtime', phase),
        fbuild.env.run('buildsystem.flx_exceptions.build_runtime', phase),
        fbuild.env.run('buildsystem.flx_pthread.build_runtime', phase),
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
