import fbuild
from fbuild.path import Path

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

    return phase.cxx.shared.build_lib(
        dst=fbuild.buildroot / 'lib/rtl/flx_gc_dynamic',
        srcs=[path / '*.cpp'],
        includes=[
            fbuild.buildroot / 'config/target',
            'src/rtl',
            'src/pthread',
            'src/exceptions',
            'src/judy',
        ],
        libs=[
            fbuild.env.run('buildsystem.judy.build_runtime', phase),
            fbuild.env.run('buildsystem.flx_exceptions.build_runtime', phase),
            fbuild.env.run('buildsystem.flx_pthread.build_runtime', phase),
        ],
        macros=['BUILD_GC'],
    )
