import fbuild
from fbuild.path import Path

import buildsystem

# -----------------------------------------------------------------------------

def build_runtime(phase):
    path = Path('src', 'rtl')

    buildsystem.copy_hpps_to_rtl(
        fbuild.buildroot / 'config/target/flx_rtl_config.hpp',
        fbuild.buildroot / 'config/target/flx_meta.hpp',
        path / 'flx_rtl.hpp',
        path / 'flx_compiler_support_headers.hpp',
        path / 'flx_compiler_support_bodies.hpp',
        path / 'flx_dynlink.hpp',
        path / 'flx_i18n.hpp',
        path / 'flx_ioutil.hpp',
        path / 'flx_strutil.hpp',
        path / 'flx_executil.hpp',
    )

    return phase.cxx.shared.build_lib(
        dst=fbuild.buildroot / 'lib/rtl/flx_dynamic',
        srcs=[path / '*.cpp'],
        includes=[
            fbuild.buildroot / 'config/target',
            'src/exceptions',
            'src/demux',
            'src/faio',
            'src/gc',
            'src/pthread',
        ],
        libs=[
            fbuild.env.run('buildsystem.demux.build_runtime', phase),
            fbuild.env.run('buildsystem.faio.build_runtime', phase),
            fbuild.env.run('buildsystem.flx_exceptions.build_runtime', phase).shared,
            fbuild.env.run('buildsystem.flx_gc.build_runtime', phase).shared,
            fbuild.env.run('buildsystem.flx_pthread.build_runtime',
                phase).shared,
        ],
        macros=['BUILD_EXCEPTIONS'],
    )
