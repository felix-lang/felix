import fbuild
from fbuild.path import Path
from fbuild.record import Record

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

    dst = fbuild.buildroot / 'lib/rtl/flx'
    srcs = [path / '*.cpp']
    includes = [
        fbuild.buildroot / 'config/target',
        'src/exceptions',
        'src/demux',
        'src/faio',
        'src/gc',
        'src/pthread',
    ]
    macros = ['BUILD_EXCEPTIONS']
    libs = [
        fbuild.env.run('buildsystem.demux.build_runtime', phase),
        fbuild.env.run('buildsystem.faio.build_runtime', phase),
        fbuild.env.run('buildsystem.flx_exceptions.build_runtime', phase),
        fbuild.env.run('buildsystem.flx_gc.build_runtime', phase),
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
