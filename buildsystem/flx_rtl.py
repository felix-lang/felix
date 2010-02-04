import fbuild
from fbuild.functools import call
from fbuild.path import Path
from fbuild.record import Record

import buildsystem

# ------------------------------------------------------------------------------

def build_runtime(phase):
    path = Path('src', 'rtl')

    buildsystem.copy_hpps_to_rtl(phase.ctx,
        phase.ctx.buildroot / 'config/target/flx_rtl_config.hpp',
        phase.ctx.buildroot / 'config/target/flx_meta.hpp',
        path / 'flx_rtl.hpp',
        path / 'flx_compiler_support_headers.hpp',
        path / 'flx_compiler_support_bodies.hpp',
        path / 'flx_dynlink.hpp',
        path / 'flx_i18n.hpp',
        path / 'flx_ioutil.hpp',
        path / 'flx_strutil.hpp',
        path / 'flx_executil.hpp',
    )

    dst = 'lib/rtl/flx'
    srcs = Path.glob(path / '*.cpp')
    includes = [
        phase.ctx.buildroot / 'config/target',
        'src/exceptions',
        'src/demux',
        'src/faio',
        'src/gc',
        'src/pthread',
    ]
    macros = ['BUILD_RTL']
    libs = [
        call('buildsystem.flx_async.build_runtime', phase),
        call('buildsystem.flx_exceptions.build_runtime', phase),
        call('buildsystem.flx_gc.build_runtime', phase),
    ]

    dlfcn_h = call('fbuild.config.c.posix.dlfcn_h',
        phase.cxx.static,
        phase.cxx.shared)

    if dlfcn_h.dlopen:
        external_libs = dlfcn_h.external_libs
    else:
        external_libs = []

    return Record(
        static=buildsystem.build_cxx_static_lib(phase, dst, srcs,
            includes=includes,
            macros=macros,
            libs=[lib.static for lib in libs],
            external_libs=external_libs),
        shared=buildsystem.build_cxx_shared_lib(phase, dst, srcs,
            includes=includes,
            macros=macros,
            libs=[lib.shared for lib in libs],
            external_libs=external_libs))
