import fbuild
from fbuild.path import Path
from fbuild.record import Record

import buildsystem

# ------------------------------------------------------------------------------

def build_runtime(phase):
    path = Path('src/exceptions')

    buildsystem.copy_hpps_to_rtl(
        fbuild.buildroot / 'config/target/flx_exceptions_config.hpp',
        path / 'flx_exceptions.hpp',
    )

    dst = fbuild.buildroot / 'lib/rtl/flx_exceptions'
    srcs = [path / 'flx_exceptions.cpp']
    includes = [fbuild.buildroot / 'config/target']
    macros = ['BUILD_FLX_EXCEPTIONS']

    return Record(
        static=buildsystem.build_cxx_static_lib(phase, dst, srcs,
            includes=includes,
            macros=macros),
        shared=buildsystem.build_cxx_shared_lib(phase, dst, srcs,
            includes=includes,
            macros=macros))
