import fbuild
from fbuild.path import Path

import buildsystem

# -----------------------------------------------------------------------------

def build_runtime(phase):
    path = Path('src/exceptions')

    buildsystem.copy_hpps_to_rtl(
        fbuild.buildroot / 'config/target/flx_exceptions_config.hpp',
        path / 'flx_exceptions.hpp',
    )

    return phase.cxx.shared.build_lib(
        dst=fbuild.buildroot / 'lib/rtl/flx_exceptions_dynamic',
        srcs=[path / 'flx_exceptions.cpp'],
        includes=[fbuild.buildroot / 'config/target'],
        macros=['BUILD_EXCEPTIONS'],
    )
