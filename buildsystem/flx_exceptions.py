import fbuild
import fbuild.packages
import fbuild.packages.cxx as cxx
from fbuild.path import Path

# -----------------------------------------------------------------------------

def build_runtime(phase):
    path = Path('src/exceptions')

    for hpp in (
            fbuild.buildroot / 'config/target/flx_exceptions_config.hpp',
            path / 'flx_exceptions.hpp'):
        fbuild.packages.Copy(fbuild.buildroot / 'lib/rtl', hpp).build()

    return cxx.SharedLibrary(fbuild.buildroot / 'lib/rtl/flx_exceptions_dynamic',
        srcs=[path / 'flx_exceptions.cpp'],
        includes=[fbuild.buildroot / 'config/target'],
        macros=['BUILD_EXCEPTIONS'],
        builder=phase.cxx)
