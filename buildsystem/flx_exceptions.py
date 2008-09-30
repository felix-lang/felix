import fbuild
import fbuild.packages.cxx as cxx
from fbuild.path import Path

# -----------------------------------------------------------------------------

def build_runtime(phase):
    path = Path('src/exceptions')

    return cxx.SharedLibrary(fbuild.buildroot / 'lib/rtl/flx_exceptions_dynamic',
        srcs=[path / 'flx_exceptions.cpp'],
        includes=[fbuild.buildroot / 'config/target'],
        macros=['BUILD_EXCEPTIONS'],
        builder=phase.cxx)
