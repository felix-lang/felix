import fbuild
from fbuild.path import Path

import buildsystem

# -----------------------------------------------------------------------------

def build_runtime(phase):
    path = Path('src/unixem')

    buildsystem.copy_hpps_to_rtl(
        fbuild.buildroot / 'config/target/flx_glob_config.hpp',
        path / 'flx_glob.hpp',
    )

    srcs = []
    if 'win32' in phase.platform:
        srcs.extend((path / 'flx_glob.cpp', path / 'unixem_util.cpp'))

    return phase.cxx.shared.build_lib(
        dst=fbuild.buildroot / 'lib/rtl/flx_glob_dynamic',
        srcs=srcs,
        macros=['BUILD_GLOB'],
        libs=[fbuild.env.run('buildsystem.flx_gc.build_runtime', phase)],
    )



def build_flx(builder):
    return buildsystem.copy_flxs_to_lib(Path.glob('src/unixem/*.flx'))
