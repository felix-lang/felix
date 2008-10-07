import fbuild
from fbuild.path import Path

import buildsystem

# -----------------------------------------------------------------------------

def build_runtime(phase):
    path = Path('src/tre')
    buildsystem.copy_hpps_to_rtl(
        fbuild.buildroot / 'config/target/flx_target_tre_config.h',
        path / 'tre-regex.h',
        path / 'tre-config.h',
    )

    return phase.c.shared.build_lib(
        dst=fbuild.buildroot / 'lib/rtl/tre_dynamic',
        srcs=['src/tre/*.c'],
        includes=[fbuild.buildroot / 'config/target'],
    )

def build_flx(builder):
    return buildsystem.copy_flxs_to_lib(Path('src/tre/*.flx').glob())
