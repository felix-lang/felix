import fbuild
from fbuild.path import Path
from fbuild.record import Record

import buildsystem

# ------------------------------------------------------------------------------

def build_runtime(phase):
    path = Path('src/tre')
    buildsystem.copy_hpps_to_rtl(
        fbuild.buildroot / 'config/target/flx_target_tre_config.h',
        path / 'tre-regex.h',
        path / 'tre-config.h',
    )

    dst = fbuild.buildroot / 'lib/rtl/tre'
    srcs = ['src/tre/*.c']
    includes = [fbuild.buildroot / 'config/target']

    return Record(
        static=phase.c.static.build_lib(dst + '_static', srcs,
            includes=includes,
            macros=['FLX_STATIC_LINK']),
        shared=phase.c.shared.build_lib(dst + '_dynamic', srcs,
            includes=includes))

def build_flx(builder):
    return buildsystem.copy_flxs_to_lib(Path('src/tre/*.flx').glob())
