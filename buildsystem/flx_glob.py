import fbuild
from fbuild.path import Path
from fbuild.record import Record

import buildsystem

# ------------------------------------------------------------------------------

def build_runtime(phase):
    path = Path('src/unixem')

    buildsystem.copy_hpps_to_rtl(
        fbuild.buildroot / 'config/target/flx_glob_config.hpp',
        path / 'flx_glob.hpp',
    )

    dst = fbuild.buildroot / 'lib/rtl/flx_glob'
    srcs = []
    macros = ['BUILD_GLOB']
    libs = [fbuild.env.run('buildsystem.flx_gc.build_runtime', phase)]

    if 'win32' in phase.platform:
        srcs.extend((path / 'flx_glob.cpp', path / 'unixem_util.cpp'))

    return Record(
        #static=phase.cxx.static.build_lib(dst + '_static', srcs,
        #    macros=macros + ['FLX_STATIC_LINK'],
        #    libs=[lib.static for lib in libs]),
        shared=phase.cxx.shared.build_lib(dst + '_dynamic', srcs,
            macros=macros,
            libs=[lib.shared for lib in libs]))

def build_flx(builder):
    return buildsystem.copy_flxs_to_lib(Path.glob('src/unixem/*.flx'))
