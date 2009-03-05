import fbuild
from fbuild.functools import call
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
    includes = [fbuild.buildroot / 'config/target']
    macros = ['BUILD_GLOB']
    libs = [call('buildsystem.flx_gc.build_runtime', phase)]

    if 'win32' in phase.platform:
        srcs.extend((path / 'flx_glob.cpp', path / 'unixem_util.cpp'))

    return Record(
        #static=buildsystem.build_cxx_static_lib(phase, dst, srcs,
        #    macros=macros,
        #    libs=[lib.static for lib in libs]),
        shared=buildsystem.build_cxx_shared_lib(phase, dst, srcs,
            includes=includes,
            macros=macros,
            libs=[lib.shared for lib in libs]))

def build_flx(builder):
    return buildsystem.copy_flxs_to_lib(Path.glob('src/unixem/*.flx'))
