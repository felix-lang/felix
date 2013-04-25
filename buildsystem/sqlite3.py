import fbuild
from fbuild.functools import call
from fbuild.path import Path
from fbuild.record import Record

import buildsystem

# ------------------------------------------------------------------------------

def build_runtime(host_phase, target_phase):
    path = Path('src/sqlite3')
    buildsystem.copy_hpps_to_rtl(target_phase.ctx,
        path / 'flx_sqlite3.hpp',
    )


    buildsystem.copy_to(target_phase.ctx, target_phase.ctx.buildroot / "share/lib/rtl", [
        path / 'sqlite3.h',
        path / 'sqlite3ext.h',
        ]
     )

    dst = 'host/lib/rtl/flx_sqlite3'
    srcs = [
        path / 'sqlite3.c',
     ]
    includes = [
      target_phase.ctx.buildroot / 'host/lib/rtl',
      target_phase.ctx.buildroot / 'share/lib/rtl',
      ]
    macros = ['BUILD_SQLITE3']
    cflags = ([], ['-Wno-sign-compare'])[not 'win32' in target_phase.platform]
    lflags = []
    libs = []
    external_libs = []

    return Record(
        static=buildsystem.build_c_static_lib(target_phase, dst, srcs,
            includes=includes,
            macros=macros,
            cflags=cflags,
            libs=libs,
            external_libs=external_libs,
            lflags=lflags),
        shared=buildsystem.build_c_shared_lib(target_phase, dst, srcs,
            includes=includes,
            macros=macros,
            cflags=cflags,
            libs=libs,
            external_libs=external_libs,
            lflags=lflags))
