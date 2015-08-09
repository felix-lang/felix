import fbuild
from fbuild.functools import call
from fbuild.path import Path
from fbuild.record import Record
from fbuild.builders.file import copy

import buildsystem

# ------------------------------------------------------------------------------

def build_runtime(host_phase, target_phase):
    path = Path(target_phase.ctx.buildroot/'share'/'src/gc')
    dst = 'host/lib/rtl/flx_gc'
    srcs = Path.glob(path / '*.cpp')
    includes = [
        target_phase.ctx.buildroot / 'host/lib/rtl',
        target_phase.ctx.buildroot / 'share/lib/rtl',
    ]
    macros = ['BUILD_FLX_GC']
    libs = [
        call('buildsystem.judy.build_runtime', host_phase, target_phase),
        call('buildsystem.flx_exceptions.build_runtime', target_phase),
        call('buildsystem.flx_pthread.build_runtime', target_phase),
    ]

    return Record(
        static=buildsystem.build_cxx_static_lib(target_phase, dst, srcs,
            includes=includes,
            macros=macros,
            libs=[lib.static for lib in libs]),
        shared=buildsystem.build_cxx_shared_lib(target_phase, dst, srcs,
            includes=includes,
            macros=macros,
            libs=[lib.shared for lib in libs]))
