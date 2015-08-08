import fbuild
from fbuild.path import Path
from fbuild.record import Record
from fbuild.builders.file import copy

import buildsystem

# ------------------------------------------------------------------------------

def build_runtime(phase):
    print('[fbuild] [rtl] build exceptions')
    path = Path(phase.ctx.buildroot/'share'/'src/exceptions')

    buildsystem.copy_hpps_to_rtl(phase.ctx,
        path / 'flx_exceptions.hpp',
        path / 'flx_eh.hpp',
    )

    srcs = [copy(ctx=phase.ctx, src=f, dst=phase.ctx.buildroot / f) for f in[
     path / 'flx_exceptions.cpp',
     path / 'flx_eh.cpp',
     ]]
    includes = ['src/rtl', phase.ctx.buildroot / 'host/lib/rtl', phase.ctx.buildroot / 'share/lib/rtl']
    macros = ['BUILD_FLX_EXCEPTIONS']

    dst = 'host/lib/rtl/flx_exceptions'
    return Record(
        static=buildsystem.build_cxx_static_lib(phase, dst, srcs,
            includes=includes,
            macros=macros),
        shared=buildsystem.build_cxx_shared_lib(phase, dst, srcs,
            includes=includes,
            macros=macros))
