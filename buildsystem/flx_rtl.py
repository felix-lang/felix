import fbuild
from fbuild.functools import call
from fbuild.path import Path
from fbuild.record import Record
from fbuild.builders.file import copy

import buildsystem
from buildsystem.config import config_call

# ------------------------------------------------------------------------------

def build_runtime(host_phase, target_phase):
    path = Path('src', 'rtl')

    buildsystem.copy_hpps_to_rtl(target_phase.ctx,
        path / 'flx_rtl.hpp',
        path / 'flx_rtl_shapes.hpp',
        path / 'flx_compiler_support_headers.hpp',
        path / 'flx_compiler_support_bodies.hpp',
        path / 'flx_dynlink.hpp',
        path / 'flx_i18n.hpp',
        path / 'flx_ioutil.hpp',
        path / 'flx_strutil.hpp',
        path / 'flx_executil.hpp',
        path / 'flx_sync.hpp',
        path / 'flx_async.hpp',
        path / 'flx_world.hpp',
        path / 'flx_async_world.hpp',
        path / 'flx_world_config.hpp',
        path / 'plat_linux.hpp',
    )

    for f in Path.glob(path/"*.hpp"):
      print("Copying " + f + " --> " +target_phase.ctx.buildroot/f )
      copy(ctx=target_phase.ctx, src=f,dst=target_phase.ctx.buildroot/f)

    srcs = [copy(ctx=target_phase.ctx, src=f, dst=target_phase.ctx.buildroot / f) for f in Path.glob(path / '*.cpp')]
    includes = [
        target_phase.ctx.buildroot / 'host/lib/rtl',
        target_phase.ctx.buildroot / 'share/lib/rtl'
    ]
    macros = ['BUILD_RTL']
    libs = [
        call('buildsystem.flx_async.build_runtime', host_phase,target_phase),
        call('buildsystem.flx_exceptions.build_runtime', target_phase),
        call('buildsystem.flx_gc.build_runtime', host_phase, target_phase),
    ]

    dlfcn_h = config_call('fbuild.config.c.posix.dlfcn_h',
        target_phase.platform,
        target_phase.cxx.static,
        target_phase.cxx.shared)

    if dlfcn_h.dlopen:
        external_libs = dlfcn_h.external_libs
    else:
        external_libs = []

    dst = 'host/lib/rtl/flx'
    return Record(
        static=buildsystem.build_cxx_static_lib(target_phase, dst, srcs,
            includes=includes,
            macros=macros,
            libs=[lib.static for lib in libs],
            external_libs=external_libs),
        shared=buildsystem.build_cxx_shared_lib(target_phase, dst, srcs,
            includes=includes,
            macros=macros,
            libs=[lib.shared for lib in libs],
            external_libs=external_libs))
