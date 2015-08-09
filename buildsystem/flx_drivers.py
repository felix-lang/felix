import fbuild
from fbuild.functools import call
from fbuild.path import Path
from fbuild.record import Record
import buildsystem

# ------------------------------------------------------------------------------

def build(host_phase, target_phase):
    path = Path(target_phase.ctx.buildroot/'share'/'src/flx_drivers')

    run_includes = [
        target_phase.ctx.buildroot / 'host/lib/rtl',
        target_phase.ctx.buildroot / 'share/lib/rtl'
    ]

    arun_includes = run_includes + [
        'src/demux',
    ] + ([], ['src/demux/win'])['win32' in target_phase.platform]

    flx_run_static_obj = target_phase.cxx.static.compile(
        dst='host/lib/rtl/flx_run_lib',
        src=path / 'flx_run_lib_static.cpp',
        includes=run_includes,
    )

    flx_run_static_main= target_phase.cxx.static.compile(
        dst='host/lib/rtl/flx_run_main',
        src=path / 'flx_run_main.cxx',
        includes=run_includes,
    )

    flx_run_exe = target_phase.cxx.shared.build_exe(
        dst='host/bin/flx_run',
        srcs=[path / 'flx_run_main.cxx', path / 'flx_run_lib_dynamic.cpp'],
        includes=run_includes,
        libs=[call('buildsystem.flx_rtl.build_runtime', host_phase, target_phase).shared],
    )

    flx_arun_static_obj = target_phase.cxx.static.compile(
        dst='host/lib/rtl/flx_arun_lib',
        src=path / 'flx_arun_lib_static.cpp',
        includes=arun_includes,
    )

    flx_arun_static_main= target_phase.cxx.static.compile(
        dst='host/lib/rtl/flx_arun_main',
        src=path / 'flx_arun_main.cxx',
        includes=arun_includes,
    )

    flx_arun_exe = target_phase.cxx.shared.build_exe(
        dst='host/bin/flx_arun',
        srcs=[path / 'flx_arun_main.cxx', path/ 'flx_arun_lib_dynamic.cpp'],
        includes=arun_includes,
        libs=[
           call('buildsystem.flx_rtl.build_runtime', host_phase, target_phase).shared,
           call('buildsystem.flx_pthread.build_runtime', target_phase).shared,
           call('buildsystem.flx_async.build_runtime', host_phase,target_phase).shared,
           call('buildsystem.demux.build_runtime', target_phase).shared,
           call('buildsystem.faio.build_runtime', host_phase,target_phase).shared],
    )

    return Record(
        flx_run_lib=flx_run_static_obj,
        flx_run_main=flx_run_static_main,
        flx_run_exe=flx_run_exe,
        flx_arun_lib=flx_arun_static_obj,
        flx_arun_main=flx_arun_static_main,
        flx_arun_exe=flx_arun_exe,
    )
