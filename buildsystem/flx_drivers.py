import fbuild
from fbuild.path import Path
from fbuild.record import Record

# -----------------------------------------------------------------------------

def build(phase):
    path = Path('src/flx_drivers')

    run_includes = [
        fbuild.buildroot / 'config/target',
        'src/exceptions',
        'src/gc',
        'src/judy',
        'src/pthread',
        'src/rtl',
    ]

    arun_includes = run_includes + [
        'src/demux',
        'src/faio',
    ]

    run_libs = [
        fbuild.env.run('buildsystem.flx_exceptions.build_runtime', phase).shared,
        fbuild.env.run('buildsystem.flx_gc.build_runtime', phase).shared,
        fbuild.env.run('buildsystem.flx_rtl.build_runtime', phase),
        fbuild.env.run('buildsystem.flx_pthread.build_runtime', phase).shared,
        fbuild.env.run('buildsystem.judy.build_runtime', phase).shared,
    ]

    arun_libs = run_libs + [
        fbuild.env.run('buildsystem.demux.build_runtime', phase).shared,
        fbuild.env.run('buildsystem.faio.build_runtime', phase),
    ]

    flx_run_lib = phase.cxx.static.compile(
        dst=fbuild.buildroot / 'lib/rtl/flx_run',
        src=path / 'flx_run.cxx',
        includes=run_includes,
        macros=['FLX_STATIC_LINK'],
    )

    flx_run_exe = phase.cxx.shared.build_exe(
        dst=fbuild.buildroot / 'bin/flx_run',
        srcs=[path / 'flx_run.cxx'],
        includes=run_includes,
        libs=run_libs,
    )

    flx_arun_lib = phase.cxx.static.compile(
        dst=fbuild.buildroot / 'lib/rtl/flx_arun',
        src=path / 'flx_arun.cxx',
        includes=arun_includes,
        macros=['FLX_STATIC_LINK'],
    )

    flx_arun_exe = phase.cxx.shared.build_exe(
        dst=fbuild.buildroot / 'bin/flx_arun',
        srcs=[path / 'flx_arun.cxx'],
        includes=arun_includes,
        libs=arun_libs,
    )

    return Record(
        flx_run_lib=flx_run_lib,
        flx_run_exe=flx_run_exe,
        flx_arun_lib=flx_arun_lib,
        flx_arun_exe=flx_arun_exe,
    )
