import fbuild
import fbuild.packages.cxx as cxx
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
        fbuild.env.run('buildsystem.flx_exceptions.build_runtime', phase),
        fbuild.env.run('buildsystem.flx_gc.build_runtime', phase),
        fbuild.env.run('buildsystem.flx_rtl.build_runtime', phase),
        fbuild.env.run('buildsystem.flx_pthread.build_runtime', phase),
        fbuild.env.run('buildsystem.judy.build_runtime', phase),
    ]

    arun_libs = run_libs + [
        fbuild.env.run('buildsystem.demux.build_runtime', phase),
        fbuild.env.run('buildsystem.faio.build_runtime', phase),
    ]

    flx_run_lib = cxx.SharedLibrary(fbuild.buildroot / 'lib/rtl/flx_run',
        srcs=[path / 'flx_run.cxx'],
        includes=run_includes,
        macros=['FLX_STATIC_LINK'],
        libs=run_libs,
        builder=phase.cxx)

    flx_run_exe = cxx.Executable(fbuild.buildroot / 'bin/flx_run',
        srcs=[path / 'flx_run.cxx'],
        includes=run_includes,
        libs=run_libs,
        builder=phase.cxx)

    flx_arun_lib = cxx.SharedLibrary(fbuild.buildroot / 'lib/rtl/flx_arun',
        srcs=[path / 'flx_arun.cxx'],
        includes=arun_includes,
        macros=['FLX_STATIC_LINK'],
        libs=run_libs,
        builder=phase.cxx)

    flx_arun_exe = cxx.Executable(fbuild.buildroot / 'bin/flx_arun',
        srcs=[path / 'flx_arun.cxx'],
        includes=arun_includes,
        libs=arun_libs,
        builder=phase.cxx)

    return Record(
        flx_run_lib=flx_run_lib,
        flx_run_exe=flx_run_exe,
        flx_arun_lib=flx_arun_lib,
        flx_arun_exe=flx_arun_exe,
    )
