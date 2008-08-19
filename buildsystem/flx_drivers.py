import fbuild
import fbuild.packages.cxx as cxx

def build(env, phase):
    path = fbuild.Path('src/rtl')

    run_includes = [
        fbuild.buildroot / 'config/target',
        fbuild.Path('src/exceptions'),
        fbuild.Path('src/gc'),
        fbuild.Path('src/judy'),
        fbuild.Path('src/pthread'),
    ]

    arun_includes = run_includes + [
        fbuild.Path('src/demux'),
        fbuild.Path('src/faio'),
    ]

    run_libs = [
        env.config('buildsystem.flx_exceptions.build', phase),
        env.config('buildsystem.flx_gc.build', phase),
        env.config('buildsystem.flx_rtl.build', phase),
        env.config('buildsystem.flx_pthread.build', phase),
        env.config('buildsystem.judy.build', phase),
    ]

    arun_libs = run_libs + [
        env.config('buildsystem.demux.build', phase),
        env.config('buildsystem.faio.build', phase),
    ]

    flx_run_lib = cxx.StaticLibrary(path / 'flx_run',
        srcs=[path / 'flx_run.cxx'],
        includes=run_includes,
        cflags={'macros': ['FLX_STATIC_LINK']},
        libs=run_libs,
        builder=phase.cxx)

    flx_run_exe = cxx.Executable(path / 'flx_run',
        srcs=[path / 'flx_run.cxx'],
        includes=run_includes,
        libs=run_libs,
        builder=phase.cxx)

    flx_arun_lib = cxx.StaticLibrary(path / 'flx_arun',
        srcs=[path / 'flx_arun.cxx'],
        includes=arun_includes,
        cflags={'macros': ['FLX_STATIC_LINK']},
        libs=run_libs,
        builder=phase.cxx)

    flx_arun_exe = cxx.Executable(path / 'flx_arun',
        srcs=[path / 'flx_arun.cxx'],
        includes=arun_includes,
        libs=arun_libs,
        builder=phase.cxx)

    return fbuild.Record(
        flx_run=fbuild.Record(lib=flx_run_lib, exe=flx_run_exe),
        flx_arun=fbuild.Record(lib=flx_arun_lib, exe=flx_arun_exe),
    )
