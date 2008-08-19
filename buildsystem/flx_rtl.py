import fbuild
import fbuild.packages.cxx as cxx

def build(env, phase):
    path = fbuild.Path('src', 'rtl')
    return cxx.SharedLibrary(path / 'flx_rtl', [path / '*.cpp'],
        includes=[
            fbuild.buildroot / 'config' / 'target',
            fbuild.Path('src/exceptions'),
            fbuild.Path('src/demux'),
            fbuild.Path('src/faio'),
            fbuild.Path('src/gc'),
            fbuild.Path('src/pthread'),
        ],
        libs=[
            env.config('buildsystem.demux.build', phase),
            env.config('buildsystem.faio.build', phase),
            env.config('buildsystem.flx_exceptions.build', phase),
            env.config('buildsystem.flx_gc.build', phase),
            env.config('buildsystem.flx_pthread.build', phase),
        ],
        cflags={'macros': ['BUILD_EXCEPTIONS']},
        builder=phase.cxx)
