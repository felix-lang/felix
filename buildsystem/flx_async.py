import fbuild
import fbuild.packages.cxx as cxx

def build(env, phase):
    path = fbuild.Path('src/rtl')

    return cxx.SharedLibrary(path / 'flx_async',
        srcs=[path / 'flx_async.cpp'],
        includes=[
            fbuild.buildroot / 'config/target',
            fbuild.Path('src/exceptions'),
            fbuild.Path('src/demux'),
            fbuild.Path('src/faio'),
            fbuild.Path('src/gc'),
            fbuild.Path('src/pthread'),
        ],
        libs=[
            env.config('buildsystem.flx_pthread.build', phase),
            env.config('buildsystem.faio.build', phase),
        ],
        macros=['BUILD_ASYNC'],
        builder=phase.cxx)
