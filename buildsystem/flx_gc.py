import fbuild
import fbuild.packages.cxx as cxx

def build(env, phase):
    path = fbuild.Path('src', 'gc')
    return cxx.SharedLibrary(path / 'flx_gc', [path / '*.cpp'],
        includes=[
            fbuild.Path('src', 'rtl'),
            fbuild.Path('src', 'pthread'),
            fbuild.Path('src', 'exceptions'),
            fbuild.Path('src', 'judy'),
            fbuild.buildroot / 'config/target',
        ],
        libs=[
            env.config('buildsystem.judy.build', phase),
            env.config('buildsystem.flx_exceptions.build', phase),
            env.config('buildsystem.flx_pthread.build', phase),
        ],
        macros=['BUILD_EXCEPTIONS'],
        builder=phase.cxx)
