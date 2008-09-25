import fbuild
import fbuild.packages.cxx as cxx

def build(env, phase):
    path = fbuild.Path('src/exceptions')
    return cxx.SharedLibrary(path / 'flx_exceptions',
        srcs=[path / 'flx_exceptions.cpp'],
        includes=[fbuild.buildroot / 'config/target'],
        macros=['BUILD_EXCEPTIONS'],
        builder=phase.cxx)
