import fbuild
import fbuild.packages.cxx as cxx
from fbuild.path import Path

# -----------------------------------------------------------------------------

def build_runtime(phase):
    path = Path('src', 'gc')

    return cxx.SharedLibrary(fbuild.buildroot / 'lib/rtl/flx_gc_dynamic',
        [path / '*.cpp'],
        includes=[
            fbuild.buildroot / 'config/target',
            'src/rtl',
            'src/pthread',
            'src/exceptions',
            'src/judy',
        ],
        libs=[
            fbuild.env.run('buildsystem.judy.build_runtime', phase),
            fbuild.env.run('buildsystem.flx_exceptions.build_runtime', phase),
            fbuild.env.run('buildsystem.flx_pthread.build_runtime', phase),
        ],
        macros=['BUILD_EXCEPTIONS'],
        builder=phase.cxx)
