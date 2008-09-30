import fbuild
import fbuild.packages.cxx as cxx
from fbuild.path import Path

# -----------------------------------------------------------------------------

def build_runtime(phase):
    path = Path('src', 'rtl')

    return cxx.SharedLibrary(fbuild.buildroot / 'lib/rtl/flx_dynamic',
        [path / '*.cpp'],
        includes=[
            fbuild.buildroot / 'config/target',
            'src/exceptions',
            'src/demux',
            'src/faio',
            'src/gc',
            'src/pthread',
        ],
        libs=[
            fbuild.env.run('buildsystem.demux.build_runtime', phase),
            fbuild.env.run('buildsystem.faio.build_runtime', phase),
            fbuild.env.run('buildsystem.flx_exceptions.build_runtime', phase),
            fbuild.env.run('buildsystem.flx_gc.build_runtime', phase),
            fbuild.env.run('buildsystem.flx_pthread.build_runtime', phase),
        ],
        macros=['BUILD_EXCEPTIONS'],
        builder=phase.cxx)
