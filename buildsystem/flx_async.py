import fbuild
import fbuild.packages.cxx as cxx

# -----------------------------------------------------------------------------

def build(phase):
    return cxx.SharedLibrary(fbuild.buildroot / 'lib/rtl/flx_async_dynamic',
        srcs=['src/rtl/flx_async.cpp'],
        includes=[
            fbuild.buildroot / 'config/target',
            'src/exceptions',
            'src/demux',
            'src/faio',
            'src/gc',
            'src/pthread',
        ],
        libs=[
            fbuild.env.run('buildsystem.flx_pthread.build_runtime', phase),
            fbuild.env.run('buildsystem.faio.build_runtime', phase),
        ],
        macros=['BUILD_ASYNC'],
        builder=phase.cxx)
