import fbuild

# -----------------------------------------------------------------------------

def build_runtime(phase):
    return phase.cxx.shared.build_lib(
        dst=fbuild.buildroot / 'lib/rtl/flx_async_dynamic',
        srcs=['src/rtl/flx_async.cpp'],
        includes=[
            fbuild.buildroot / 'config/target',
            'src/exceptions',
            'src/demux',
            'src/faio',
            'src/gc',
            'src/pthread',
        ],
        macros=['BUILD_ASYNC'],
        libs=[
            fbuild.env.run('buildsystem.flx_pthread.build_runtime', phase),
            fbuild.env.run('buildsystem.faio.build_runtime', phase),
        ],
    )
