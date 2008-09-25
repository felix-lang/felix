import fbuild
import fbuild.packages.cxx as cxx

def build(env, phase):
    path = fbuild.Path('src', 'faio')
    srcs = [
        path / 'faio_asyncio.cpp',
        path / 'faio_job.cpp',
        path / 'faio_timer.cpp',
    ]
    includes = [
        fbuild.buildroot / 'config/target',
        fbuild.Path('src', 'pthread'),
        fbuild.Path('src', 'demux'),
        fbuild.Path('src', 'rtl'),
        fbuild.Path('src', 'exceptions'),
        fbuild.Path('src', 'gc'),
        path,
    ]

    if 'win32' in phase.platform:
        srcs.append(path / 'faio_winio.cpp')
        includes.append(fbuild.Path('src', 'demux', 'win'))

    if 'posix' in phase.platform:
        srcs.append(path / 'faio_posixio.cpp')
        includes.append(fbuild.Path('src', 'demux', 'posix'))

    return cxx.SharedLibrary(path / 'faio', srcs,
        includes=includes,
        libs=[
            env.config('buildsystem.flx_pthread.build', phase),
            env.config('buildsystem.demux.build', phase),
        ],
        macros=['BUILD_FAIO'],
        builder=phase.cxx)
