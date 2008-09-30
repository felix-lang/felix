import fbuild
import fbuild.packages
import fbuild.packages.cxx as cxx
from fbuild.path import Path

import buildsystem.flx as flx

# -----------------------------------------------------------------------------

def build_runtime(phase):
    path = Path('src', 'faio')

    for hpp in (
            fbuild.buildroot / 'config/target/flx_faio_config.hpp',
            path / 'faio_asyncio.hpp',
            path / 'faio_job.hpp',
            path / 'faio_timer.hpp',
            path / 'faio_posixio.hpp',
            path / 'faio_winio.hpp'):
        fbuild.packages.Copy(fbuild.buildroot / 'lib/rtl', hpp).build()

    srcs = [
        path / 'faio_asyncio.cpp',
        path / 'faio_job.cpp',
        path / 'faio_timer.cpp',
    ]
    includes = [
        fbuild.buildroot / 'config/target',
        Path('src', 'pthread'),
        Path('src', 'demux'),
        Path('src', 'rtl'),
        Path('src', 'exceptions'),
        Path('src', 'gc'),
        path,
    ]

    if 'win32' in phase.platform:
        srcs.append(path / 'faio_winio.cpp')
        includes.append(Path('src', 'demux', 'win'))

    if 'posix' in phase.platform:
        srcs.append(path / 'faio_posixio.cpp')
        includes.append(Path('src', 'demux', 'posix'))

    return cxx.SharedLibrary(fbuild.buildroot / 'lib/rtl/faio_dynamic',
        srcs,
        includes=includes,
        libs=[
            fbuild.env.run('buildsystem.flx_pthread.build_runtime', phase),
            fbuild.env.run('buildsystem.demux.build_runtime', phase),
        ],
        macros=['BUILD_FAIO'],
        builder=phase.cxx)

def build_flx(builder):
    return flx.copy_flxs_to_lib(builder,
        Path('src/faio/*.flx').glob())
