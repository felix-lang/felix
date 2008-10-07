import fbuild
from fbuild.path import Path

import buildsystem

# -----------------------------------------------------------------------------

def build_runtime(phase):
    path = Path('src/faio')

    buildsystem.copy_hpps_to_rtl(
        fbuild.buildroot / 'config/target/flx_faio_config.hpp',
        path / 'faio_asyncio.hpp',
        path / 'faio_job.hpp',
        path / 'faio_timer.hpp',
        path / 'faio_posixio.hpp',
        path / 'faio_winio.hpp',
    )

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

    return phase.cxx.shared.build_lib(
        dst=fbuild.buildroot / 'lib/rtl/faio_dynamic',
        srcs=srcs,
        includes=includes,
        libs=[
            fbuild.env.run('buildsystem.flx_pthread.build_runtime', phase).shared,
            fbuild.env.run('buildsystem.demux.build_runtime', phase).shared,
        ],
        macros=['BUILD_FAIO'],
    )

def build_flx(builder):
    return buildsystem.copy_flxs_to_lib(Path('src/faio/*.flx').glob())
