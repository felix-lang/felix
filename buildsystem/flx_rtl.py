import fbuild
import fbuild.packages
import fbuild.packages.cxx as cxx
from fbuild.path import Path

# -----------------------------------------------------------------------------

def build_runtime(phase):
    path = Path('src', 'rtl')

    for hpp in (
            fbuild.buildroot / 'config/target/flx_rtl_config.hpp',
            fbuild.buildroot / 'config/target/flx_meta.hpp',
            path / 'flx_rtl.hpp',
            path / 'flx_compiler_support_headers.hpp',
            path / 'flx_compiler_support_bodies.hpp',
            path / 'flx_dynlink.hpp',
            path / 'flx_i18n.hpp',
            path / 'flx_ioutil.hpp',
            path / 'flx_strutil.hpp',
            path / 'flx_executil.hpp'):
        fbuild.packages.Copy(fbuild.buildroot / 'lib/rtl', hpp).build()

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
