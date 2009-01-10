from itertools import chain
from optparse import make_option

import fbuild
import fbuild.db
from fbuild.functools import call
from fbuild.path import Path
from fbuild.record import Record

# ------------------------------------------------------------------------------

def pre_options(parser):
    group = parser.add_option_group('config options')
    group.add_options((
        make_option('--prefix', default='/usr/local'),
        make_option('--build-platform'),
        make_option('--build-cc'),
        make_option('--build-cxx'),
        make_option('--host-platform'),
        make_option('--host-cc'),
        make_option('--host-cxx'),
        make_option('--target-platform'),
        make_option('--target-cc'),
        make_option('--target-cxx'),
        make_option('--ocamlc'),
        make_option('--ocamlopt'),
        make_option('--ocamllex'),
        make_option('--ocamlyacc'),
    ))

# ------------------------------------------------------------------------------

def make_c_builder(*args, **kwargs):
    return call('fbuild.builders.c.guess.config', *args, **kwargs)

def make_cxx_builder(*args, **kwargs):
    return call('fbuild.builders.cxx.guess.config', *args, **kwargs)

def config_build(*, platform, cc, cxx):
    fbuild.logger.log('configuring build phase', color='cyan')

    return Record(
        platform=call('fbuild.builders.platform.config', platform),
        c=make_c_builder(exe=cc),
        cxx=make_cxx_builder(exe=cxx),
    )

def config_host(build, *,
        platform, cc, cxx, ocamlc, ocamlopt, ocamllex, ocamlyacc):
    fbuild.logger.log('configuring host phase', color='cyan')

    platform = call('fbuild.builders.platform.config', platform)

    if platform == build.platform:
        fbuild.logger.log("using build's c and cxx compiler", color='cyan')
        phase = build
    else:
        phase = Record(
            platform=platform,
            c=make_c_builder(exe=cc),
            cxx=make_cxx_builder(exe=cxx))

    phase.ocaml = call('fbuild.builders.ocaml.config',
        ocamlc=ocamlc,
        ocamlopt=ocamlopt,
        ocamllex=ocamllex,
        ocamlyacc=ocamlyacc)

    # we prefer the native ocaml as it's much faster
    if hasattr(phase.ocaml.ocaml, 'native'):
        phase.ocaml.ocaml = phase.ocaml.ocaml.native
    else:
        phase.ocaml.ocaml = phase.ocaml.ocaml.bytecode

    return phase

def config_target(host, *, platform, cc, cxx):
    fbuild.logger.log('configuring target phase', color='cyan')

    platform = call('fbuild.builders.platform.config', platform)

    if platform == host.platform:
        fbuild.logger.log("using host's c and cxx compiler", color='cyan')
        phase = host
    else:
        phase = Record(
            platform=platform,
            c=make_c_builder(exe=cc),
            cxx=make_cxx_builder(exe=cxx))

    return phase

# ------------------------------------------------------------------------------

@fbuild.db.caches
def prefix():
    prefix = Path(fbuild.options.prefix)
    fbuild.logger.check('install prefix', prefix, color='cyan')

    return prefix

@fbuild.db.caches
def src_dir():
    return Path(__file__).parent

def build():
    # configure the phases
    build = config_build(
        platform=fbuild.options.build_platform,
        cc=fbuild.options.build_cc,
        cxx=fbuild.options.build_cxx)

    host = config_host(build,
        platform=fbuild.options.host_platform,
        cc=fbuild.options.host_cc,
        cxx=fbuild.options.host_cxx,
        ocamlc=fbuild.options.ocamlc,
        ocamlopt=fbuild.options.ocamlopt,
        ocamllex=fbuild.options.ocamllex,
        ocamlyacc=fbuild.options.ocamlyacc)

    target = config_target(host,
        platform=fbuild.options.target_platform,
        cc=fbuild.options.target_cc,
        cxx=fbuild.options.target_cxx)

    # extract the configuration
    iscr = call('buildsystem.iscr.config_iscr')
    iscr('lpsrc/flx_config.pak')

    # convert the config into something iscr can use
    call('buildsystem.iscr.config_iscr_config', build, host, target)

    # re-extract packages if any of them changed
    fbuild.scheduler.map(iscr, (src_dir()/'lpsrc/*.pak').glob())

    # --------------------------------------------------------------------------

    compilers = call('buildsystem.flx_compiler.build_flx_drivers',
        host.ocaml)

    drivers = call('buildsystem.flx_drivers.build', target)

    flx = call('buildsystem.flx.build',
        compilers.flxg, target.cxx.shared, drivers)

    # copy files into the library
    for module in 'flx_pthread', 'demux', 'faio', 'judy':
        call('buildsystem.' + module + '.build_flx', flx)

    flx_pkgconfig = call('buildsystem.flx.build_flx_pkgconfig',
        flx, target)

    # --------------------------------------------------------------------------
    # build the secondary libraries

    call('buildsystem.elk.build_exe', host)
    call('buildsystem.elk.build_runtime', target)
    call('buildsystem.flx_async.build_runtime', target)
    call('buildsystem.flx_glob.build_runtime', target)
    call('buildsystem.tre.build_runtime', target)

    for module in 'flx_glob', 'tre':
        call('buildsystem.' + module + '.build_flx', flx)

    # --------------------------------------------------------------------------
    # now, try building a file

    felix = call('fbuild.builders.felix.config',
        exe=fbuild.buildroot / 'bin/flx',
        flags=['--test=' + fbuild.buildroot])

    from buildsystem.flx import test_flxs

    test_flxs(felix, chain(
        Path.glob('test/*/*.flx', exclude=[
            'test/drivers/*.flx',
            'test/faio/win-*.flx',
        ]),
        Path.glob('test/*/*/*.flx', exclude=[
            'test/regress/bt/*.flx',
            'test/regress/kf/*.flx',
            'test/regress/nd/*.flx',
        ]),
        Path.glob(fbuild.buildroot / 'tut/*/*.flx'),
    ))
