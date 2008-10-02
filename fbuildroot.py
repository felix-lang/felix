from optparse import make_option

import fbuild
from fbuild.path import Path
from fbuild.record import Record

# -----------------------------------------------------------------------------

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

# -----------------------------------------------------------------------------

def make_c_builder(**kwargs):
    return fbuild.env.cache('fbuild.builders.c.guess.config', **kwargs)

def make_cxx_builder(**kwargs):
    return fbuild.env.cache('fbuild.builders.cxx.guess.config', **kwargs)

def config_build(*, platform, cc, cxx):
    fbuild.logger.log('configuring build phase', color='cyan')

    return Record(
        platform=fbuild.env.cache('fbuild.builders.platform.config', platform),
        c=make_c_builder(exe=cc),
        cxx=make_cxx_builder(exe=cxx),
    )

def config_host(build, *,
        platform, cc, cxx, ocamlc, ocamlopt, ocamllex, ocamlyacc):
    fbuild.logger.log('configuring host phase', color='cyan')

    platform = fbuild.env.cache('fbuild.builders.platform.config', platform)

    if platform == build.platform:
        fbuild.logger.log("using build's c and cxx compiler", color='cyan')
        phase = build
    else:
        phase = Record(
            platform=platform,
            c=make_c_builder(exe=cc),
            cxx=make_cxx_builder(exe=cxx))

    phase.ocaml = fbuild.env.cache('fbuild.builders.ocaml.config',
        ocamlc=ocamlc,
        ocamlopt=ocamlopt,
        ocamllex=ocamllex,
        ocamlyacc=ocamlyacc)

    # we prefer the native ocaml as it's much faster
    if 'native' in phase.ocaml:
        phase.ocaml.builder = phase.ocaml.native
    else:
        phase.ocaml.builder = phase.ocaml.bytecode

    return phase

def config_target(host, *, platform, cc, cxx):
    fbuild.logger.log('configuring target phase', color='cyan')

    platform = fbuild.env.cache('fbuild.builders.platform.config', platform)

    if platform == host.platform:
        fbuild.logger.log("using host's c and cxx compiler", color='cyan')
        phase = host
    else:
        phase = Record(
            platform=platform,
            c=make_c_builder(exe=cc),
            cxx=make_cxx_builder(exe=cxx))

    return phase

# -----------------------------------------------------------------------------

def prefix():
    prefix = Path(fbuild.options.prefix)
    fbuild.logger.check('install prefix', prefix, color='cyan')

    return prefix

def src_dir():
    return Path(__file__).parent

def build():
    # configure the phases
    build = fbuild.env.cache(config_build,
        platform=fbuild.options.build_platform,
        cc=fbuild.options.build_cc,
        cxx=fbuild.options.build_cxx)

    host = fbuild.env.cache(config_host, build,
        platform=fbuild.options.host_platform,
        cc=fbuild.options.host_cc,
        cxx=fbuild.options.host_cxx,
        ocamlc=fbuild.options.ocamlc,
        ocamlopt=fbuild.options.ocamlopt,
        ocamllex=fbuild.options.ocamllex,
        ocamlyacc=fbuild.options.ocamlyacc)

    target = fbuild.env.cache(config_target, host,
        platform=fbuild.options.target_platform,
        cc=fbuild.options.target_cc,
        cxx=fbuild.options.target_cxx)

    # extract the configuration
    from buildsystem.iscr import Iscr
    Iscr('lpsrc/flx_config.pak').build()

    # convert the config into something iscr can use
    fbuild.env.cache('buildsystem.iscr.config_iscr_config', build, host, target)

    # re-extract packages if any of them changed
    fbuild.scheduler.map(fbuild.packages.build,
        [Iscr(p) for p in (fbuild.env.cache(src_dir)/'lpsrc/*.pak').glob()])

    ####

    # make the rtl header directory
    (fbuild.buildroot / 'lib/rtl').make_dirs()

    import buildsystem.flx as flx

    compilers = fbuild.env.run('buildsystem.flx_compiler.build_flx_drivers',
        host.ocaml)

    drivers = fbuild.env.run('buildsystem.flx_drivers.build',
        target)

    elk = fbuild.env.run('buildsystem.elk.build', host, target)
    elk.exe.build()
    elk.lib.build()

    flx_config = fbuild.env.run('buildsystem.flx.build',
        compilers.flxg, target.cxx.shared, drivers)

    # copy files into the library
    for module in 'flx_pthread', 'demux', 'faio', 'judy':
        for f in fbuild.env.run('buildsystem.' + module + '.build_flx', flx):
            f.build()

    flx_pkgconfig = fbuild.env.run('buildsystem.flx.build_flx_pkgconfig',
        flx_config, target).build()

    # now, try building a file

    felix = fbuild.env.cache('fbuild.builders.felix.config',
        exe=fbuild.buildroot / 'bin/flx',
        flags=['--test=' + fbuild.buildroot])

    run_tests(felix, Path.glob(fbuild.buildroot / 'tut/tutorial/*.flx'))

def run_tests(felix, srcs):
    from fbuild.packages.felix import Felix
    from buildsystem.flx import Test

    tests = [Test(src, config=felix) for src in sorted(srcs)]
    def f(test):
        try:
            test.build()
        except fbuild.ConfigFailed as e:
            fbuild.logger.log(e)

    fbuild.scheduler.map(f, tests)
