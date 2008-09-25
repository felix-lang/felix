from optparse import make_option
from functools import partial

import fbuild

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

def make_c_builder(env, **kwargs):
    return env.config('fbuild.builders.c.guess.config', **kwargs)

def make_cxx_builder(env, **kwargs):
    return env.config('fbuild.builders.cxx.guess.config', **kwargs)

def config_build(env, *, platform, cc, cxx):
    fbuild.logger.log('configuring build phase', color='cyan')

    return fbuild.Record(
        platform=env.config('fbuild.builders.platform.config', platform),
        c=make_c_builder(env, exe=cc),
        cxx=make_cxx_builder(env, exe=cxx),
    )

def config_host(env, build, *,
        platform, cc, cxx, ocamlc, ocamlopt, ocamllex, ocamlyacc):
    fbuild.logger.log('configuring host phase', color='cyan')

    platform = env.config('fbuild.builders.platform.config', platform)

    if platform == build.platform:
        fbuild.logger.log("using build's c and cxx compiler", color='cyan')
        phase = build
    else:
        phase = fbuild.Record(
            platform=platform,
            c=make_c_builder(env, exe=cc),
            cxx=make_cxx_builder(env, exe=cxx))

    phase.ocaml = env.config('fbuild.builders.ocaml.config',
        ocamlc=ocamlc,
        ocamlopt=ocamlopt,
        ocamllex=ocamllex,
        ocamlyacc=ocamlyacc)

    return phase

def config_target(env, host, *, platform, cc, cxx):
    fbuild.logger.log('configuring target phase', color='cyan')

    platform = env.config('fbuild.builders.platform.config', platform)

    if platform == host.platform:
        fbuild.logger.log("using host's c and cxx compiler", color='cyan')
        phase = host
    else:
        phase = fbuild.Record(
            platform=platform,
            c=make_c_builder(env, exe=cc),
            cxx=make_cxx_builder(env, exe=cxx))

    return phase

# -----------------------------------------------------------------------------

def prefix(env):
    prefix = fbuild.Path(fbuild.options.prefix)
    fbuild.logger.check('install prefix', prefix, color='cyan')

    return prefix

def src_dir(env):
    return fbuild.Path(__file__).parent

def build(env):
    # configure the phases
    build = env.config(config_build,
        platform=fbuild.options.build_platform,
        cc=fbuild.options.build_cc,
        cxx=fbuild.options.build_cxx)

    host = env.config(config_host, build,
        platform=fbuild.options.host_platform,
        cc=fbuild.options.host_cc,
        cxx=fbuild.options.host_cxx,
        ocamlc=fbuild.options.ocamlc,
        ocamlopt=fbuild.options.ocamlopt,
        ocamllex=fbuild.options.ocamllex,
        ocamlyacc=fbuild.options.ocamlyacc)

    target = env.config(config_target, host,
        platform=fbuild.options.target_platform,
        cc=fbuild.options.target_cc,
        cxx=fbuild.options.target_cxx)

    # extract the configuration
    from buildsystem.iscr import Iscr
    Iscr('lpsrc/flx_config.pak').build(env)

    # convert the config into something iscr can use
    env.config('buildsystem.iscr.config_iscr_config', build, host, target)

    # re-extract packages if any of them changed
    fbuild.scheduler.map(
        partial(Iscr.build, env=env),
        [Iscr(p) for p in (env.config(src_dir)/'lpsrc/*.pak').glob()])

    drivers = env.config('buildsystem.flx_compiler.build_flx_drivers', host.ocaml)
    for driver in drivers.values():
        driver.build(env)

    import buildsystem.flx_drivers

    flx_drivers = buildsystem.flx_drivers.build(env, target)
    flx_drivers.flx_run.lib.build(env)
    flx_drivers.flx_run.exe.build(env)
    flx_drivers.flx_arun.lib.build(env)
    flx_drivers.flx_arun.exe.build(env)
