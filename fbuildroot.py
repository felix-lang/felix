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
        make_option('--prefix',
            default='/usr/local',
            help='specify the install location'),
        make_option('-I', '--include',
            dest='includes',
            default=[],
            action='append',
            help='Add this path to the c header search path for all phases'),
        make_option('-L', '--library-path',
            dest='libpaths',
            default=[],
            action='append',
            help='Add this path to the c library search path for all phases'),
        make_option('--c-flag',
            dest='c_flags',
            default=[],
            action='append',
            help='Add this flag to the c compiler'),
        make_option('-g', '--debug',
            default=False,
            action='store_true',
            help='enable debugging for all phases'),
        make_option('-O', '--optimize',
            default=False,
            action='store_true',
            help='enable optimization for all phases'),
        make_option('--skip-tests',
            default=False,
            action='store_true',
            help='skip running tests'),
    ))

    group = parser.add_option_group('build phase options')
    group.add_options((
        make_option('--build-platform',
            help='specify the build phase platform'),
        make_option('--build-cc',
            help='specify the build phase c compiler'),
        make_option('--build-cxx',
            help='specify the build phase c++ compiler'),
        make_option('--build-include',
            dest='build_includes',
            default=[],
            action='append',
            help='Add this path to the c header search path for the build ' \
                    'phase'),
        make_option('--build-library-path',
            dest='build_libpaths',
            default=[],
            action='append',
            help='Add this path to the c library search path for the build ' \
                    'phase'),
        make_option('--build-c-flag',
            dest='build_c_flags',
            default=[],
            action='append',
            help='Add this flag to the c compiler for the build phase'),
        make_option('--build-c-debug',
            default=False,
            action='store_true',
            help='turn on c/c++ build phase debugging'),
        make_option('--build-c-optimize',
            default=False,
            action='store_true',
            help='turn on optimizations for c/c++ build phase'),
    ))

    group = parser.add_option_group('host phase options')
    group.add_options((
        make_option('--host-platform',
            help='specify the host phase platform'),
        make_option('--host-cc',
            help='specify the host phase c compiler'),
        make_option('--host-cxx',
            help='specify the host phase c++ compiler'),
        make_option('--host-include',
            dest='host_includes',
            default=[],
            action='append',
            help='Add this path to the c header search path for the host ' \
                    'phase'),
        make_option('--host-library-path',
            dest='host_libpaths',
            default=[],
            action='append',
            help='Add this path to the c library search path for the host ' \
                    'phase'),
        make_option('--host-c-flag',
            dest='host_c_flags',
            default=[],
            action='append',
            help='Add this flag to the c compiler for the host phase'),
        make_option('--host-c-debug',
            default=False,
            action='store_true',
            help='turn on c/c++ host phase debugging'),
        make_option('--host-c-optimize',
            default=False,
            action='store_true',
            help='turn on optimization for c/c++ host phase'),
        make_option('--host-ocaml-debug',
            default=False,
            action='store_true',
            help='turn on ocaml debugging'),
        make_option('--host-ocamlc',
            help='specify the ocaml bytecode compiler'),
        make_option('--host-ocamlopt',
            help='specify the ocaml native compiler'),
        make_option('--host-ocamllex',
            help='specify the ocaml lexer'),
    ))

    group = parser.add_option_group('target phase options')
    group.add_options((
        make_option('--target-platform',
            help='specify the target phase platform'),
        make_option('--target-cc',
            help='specify the target phase c compiler'),
        make_option('--target-cxx',
            help='specify the target phase c++ compiler'),
        make_option('--target-include',
            dest='target_includes',
            default=[],
            action='append',
            help='Add this path to the c header search path for the target ' \
                    'phase'),
        make_option('--target-library-path',
            dest='target_libpaths',
            default=[],
            action='append',
            help='Add this path to the c library search path for the target ' \
                    'phase'),
        make_option('--target-c-debug',
            default=False,
            action='store_true',
            help='turn on c/c++ target phase debugging'),
        make_option('--target-c-optimize',
            default=False,
            action='store_true',
            help='turn on optimization for c/c++ target phase'),
        make_option('--target-c-flag',
            dest='target_c_flags',
            default=[],
            action='append',
            help='Add this flag to the c compiler for the target phase'),
    ))

def post_options(options, args):
    options.prefix = Path(options.prefix)

    if options.debug:
        options.buildroot = Path(options.buildroot, 'debug')
    else:
        options.buildroot = Path(options.buildroot, 'release')

    if options.optimize:
        options.buildroot += '-optimized'

    return options, args

# ------------------------------------------------------------------------------

def make_c_builder(ctx, *args, includes=[], libpaths=[], flags=[], **kwargs):
    flags = list(chain(ctx.options.c_flags, flags))

    kwargs['platform_options'] = [
        ({'posix'},
            {'warnings': ['all', 'fatal-errors'],
            'flags': ['-fno-common'] + flags,
            'optimize_flags': ['-O3', '-fomit-frame-pointer', '--inline']}),
        ({'windows'}, {
            'flags': ['/GR', '/MD', '/EHs', '/wd4291'] + flags,
            'optimize_flags': ['/Ox']}),
    ]
    kwargs['includes'] = list(chain(ctx.options.includes, includes))
    kwargs['libpaths'] = list(chain(ctx.options.libpaths, libpaths))

    return Record(
        static=call('fbuild.builders.c.guess_static', ctx, *args, **kwargs),
        shared=call('fbuild.builders.c.guess_shared', ctx, *args, **kwargs))

def make_cxx_builder(ctx, *args, includes=[], libpaths=[], flags=[], **kwargs):
    flags = list(chain(ctx.options.c_flags, flags))

    kwargs['platform_options'] = [
        ({'posix'}, {
            'warnings': ['all', 'fatal-errors', 'no-invalid-offsetof'],
            'flags': ['-fno-common'] + flags,
            'optimize_flags': ['-O3', '-fomit-frame-pointer', '--inline']}),
        ({'windows'}, {
            'flags': ['/GR', '/MD', '/EHs', '/wd4291'] + flags,
            'optimize_flags': ['/Ox']}),
    ]
    kwargs['includes'] = list(chain(ctx.options.includes, includes))
    kwargs['libpaths'] = list(chain(ctx.options.libpaths, libpaths))

    return Record(
        static=call('fbuild.builders.cxx.guess_static', ctx, *args, **kwargs),
        shared=call('fbuild.builders.cxx.guess_shared', ctx, *args, **kwargs))

def config_build(ctx):
    ctx.logger.log('configuring build phase', color='cyan')

    platform = call('fbuild.builders.platform.platform', ctx,
        ctx.options.build_platform)

    return Record(
        ctx=ctx,
        platform=platform,
        c=make_c_builder(ctx, ctx.options.build_cc,
            platform=platform,
            debug=ctx.options.debug or ctx.options.build_c_debug,
            optimize=ctx.options.optimize or ctx.options.build_c_optimize,
            includes=ctx.options.build_includes,
            libpaths=ctx.options.build_libpaths,
            flags=ctx.options.build_c_flags),
        cxx=make_cxx_builder(ctx, ctx.options.build_cxx,
            platform=platform,
            debug=ctx.options.debug or ctx.options.build_c_debug,
            optimize=ctx.options.optimize or ctx.options.build_c_optimize,
            includes=ctx.options.build_includes,
            libpaths=ctx.options.build_libpaths,
            flags=ctx.options.build_c_flags))

def config_host(ctx, build):
    ctx.logger.log('configuring host phase', color='cyan')

    platform = call('fbuild.builders.platform.platform', ctx,
        ctx.options.build_platform)

    if platform == build.platform:
        ctx.logger.log("using build's c and cxx compiler", color='cyan')
        phase = build
    else:
        phase = Record(
            ctx=ctx,
            platform=platform,
            c=make_c_builder(ctx, fbuild.builders.host_cc,
                platform=platform,
                debug=ctx.options.debug or ctx.options.host_c_debug,
                optimize=ctx.options.optimize or
                    ctx.options.host_c_optimize,
                includes=ctx.options.host_includes,
                libpaths=ctx.options.host_libpaths,
                flags=ctx.options.host_c_flags),
            cxx=make_cxx_builder(ctx, fbuild.buildesr.host_cxx,
                platform=platform,
                debug=ctx.options.debug or ctx.options.host_c_debug,
                optimize=ctx.options.optimize or
                    ctx.options.host_c_optimize,
                includes=ctx.options.host_includes,
                libpaths=ctx.options.host_libpaths,
                flags=ctx.options.host_c_flags))

    phase.ocaml = call('fbuild.builders.ocaml.Ocaml', ctx,
        debug=ctx.options.debug or ctx.options.host_ocaml_debug,
        ocamlc=ctx.options.host_ocamlc,
        ocamlopt=ctx.options.host_ocamlopt,
        flags=['-w', 'yzex', '-warn-error', 'FDPSU'],
        requires_at_least_version=(3, 11))

    phase.ocamllex = call('fbuild.builders.ocaml.Ocamllex', ctx,
        ctx.options.host_ocamllex)

    # we prefer the native ocaml as it's much faster
    if hasattr(phase.ocaml, 'ocamlopt'):
        phase.ocaml = phase.ocaml.ocamlopt
    else:
        phase.ocaml = phase.ocaml.ocamlc

    return phase

def config_target(ctx, host):
    ctx.logger.log('configuring target phase', color='cyan')

    platform = call('fbuild.builders.platform.platform', ctx,
        ctx.options.target_platform)

    if platform == host.platform:
        ctx.logger.log("using host's c and cxx compiler", color='cyan')
        phase = host
    else:
        phase = Record(
            ctx=ctx,
            platform=platform,
            c=make_c_builder(ctx, ctx.options.target_cc,
                platform=platform,
                debug=ctx.options.debug or ctx.options.target_c_debug,
                optimize=ctx.options.optimize or
                    ctx.options.target_c_optimize,
                includes=ctx.options.target_includes,
                libpaths=ctx.options.target_libpaths,
                flags=ctx.options.target_c_flags),
            cxx=make_cxx_builder(ctx, ctx.options.target_cxx,
                platform=platform,
                debug=ctx.options.debug or ctx.options.target_c_debug,
                optimize=ctx.options.optimize or
                    ctx.options.target_c_optimize,
                includes=ctx.options.target_includes,
                libpaths=ctx.options.target_libpaths,
                flags=ctx.options.target_c_flags))

    return phase

# ------------------------------------------------------------------------------

@fbuild.db.caches
def prefix(ctx):
    prefix = Path(ctx.options.prefix)
    ctx.logger.check('install prefix', prefix, color='cyan')

    return prefix

@fbuild.db.caches
def src_dir(ctx):
    return Path(__file__).parent

def build(ctx):
    # configure the phases
    build = config_build(ctx)
    host = config_host(ctx, build)
    target = config_target(ctx, host)

    # extract the configuration
    iscr = call('buildsystem.iscr.Iscr', ctx)
    iscr('lpsrc/flx_config.pak')

    # convert the config into something iscr can use
    call('buildsystem.iscr.config_iscr_config', ctx, build, host, target)

    # re-extract packages if any of them changed
    ctx.scheduler.map(iscr, (src_dir(ctx)/'lpsrc/*.pak').glob())

    # overwrite or add *.fpc files to the config directory
    call('buildsystem.post_config.copy_user_fpcs', ctx)

    # --------------------------------------------------------------------------

    compilers = call('buildsystem.flx_compiler.build_flx_drivers', ctx, host)
    drivers = call('buildsystem.flx_drivers.build', target)

    target.flx = call('buildsystem.flx.build', ctx,
        compilers.flxg, target.cxx.static, drivers)

    # copy files into the library
    for module in 'flx_stdlib', 'flx_pthread', 'demux', 'faio', 'judy':
        call('buildsystem.' + module + '.build_flx', target)

    flx_pkgconfig = call('buildsystem.flx.build_flx_pkgconfig', target)

    # --------------------------------------------------------------------------
    # build the secondary libraries

    call('buildsystem.flx_glob.build_runtime', target)
    call('buildsystem.tre.build_runtime', target)

    for module in 'flx_glob', 'tre':
        call('buildsystem.' + module + '.build_flx', target)

    call('buildsystem.bindings.build_flx', target)

    # --------------------------------------------------------------------------
    # now, try building a file

    target.felix = call('fbuild.builders.felix.Felix', ctx,
        exe=ctx.buildroot / 'bin/flx.py',
        debug=ctx.options.debug,
        flags=['--test=' + ctx.buildroot])

    # --------------------------------------------------------------------------
    # run the felix tests and other commands

    if 'speed' in ctx.args:
        call('buildsystem.speed.run_tests', target)
    else:
        if not ctx.options.skip_tests:
            run_tests(target)

# ------------------------------------------------------------------------------

def run_tests(target):
    from buildsystem.flx import test_flx

    failed_srcs = []

    def test(src):
        try:
            passed = test_flx(target, src)
        except fbuild.ConfigFailed as e:
            ctx.logger.log(str(e))
            passed = False
        return src, passed

    # Run the dynamic loading tests first
    try:
        lib1 = target.felix.compile('test/regress/drt/lib1.flx', static=False)
        lib2 = target.felix.compile('test/regress/drt/lib2.flx', static=False)
    except fbuild.ExecutionError as e:
        target.ctx.logger.log(e, verbose=1)
    else:
        if not test_flx(target, 'test/regress/drt/main1.flx',
                env={'lib1': lib1, 'lib2': lib2}):
            failed_srcs.append('test/regress/drt/main1.flx')

    srcs = Path.globall(
        'test/*/*.flx',
        'test/*/*/*.flx',
        target.ctx.buildroot / 'tut/*/*.flx',
        exclude=[
            'test/drivers/*.flx',
            'test/faio/posix-*.flx',
            'test/faio/win-*.flx',
            'test/regress/drt/*.flx',
            'test/regress/bt/*.flx',
            'test/regress/kf/*.flx',
            'test/regress/nd/*.flx',
            'test/test-data/*.flx',
        ])

    if 'posix' in target.platform:
        srcs.extend(Path.glob('test/faio/posix-*.flx'))

    if 'windows' in target.platform:
        srcs.extend(Path.glob('test/faio/win-*.flx'))

    for src, passed in target.ctx.scheduler.map(test, sorted(srcs, reverse=True)):
        if not passed:
            failed_srcs.append(src)

    if failed_srcs:
        target.ctx.logger.log('\nThe following tests failed:')
        for src in failed_srcs:
            target.ctx.logger.log('  %s' % src, color='yellow')
