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
        make_option('-g', '--debug',
            default=False,
            action='store_true',
            help='enable debugging for all phases'),
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
        make_option('--build-c-debug',
            default=False,
            action='store_true',
            help='turn on c/c++ build phase debugging'),
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
        make_option('--host-c-debug',
            default=False,
            action='store_true',
            help='turn on c/c++ host phase debugging'),
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
            help='Add this path to the c library search path for the build ' \
                    'phase'),
        make_option('--target-c-debug',
            default=False,
            action='store_true',
            help='turn on c/c++ target phase debugging'),
    ))

def post_options(options, args):
    options.prefix = Path(options.prefix)

    if options.debug:
        options.buildroot = Path(options.buildroot, 'debug')
    else:
        options.buildroot = Path(options.buildroot, 'release')

    return options, args

# ------------------------------------------------------------------------------

def make_c_builder(*args, includes=[], libpaths=[], **kwargs):
    kwargs['platform_options'] = [
        ({'posix'},
            {'warnings': ['all', 'fatal-errors'],
            'flags': ['-fno-common']}),
        ({'windows'}, {
            'flags': ['/GR', '/MD', '/EHs', '/wd4291']}),
    ]
    kwargs['includes'] = list(chain(fbuild.options.includes, includes))
    kwargs['libpaths'] = list(chain(fbuild.options.libpaths, libpaths))

    return Record(
        static=call('fbuild.builders.c.guess_static', *args, **kwargs),
        shared=call('fbuild.builders.c.guess_shared', *args, **kwargs))

def make_cxx_builder(*args, includes=[], libpaths=[], **kwargs):
    kwargs['platform_options'] = [
        ({'posix'}, {
            'warnings': ['all', 'fatal-errors', 'no-invalid-offsetof'],
            'flags': ['-fno-common']}),
        ({'windows'}, {
            'flags': ['/GR', '/MD', '/EHs', '/wd4291']}),
    ]
    kwargs['includes'] = list(chain(fbuild.options.includes, includes))
    kwargs['libpaths'] = list(chain(fbuild.options.libpaths, libpaths))

    return Record(
        static=call('fbuild.builders.cxx.guess_static', *args, **kwargs),
        shared=call('fbuild.builders.cxx.guess_shared', *args, **kwargs))

def config_build():
    fbuild.logger.log('configuring build phase', color='cyan')

    platform = call('fbuild.builders.platform.platform',
        fbuild.options.build_platform)

    return Record(
        platform=platform,
        c=make_c_builder(fbuild.options.build_cc,
            platform=platform,
            debug=fbuild.options.debug or fbuild.options.build_c_debug,
            includes=fbuild.options.build_includes,
            libpaths=fbuild.options.build_libpaths),
        cxx=make_cxx_builder(fbuild.options.build_cxx,
            platform=platform,
            debug=fbuild.options.debug or fbuild.options.build_c_debug,
            includes=fbuild.options.build_includes,
            libpaths=fbuild.options.build_libpaths))

def config_host(build):
    fbuild.logger.log('configuring host phase', color='cyan')

    platform = call('fbuild.builders.platform.platform',
        fbuild.options.build_platform)

    if platform == build.platform:
        fbuild.logger.log("using build's c and cxx compiler", color='cyan')
        phase = build
    else:
        phase = Record(
            platform=platform,
            c=make_c_builder(fbuild.builders.host_cc,
                platform=platform,
                debug=fbuild.options.debug or fbuild.options.host_c_debug,
                includes=fbuild.options.host_includes,
                libpaths=fbuild.options.host_libpaths),
            cxx=make_cxx_builder(fbuild.buildesr.host_cxx,
                platform=platform,
                debug=fbuild.options.debug or fbuild.options.host_c_debug,
                includes=fbuild.options.host_includes,
                libpaths=fbuild.options.host_libpaths))

    phase.ocaml = call('fbuild.builders.ocaml.Ocaml',
        debug=fbuild.options.debug or fbuild.options.host_ocaml_debug,
        ocamlc=fbuild.options.host_ocamlc,
        ocamlopt=fbuild.options.host_ocamlopt,
        flags=['-w', 'yzex', '-warn-error', 'FDPSU'],
        requires_at_least_version=(3, 11))

    phase.ocamllex = call('fbuild.builders.ocaml.Ocamllex',
        fbuild.options.host_ocamllex)

    # we prefer the native ocaml as it's much faster
    if hasattr(phase.ocaml, 'ocamlopt'):
        phase.ocaml = phase.ocaml.ocamlopt
    else:
        phase.ocaml = phase.ocaml.ocamlc

    return phase

def config_target(host):
    fbuild.logger.log('configuring target phase', color='cyan')

    platform = call('fbuild.builders.platform.platform',
        fbuild.options.target_platform)

    if platform == host.platform:
        fbuild.logger.log("using host's c and cxx compiler", color='cyan')
        phase = host
    else:
        phase = Record(
            platform=platform,
            c=make_c_builder(fbuild.options.target_cc,
                platform=platform,
                debug=fbuild.options.debug or fbuild.options.target_c_debug,
                includes=fbuild.options.target_includes,
                libpaths=fbuild.options.target_libpaths),
            cxx=make_cxx_builder(fbuild.options.target_cxx,
                platform=platform,
                debug=fbuild.options.debug or fbuild.options.target_c_debug,
                includes=fbuild.options.target_includes,
                libpaths=fbuild.options.target_libpaths))

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
    build = config_build()
    host = config_host(build)
    target = config_target(host)

    # extract the configuration
    iscr = call('buildsystem.iscr.Iscr')
    iscr('lpsrc/flx_config.pak')

    # convert the config into something iscr can use
    call('buildsystem.iscr.config_iscr_config', build, host, target)

    # re-extract packages if any of them changed
    fbuild.scheduler.map(iscr, (src_dir()/'lpsrc/*.pak').glob())

    # overwrite or add *.fpc files to the config directory
    call('buildsystem.post_config.copy_user_fpcs')

    # --------------------------------------------------------------------------

    compilers = call('buildsystem.flx_compiler.build_flx_drivers',
        host.ocaml, host.ocamllex)

    drivers = call('buildsystem.flx_drivers.build', target)

    flx = call('buildsystem.flx.build',
        compilers.flxg, target.cxx.static, drivers)

    # copy files into the library
    for module in 'flx_stdlib', 'flx_pthread', 'demux', 'faio', 'judy':
        call('buildsystem.' + module + '.build_flx', flx)

    flx_pkgconfig = call('buildsystem.flx.build_flx_pkgconfig',
        flx, target)

    # --------------------------------------------------------------------------
    # build the secondary libraries

    call('buildsystem.flx_glob.build_runtime', target)
    call('buildsystem.tre.build_runtime', target)

    for module in 'flx_glob', 'tre':
        call('buildsystem.' + module + '.build_flx', flx)

    call('buildsystem.bindings.build_flx', target)

    # --------------------------------------------------------------------------
    # now, try building a file

    felix = call('fbuild.builders.felix.Felix',
        exe=fbuild.buildroot / 'bin/flx.py',
        debug=fbuild.options.debug,
        flags=['--test=' + fbuild.buildroot])

    # --------------------------------------------------------------------------
    # run the felix tests

    if not fbuild.options.skip_tests:
        run_tests(target, felix)

def run_tests(target, felix):
    from buildsystem.flx import test_flx

    failed_srcs = []

    def test(src):
        try:
            passed = test_flx(felix, src)
        except fbuild.ConfigFailed as e:
            fbuild.logger.log(str(e))
            passed = False
        return src, passed

    # Run the dynamic loading tests first
    try:
        lib1 = felix.compile('test/regress/drt/lib1.flx', static=False)
        lib2 = felix.compile('test/regress/drt/lib2.flx', static=False)
    except fbuild.ExecutionError as e:
        fbuild.logger.log(e, verbose=1)
    else:
        if not test_flx(felix, 'test/regress/drt/main1.flx',
                env={'lib1': lib1, 'lib2': lib2}):
            failed_srcs.append('test/regress/drt/main1.flx')

    srcs = Path.globall(
        'test/*/*.flx',
        'test/*/*/*.flx',
        fbuild.buildroot / 'tut/*/*.flx',
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

    for src, passed in fbuild.scheduler.map(test, sorted(srcs, reverse=True)):
        if not passed:
            failed_srcs.append(src)

    if failed_srcs:
        fbuild.logger.log('\nThe following tests failed:')
        for src in failed_srcs:
            fbuild.logger.log('  %s' % src, color='yellow')
