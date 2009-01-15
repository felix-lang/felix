import io
import re
import time
from itertools import chain

import fbuild
import fbuild.builders.platform
import fbuild.config.c
import fbuild.db
from fbuild import ConfigFailed
from fbuild.functools import call
from fbuild.path import Path

# ------------------------------------------------------------------------------

class Iscr(fbuild.db.PersistentObject):
    def __init__(self, exe=None):
        if exe is None:
            exe = call('fbuildroot.src_dir') / 'interscript/bin/iscr.py'

        self.exe = Path(exe)

    @fbuild.db.cachemethod
    def __call__(self, src:fbuild.db.SRC, *,
            break_on_error=True,
            flags=[],
            buildroot=fbuild.buildroot,
            **kwargs) -> fbuild.db.DSTS:
        src = Path(src)

        cmd = [
            self.exe.relpath(buildroot),
            '--cache-prefix=lpsrc-cache',
            '--trace=sources',
            '--trace=changes',
            '--nocache',
        ]

        if break_on_error:
            cmd.append('--break-on-error')
        cmd.extend(flags)
        cmd.append(src.relpath(buildroot))

        stdout, stderr = fbuild.execute(cmd, 'iscr extracting', src,
            color='green',
            cwd=buildroot,
            env={'PYTHONPATH': Path.relpath('.', buildroot)},
            **kwargs)

        dsts = []
        regex = re.compile('^File (.*) is (NEW|CHANGED|unchanged)$')
        for line in io.StringIO(stdout.decode()):
            m = regex.match(line)
            if m:
                dsts.append(Path(m.group(1)))

        return dsts

# ------------------------------------------------------------------------------

def config_iscr_config(build, host, target) -> fbuild.db.DST:
    # allow us to import the buildroot
    with open(fbuild.buildroot/'__init__.py', 'w'):
        pass

    dst = fbuild.buildroot/'config/__init__.py'

    with open(dst, 'w') as f:
        _print_config(f, build, host, target)

    return dst

def _print_config(f, build, host, target):
    def p(msg, *args):
        if not args:
            print(msg, file=f)
        else:
            print(msg, '=', file=f, *(repr(a) for a in args))

    def pp(msg, *args):
        p('    ' + msg, *args)

    # --------------------------------------------------------------------------
    # version information

    p('import version')
    p('flx_version = version.flx_version')
    p('flx_version_major = version.flx_version_major')
    p('godi_revision = version.godi_revision')
    p('debian_revision = version.debian_revision')

    # --------------------------------------------------------------------------
    # setup paths

    p('src_dir',   str(call('fbuildroot.src_dir')))
    p('PREFIX',    str(call('fbuildroot.prefix')))
    p('FLXCC_CPP', 'cpp ')

    # --------------------------------------------------------------------------
    # get all the platform information

    supported_platforms = set()
    for platforms in fbuild.builders.platform.archmap.values():
        supported_platforms |= platforms
    supported_platforms = sorted(supported_platforms)

    windows_h = call('fbuild.config.c.win32.windows_h', target.c.static)

    p('HAVE_MSVC', bool(windows_h.header))
    p('HAVE_GNU', 'windows' not in target.platform)

    # --------------------------------------------------------------------------
    # phases

    for phase_name, phase in (
            ('build', build), ('host', host), ('target', target)):
        for lang in 'c', 'cxx':
            p('class ' + phase_name.upper() + '_' + lang.upper() + ':')
            p('  class options:')

            platform = phase.platform

            for name in supported_platforms:
                pp(name.upper(), name in platform)

            lang = phase[lang]

            _print_compiler(lang, platform, pp)
            _print_types(lang, pp)
            _print_c99_support(lang, pp)
            _print_posix_support(lang, platform, pp)
            _print_windows_support(lang, platform, pp)
            _print_math_support(lang, pp)
            _print_openmp_support(lang, pp)
            _print_cxx_bugs(lang, pp)
            _print_gcc_extensions(lang, pp)

    # --------------------------------------------------------------------------
    # expose target_cxx to the top level config

    for platform in supported_platforms:
        p(platform.upper() + '=TARGET_CXX.options.' + platform.upper())

    # --------------------------------------------------------------------------
    # figure out the global library loading scheme

    p('SUPPORT_STATIC_LINKAGE', True)
    p('SUPPORT_DYNAMIC_LOADING=TARGET_CXX.options.SUPPORT_DYNAMIC_LOADING')

    p('if SUPPORT_DYNAMIC_LOADING:')
    p('  DEFAULT_LINK_MODEL', 'dynamic')
    p('else:')
    p('  DEFAULT_LINK_MODEL', 'static')


def _print_compiler(lang, platform, p):
    # determine information about the compiler

    p('HAVE_PIC', '-fPIC' in lang.shared.compiler.flags)
    p('PIC', '-fPIC')

    static = lang.static
    shared = lang.shared

    if 'windows' in platform:
        p('SPEC_OBJ_FILENAME', '/Fo')
        p('SPEC_EXE_FILENAME', '/Fe')

        p('CCOBJ_STATIC_FLX',  static.compiler.exe + ' /MD /c /EHs')
        p('CCOBJ_DYNAMIC_FLX', shared.compiler.exe + ' /MD /LD')
        p('CCLINK_STATIC',     static.compiler.exe + ' /MD')
    else:
        p('SPEC_OBJ_FILENAME', '-o ')
        p('SPEC_EXE_FILENAME', '-o ')

        p('CCOBJ_STATIC_FLX',
            ' '.join(chain([static.compiler.gcc.exe], static.compiler.flags)) +
            ' -Wall -Wno-invalid-offsetof -Wfatal-errors')

        p('CCLINK_STATIC', static.exe_linker.gcc.exe)

        p('CCOBJ_DYNAMIC_FLX',
            ' '.join(chain([shared.compiler.gcc.exe], shared.compiler.flags)) +
            ' -Wall -Wno-invalid-offsetof -Wfatal-errors')

        p('CCLINK_DYNAMIC_FLX', ' '.join(
            chain([shared.lib_linker.gcc.exe], shared.lib_linker.flags)))

    p('EXT_STATIC_OBJ', static.compiler.suffix)
    p('EXT_SHARED_OBJ', shared.compiler.suffix)
    p('EXT_LIB',        static.lib_linker.suffix)
    p('EXT_SHLIB',      shared.lib_linker.suffix)
    p('EXT_EXE',        static.exe_linker.suffix)
    p('OPTIMISE',       ' '.join(static.compiler.optimize_flags))
    p('DEBUG_FLAGS',    ' '.join(static.compiler.debug_flags))

    p('LITTLE_ENDIAN',
        call('fbuild.builders.c.config_little_endian', static))

def _print_types(lang, p):
    cxx_types = call('fbuild.config.cxx.cxx03.types', lang.static)

    def write(name, type_):
        if name == '_Bool':
            name = 'cbool'
        elif name == 'void*':
            name = 'VOIDP'

        name = name.replace(' ', '')
        alias = cxx_types.structural_alias(type_)
        if isinstance(type_, fbuild.config.c.IntType):
            p('ALIAS_' + name, alias)

        name = name.upper()
        p('SIZEOF_' + name,  type_.size)
        p('ALIGNOF_' + name, type_.alignment)

    # standard language type info
    p('CHAR_IS_UNSIGNED', not cxx_types.char.signed)
    p('HAVE_BOOL',        bool(cxx_types.bool))
    p('HAVE_LONGLONG',    bool(cxx_types.long_long))
    p('HAVE_LONGDOUBLE',  bool(cxx_types.long_double))

    max_align = 1
    aligns = {1: 'char'}
    for name, type_ in cxx_types.types():
        if type_ is not None:
            aligns.setdefault(type_.alignment, name)
            max_align = max(max_align, type_.alignment)

        if name == 'void*': name = 'VOIDP'
        write(name, type_)
    p('MAX_ALIGN', max_align)
    p('flx_aligns', aligns)
    p('arith_conv', cxx_types.conversion_map())

    c99_types = call('fbuild.config.c.c99.types', lang.static)
    stddef_h = call('fbuild.config.c.c99.stddef_h', lang.static)
    complex_h = call('fbuild.config.c.c99.complex_h', lang.static)
    stdint_h = call('fbuild.config.c.c99.stdint_h', lang.static)

    p('HAVE_STDINT', bool(stdint_h.header))

    # write out data for the types.
    for name, type_ in set(chain(
            cxx_types.types(),
            c99_types.types(),
            stddef_h.types(),
            complex_h.types(),
            stdint_h.types())):
        if type_ is None:
            continue
        write(name, type_)

def _print_c99_support(lang, p):
    stdio_h = call('fbuild.config.c.c99.stdio_h', lang.static)
    p('HAVE_VSNPRINTF', bool(stdio_h.vsnprintf))

def _print_posix_support(lang, platform, p):
    # print out information about the posix libraries
    dlfcn_h = call('fbuild.config.c.posix04.dlfcn_h', lang.static)
    if dlfcn_h.dlopen:
        p('HAVE_DLOPEN', True)
        p('SUPPORT_DYNAMIC_LOADING', True)
    else:
        p('HAVE_DLOPEN', False)

    socket_h = call('fbuild.config.c.posix04.sys_socket_h', lang.static)
    if socket_h.socklen_t:
        # FIXME: Need to figure out how to do this in the new buildsystem.
        p('FLX_SOCKLEN_T', 'socklen_t')

    pthread_h = call('fbuild.config.c.posix04.pthread_h', lang.static)
    if pthread_h.header:
        p('HAVE_PTHREADS', True)
        # FIXME: Need to figure out how to do this in the new buildsystem.
        p('PTHREAD_SWITCH', '')
    else:
        p('HAVE_PTHREADS', False)

    poll_h = call('fbuild.config.c.posix04.poll_h', lang.static)
    p('HAVE_POLL', bool(poll_h.header))

    mman_h = call('fbuild.config.c.sys_mman_h.sys_mman_h', lang.static)

    p('HAVE_MMAP', bool(mman_h.header))
    for name, macro in mman_h.macros():
        p('HAVE_' + name, bool(macro))

    event_h = call('fbuild.config.c.bsd.sys_event_h', lang.static)
    p('HAVE_KQUEUE_DEMUXER', bool(event_h.kqueue))

    epoll_h = call('fbuild.config.c.linux.sys_epoll_h', lang.static)
    p('HAVE_EPOLL', bool(epoll_h.epoll_create))

    port_h = call('fbuild.config.c.solaris.port_h', lang.static)
    p('HAVE_EVTPORTS', bool(port_h.port_create))

def _print_windows_support(lang, platform, p):
    if 'windows' not in platform:
        return

    p('SUPPORT_DYNAMIC_LOADING', True)
    p('FLX_SOCKLEN_T', 'int')

def _print_math_support(lang, p):
    cmath = call('fbuild.config.cxx.cmath.cmath', lang.static)
    math_h = call('fbuild.config.c.math_h.math_h', lang.static)
    ieeefp_h = call('fbuild.config.c.ieeefp_h.ieeefp_h', lang.static)

    p('HAVE_CXX_ISNAN_IN_CMATH', bool(cmath.isnan))
    p('HAVE_CXX_ISINF_IN_CMATH', bool(cmath.isinf))
    p('HAVE_CXX_ISFINITE_IN_CMATH', bool(cmath.isfinite))
    p('HAVE_C99_ISFINITE_IN_MATH', bool(math_h.isfinite))
    p('HAVE_C99_ISINF_IN_MATH', bool(math_h.isinf))
    p('HAVE_C99_ISNAN_IN_MATH', bool(math_h.isnan))
    p('HAVE_BSD_FINITEF_IN_MATH', bool(math_h.finitef))
    p('HAVE_BSD_ISINFF_IN_MATH', bool(math_h.isinff))
    p('HAVE_BSD_ISNANF_IN_MATH', bool(math_h.isnanf))
    p('HAVE_BSD_FINITEF_IN_IEEEFP', bool(ieeefp_h.finitef))
    p('HAVE_BSD_ISINFF_IN_IEEEFP', bool(ieeefp_h.isinff))
    p('HAVE_BSD_ISNANF_IN_IEEEFP', bool(ieeefp_h.isnanf))
    p('HAVE_FINITE_IN_IEEEFP', bool(ieeefp_h.finite))
    p('HAVE_ISINF_IN_IEEEFP', bool(ieeefp_h.isinf))
    p('HAVE_ISNANF_IN_IEEEFP', bool(ieeefp_h.isnanf))

def _print_openmp_support(lang, p):
    try:
        static = call('fbuild.builders.c.openmp.config', lang.static)
    except ConfigFailed:
        static = {}

    try:
        shared = call('fbuild.builders.c.openmp.config', lang.shared)
    except ConfigFailed:
        shared = {}

    p('HAVE_OPENMP',        bool(static) or bool(shared))
    p('HAVE_STATIC_OPENMP', bool(static))
    p('HAVE_SHARED_OPENMP', bool(shared))

def _print_cxx_bugs(lang, p):
    try:
        bugs = call('fbuild.builders.cxx.std.config_compiler_bugs',
            lang.static)
    except fbuild.ConfigFailed:
        bugs = {}
    try:
        test = bugs.class_member_intialization
    except AttributeError:
        p('HAVE_INCLASS_MEMBER_INITIALIZATION', False)
    else:
        p('HAVE_INCLASS_MEMBER_INITIALIZATION', test)

def _print_gcc_extensions(lang, p):
    try:
        gcc = call('fbuild.builders.c.gcc.config_extensions',
            lang.static)
    except ConfigFailed:
        gcc = {}

    try:
        gxx = call('fbuild.builders.cxx.gxx.config_extensions',
            lang.static)
    except ConfigFailed:
        gxx = {}

    have_gnu_x86 = not gcc.get('named_registers_x86_64') and \
            gcc.get('named_registers_x86')

    p('HAVE_GNU_X86',            have_gnu_x86)
    p('HAVE_GNU_X86_64',         gcc.get('named_registers_x86_64'))
    p('HAVE_CGOTO',              gcc.get('computed_gotos'))
    p('HAVE_ASM_LABELS',         gcc.get('asm_labels'))
    p('HAVE_GNU_BUILTIN_EXPECT', gcc.get('builtin_expect'))
    p('USE_REGPARM3',            have_gnu_x86)
    p('HAVE_STL_GNU_CXX',        gxx.get('headers', {}).get('hash_map'))
