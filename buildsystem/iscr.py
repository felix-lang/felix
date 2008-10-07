import time

import fbuild
from fbuild import ConfigFailed
from fbuild.path import Path

# -----------------------------------------------------------------------------

class Iscr:
    def __init__(self, exe):
        self.exe = Path(exe)

    def __call__(self, src, *,
            break_on_error=True,
            flags=[],
            buildroot=fbuild.buildroot,
            **kwargs):
        src = Path(src)
        dst = src.replace_root(buildroot) + '.stdout'

        # if the iscr file hasn't changed, read the .stdout file and return it.
        if not dst.is_dirty(src):
            with open(dst, 'rb') as f:
                return f.read()

        cmd = [
            self.exe.relative_path_to(buildroot),
            '--cache-prefix=lpsrc-cache',
        ]

        if break_on_error:
            cmd.append('--break-on-error')
        cmd.extend(flags)
        cmd.append(src.relative_path_to(buildroot))

        stdout, stderr = fbuild.execute(cmd, 'iscr extracting', src,
            color='green',
            cwd=buildroot,
            env={'PYTHONPATH': Path.relative_path_to('.', buildroot)},
            **kwargs)

        # make sure the dst parent exists, or we'll get an error
        dst.parent.make_dirs()

        # cache the output
        with open(dst, 'wb') as f:
            f.write(stdout)

        return stdout

def config_iscr(exe=None):
    if exe is None:
        exe = fbuild.env.cache('fbuildroot.src_dir') / 'interscript/bin/iscr.py'

    return Iscr(exe)

def config_iscr_config(build, host, target):
    # allow us to import the buildroot
    with open(fbuild.buildroot/'__init__.py', 'w'):
        pass

    with open(fbuild.buildroot/'config/__init__.py', 'w') as f:
        _print_config(f, build, host, target)

def _print_config(f, build, host, target):
    def p(msg, *args):
        if not args:
            print(msg, file=f)
        else:
            print(msg, '=', file=f, *(repr(a) for a in args))

    def pp(msg, *args):
        p('    ' + msg, *args)

    # -------------------------------------------------------------------------
    # version information

    p('import version')
    p('flx_version = version.flx_version')
    p('flx_version_major = version.flx_version_major')
    p('godi_revision = version.godi_revision')
    p('debian_revision = version.debian_revision')

    # -------------------------------------------------------------------------
    # setup paths

    p('src_dir',   str(fbuild.env.cache('fbuildroot.src_dir')))
    p('PREFIX',    str(fbuild.env.cache('fbuildroot.prefix')))
    p('FLXCC_CPP', 'cpp ')

    # -------------------------------------------------------------------------
    # get all the platform information

    from fbuild.builders.platform import archmap

    supported_platforms = set()
    for platforms in archmap.values():
        supported_platforms |= platforms
    supported_platforms = sorted(supported_platforms)

    try:
        fbuild.env.cache('fbuild.builders.c.win32.config', target.c.static)
    except ConfigFailed:
        p('HAVE_MSVC', False)
    else:
        p('HAVE_MSVC', True)
    p('HAVE_GNU', 'windows' not in target.platform)

    # -------------------------------------------------------------------------
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
            _print_math_support(lang, pp)
            _print_openmp_support(lang, pp)
            _print_cxx_bugs(lang, pp)
            _print_gcc_extensions(lang, pp)

    # -------------------------------------------------------------------------
    # expose target_cxx to the top level config

    for platform in supported_platforms:
        p(platform.upper() + '=TARGET_CXX.options.' + platform.upper())

    # -------------------------------------------------------------------------
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
            ' '.join([static.compiler.gcc.exe] + static.compiler.flags) +
            ' -Wall -Wno-invalid-offsetof -Wfatal-errors')

        p('CCLINK_STATIC', static.exe_linker.gcc.exe)

        p('CCOBJ_DYNAMIC_FLX',
            ' '.join([shared.compiler.gcc.exe] + shared.compiler.flags) +
            ' -Wall -Wno-invalid-offsetof -Wfatal-errors')

        p('CCLINK_DYNAMIC_FLX',
            ' '.join([shared.lib_linker.gcc.exe] + shared.lib_linker.flags))

    p('EXT_STATIC_OBJ', static.compiler.suffix)
    p('EXT_SHARED_OBJ', shared.compiler.suffix)
    p('EXT_LIB',        static.lib_linker.suffix)
    p('EXT_SHLIB',      shared.lib_linker.suffix)
    p('EXT_EXE',        static.exe_linker.suffix)
    p('OPTIMISE',       ' '.join(static.compiler.optimize_flags))
    p('DEBUG_FLAGS',    ' '.join(static.compiler.debug_flags))

    p('LITTLE_ENDIAN',
        fbuild.env.cache('fbuild.builders.c.config_little_endian', static))

def _print_types(lang, p):
    import fbuild.builders.c.std as c_std
    int_aliases = c_std.type_aliases_int(lang.static)

    def write(name, data):
        try:
            alias = int_aliases[data['size'], data['signed']]
        except KeyError:
            pass
        else:
            p('ALIAS_' + name.replace(' ', ''), alias)

        name = name.upper().replace(' ', '')
        p('SIZEOF_' + name,  data['size'])
        p('ALIGNOF_' + name, data['alignment'])

    # standard language type info
    std = fbuild.env.cache('fbuild.builders.cxx.std.config', lang.static)
    p('CHAR_IS_UNSIGNED', not std.types['char']['signed'])
    p('HAVE_BOOL',        'bool' in std.types)
    p('HAVE_LONGLONG',    'long long' in std.types)
    p('HAVE_LONGDOUBLE',  'long double' in std.types)

    max_align = 1
    aligns = {1: 'char'}
    for name, data in std.types.items():
        aligns.setdefault(data['alignment'], name)
        max_align = max(max_align, data['alignment'])

        if name == 'void*': name = 'VOIDP'
        write(name, data)
    p('MAX_ALIGN', max_align)
    p('flx_aligns', aligns)
    p('arith_conv', c_std.type_conversions_int(lang.static))

    # stddef.h types
    for name, data in std.headers.stddef_h.types.items():
        write(name, data)

    # c99 types
    c99 = fbuild.env.cache('fbuild.builders.c.c99.config', lang.static)
    for name, data in c99.types.items():
        if name == '_Bool': name = 'cbool'
        write(name, data)

    # complex.h types
    for name, data in c99.headers.complex_h.types.items():
        write(name, data)

    # stdint.h types
    try:
        types = c99.headers.stdint_h.types
    except AttributeError:
        p('HAVE_STDINT', False)
    else:
        p('HAVE_STDINT', True)
        for name, data in types.items():
            write(name, data)

def _print_c99_support(lang, p):
    c99 = fbuild.env.cache('fbuild.builders.c.c99.config', lang.static)
    try:
        vsnprintf = c99.headers.stdio_h.vsnprintf
    except KeyError:
        p('HAVE_VSNPRINTF', False)
    else:
        p('HAVE_VSNPRINTF', vsnprintf)

def _print_posix_support(lang, platform, p):
    # print out information about the posix libraries

    posix = fbuild.env.cache('fbuild.builders.c.posix.config', lang.static)
    try:
        dlopen = posix.headers.dlfcn_h.dlopen
    except KeyError:
        p('HAVE_DLOPEN', False)
        p('SUPPORT_DYNAMIC_LOADING',
            'windows' in platform or 'osx' in platform)
    else:
        p('HAVE_DLOPEN', dlopen)
        p('SUPPORT_DYNAMIC_LOADING',
            dlopen or 'windows' in platform or 'osx' in platform)

    try:
        socklen_t = posix.headers.sys.socket_h.socklen_t
    except KeyError:
        pass
    else:
        p('FLX_SOCKLEN_T', socklen_t)

    try:
        pthread_h = posix.headers.pthread_h
    except KeyError:
        p('HAVE_PTHREADS', False)
    else:
        p('HAVE_PTHREADS',  True)
        p('PTHREAD_SWITCH', ' '.join(pthread_h.flags))

    try:
        fbuild.env.cache('fbuild.builders.c.bsd.config_sys_event_h', lang.static)
    except ConfigFailed:
        p('HAVE_KQUEUE_DEMUXER', False)
    else:
        p('HAVE_KQUEUE_DEMUXER', True)

    try:
        fbuild.env.cache('fbuild.builders.c.linux.config_sys_epoll_h', lang.static)
    except ConfigFailed:
        p('HAVE_EPOLL', False)
    else:
        p('HAVE_EPOLL', True)

    try:
        fbuild.env.cache('fbuild.builders.c.solaris.config_port_h', lang.static)
    except ConfigFailed:
        p('HAVE_EVTPORTS', False)
    else:
        p('HAVE_EVTPORTS', True)

    p('HAVE_POLL', 'poll_h' in posix.headers)

    try:
        mman_h = posix.headers.sys.mman_h
    except KeyError:
        p('HAVE_MMAP', False)
    else:
        p('HAVE_MMAP', True)

        for macro, exists in mman_h['macros'].items():
            if macro == 'MAP_ANON': macro = 'MAP_ANONYMOUS'

            if macro.startswith('MAP'):
                p('HAVE_M' + macro, exists)
            else:
                p('HAVE_MMAP_' + macro, exists)

def _print_math_support(lang, p):
    try:
        cmath = fbuild.env.cache('fbuild.builders.cxx.cmath.config', lang.static)
    except ConfigFailed:
        cmath = {}

    for function in ('isnan', 'isinf', 'isfinite'):
        p('HAVE_CXX_' + function.upper() + '_IN_CMATH',
            cmath.get(function, False))

    math = fbuild.env.cache('fbuild.builders.c.math.config', lang.static)
    for mode, function in \
            ('C99', 'isnan'), ('C99', 'isinf'), ('C99', 'isfinite'), \
            ('BSD', 'isnanf'), ('BSD', 'isinff'), ('BSD', 'finitef'):
        for header in ('math', 'ieeefp'):
            h = math
            p('HAVE_%s_%s_IN_%s' % (mode, function.upper(), header.upper()),
                h.get(function, False))

def _print_openmp_support(lang, p):
    try:
        static = fbuild.env.cache('fbuild.builders.c.openmp.config', lang.static)
    except ConfigFailed:
        static = {}

    try:
        shared = fbuild.env.cache('fbuild.builders.c.openmp.config', lang.shared)
    except ConfigFailed:
        shared = {}

    p('HAVE_OPENMP',        bool(static) or bool(shared))
    p('HAVE_STATIC_OPENMP', bool(static))
    p('HAVE_SHARED_OPENMP', bool(shared))

def _print_cxx_bugs(lang, p):
    try:
        bugs = fbuild.env.cache('fbuild.builders.cxx.std.config_compiler_bugs',
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
        gcc = fbuild.env.cache('fbuild.builders.c.gcc.config_extensions',
            lang.static)
    except ConfigFailed:
        gcc = {}

    try:
        gxx = fbuild.env.cache('fbuild.builders.cxx.gxx.config_extensions',
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
