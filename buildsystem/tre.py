import fbuild.builders.text
import fbuild.config.c.c99 as c99
import fbuild.config.c.posix as posix
from fbuild.path import Path
from fbuild.record import Record

import buildsystem
from buildsystem.config import config_call

# ------------------------------------------------------------------------------

def build_runtime(phase):
    path = Path('src/tre/tre')

    alloca_h = config_call('fbuild.config.c.malloc.alloca_h', phase.platform, phase.c.static)
    dlfcn_h = config_call('fbuild.config.c.posix.dlfcn_h', phase.platform, phase.c.static, phase.c.shared)
    getopt_h = config_call('fbuild.config.c.gnu.getopt_h', phase.platform, phase.c.static)
    inttypes_h = config_call('fbuild.config.c.posix.inttypes_h', phase.platform, phase.c.static)
    libutf8_h = config_call('fbuild.config.c.libutf8.libutf8_h', phase.platform, phase.c.static)
    memory_h = config_call('fbuild.config.c.bsd.memory_h', phase.platform, phase.c.static)
    regex_h = config_call('fbuild.config.c.posix.regex_h', phase.platform, phase.c.static)
    stdint_h = config_call('fbuild.config.c.c99.stdint_h', phase.platform, phase.c.static)
    stdlib_h = config_call('fbuild.config.c.posix.stdlib_h', phase.platform, phase.c.static)
    string_h = config_call('fbuild.config.c.c99.string_h', phase.platform, phase.c.static)
    strings_h = config_call('fbuild.config.c.posix.strings_h', phase.platform, phase.c.static)
    sys_stat_h = config_call('fbuild.config.c.posix.sys_stat_h', phase.platform, phase.c.static)
    sys_types_h = config_call('fbuild.config.c.posix.sys_types_h', phase.platform, phase.c.static)
    unistd_h = config_call('fbuild.config.c.posix.unistd_h', phase.platform, phase.c.static)
    wchar_h = config_call('fbuild.config.c.posix.wchar_h', phase.platform, phase.c.static)
    wctype_h = config_call('fbuild.config.c.posix.wctype_h', phase.platform, phase.c.static)

    if 'win32' in phase.platform:
        inline = '__inline'
    else:
        inline = None

    patterns = {
        'CRAY_STACKSEG_END': None,
        'C_ALLOCA': None,
        'ENABLE_NLS': 0,
        'HAVE_ALLOCA': alloca_h.alloca is not None,
        'HAVE_WCSLEN': wchar_h.wcslen is not None,
        'NDEBUG': 1,
        'HAVE_ALLOCA_H': alloca_h.header is not None,
        'HAVE_CFLOCALECOPYCURRENT': None,
        'HAVE_CFPREFERENCESCOPYAPPVALUE': None,
        'HAVE_DCGETTEXT': None,
        'HAVE_DLFCN_H': dlfcn_h.header is not None,
        'HAVE_GETOPT_H': getopt_h.header is not None ,
        'HAVE_GETOPT_LONG': getopt_h.getopt_long is not None,
        'HAVE_GETTEXT': None,
        'HAVE_ICONV': None,
        'HAVE_INTTYPES_H': inttypes_h.header is not None,
        'HAVE_ISASCII': 1,
        'HAVE_ISBLANK': 1,
        'HAVE_ISWASCII': wctype_h.iswascii is not None,
        'HAVE_ISWBLANK': wctype_h.iswblank is not None,
        'HAVE_ISWCTYPE': wctype_h.iswctype is not None,
        'HAVE_ISWLOWER': wctype_h.iswlower is not None,
        'HAVE_ISWUPPER': wctype_h.iswupper is not None,
        'HAVE_LIBUTF8_H': libutf8_h.header is not None,
        'HAVE_MBRTOWC': wchar_h.mbrtowc is not None,
        'HAVE_MBSTATE_T': wchar_h.mbstate_t is not None,
        'HAVE_MBTOWC': stdlib_h.mbtowc is not None,
        'HAVE_MEMORY_H': memory_h.header is not None,
        'HAVE_REGEX_H': regex_h.header is not None,
        'HAVE_REG_ERRCODE_T': regex_h.reg_errcode_t is not None,
        'HAVE_STDINT_H': stdint_h.header is not None,
        'HAVE_STDLIB_H': stdlib_h.header is not None,
        'HAVE_STRINGS_H': strings_h.header is not None,
        'HAVE_STRING_H': string_h.header is not None,
        'HAVE_SYS_STAT_H': sys_stat_h.header is not None,
        'HAVE_SYS_TYPES_H': sys_types_h.header is not None,
        'HAVE_TOWLOWER': wctype_h.towlower is not None,
        'HAVE_TOWUPPER': wctype_h.towupper is not None,
        'HAVE_UNISTD_H': unistd_h.header is not None,
        'HAVE_WCHAR_H': wchar_h.header is not None,
        'HAVE_WCHAR_T': wchar_h.wchar_t is not None,
        'HAVE_WCSCHR': wchar_h.wcschr is not None,
        'HAVE_WCSCPY': wchar_h.wcscpy is not None,
        'HAVE_WCSNCPY': wchar_h.wcsncpy is not None,
        'HAVE_WCSRTOMBS': wchar_h.wcsrtombs is not None,
        'HAVE_WCSTOMBS': stdlib_h.wcstombs is not None,
        'HAVE_WCTYPE': wctype_h.wctype is not None,
        'HAVE_WCTYPE_H': wctype_h.header is not None,
        'HAVE_WINT_T': wctype_h.wint_t is not None,
        'NO_MINUS_C_MINUS_O': None,
        'PACKAGE': '"tre"',
        'PACKAGE_TARNAME': '"tre"',
        'PACKAGE_VERSION': '"0.8.0"',
        'STACK_DIRECTION': None,
        'VERSION': '"0.8.0"',
        'PACKAGE_BUGREPORT': '"tre-general@lists.laurikari.net"',
        'PACKAGE_NAME': '"TRE"',
        'PACKAGE_STRING': '"TRE 0.8.0"',
        'PACKAGE_TARFILE': 'tre',
        'STDC_HEADERS': 1,
        'TRE_APPROX': 1,
        'TRE_DEBUG': None,
        'TRE_MULTIBYTE': wchar_h.header is not None,
        'TRE_REGEX_T_FIELD': 'value',
        'TRE_SYSTEM_REGEX_H_PATH': None,
        'TRE_USE_ALLOCA': alloca_h.alloca is not None,
        'TRE_USE_SYSTEM_REGEX_H': None,
        'TRE_VERSION': '"0.8.0"',
        'TRE_VERSION_1': '0',
        'TRE_VERSION_2': '8',
        'TRE_VERSION_3': '0',
        'TRE_WCHAR': wchar_h.header is not None,
        'WCHAR_MAX': None,
        'WCHAR_T_SIGNED': None,
        'WCHAR_T_UNSIGNED': None,
        '_FILE_OFFSET_BITS': None,
        '_GNU_SOURCE': 1,
        '_LARGE_FILES': None,
        '_REGCOMP_INTERNAL': None,
        'const': None,
        'inline': inline,
    }

    fbuild.builders.text.autoconf_config_header(phase.ctx,
        phase.ctx.buildroot/'host/lib/rtl/tre/config.h',
        path / 'config.h.in',
        patterns)

    fbuild.builders.text.autoconf_config_header(phase.ctx,
        phase.ctx.buildroot/'host/lib/rtl/tre/lib/tre-config.h',
        path / 'lib/tre-config.h.in',
        patterns)

    #buildsystem.copy_to(phase.ctx, phase.ctx.buildroot / 'share/lib/rtl/tre', [
    #    path / 'lib/tre.h',
    #    phase.ctx.buildroot / path / 'lib/tre-config.h'])

    dst = 'host/lib/rtl/tre'
    srcs = Path.glob('src/tre/tre/lib/*.c')
    includes = [
        phase.ctx.buildroot / 'host/lib/rtl',
        phase.ctx.buildroot / 'host/lib/rtl/tre',
        phase.ctx.buildroot / 'host/lib/rtl/tre/lib',
        phase.ctx.buildroot / 'share'/ path,
        phase.ctx.buildroot / 'share'/ path / 'lib',
        path / 'gnulib/lib',
    ]
    macros = ['HAVE_CONFIG_H', 'BUILD_TRE']

    #Workaround link error : unresolved external symbol _snprintf
    #referenced in function _tre_version.

    if 'win32' in phase.platform:
        macros.append('snprintf=_snprintf')

    return Record(
        static=buildsystem.build_c_static_lib(phase, dst, srcs,
            includes=includes,
            macros=macros),
        shared=buildsystem.build_c_shared_lib(phase, dst, srcs,
            includes=includes,
            macros=macros))
