import functools

import fbuild.config.c as c
import fbuild.config.c.bsd as bsd
import fbuild.config.c.c90 as c90
import fbuild.config.c.c99 as c99
import fbuild.config.c.ieeefp_h as ieeefp_h
import fbuild.config.c.gnu as gnu
import fbuild.config.c.libutf8 as libutf8
import fbuild.config.c.linux as linux
import fbuild.config.c.malloc as malloc
import fbuild.config.c.platform as platform
import fbuild.config.c.posix as posix
import fbuild.config.c.solaris as solaris
import fbuild.config.c.stdlib as stdlib
import fbuild.config.c.win32 as win32
import fbuild.config.cxx.cmath as cmath
import fbuild.config.cxx.gnu as cxx_gnu
from fbuild.functools import call, import_function

# ------------------------------------------------------------------------------

class c_bsd_memory_h(bsd.memory_h):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.header = None

class c_bsd_sys_event_h(bsd.sys_event_h):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.header = None

class c_ieeefp_h_ieeefp_h(ieeefp_h.ieeefp_h):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.header = None

class c_c90_types(c.c90.types):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

        self.signed_char = c.IntType(1, 1, True)
        self.unsigned_char = c.IntType(1, 1, False)
        self.char = c.IntType(1, 1, True)

        self.short = c.IntType(2, 2, True)
        self.signed_short = c.IntType(2, 2, True)
        self.unsigned_short = c.IntType(2, 2, False)

        self.int = c.IntType(4, 4, True)
        self.signed_int = c.IntType(4, 4, True)
        self.unsigned_int = c.IntType(4, 4, False)

        self.long = c.IntType(4, 4, True)
        self.signed_long = c.IntType(4, 4, True)
        self.unsigned_long = c.IntType(4, 4, False)

        self.long_long = c.IntType(8, 8, True)
        self.signed_long_long = c.IntType(8, 8, True)
        self.unsigned_long_long = c.IntType(8, 8, False)

        self.float = c.Type(4, 4)
        self.double = c.Type(8, 8)
        self.long_double = c.Type(16, 16)

        self.voidp = c.Type(4, 4)
        self.enum = c.Type(4, 4)

    def conversion_map(self):
        return {
            ('char', 'char'): 'int',
            ('char', 'int'): 'int',
            ('char', 'long'): 'int',
            ('char', 'long long'): 'long long',
            ('char', 'short'): 'int',
            ('char', 'signed char'): 'int',
            ('char', 'signed int'): 'int',
            ('char', 'signed long'): 'int',
            ('char', 'signed long long'): 'long long',
            ('char', 'signed short'): 'int',
            ('char', 'unsigned char'): 'int',
            ('char', 'unsigned int'): 'unsigned int',
            ('char', 'unsigned long'): 'unsigned int',
            ('char', 'unsigned long long'): 'unsigned long long',
            ('char', 'unsigned short'): 'int',
            ('int', 'char'): 'int',
            ('int', 'int'): 'int',
            ('int', 'long'): 'int',
            ('int', 'long long'): 'long long',
            ('int', 'short'): 'int',
            ('int', 'signed char'): 'int',
            ('int', 'signed int'): 'int',
            ('int', 'signed long'): 'int',
            ('int', 'signed long long'): 'long long',
            ('int', 'signed short'): 'int',
            ('int', 'unsigned char'): 'int',
            ('int', 'unsigned int'): 'unsigned int',
            ('int', 'unsigned long'): 'unsigned int',
            ('int', 'unsigned long long'): 'unsigned long long',
            ('int', 'unsigned short'): 'int',
            ('long', 'char'): 'int',
            ('long', 'int'): 'int',
            ('long', 'long'): 'int',
            ('long', 'long long'): 'long long',
            ('long', 'short'): 'int',
            ('long', 'signed char'): 'int',
            ('long', 'signed int'): 'int',
            ('long', 'signed long'): 'int',
            ('long', 'signed long long'): 'long long',
            ('long', 'signed short'): 'int',
            ('long', 'unsigned char'): 'int',
            ('long', 'unsigned int'): 'unsigned int',
            ('long', 'unsigned long'): 'unsigned int',
            ('long', 'unsigned long long'): 'unsigned long long',
            ('long', 'unsigned short'): 'int',
            ('long long', 'char'): 'long long',
            ('long long', 'int'): 'long long',
            ('long long', 'long'): 'long long',
            ('long long', 'long long'): 'long long',
            ('long long', 'short'): 'long long',
            ('long long', 'signed char'): 'long long',
            ('long long', 'signed int'): 'long long',
            ('long long', 'signed long'): 'long long',
            ('long long', 'signed long long'): 'long long',
            ('long long', 'signed short'): 'long long',
            ('long long', 'unsigned char'): 'long long',
            ('long long', 'unsigned int'): 'long long',
            ('long long', 'unsigned long'): 'long long',
            ('long long', 'unsigned long long'): 'unsigned long long',
            ('long long', 'unsigned short'): 'long long',
            ('short', 'char'): 'int',
            ('short', 'int'): 'int',
            ('short', 'long'): 'int',
            ('short', 'long long'): 'long long',
            ('short', 'short'): 'int',
            ('short', 'signed char'): 'int',
            ('short', 'signed int'): 'int',
            ('short', 'signed long'): 'int',
            ('short', 'signed long long'): 'long long',
            ('short', 'signed short'): 'int',
            ('short', 'unsigned char'): 'int',
            ('short', 'unsigned int'): 'unsigned int',
            ('short', 'unsigned long'): 'unsigned int',
            ('short', 'unsigned long long'): 'unsigned long long',
            ('short', 'unsigned short'): 'int',
            ('signed char', 'char'): 'int',
            ('signed char', 'int'): 'int',
            ('signed char', 'long'): 'int',
            ('signed char', 'long long'): 'long long',
            ('signed char', 'short'): 'int',
            ('signed char', 'signed char'): 'int',
            ('signed char', 'signed int'): 'int',
            ('signed char', 'signed long'): 'int',
            ('signed char', 'signed long long'): 'long long',
            ('signed char', 'signed short'): 'int',
            ('signed char', 'unsigned char'): 'int',
            ('signed char', 'unsigned int'): 'unsigned int',
            ('signed char', 'unsigned long'): 'unsigned int',
            ('signed char', 'unsigned long long'): 'unsigned long long',
            ('signed char', 'unsigned short'): 'int',
            ('signed int', 'char'): 'int',
            ('signed int', 'int'): 'int',
            ('signed int', 'long'): 'int',
            ('signed int', 'long long'): 'long long',
            ('signed int', 'short'): 'int',
            ('signed int', 'signed char'): 'int',
            ('signed int', 'signed int'): 'int',
            ('signed int', 'signed long'): 'int',
            ('signed int', 'signed long long'): 'long long',
            ('signed int', 'signed short'): 'int',
            ('signed int', 'unsigned char'): 'int',
            ('signed int', 'unsigned int'): 'unsigned int',
            ('signed int', 'unsigned long'): 'unsigned int',
            ('signed int', 'unsigned long long'): 'unsigned long long',
            ('signed int', 'unsigned short'): 'int',
            ('signed long', 'char'): 'int',
            ('signed long', 'int'): 'int',
            ('signed long', 'long'): 'int',
            ('signed long', 'long long'): 'long long',
            ('signed long', 'short'): 'int',
            ('signed long', 'signed char'): 'int',
            ('signed long', 'signed int'): 'int',
            ('signed long', 'signed long'): 'int',
            ('signed long', 'signed long long'): 'long long',
            ('signed long', 'signed short'): 'int',
            ('signed long', 'unsigned char'): 'int',
            ('signed long', 'unsigned int'): 'unsigned int',
            ('signed long', 'unsigned long'): 'unsigned int',
            ('signed long', 'unsigned long long'): 'unsigned long long',
            ('signed long', 'unsigned short'): 'int',
            ('signed long long', 'char'): 'long long',
            ('signed long long', 'int'): 'long long',
            ('signed long long', 'long'): 'long long',
            ('signed long long', 'long long'): 'long long',
            ('signed long long', 'short'): 'long long',
            ('signed long long', 'signed char'): 'long long',
            ('signed long long', 'signed int'): 'long long',
            ('signed long long', 'signed long'): 'long long',
            ('signed long long', 'signed long long'): 'long long',
            ('signed long long', 'signed short'): 'long long',
            ('signed long long', 'unsigned char'): 'long long',
            ('signed long long', 'unsigned int'): 'long long',
            ('signed long long', 'unsigned long'): 'long long',
            ('signed long long', 'unsigned long long'): 'unsigned long long',
            ('signed long long', 'unsigned short'): 'long long',
            ('signed short', 'char'): 'int',
            ('signed short', 'int'): 'int',
            ('signed short', 'long'): 'int',
            ('signed short', 'long long'): 'long long',
            ('signed short', 'short'): 'int',
            ('signed short', 'signed char'): 'int',
            ('signed short', 'signed int'): 'int',
            ('signed short', 'signed long'): 'int',
            ('signed short', 'signed long long'): 'long long',
            ('signed short', 'signed short'): 'int',
            ('signed short', 'unsigned char'): 'int',
            ('signed short', 'unsigned int'): 'unsigned int',
            ('signed short', 'unsigned long'): 'unsigned int',
            ('signed short', 'unsigned long long'): 'unsigned long long',
            ('signed short', 'unsigned short'): 'int',
            ('unsigned char', 'char'): 'unsigned int',
            ('unsigned char', 'int'): 'unsigned int',
            ('unsigned char', 'long'): 'unsigned int',
            ('unsigned char', 'long long'): 'unsigned long long',
            ('unsigned char', 'short'): 'unsigned int',
            ('unsigned char', 'signed char'): 'unsigned int',
            ('unsigned char', 'signed int'): 'unsigned int',
            ('unsigned char', 'signed long'): 'unsigned int',
            ('unsigned char', 'signed long long'): 'unsigned long long',
            ('unsigned char', 'signed short'): 'unsigned int',
            ('unsigned char', 'unsigned char'): 'unsigned int',
            ('unsigned char', 'unsigned int'): 'unsigned int',
            ('unsigned char', 'unsigned long'): 'unsigned int',
            ('unsigned char', 'unsigned long long'): 'unsigned long long',
            ('unsigned char', 'unsigned short'): 'unsigned int',
            ('unsigned int', 'char'): 'unsigned int',
            ('unsigned int', 'int'): 'unsigned int',
            ('unsigned int', 'long'): 'unsigned int',
            ('unsigned int', 'long long'): 'unsigned long long',
            ('unsigned int', 'short'): 'unsigned int',
            ('unsigned int', 'signed char'): 'unsigned int',
            ('unsigned int', 'signed int'): 'unsigned int',
            ('unsigned int', 'signed long'): 'unsigned int',
            ('unsigned int', 'signed long long'): 'unsigned long long',
            ('unsigned int', 'signed short'): 'unsigned int',
            ('unsigned int', 'unsigned char'): 'unsigned int',
            ('unsigned int', 'unsigned int'): 'unsigned int',
            ('unsigned int', 'unsigned long'): 'unsigned int',
            ('unsigned int', 'unsigned long long'): 'unsigned long long',
            ('unsigned int', 'unsigned short'): 'unsigned int',
            ('unsigned long', 'char'): 'unsigned int',
            ('unsigned long', 'int'): 'unsigned int',
            ('unsigned long', 'long'): 'unsigned int',
            ('unsigned long', 'long long'): 'unsigned long long',
            ('unsigned long', 'short'): 'unsigned int',
            ('unsigned long', 'signed char'): 'unsigned int',
            ('unsigned long', 'signed int'): 'unsigned int',
            ('unsigned long', 'signed long'): 'unsigned int',
            ('unsigned long', 'signed long long'): 'unsigned long long',
            ('unsigned long', 'signed short'): 'unsigned int',
            ('unsigned long', 'unsigned char'): 'unsigned int',
            ('unsigned long', 'unsigned int'): 'unsigned int',
            ('unsigned long', 'unsigned long'): 'unsigned int',
            ('unsigned long', 'unsigned long long'): 'unsigned long long',
            ('unsigned long', 'unsigned short'): 'unsigned int',
            ('unsigned long long', 'char'): 'unsigned long long',
            ('unsigned long long', 'int'): 'unsigned long long',
            ('unsigned long long', 'long'): 'unsigned long long',
            ('unsigned long long', 'long long'): 'unsigned long long',
            ('unsigned long long', 'short'): 'unsigned long long',
            ('unsigned long long', 'signed char'): 'unsigned long long',
            ('unsigned long long', 'signed int'): 'unsigned long long',
            ('unsigned long long', 'signed long'): 'unsigned long long',
            ('unsigned long long', 'signed long long'): 'unsigned long long',
            ('unsigned long long', 'signed short'): 'unsigned long long',
            ('unsigned long long', 'unsigned char'): 'unsigned long long',
            ('unsigned long long', 'unsigned int'): 'unsigned long long',
            ('unsigned long long', 'unsigned long'): 'unsigned long long',
            ('unsigned long long', 'unsigned long long'): 'unsigned long long',
            ('unsigned long long', 'unsigned short'): 'unsigned long long',
            ('unsigned short', 'char'): 'unsigned int',
            ('unsigned short', 'int'): 'unsigned int',
            ('unsigned short', 'long'): 'unsigned int',
            ('unsigned short', 'long long'): 'unsigned long long',
            ('unsigned short', 'short'): 'unsigned int',
            ('unsigned short', 'signed char'): 'unsigned int',
            ('unsigned short', 'signed int'): 'unsigned int',
            ('unsigned short', 'signed long'): 'unsigned int',
            ('unsigned short', 'signed long long'): 'unsigned long long',
            ('unsigned short', 'signed short'): 'unsigned int',
            ('unsigned short', 'unsigned char'): 'unsigned int',
            ('unsigned short', 'unsigned int'): 'unsigned int',
            ('unsigned short', 'unsigned long'): 'unsigned int',
            ('unsigned short', 'unsigned long long'): 'unsigned long long',
            ('unsigned short', 'unsigned short'): 'unsigned int'
        }

class c_c99_types(c_c90_types):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

        self._Bool = c.IntType(1, 1, False)

        self.float__Complex = c.Type(8, 4)
        self.double__Complex = c.Type(16, 8)
        self.long_double__Complex = c.Type(32, 16)

        self.float__Imaginary = None
        self.double__Imaginary = None
        self.long_double__Imaginary = None

class c_c99_complex_h(c99.complex_h):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.header = None

class c_c99_math_h(c99.math_h):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.header = None

class c_c99_stddef_h(c99.stddef_h):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.header = 'stddef.h'
        self.size_t = c.IntType(4, 4, False)
        self.ptrdiff_t = c.IntType(4, 4, True)
        self.wchar_t = c.IntType(4, 4, True)

class c_c99_stdlib_h(c99.stdlib_h):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.header = None

class c_c99_stdint_h(c99.stdint_h):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.header = None

class c_c99_stdio_h(c99.stdio_h):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.header = None

class c_c99_string_h(c99.string_h):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.header = None

class c_gnu_extensions(gnu.extensions):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.named_registers_x86_64 = False
        self.computed_gotos = False
        self.asm_labels = False
        self.builtin_expect = False

class c_gnu_getopt_h(gnu.getopt_h):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.header = None

class c_libutf8_libutf8_h(libutf8.libutf8_h):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.header = None

class c_linux_sys_epoll_h(linux.sys_epoll_h):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.header = None

class c_malloc_alloca_h(malloc.alloca_h):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.header = None

class c_platform_arch(platform.arch):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.little_endian = True

class c_posix_dlfcn_h(posix.dlfcn_h):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.header = 'dlfcn.h'
        self.flags = []
        self.dlopen = c.Function('void*', 'const char*', 'int')

class c_posix_inttypes_h(posix.inttypes_h):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.header = None

class c_posix_poll_h(posix.poll_h):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.header = None

class c_posix_sys_mman_h(posix.sys_mman_h):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.header = None

class c_posix_pthread_h(posix.pthread_h):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.header = None

class c_posix_regex_h(posix.regex_h):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.header = None

class c_posix_stdlib_h(posix.stdlib_h):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.header = None

class c_posix_sys_socket_h(posix.sys_socket_h):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.header = 'sys/socket.h'
        self.socklen_t = c.IntType(4, 4, False)
        self.sa_family_t = c.IntType(1, 1, False)
        self.accept = c.Function('int', 'int', 'struct sockaddr*', 'socklen_t*')

class c_posix_strings_h(posix.strings_h):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.header = None

class c_posix_sys_stat_h(posix.sys_stat_h):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.header = None

class c_posix_sys_types_h(posix.sys_types_h):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.header = None

class c_posix_unistd_h(posix.unistd_h):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.header = None

class c_posix_wchar_h(posix.wchar_h):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.header = None

class c_posix_wctype_h(posix.wctype_h):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.header = None

c_stdlib_types = c_c99_types
c_stdlib_complex_h = c_c99_complex_h

class c_stdlib_math_h(stdlib.math_h):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.header = None

c_stdlib_stddef_h = c_c99_stddef_h
c_stdlib_stdint_h = c_c99_stdint_h
c_stdlib_stdio_h = c_c99_stdio_h

class c_solaris_port_h(solaris.port_h):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.header = None

class c_win32_windows_h(win32.windows_h):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.header = None

class cxx_cxx03_types(c_c90_types):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

        self.bool = c.IntType(1, 1, False)

    def conversion_map(self):
        d = super().conversion_map()
        d.update({
            ('bool', 'bool'): 'unsigned int',
            ('bool', 'char'): 'unsigned int',
            ('bool', 'int'): 'unsigned int',
            ('bool', 'long'): 'unsigned int',
            ('bool', 'long long'): 'unsigned long long',
            ('bool', 'short'): 'unsigned int',
            ('bool', 'signed char'): 'unsigned int',
            ('bool', 'signed int'): 'unsigned int',
            ('bool', 'signed long'): 'unsigned int',
            ('bool', 'signed long long'): 'unsigned long long',
            ('bool', 'signed short'): 'unsigned int',
            ('bool', 'unsigned char'): 'unsigned int',
            ('bool', 'unsigned int'): 'unsigned int',
            ('bool', 'unsigned long'): 'unsigned int',
            ('bool', 'unsigned long long'): 'unsigned long long',
            ('bool', 'unsigned short'): 'unsigned int',
            ('char', 'bool'): 'int',
            ('int', 'bool'): 'int',
            ('long', 'bool'): 'int',
            ('long long', 'bool'): 'long long',
            ('short', 'bool'): 'int',
            ('signed char', 'bool'): 'int',
            ('signed int', 'bool'): 'int',
            ('signed long', 'bool'): 'int',
            ('signed long long', 'bool'): 'long long',
            ('signed short', 'bool'): 'int',
            ('unsigned char', 'bool'): 'unsigned int',
            ('unsigned int', 'bool'): 'unsigned int',
            ('unsigned long', 'bool'): 'unsigned int',
            ('unsigned long long', 'bool'): 'unsigned long long',
            ('unsigned short', 'bool'): 'unsigned int',
        })

        return d

class cxx_cmath_cmath(cmath.cmath):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.header = None

class cxx_gnu_gcc_cxx_hash_map(cxx_gnu.gcc_cxx_hash_map):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.header = None
