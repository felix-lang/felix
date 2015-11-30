"""fbuild.config.c.posix extends fbuild.config.c.posix04 to expose cross
platform flags and libraries, and exposes many common extensions."""

import fbuild.config.c as c
from fbuild.config.c.posix04 import *

# ------------------------------------------------------------------------------

class dlfcn_h(dlfcn_h):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

        # Linux needs to link against libdl for dl* support.
        if 'linux' in self.platform:
            self.external_libs.append('dl')

class regex_h(regex_h):
    reg_errcode_t = c.type_test()

class pthread_h(pthread_h):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

        if 'linux' in self.platform:
            self.external_libs.append('pthread')

        # Solaris needs to link against librt for posix support.
        elif 'solaris' in self.platform:
            self.external_libs.append('rt')

class stdlib_h(stdlib_h):
    mkdtemp = c.function_test('char*', 'char*', test='''
        #include <stdlib.h>
        #include <unistd.h>
        int main() {
            char s[] = "XXXXXX";
            if (mkdtemp(s) == NULL) return 1;
            if (rmdir(s) == -1) return 1;
            return 0;
        }
        ''')
    strtoq = c.function_test('quad_t', 'const char*', 'char**', test='''
        #include <stdlib.h>
        #include <sys/types.h>
        int main() {
            char* s1 = "15";
            char* s2 = "abc";
            char* endp;
            quad_t d = strtoq(s1, &endp, 8);
            if (s1 != endp && *endp == '\\0' && d == 13l) {
                d = strtoq(s2, &endp, 8);
                return s1 == endp || *endp != '\\0' ? 0 : 1;
            }
            return 1;
        }
        ''')

class string_h(string_h):
    @c.cacheproperty
    def strerror_r(self):
        if not self.header:
            return None

        # Some implementations return a char* of the message.
        self.ctx.logger.check("checking strerror_r in 'string.h'")

        if self.builder.try_run('''
                #include <string.h>
                int main() {
                    char b[50];
                    int r = strerror_r(0, b, 50);
                    return r == 0 ? 0 : 1;
                }
                '''):
            self.ctx.logger.passed()
            return c.Function('int', 'int', 'char*', 'size_t')

        if self.builder.try_run('''
                #include <string.h>
                int main() {
                    char b[50];
                    char* r = strerror_r(0, b, 50);
                    return 0;
                }
                '''):
            self.ctx.logger.passed()
            return c.Function('char*', 'int', 'char*', 'size_t')

        self.ctx.logger.failed()

class sys_mman_h(sys_mman_h):
    MADV_DOFORK = c.macro_test()
    MADV_DONTFORK = c.macro_test()
    MADV_DONTNEED = c.macro_test()
    MADV_FREE = c.macro_test()
    MADV_NORMAL = c.macro_test()
    MADV_RANDOM = c.macro_test()
    MADV_REMOVE = c.macro_test()
    MADV_SEQUENTIAL = c.macro_test()
    MADV_WILLNEED = c.macro_test()
    MAP_32BIT = c.macro_test()
    MAP_ANON = c.macro_test()
    MAP_ANONYMOUS = c.macro_test()
    MAP_COPY = c.macro_test()
    MAP_DENYWRITE = c.macro_test()
    MAP_EXECUTABLE = c.macro_test()
    MAP_FILE = c.macro_test()
    MAP_GROWSDOWN = c.macro_test()
    MAP_HASSEMAPHORE = c.macro_test()
    MAP_LOCKED = c.macro_test()
    MAP_NOCACHE = c.macro_test()
    MAP_NOEXTEND = c.macro_test()
    MAP_NONBLOCK = c.macro_test()
    MAP_NORESERVE = c.macro_test()
    MAP_POPULATE = c.macro_test()
    MAP_RENAME = c.macro_test()
    MAP_SHARED = c.macro_test()
    MAP_TYPE = c.macro_test()
    MINCORE_INCORE = c.macro_test()
    MINCORE_MODIFIED = c.macro_test()
    MINCORE_MODIFIED_OTHER = c.macro_test()
    MINCORE_REFERENCED = c.macro_test()
    MINCORE_REFERENCED_OTHER = c.macro_test()
    MREMAP_FIXED = c.macro_test()
    MREMAP_MAYMOVE = c.macro_test()
    PROT_GROWSDOWN = c.macro_test()
    PROT_GROWSUP = c.macro_test()
    madvise = c.function_test('void*', 'size_t', 'int')
    mincore = c.function_test('int', 'const void*', 'size_t', 'char*')
    minherit = c.function_test('void*', 'size_t', 'int')

class sys_socket_h(sys_socket_h):
    @c.cacheproperty
    def accept(self):
        if not self.header:
            return

        if self.socklen_t:
            return super().accept

        # Some old implementations use 'int' or 'unsigned int' instead of
        # socklen_t.
        self.ctx.logger.check("checking accept in 'sys/socket.h'")
        for typename in 'unsigned int*', 'int*':
           if self.builder.try_run('''
                    #include <sys/socket.h>
                    int (*x)(int, struct sockaddr*, %s) = accept;
                    int main() { return 0; }
                    ''' % typename):
                self.ctx.logger.passed()
                return c.Function('int', 'int', 'struct sockaddr*', typename)

        self.ctx.logger.failed()

    @c.cacheproperty
    def bind(self):
        if not self.header:
            return

        if self.socklen_t:
            return super().bind

        # Some old implementations use 'int' or 'unsigned int' instead of
        # socklen_t.
        self.ctx.logger.check("checking bind in 'sys/socket.h'")
        for typename in 'unsigned int*', 'int*':
            if self.builder.try_run('''
                    #include <sys/socket.h>
                    int (*x)(int, const struct sockaddr*, %s) = bind;
                    int main() { return 0; }
                    ''' % typename):
                self.ctx.logger.passed()
                return c.Function('int', 'int', 'const struct sockaddr*', typename)

        self.ctx.logger.failed()

    @c.cacheproperty
    def connect(self):
        if not self.header:
            return

        if self.socklen_t:
            return super().connect

        # Some old implementations use 'int' or 'unsigned int' instead of
        # socklen_t.
        self.ctx.logger.check("checking connect in 'sys/socket.h'")
        for typename in 'unsigned int*', 'int*':
           if self.builder.try_run('''
                    #include <sys/socket.h>
                    int (*x)(int, const struct sockaddr*, %s) = connect;
                    int main() { return 0; }
                    ''' % typename):
                self.ctx.logger.passed()
                return c.Function('int', 'int', 'const struct sockaddr*', typename)

        self.ctx.logger.failed()

    @c.cacheproperty
    def getpeername(self):
        if not self.header:
            return

        if self.socklen_t:
            return super().getpeername

        # Some old implementations use 'int' or 'unsigned int' instead of
        # socklen_t.
        self.ctx.logger.check("checking getpeername in 'sys/socket.h'")
        for typename in 'unsigned int*', 'int*':
           if self.builder.try_run('''
                    #include <sys/socket.h>
                    int (*x)(int, struct sockaddr*, %s) = getpeername;
                    int main() { return 0; }
                    ''' % typename):
                self.ctx.logger.passed()
                return c.Function('int', 'int', 'struct sockaddr*', typename)

        self.ctx.logger.failed()

    @c.cacheproperty
    def getsockname(self):
        if not self.header:
            return

        if self.socklen_t:
            return super().getsockname

        # Some old implementations use 'int' or 'unsigned int' instead of
        # socklen_t.
        self.ctx.logger.check("checking getsockname in 'sys/socket.h'")
        for typename in 'unsigned int*', 'int*':
           if self.builder.try_run('''
                    #include <sys/socket.h>
                    int (*x)(int, struct sockaddr*, %s) = getsockname;
                    int main() { return 0; }
                    ''' % typename):
                self.ctx.logger.passed()
                return c.Function('int', 'int', 'struct sockaddr*', typename)

        self.ctx.logger.failed()

    @c.cacheproperty
    def getsockopt(self):
        if not self.header:
            return

        if self.socklen_t:
            return super().getsockopt

        # Some old implementations use 'int' or 'unsigned int' instead of
        # socklen_t.
        self.ctx.logger.check("checking getsockopt in 'sys/socket.h'")
        for typename in 'unsigned int*', 'int*':
           if self.builder.try_run('''
                    #include <sys/socket.h>
                    int (*x)(int, int, int, void*, %s) = getsockopt;
                    int main() { return 0; }
                    ''' % typename):
                self.ctx.logger.passed()
                return c.Function('int', 'int', 'int', 'void*', typename)

        self.ctx.logger.failed()

    @c.cacheproperty
    def recvfrom(self):
        if not self.header:
            return

        if self.socklen_t:
            return super().recvfrom

        # Some old implementations use 'int' or 'unsigned int' instead of
        # socklen_t.
        self.ctx.logger.check("checking recvfrom in 'sys/socket.h'")
        for typename in 'unsigned int*', 'int*':
           if self.builder.try_run('''
                    #include <sys/socket.h>
                    ssize_t (*x)(int, void*, size_t, int, struct sockaddr*, %s) = recvfrom;
                    int main() { return 0; }
                    ''' % typename):
                self.ctx.logger.passed()
                return c.Function('ssize_t', 'int', 'void*', 'size_t', 'int', 'struct sockaddr*', typename)

        self.ctx.logger.failed()

    @c.cacheproperty
    def sendto(self):
        if not self.header:
            return

        if self.socklen_t:
            return super().sendto

        # Some old implementations use 'int' or 'unsigned int' instead of
        # socklen_t.
        self.ctx.logger.check("checking sendto in 'sys/socket.h'")
        for typename in 'unsigned int*', 'int*':
           if self.builder.try_run('''
                    #include <sys/socket.h>
                    ssize_t (*x)(int, const void*, size_t, int, const struct sockaddr*, %s) = sendto;
                    int main() { return 0; }
                    ''' % typename):
                self.ctx.logger.passed()
                return c.Function('ssize_t', 'int', 'const void*', 'size_t', 'int', 'const struct sockaddr*', typename)

        self.ctx.logger.failed()

    @c.cacheproperty
    def setsockopt(self):
        if not self.header:
            return

        if self.socklen_t:
            return super().setsockopt

        # Some old implementations use 'int' or 'unsigned int' instead of
        # socklen_t.
        self.ctx.logger.check("checking setsockopt in 'sys/socket.h'")
        for typename in 'unsigned int*', 'int*':
           if self.builder.try_run('''
                    #include <sys/socket.h>
                    int (*x)(int, int, int, const void*, %s) = setsockopt;
                    int main() { return 0; }
                    ''' % typename):
                self.ctx.logger.passed()
                return c.Function('int', 'int', 'int', 'const void*', typename)

        self.ctx.logger.failed()

class sys_types_h(sys_types_h):
    u_int32_t = c.type_test()
    u_int64_t = c.type_test()

class unistd_h(unistd_h):
    mkstemps = c.function_test('int', 'char*', 'int')
    mkdtemp = c.function_test('char*', 'char*')
