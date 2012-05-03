import tempfile

import fbuild.config.c as c
import fbuild.config.c.c99 as c99
import fbuild.config.c.posix01 as posix01
import fbuild.temp

# ------------------------------------------------------------------------------

class aio_h(c.Test):
    header = c.header_test('aio.h')

    aiocb = c.struct_test(
        ('int', 'aio_fildes'),
        ('off_t', 'aio_offset'),
        ('volatile void*', 'aio_buf'),
        ('size_t', 'aio_nbytes'),
        ('int', 'aio_reqprio'),
        ('struct sigevent', 'aio_sigevent'),
        ('int', 'aio_lio_opcode'))

    AIO_ALLDONE = c.variable_test()
    AIO_CANCELED = c.variable_test()
    AIO_NOTCANCELED = c.variable_test()
    LIO_NOP = c.variable_test()
    LIO_NOWAIT = c.variable_test()
    LIO_READ = c.variable_test()
    LIO_WAIT = c.variable_test()
    LIO_WRITE = c.variable_test()
    aio_cancel = c.function_test('int', 'int', 'struct aiocb*')
    aio_error = c.function_test('int', 'const struct aiocb*')
    aio_fsync = c.function_test('int', 'int, struct aiocb*')
    aio_read = c.function_test('int', 'struct aiocb*')
    aio_return = c.function_test('ssize_t', 'struct aiocb*')
    aio_suspend = c.function_test('int', 'const struct aiocb**', 'int', 'const struct timespec*')
    aio_write = c.function_test('int', 'struct aiocb*')
    lio_listio = c.function_test('int', 'int', 'struct aiocb* const', 'int', 'struct sigevent*')

# ------------------------------------------------------------------------------

class arpa_inet_h(c.Test):
    header = c.header_test('arpa/inet.h')

    in_port_t = c.type_test()
    in_addr_t = c.type_test()
    in_addr = c.struct_test(
        ('sa_family_t', 'sin_family'),
        ('in_port_t', 'sin_port'),
        ('struct in_addr', 'sin_addr'))
    INET_ADDRSTRLEN = c.macro_test()
    INET6_ADDRSTRLEN = c.macro_test()
    htonl = c.function_test('uint32_t', 'uint32_t')
    htons = c.function_test('uint16_t', 'uint16_t')
    ntohl = c.function_test('uint32_t', 'uint32_t')
    ntohs = c.function_test('uint16_t', 'uint16_t')
    inet_addr = c.function_test('in_addr_t', 'constchar*')
    inet_ntoa = c.function_test('char*', 'struct in_addr')
    inet_ntop = c.function_test('const char*', 'int', 'const void*', 'char*', 'socklen_t')
    inet_pton = c.function_test('int', 'int', 'const char*', 'void*')

# ------------------------------------------------------------------------------

assert_h = c99.assert_h
complex_h = c99.complex_h

# ------------------------------------------------------------------------------

class cpio_h(c.Test):
    header = c.header_test('cpio.h')

    C_IRUSR = c.variable_test()
    C_IWUSR = c.variable_test()
    C_IXUSR = c.variable_test()
    C_IRGRP = c.variable_test()
    C_IWGRP = c.variable_test()
    C_IXGRP = c.variable_test()
    C_IROTH = c.variable_test()
    C_IWOTH = c.variable_test()
    C_IXOTH = c.variable_test()
    C_ISUID = c.variable_test()
    C_ISGID = c.variable_test()
    C_ISVTX = c.variable_test()
    C_ISDIR = c.variable_test()
    C_ISFIFO = c.variable_test()
    C_ISREG = c.variable_test()
    C_ISBLK = c.variable_test()
    C_ISCHR = c.variable_test()
    C_ISCTG = c.variable_test()
    C_ISLNK = c.variable_test()
    C_ISSOCK = c.variable_test()
    MAGIC = c.variable_test()

# ------------------------------------------------------------------------------

class ctype_h(c99.ctype_h):
    isascii = c.function_test('int', 'int')
    toascii = c.function_test('int', 'int')
    _toupper = c.macro_test()
    _tolower = c.macro_test()

# ------------------------------------------------------------------------------

class dirent_h(c.Test):
    header = c.header_test('dirent.h')

    DIR = c.type_test()
    dirent = c.struct_test(
        ('ino_t', 'd_ino'),
        ('char*', 'd_name'))
    ino_t = c.type_test()
    closedir = c.function_test('int', 'DIR*', test='''
        #include <dirent.h>
        int main() {
            DIR* d = opendir(".");
            struct dirent* s;
            long l;

            if (!d) return 1;
            s = readdir(d);
            seekdir(d, 0);
            l = telldir(d);
            rewinddir(d);

            return closedir(d);
        }
        ''')

    @property
    def opendir(self):
        if self.closedir:
            return c.Function('DIR*', 'const char*')

    @property
    def readdir(self):
        if self.closedir:
            return c.Function('struct dirent*', 'DIR*')

    readdir_r = c.function_test('int', 'DIR*', 'struct dirent*', 'struct dirent**',
        test='''
        #include <dirent.h>
        int main() {
            DIR* d = opendir(".");
            struct dirent s;
            struct dirent* p;
            if (!d) return 1;
            if (readdir_r(d, &s, &p) != 0) return 1;
            return closedir(d);
        }
        ''')

    @property
    def rewinddir(self):
        if self.closedir:
            return c.Function('void', 'DIR*')

    @property
    def seekdir(self):
        if self.closedir:
            return c.Function('void', 'DIR*', 'long')

    @property
    def telldir(self):
        if self.closedir:
            return c.Function('long', 'DIR*')

# ------------------------------------------------------------------------------

class dlfcn_h(c.Test):
    header = c.header_test('dlfcn.h')

    RTLD_LAZY = c.macro_test()
    RTLD_NOW = c.macro_test()
    RTLD_GLOBAL = c.macro_test()
    RTLD_LOCAL = c.macro_test()

    def __init__(self, builder, shared, *args, **kwargs):
        """Overloaded because we need a compiler that can generate
        shared libraries in order to test dlfcn.h works."""
        super().__init__(builder, *args, **kwargs)

        self.shared = shared

    @property
    def dlclose(self):
        if self.dlopen:
            return c.Function('int', 'void*')

    @property
    def dlerror(self):
        if self.dlopen:
            return c.Function('char*', 'void')

    @c.cacheproperty
    def dlopen(self):
        if not self.header:
            return

        lib_code = '''
            #ifdef __cplusplus
            extern "C" {
            #endif
            int fred(int argc, char** argv) { return 0; }
            #ifdef __cplusplus
            }
            #endif
        '''

        exe_code = '''
            #include <dlfcn.h>
            #include <stdlib.h>

            int main(int argc, char** argv) {
                void* lib = dlopen("%s", RTLD_NOW);
                void* fred = 0;
                if(!lib) return 1;
                fred = dlsym(lib,"fred");
                if(!fred) return 1;
                return dlclose(lib) == 0 ? 0 : 1;
            }
        '''

        self.ctx.logger.check("checking dlopen in 'dlfcn.h'")

        with fbuild.temp.tempfile(lib_code, self.builder.src_suffix) as lib_src:
            try:
                obj = self.shared.uncached_compile(lib_src, quieter=1)
                lib = self.shared.uncached_link_lib(
                    lib_src.parent / 'temp', [obj],
                    quieter=1)
            except fbuild.ExecutionError:
                pass
            else:
                if self.builder.try_run(exe_code % lib,
                        lkwargs={
                            'flags': self.flags,
                            'libpaths': self.libpaths,
                            'libs': self.libs,
                            'external_libs': self.external_libs},
                        quieter=1):
                    self.ctx.logger.passed()
                    return c.Function('void*', 'const char*', 'int')

            self.ctx.logger.failed()
            return None

    @property
    def dlsym(self):
        if self.dlclose:
            return c.Function('void*', 'void*', 'const char*')

# ------------------------------------------------------------------------------

class errno_h(c99.errno_h):
    E2BIG = c.variable_test()
    EACCES = c.variable_test()
    EADDRINUSE = c.variable_test()
    EADDRNOTAVAIL = c.variable_test()
    EAFNOSUPPORT = c.variable_test()
    EAGAIN = c.variable_test()
    EWOULDBLOCK = c.variable_test()
    EALREADY = c.variable_test()
    EBADF = c.variable_test()
    EBADMSG = c.variable_test()
    EBUSY = c.variable_test()
    ECANCELED = c.variable_test()
    ECHILD = c.variable_test()
    ECONNABORTED = c.variable_test()
    ECONNREFUSED = c.variable_test()
    ECONNRESET = c.variable_test()
    EDEADLK = c.variable_test()
    EDESTADDRREQ = c.variable_test()
    EDQUOT = c.variable_test()
    EEXIST = c.variable_test()
    EFAULT = c.variable_test()
    EFBIG = c.variable_test()
    EHOSTUNREACH = c.variable_test()
    EIDRM = c.variable_test()
    EINPROGRESS = c.variable_test()
    EINTR = c.variable_test()
    EINVAL = c.variable_test()
    EIO = c.variable_test()
    EISCONN = c.variable_test()
    EISDIR = c.variable_test()
    ELOOP = c.variable_test()
    EMFILE = c.variable_test()
    EMLINK = c.variable_test()
    EMSGSIZE = c.variable_test()
    EMULTIHOP = c.variable_test()
    ENAMETOOLONG = c.variable_test()
    ENETDOWN = c.variable_test()
    ENETRESET = c.variable_test()
    ENETUNREACH = c.variable_test()
    ENFILE = c.variable_test()
    ENOBUFS = c.variable_test()
    ENODATA = c.variable_test()
    ENODEV = c.variable_test()
    ENOENT = c.variable_test()
    ENOEXEC = c.variable_test()
    ENOLCK = c.variable_test()
    ENOLINK = c.variable_test()
    ENOMEM = c.variable_test()
    ENOMSG = c.variable_test()
    ENOPROTOOPT = c.variable_test()
    ENOSPC = c.variable_test()
    ENOSP = c.variable_test()
    ENOSTP = c.variable_test()
    ENOSYS = c.variable_test()
    ENOTCONN = c.variable_test()
    ENOTDIR = c.variable_test()
    ENOTEMPTY = c.variable_test()
    ENOTSOCK = c.variable_test()
    ENOTSUP = c.variable_test()
    ENOTTY = c.variable_test()
    ENXIO = c.variable_test()
    EOPNOTSUPP = c.variable_test()
    EOVERFLOW = c.variable_test()
    EPERM = c.variable_test()
    EPIPE = c.variable_test()
    EPROTO = c.variable_test()
    EPROTONOSUPPORT = c.variable_test()
    EPROTOTYPE = c.variable_test()
    EROFS = c.variable_test()
    ESPIPE = c.variable_test()
    ESRCH = c.variable_test()
    ESTALE = c.variable_test()
    ETIME = c.variable_test()
    ETIMEDOUT = c.variable_test()
    ETXTBSY = c.variable_test()
    EWOULDBLOCK = c.variable_test()
    EXDEV = c.variable_test()

# ------------------------------------------------------------------------------

class fcntl_h(c.Test):
    header = c.header_test('fcntl.h')

    F_DUPFD = c.variable_test()
    F_GETFD = c.variable_test()
    F_SETFD = c.variable_test()
    F_GETFL = c.variable_test()
    F_SETFL = c.variable_test()
    F_GETLK = c.variable_test()
    F_SETLK = c.variable_test()
    F_SETLKW = c.variable_test()
    F_GETOWN = c.variable_test()
    F_SETOWN = c.variable_test()
    FD_CLOEXEC = c.variable_test()
    F_RDLCK = c.variable_test()
    F_UNLCK = c.variable_test()
    F_WRLCK = c.variable_test()
    O_CREAT = c.variable_test()
    O_EXCL = c.variable_test()
    O_NOCTTY = c.variable_test()
    O_TRUNC = c.variable_test()
    O_APPEND = c.variable_test()
    SIO = c.variable_test()
    O_NONBLOCK = c.variable_test()
    SIO = c.variable_test()
    O_SYNC = c.variable_test()
    O_ACCMODE = c.variable_test()
    O_RDONLY = c.variable_test()
    O_RDWR = c.variable_test()
    O_WRONLY = c.variable_test()
    mode_t = c.type_test()
    off_t = c.type_test()
    pid_t = c.type_test()
    POSIX_FADV_NORMAL = c.variable_test()
    POSIX_FADV_SEQUENTIAL = c.variable_test()
    POSIX_FADV_RANDOM = c.variable_test()
    POSIX_FADV_WILLNEED = c.variable_test()
    POSIX_FADV_DONTNEED = c.variable_test()
    POSIX_FADV_NOREUSE = c.variable_test()
    flock = c.struct_test(
        ('short', 'l_type'),
        ('short', 'l_whence'),
        ('off_t', 'l_start'),
        ('off_t', 'l_len'),
        ('pid_t', 'l_pid'))
    creat = c.function_test('int', 'const char*', 'mode_t')
    fcntl = c.function_test('int', 'int', 'int')

    @c.cacheproperty
    def open(self):
        with tempfile.NamedTemporaryFile() as f:
            test = '''
                #include <fcntl.h>
                int main() {
                    int fd;
                    if ((fd = open("%s", O_RDONLY)) == -1) return 1;
                    return 0;
                }
                ''' % f.name

            if self.builder.check_run(test, "checking open in 'fcntl.h'"):
                return c.function_test('int', 'const char*', 'int')

    posix_fadvise = c.function_test('int', 'int', 'off_t', 'off_t', 'int')
    posix_fallocate = c.function_test('int', 'int', 'off_t', 'off_t')

# ------------------------------------------------------------------------------

fenv_h = c99.fenv_h
float_h = c99.float_h

# ------------------------------------------------------------------------------

class fmtmsg_h(c.Test):
    header = c.header_test('fmtmsg.h')

# ------------------------------------------------------------------------------

class fnmatch_h(c.Test):
    header = c.header_test('fnmatch.h')

class ftw_h(c.Test):
    header = c.header_test('ftw.h')

# ------------------------------------------------------------------------------

class glob_h(c.Test):
    header = c.header_test('glob.h')

# ------------------------------------------------------------------------------

class grp_h(c.Test):
    header = c.header_test('grp.h')

# ------------------------------------------------------------------------------

class iconv_h(c.Test):
    header = c.header_test('iconv.h')

# ------------------------------------------------------------------------------

class inttypes_h(c99.inttypes_h):
    pass

# ------------------------------------------------------------------------------

iso646_h = c99.iso646_h

# ------------------------------------------------------------------------------

class langinfo_h(c.Test):
    header = c.header_test('langinfo.h')

# ------------------------------------------------------------------------------

libgen_h = posix01.libgen_h

# ------------------------------------------------------------------------------

class limits_h(c99.limits_h):
    pass

# ------------------------------------------------------------------------------

class locale_h(c99.locale_h):
    pass

# ------------------------------------------------------------------------------

class math_h(c99.math_h):
    pass

class monetary_h(c.Test):
    header = c.header_test('monetary.h')

class mqueue_h(c.Test):
    header = c.header_test('mqueue.h')

class ndbm_h(c.Test):
    header = c.header_test('ndbm.h')

class netdb_h(c.Test):
    header = c.header_test('netdb.h')

class netinet_in_h(c.Test):
    header = c.header_test('netinet/in.h')

class netinet_tcp_h(c.Test):
    header = c.header_test('netinet/tcp.h')

class nl_types_h(c.Test):
    header = c.header_test('nl_types.h')

class poll_h(c.Test):
    header = c.header_test('poll.h')

class pthread_h(c.Test):
    header = c.header_test('pthread.h')

    PTHREAD_BARRIER_SERIAL_THREAD = c.macro_test()
    PTHREAD_CANCEL_ASYNCHRONOUS = c.macro_test()
    PTHREAD_CANCEL_ENABLE = c.macro_test()
    PTHREAD_CANCEL_DEFERRED = c.macro_test()
    PTHREAD_CANCEL_DISABLE = c.macro_test()
    PTHREAD_CANCELED = c.macro_test()
    PTHREAD_COND_INITIALIZER = c.macro_test()
    PTHREAD_CREATE_DETACHED = c.macro_test()
    PTHREAD_CREATE_JOINABLE = c.macro_test()
    PTHREAD_EXPLICIT_SCHED = c.macro_test()
    PTHREAD_INHERIT_SCHED = c.macro_test()
    PTHREAD_MUTEX_DEFAULT = c.macro_test()
    PTHREAD_MUTEX_ERRORCHECK = c.macro_test()
    PTHREAD_MUTEX_INITIALIZER = c.macro_test()
    PTHREAD_MUTEX_NORMAL = c.macro_test()
    PTHREAD_MUTEX_RECURSIVE = c.macro_test()
    PTHREAD_ONCE_INIT = c.macro_test()
    PTHREAD_PRIO_INHERIT = c.macro_test()
    PTHREAD_PRIO_NONE = c.macro_test()
    PTHREAD_PRIO_PROTECT = c.macro_test()
    PTHREAD_PROCESS_SHARED = c.macro_test()
    PTHREAD_PROCESS_PRIVATE = c.macro_test()
    PTHREAD_SCOPE_PROCESS = c.macro_test()
    PTHREAD_SCOPE_SYSTEM = c.macro_test()
    pthread_attr_t = c.type_test()
    pthread_barrier_t = c.type_test()
    pthread_barrierattr_t = c.type_test()
    pthread_cond_t = c.type_test()
    pthread_condattr_t = c.type_test()
    pthread_key_t = c.type_test()
    pthread_mutex_t = c.type_test()
    pthread_mutexattr_t = c.type_test()
    pthread_once_t = c.type_test()
    pthread_rwlock_t = c.type_test()
    pthread_rwlockattr_t = c.type_test()
    pthread_spinlock_t = c.type_test()
    pthread_t = c.type_test()
    pthread_atfork = c.function_test('int', 'void (*)(void)', 'void (*)(void)', 'void (*)(void)')

    @property
    def pthread_attr_destroy(self):
        if self.pthread_create:
            return c.Function('int', 'pthread_attr_t*')

    pthread_attr_getdetachstate = c.function_test('int', 'const pthread_attr_t*', 'int*')
    pthread_attr_getguardsize = c.function_test('int', 'const pthread_attr_t*', 'size_t*')
    pthread_attr_getinheritsched = c.function_test('int', 'const pthread_attr_t*', 'int*')
    pthread_attr_getschedparam = c.function_test('int', 'const pthread_attr_t*', 'struct sched_param*')
    pthread_attr_getschedpolicy = c.function_test('int', 'const pthread_attr_t*', 'int*')
    pthread_attr_getscope = c.function_test('int', 'const pthread_attr_t*', 'int*')
    pthread_attr_getstack = c.function_test('int', 'const pthread_attr_t*', 'void**', 'size_t*')
    pthread_attr_getstackaddr = c.function_test('int', 'const pthread_attr_t*', 'void**')
    pthread_attr_getstacksize = c.function_test('int', 'const pthread_attr_t*', 'size_t*')

    @property
    def pthread_attr_init(self):
        if self.pthread_create:
            return c.Function('int', 'pthread_attr_t*')

    @property
    def pthread_attr_setdetachstate(self):
        if self.pthread_create:
            return c.Function('int', 'pthread_attr_t*', 'int')

    pthread_attr_setguardsize = c.function_test('int', 'pthread_attr_t*', 'size_t')
    pthread_attr_setinheritsched = c.function_test('int', 'pthread_attr_t*', 'int')
    pthread_attr_setschedparam = c.function_test('int', 'pthread_attr_t*', 'const struct sched_param*')
    pthread_attr_setschedpolicy = c.function_test('int', 'pthread_attr_t*', 'int')
    pthread_attr_setscope = c.function_test('int', 'pthread_attr_t*', 'int')
    pthread_attr_setstack = c.function_test('int', 'pthread_attr_t*', 'void*', 'size_t')
    pthread_attr_setstackaddr = c.function_test('int', 'pthread_attr_t*', 'void*')
    pthread_attr_setstacksize = c.function_test('int', 'pthread_attr_t*', 'size_t')

    @property
    def pthread_barrier_destroy(self):
        if self.pthread_barrier_init:
            return c.Function('int', 'pthread_barrier_t*')

    pthread_barrier_init = c.function_test('int', 'pthread_barrier_t*', 'const pthread_barrierattr_t*', 'unsigned', test='''
        #include <pthread.h>
        int main() {
            pthread_barrier_t barrier;
            if (pthread_barrier_init(&barrier, 0, 1) != 0) return 1;
            if (pthread_barrier_wait(&barrier) != PTHREAD_BARRIER_SERIAL_THREAD) return 1;
            if (pthread_barrier_destroy(&barrier) != 0) return 1;
            return 0;
        }
    ''')

    @property
    def pthread_barrier_wait(self):
        if self.pthread_barrier_init:
            return c.Function('int', 'pthread_barrier_t*')

    pthread_barrierattr_destroy = c.function_test('int', 'pthread_barrierattr_t*')
    pthread_barrierattr_getpshared = c.function_test('int', 'const pthread_barrierattr_t*', 'int*')
    pthread_barrierattr_init = c.function_test('int', 'pthread_barrierattr_t*')
    pthread_barrierattr_setpshared = c.function_test('int', 'pthread_barrierattr_t*', 'int')
    pthread_cancel = c.function_test('int', 'pthread_t')
    pthread_cleanup_push = c.function_test('void', 'void (*)(void*)', 'void*')
    pthread_cleanup_pop = c.function_test('void', 'int')

    @property
    def pthread_cond_broadcast(self):
        if self.pthread_cond_init:
            return c.Function('int', 'pthread_cond_t*')

    @property
    def pthread_cond_destroy(self):
        if self.pthread_cond_init:
            return c.Function('int', 'pthread_cond_t*')

    pthread_cond_init = c.function_test('int', 'pthread_cond_t*', 'const pthread_condattr_t*', test='''
        #include <pthread.h>
        int main() {
            pthread_cond_t cond = PTHREAD_COND_INITIALIZER;
            pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
            struct timespec t = { 0, 0 };
            if (pthread_mutex_init(&mutex, 0) != 0) return 1;
            if (pthread_mutex_lock(&mutex) != 0) return 1;
            if (pthread_cond_init(&cond, 0) != 0) return 1;
            if (pthread_cond_broadcast(&cond) != 0) return 1;
            if (pthread_cond_signal(&cond) != 0) return 1;
            if (pthread_cond_timedwait(&cond, &mutex, &t) != 0) return 1;
            if (pthread_cond_wait(&cond, &mutex) != 0) return 1;
            if (pthread_cond_destroy(&cond) != 0) return 1;
            return 0;
        }
        ''')

    @property
    def pthread_cond_signal(self):
        if self.pthread_cond_init:
            return c.Function ('int', 'pthread_cond_t*')

    @property
    def pthread_cond_timedwait(self):
        if self.pthread_cond_init:
            return c.Function('int', 'pthread_cond_t*', 'pthread_mutex_t*', 'const struct timespec*')

    @property
    def pthread_cond_wait(self):
        if self.pthread_cond_init:
            return c.Function('int', 'pthread_cond_t*', 'pthread_mutex_t*')

    pthread_condattr_destroy = c.function_test('int', 'pthread_condattr_t*')
    pthread_condattr_getclock = c.function_test('int', 'const pthread_condattr_t*', 'clockid_t*')
    pthread_condattr_getpshared = c.function_test('int', 'const pthread_condattr_t*', 'int*')
    pthread_condattr_init = c.function_test('int', 'pthread_condattr_t*')
    pthread_condattr_setclock = c.function_test('int', 'pthread_condattr_t*', 'clockid_t')
    pthread_condattr_setpshared = c.function_test('int', 'pthread_condattr_t*', 'int')
    pthread_create = c.function_test('int', 'pthread_t*', 'const pthread_attr_t*', 'void* (*)(void*)', 'void*', test='''
        #include <pthread.h>

        void* start(void* data) { return NULL; }

        int main(int argc, char** argv) {
            pthread_t thr;
            pthread_attr_t attr;
            pthread_attr_init(&attr);
            pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
            int res = pthread_create(&thr, &attr, start, NULL);
            pthread_attr_destroy(&attr);
            return res;
        }
        ''')
    pthread_detach = c.function_test('int', 'pthread_t')
    pthread_equal = c.function_test('int', 'pthread_t', 'pthread_t')
    pthread_exit = c.function_test('void', 'void*')
    pthread_getconcurrency = c.function_test('int', 'void')
    pthread_getcpuclockid = c.function_test('int', 'pthread_t', 'clockid_t*')
    pthread_getschedparam = c.function_test('int', 'pthread_t', 'int*', 'struct sched_param*')
    pthread_getspecific = c.function_test('void*', 'pthread_key_t')
    pthread_join = c.function_test('int', 'pthread_t', 'void**')
    pthread_key_create = c.function_test('int', 'pthread_key_t*', 'void (*)(void*)')
    pthread_key_delete = c.function_test('int', 'pthread_key_t')

    @property
    def pthread_mutex_destroy(self):
        if self.pthread_mutex_init:
            return c.Function('int', 'pthread_mutex_t*')

    pthread_mutex_getprioceiling = c.function_test('int', 'const pthread_mutex_t*', 'int*')

    pthread_mutex_init = c.function_test('int', 'pthread_mutex_t*', 'const pthread_mutexattr_t*', test='''
        #include <pthread.h>
        int main() {
            pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
            if (pthread_mutex_init(&mutex, 0) != 0) return 1;
            if (pthread_mutex_lock(&mutex) != 0) return 1;
            if (pthread_mutex_unlock(&mutex) != 0) return 1;
            if (pthread_mutex_destroy(&mutex) != 0) return 1;
            return 0;
        }
        ''')

    def pthread_mutex_lock(self):
        if self.pthread_mutex_init:
            return c.Function('int', 'pthread_mutex_t*')

    pthread_mutex_setprioceiling = c.function_test('int', 'pthread_mutex_t*', 'int', 'int*')

    pthread_mutex_timedlock = c.function_test('int', 'pthread_mutex_t*', 'const struct timespec*', test='''
        #include <pthread.h>
        int main() {
            pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
            struct timespec t = { 0, 0 };
            if (pthread_mutex_init(&mutex, 0) != 0) return 1;
            if (pthread_mutex_timedlock(&mutex, &t) != 0) return 1;
            if (pthread_mutex_unlock(&mutex) != 0) return 1;
            if (pthread_mutex_destroy(&mutex) != 0) return 1;
            return 0;
        }
        ''')

    pthread_mutex_trylock = c.function_test('int', 'pthread_mutex_t*', test='''
        #include <pthread.h>
        int main() {
            pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
            if (pthread_mutex_init(&mutex, 0) != 0) return 1;
            if (pthread_mutex_trylock(&mutex) != 0) return 1;
            if (pthread_mutex_unlock(&mutex) != 0) return 1;
            if (pthread_mutex_destroy(&mutex) != 0) return 1;
            return 0;
        }
        ''')

    def pthread_mutex_unlock(self):
        if self.pthread_mutex_init:
            return c.Function('int', 'pthread_mutex_t*')

    pthread_mutexattr_destroy = c.function_test('int', 'pthread_mutexattr_t*')
    pthread_mutexattr_getprioceiling = c.function_test('int', 'const pthread_mutexattr_t*', 'int*')
    pthread_mutexattr_getprotocol = c.function_test('int', 'const pthread_mutexattr_t*', 'int*')
    pthread_mutexattr_getpshared = c.function_test('int', 'const pthread_mutexattr_t*', 'int*')
    pthread_mutexattr_gettype = c.function_test('int', 'const pthread_mutexattr_t*', 'int*')
    pthread_mutexattr_init = c.function_test('int', 'pthread_mutexattr_t*')
    pthread_mutexattr_setprioceiling = c.function_test('int', 'pthread_mutexattr_t*', 'int')
    pthread_mutexattr_setprotocol = c.function_test('int', 'pthread_mutexattr_t*', 'int')
    pthread_mutexattr_setpshared = c.function_test('int', 'pthread_mutexattr_t*', 'int')
    pthread_mutexattr_settype = c.function_test('int', 'pthread_mutexattr_t*', 'int')
    pthread_once = c.function_test('int', 'pthread_once_t*', 'void (*)(void)')

    @property
    def pthread_rwlock_destroy(self):
        if self.pthread_rwlock_init:
            return c.Function('int', 'pthread_rwlock_t*')

    pthread_rwlock_init = c.function_test('int', 'pthread_rwlock_t*', 'const pthread_rwlockattr_t*', test='''
        #include <pthread.h>
        int main() {
            pthread_rwlock_t rwlock = PTHREAD_RWLOCK_INITIALIZER;
            if (pthread_rwlock_init(&rwlock, 0) != 0) return 1;
            if (pthread_rwlock_rdlock(&rwlock) != 0) return 1;
            if (pthread_rwlock_unlock(&rwlock) != 0) return 1;
            if (pthread_rwlock_wrlock(&rwlock) != 0) return 1;
            if (pthread_rwlock_unlock(&rwlock) != 0) return 1;
            if (pthread_rwlock_destroy(&rwlock) != 0) return 1;
            return 0;
        }
        ''')

    @property
    def pthread_rwlock_rdlock(self):
        if self.pthread_rwlock_init:
            return c.Function('int', 'pthread_rwlock_t*')

    pthread_rwlock_timedrdlock = c.function_test('int', 'pthread_rwlock_t*', 'const struct timespec*', test='''
        #include <pthread.h>
        int main() {
            pthread_rwlock_t rwlock = PTHREAD_RWLOCK_INITIALIZER;
            struct timespec t = { 0, 0 };
            if (pthread_rwlock_init(&rwlock, 0) != 0) return 1;
            if (pthread_rwlock_timedrdlock(&rwlock, &t) != 0) return 1;
            if (pthread_rwlock_unlock(&rwlock) != 0) return 1;
            if (pthread_rwlock_destroy(&rwlock) != 0) return 1;
            return 0;
        }
        ''')

    pthread_rwlock_timedwrlock = c.function_test('int', 'pthread_rwlock_t*', 'const struct timespec*', test='''
        #include <pthread.h>
        int main() {
            pthread_rwlock_t rwlock = PTHREAD_RWLOCK_INITIALIZER;
            struct timespec t = { 0, 0 };
            if (pthread_rwlock_init(&rwlock, 0) != 0) return 1;
            if (pthread_rwlock_timedwrlock(&rwlock, &t) != 0) return 1;
            if (pthread_rwlock_unlock(&rwlock) != 0) return 1;
            if (pthread_rwlock_destroy(&rwlock) != 0) return 1;
            return 0;
        }
        ''')

    pthread_rwlock_tryrdlock = c.function_test('int', 'pthread_rwlock_t*', test='''
        #include <pthread.h>
        int main() {
            pthread_rwlock_t rwlock = PTHREAD_RWLOCK_INITIALIZER;
            if (pthread_rwlock_init(&rwlock, 0) != 0) return 1;
            if (pthread_rwlock_tryrdlock(&rwlock) != 0) return 1;
            if (pthread_rwlock_unlock(&rwlock) != 0) return 1;
            if (pthread_rwlock_destroy(&rwlock) != 0) return 1;
            return 0;
        }
        ''')

    pthread_rwlock_trywrlock = c.function_test('int', 'pthread_rwlock_t*', test='''
        #include <pthread.h>
        int main() {
            pthread_rwlock_t rwlock = PTHREAD_RWLOCK_INITIALIZER;
            if (pthread_rwlock_init(&rwlock, 0) != 0) return 1;
            if (pthread_rwlock_trywrlock(&rwlock) != 0) return 1;
            if (pthread_rwlock_unlock(&rwlock) != 0) return 1;
            if (pthread_rwlock_destroy(&rwlock) != 0) return 1;
            return 0;
        }
        ''')

    @property
    def pthread_rwlock_unlock(self):
        if self.pthread_rwlock_init:
            return c.Function('int', 'pthread_rwlock_t*')

    @property
    def pthread_rwlock_wrlock(self):
        if self.pthread_rwlock_init:
            return c.Function('int', 'pthread_rwlock_t*')

    pthread_rwlockattr_destroy = c.function_test('int', 'pthread_rwlockattr_t*')
    pthread_rwlockattr_getpshared = c.function_test('int', 'const pthread_rwlockattr_t*', 'int*')
    pthread_rwlockattr_init = c.function_test('int', 'pthread_rwlockattr_t*')
    pthread_rwlockattr_setpshared = c.function_test('int', 'pthread_rwlockattr_t*', 'int')
    pthread_self = c.function_test('pthread_t', 'void')
    pthread_setcancelstate = c.function_test('int', 'int', 'int*')
    pthread_setcanceltype = c.function_test('int', 'int', 'int*')
    pthread_setconcurrency = c.function_test('int', 'int')
    pthread_setschedparam = c.function_test('int', 'pthread_t', 'int', 'const struct sched_param*')
    pthread_setschedprio = c.function_test('int', 'pthread_t', 'int')
    pthread_setspecific = c.function_test('int', 'pthread_key_t', 'const void*')

    @property
    def pthread_spin_destroy(self):
        if self.pthread_spin_init:
            return c.Function('int', 'pthread_spinlock_t*')

    pthread_spin_init = c.function_test('int', 'pthread_spinlock_t*', 'int', test='''
        #include <pthread.h>
        int main() {
            pthread_spinlock_t spin;
            if (pthread_spin_init(&spin, 0) != 0) return 1;

            if (pthread_spin_lock(&spin) != 0) return 1;
            if (pthread_spin_unlock(&spin) != 0) return 1;

            if (pthread_spin_trylock(&spin) != 0) return 1;
            if (pthread_spin_unlock(&spin) != 0) return 1;

            if (pthread_spin_destroy(&spin) != 0) return 1;
            return 0;
        }
        ''')

    @property
    def pthread_spin_lock(self):
        if self.pthread_spin_init:
            return c.Function('int', 'pthread_spinlock_t*')

    @property
    def pthread_spin_trylock(self):
        if self.pthread_spin_init:
            return c.Function('int', 'pthread_spinlock_t*')

    @property
    def pthread_spin_unlock(self):
        if self.pthread_spin_init:
            return c.Function('int', 'pthread_spinlock_t*')

    pthread_testcancel = c.function_test('void', 'void')

class pwd_h(c.Test):
    header = c.header_test('pwd.h')

class regex_h(c.Test):
    header = c.header_test('regex.h')

class sched_h(c.Test):
    header = c.header_test('sched.h')

class search_h(c.Test):
    header = c.header_test('search.h')

class semaphore_h(c.Test):
    header = c.header_test('semaphore.h')

class setjmp_h(c99.setjmp_h):
    sigjmp_buf = c.type_test()

    siglongjmp = c.function_test('void', 'sigjmp_buf', 'int', test='''
        #include <setjmp.h>
        int main() {
            jmp_buf env;
            int i = sigsetjmp(env, 0);
            if (i == 2) return 0;
            siglongjmp(env, 2);
            return 2;
        }
        ''')

    @property
    def sigsetjmp(self):
        if self.siglongjmp:
            return c.Function('int', 'sigjmp_buf', 'int')

class signal_h(c99.signal_h):
    pass

class spawn_h(c.Test):
    header = c.header_test('spawn.h')

class stdarg_h(c99.stdarg_h):
    pass

stdbool_h = c99.stdbool_h
stddef_h = c99.stddef_h

class stdint_h(c99.stdint_h):
    pass

class stdio_h(c99.stdio_h):
    pass

class stdlib_h(c99.stdlib_h):
    drand48 = c.function_test('double', 'void')
    lrand48 = c.function_test('long', 'void')
    mkstemp = c.function_test('int', 'char*', test='''
        #include <stdlib.h>
        #include <unistd.h>
        int main() {
            char s[] = "XXXXXX";
            int fd;
            if ((fd = mkstemp(s)) == -1) return 1;
            if (close(fd) == -1) return 1;
            return 0;
        }
        ''')
    realpath = c.function_test('char*', 'const char*', 'char*')
    srand = c.function_test('void', 'unsigned int')
    srand48 = c.function_test('void', 'long')

class string_h(c99.string_h):
    strdup = c.function_test('char*', 'const char*')
    strerror_r = c.function_test('int', 'int', 'char*', 'size_t', test='''
        #include <string.h>
        int main() {
            char b[50];
            int r = strerror_r(0, b, 50);
            return r == 0 ? 0 : 1;
        }
        ''')

class strings_h(c.Test):
    header = c.header_test('strings.h')

class stropts_h(c.Test):
    header = c.header_test('stropts.h')

class sys_ipc_h(c.Test):
    header = c.header_test('sys/ipc.h')

class sys_mman_h(c.Test):
    header = c.header_test('sys/mman.h')

    MAP_FAILED = c.macro_test()
    MAP_FIXED = c.macro_test()
    MAP_PRIVATE = c.macro_test()
    MAP_SHARED = c.macro_test()
    MCL_CURRENT = c.macro_test()
    MCL_FUTURE = c.macro_test()
    MS_ASYNC = c.macro_test()
    MS_INVALIDATE = c.macro_test()
    MS_SYNC = c.macro_test()
    POSIX_MADV_DONTNEED = c.macro_test()
    POSIX_MADV_NORMAL = c.macro_test()
    POSIX_MADV_RANDOM = c.macro_test()
    POSIX_MADV_SEQUENTIAL = c.macro_test()
    POSIX_MADV_WILLNEED = c.macro_test()
    POSIX_TYPED_MEM_ALLOCATE = c.macro_test()
    POSIX_TYPED_MEM_ALLOCATE_CONTIG = c.macro_test()
    POSIX_TYPED_MEM_MAP_ALLOCATABLE = c.macro_test()
    PROT_EXEC = c.macro_test()
    PROT_NONE = c.macro_test()
    PROT_READ = c.macro_test()
    PROT_WRITE = c.macro_test()
    mode_t = c.type_test()
    off_t = c.type_test()
    size_t = c.type_test()
    posix_typed_mem_info = c.struct_test(
        ('size_t', 'posix_tmi_length'))
    mlock = c.function_test('int', 'const void*', 'size_t')
    mlockall = c.function_test('int', 'int')
    mmap = c.function_test('void*', 'size_t', 'int', 'int', 'int', 'off_t')
    mprotect = c.function_test('int', 'void*', 'size_t', 'int')
    msync = c.function_test('int', 'void*', 'size_t', 'int')
    munlock = c.function_test('int', 'const void*', 'size_t')
    munlockall = c.function_test('int', 'void')
    munmap = c.function_test('int', 'void*', 'size_t')
    posix_madvise = c.function_test('int', 'void*', 'size_t', 'int')
    posix_mem_offset = c.function_test('int', 'const void*', 'size_t', 'off_t*', 'size_t*', 'int*')
    posix_typed_mem_get_info = c.function_test('int', 'int', 'struct posix_typed_mem_info*')
    posix_typed_mem_open = c.function_test('int', 'const char*', 'int', 'int')
    shm_open = c.function_test('int', 'const char*', 'int', 'mode_t')
    shm_unlink = c.function_test('int', 'const char*')

class sys_msg_h(c.Test):
    header = c.header_test('sys/msg.h')

class sys_resource_h(c.Test):
    header = c.header_test('sys/resource.h')

    getrlimit = c.function_test('int', 'int', 'struct rlimit*')
    getrusage = c.function_test('int', 'int', 'struct rusage*')
    setrlimit = c.function_test('int', 'int', 'struct rlimit*')

class sys_select_h(c.Test):
    header = c.header_test('sys/select.h')

class sys_sem_h(c.Test):
    header = c.header_test('sys/sem.h')

class sys_shm_h(c.Test):
    header = c.header_test('sys/shm.h')

class sys_socket_h(c.Test):
    header = c.header_test('sys/socket.h')

    socklen_t = c.int_type_test()
    sa_family_t = c.int_type_test()
    sockaddr = c.struct_test(
        ('sa_family_t', 'sa_family'),
        ('char*', 'sa_data'))
    sockaddr_storage = c.struct_test(
        ('sa_family_t', 'ss_family'))
    msghdr = c.struct_test(
        ('void*', 'msg_name'),
        ('socklen_t', 'msg_namelen'),
        ('struct iovec*', 'msg_iov'),
        ('int', 'msg_iovlen'),
        ('void*', 'msg_control'),
        ('socklen_t', 'msg_controllen'),
        ('int', 'msg_flags'))
    cmsghdr = c.struct_test(
        ('socklen_t', 'cmsg_len'),
        ('int', 'cmsg_level'),
        ('int', 'cmsg_type'))
    SCM_RIGHTS = c.macro_test()
    CMSG_DATA = c.macro_test()
    CMSG_NXTHDR = c.macro_test()
    CMSG_FIRSTHDR = c.macro_test()
    linger = c.struct_test(
        ('int', 'l_onoff'),
        ('int', 'l_linger'))
    SOCK_DGRAM = c.macro_test()
    SOCK_RAW = c.macro_test()
    SOCK_SEQPACKET = c.macro_test()
    SOCK_STREAM = c.macro_test()
    SOL_SOCKET = c.macro_test()
    SO_ACCEPTCONN = c.macro_test()
    SO_BROADCAST = c.macro_test()
    SO_DEBUG = c.macro_test()
    SO_DONTROUTE = c.macro_test()
    SO_ERROR = c.macro_test()
    SO_KEEPALIVE = c.macro_test()
    SO_LINGER = c.macro_test()
    SO_OOBINLINE = c.macro_test()
    SO_RCVBUF = c.macro_test()
    SO_RCVLOWAT = c.macro_test()
    SO_RCVTIMEO = c.macro_test()
    SO_REUSEADDR = c.macro_test()
    SO_SNDBUF = c.macro_test()
    SO_SNDLOWAT = c.macro_test()
    SO_SNDTIMEO = c.macro_test()
    SO_TYPE = c.macro_test()
    SOMAXCONN = c.macro_test()
    MSG_CTRUNC = c.macro_test()
    MSG_DONTROUTE = c.macro_test()
    MSG_EOR = c.macro_test()
    MSG_OOB = c.macro_test()
    MSG_PEEK = c.macro_test()
    MSG_TRUNC = c.macro_test()
    MSG_WAITALL = c.macro_test()
    AF_INET = c.macro_test()
    AF_INET6 = c.macro_test()
    AF_UNIX = c.macro_test()
    AF_UNSPEC = c.macro_test()
    SHUT_RD = c.macro_test()
    SHUT_RDWR = c.macro_test()
    SHUT_WR = c.macro_test()
    accept = c.function_test('int', 'int', 'struct sockaddr*', 'socklen_t*')
    bind = c.function_test('int', 'int', 'const struct sockaddr*', 'socklen_t')
    connect = c.function_test('int', 'int', 'const struct sockaddr*', 'socklen_t')
    getpeername = c.function_test('int', 'int', 'struct sockaddr*', 'socklen_t*')
    getsockname = c.function_test('int', 'int', 'struct sockaddr*', 'socklen_t*')
    getsockopt = c.function_test('int', 'int', 'int', 'int', 'void*', 'socklen_t*')
    listen = c.function_test('int', 'int', 'int')
    recv = c.function_test('ssize_t', 'int', 'void*', 'size_t', 'int')
    recvfrom = c.function_test('ssize_t', 'int', 'void*', 'size_t', 'int', 'struct sockaddr*', 'socklen_t*')
    recvmsg = c.function_test('ssize_t', 'int', 'struct msghdr*', 'int')
    send = c.function_test('ssize_t', 'int', 'const void*', 'size_t', 'int')
    sendmsg = c.function_test('ssize_t', 'int', 'const struct msghdr*', 'int')
    sendto = c.function_test('ssize_t', 'int', 'const void*', 'size_t', 'int', 'const struct sockaddr*', 'socklen_t')
    setsockopt = c.function_test('int', 'int', 'int', 'int', 'const void*', 'socklen_t')
    shutdown = c.function_test('int', 'int', 'int')
    socket = c.function_test('int', 'int', 'int', 'int')
    sockatmark = c.function_test('int', 'int')
    socketpair = c.function_test('int', 'int', 'int', 'int', 'int[2]', test='''
        #include <sys/socket.h>
        int main() {
            int arg_0;
            int arg_1;
            int arg_2;
            int arg_3[2];
            int res = socketpair(arg_0, arg_1, arg_2, arg_3);
            return 0;
        }
        ''')

class sys_stat_h(c.Test):
    header = c.header_test('sys/stat.h')

class sys_statvfs_h(c.Test):
    header = c.header_test('sys/statvfs.h')

class sys_time_h(c.Test):
    header = c.header_test('sys/time.h')

    gettimeofday = c.function_test('int', 'struct timeval*', 'void*', test='''
        #include <sys/time.h>
        int main() {
            struct timeval tp;
            return gettimeofday(&tp, 0) == 0 ? 0 : 1;
        }
        ''')
    settimeofday = c.function_test('int', 'struct timeval*', 'void*', test='''
        #include <sys/time.h>
        /* Don't actually call settimeofday because it modifies the global
         * time. */
        void foo() { settimeofday(0, 0); }
        int main() { return 0; }
        ''')

class sys_timeb_h(c.Test):
    header = c.header_test('sys/timeb.h')

class sys_times_h(c.Test):
    header = c.header_test('sys/times.h')

class sys_types_h(c.Test):
    header = c.header_test('sys/types.h')

    blkcnt_t = c.int_type_test()
    blksize_t = c.int_type_test()
    clock_t = c.type_test()
    clockid_t = c.int_type_test()
    dev_t = c.int_type_test()
    fsblkcnt_t = c.int_type_test()
    fsfilcnt_t = c.int_type_test()
    gid_t = c.int_type_test()
    id_t = c.int_type_test()
    key_t = c.type_test()
    mode_t = c.int_type_test()
    nlink_t = c.int_type_test()
    off_t = c.int_type_test()
    pid_t = c.int_type_test()
    pthread_attr_t = c.type_test()
    pthread_barrier_t = c.type_test()
    pthread_barrierattr_t = c.type_test()
    pthread_cond_t = c.type_test()
    pthread_condattr_t = c.type_test()
    pthread_mutex_t = c.type_test()
    pthread_mutexattr_t = c.type_test()
    pthread_rwlock_t = c.type_test()
    pthread_rwlockattr_t = c.type_test()
    pthread_spinlock_t = c.type_test()
    pthread_t = c.type_test()
    size_t = c.int_type_test()
    ssize_t = c.int_type_test()
    suseconds_t = c.int_type_test()
    time_t = c.type_test()
    timer_t = c.int_type_test()
    trace_attr_t = c.type_test()
    trace_event_id_t = c.type_test()
    trace_event_set_t = c.type_test()
    trace_id_t = c.type_test()
    uid_t = c.int_type_test()
    useconds_t = c.int_type_test()

class sys_uio_h(c.Test):
    header = c.header_test('sys/uio.h')

class sys_un_h(c.Test):
    header = c.header_test('sys/un.h')

class sys_utsname_h(c.Test):
    header = c.header_test('sys/utsname.h')

class sys_wait_h(c.Test):
    header = c.header_test('sys/wait.h')

class syslog_h(c.Test):
    header = c.header_test('syslog.h')

class tar_h(c.Test):
    header = c.header_test('tar.h')

class termios_h(c.Test):
    header = c.header_test('termios.h')

tgmath_h = c99.tgmath_h

class time_h(c99.time_h):
    pass

class trace_h(c.Test):
    header = c.header_test('trace.h')

class ucontext_h(c.Test):
    header = c.header_test('ucontext.h')

class ulimit_h(c.Test):
    header = c.header_test('ulimit.h')

class unistd_h(c.Test):
    header = c.header_test('unistd.h')

    getcwd = c.function_test('char*', 'char*', 'size_t')
    getopt = c.function_test('int', 'int', 'char**', 'char*', test='''
        #include <unistd.h>
        int main(int argc, char** argv) {
            int ch, ret = 0;
            while ((ch = getopt(argc, argv, "f")) != -1) {
                switch (ch) {
                case 'f':
                    break;
                default:
                    ret = 1;
                }
            }

            return ret;
        }
        ''')
    getpagesize = c.function_test('int', 'void')
    isatty = c.function_test('int', 'int')
    sysconf = c.function_test('long', 'int', default_args=('0',))
    ttyname = c.function_test('char*', 'int')

class utime_h(c.Test):
    header = c.header_test('utime.h')

class utmpx_h(c.Test):
    header = c.header_test('utmpx.h')

class wchar_h(c99.wchar_h):
    pass

class wctype_h(c99.wctype_h):
    iswalnum = c.function_test('int', 'wint_t')
    iswalpha = c.function_test('int', 'wint_t')
    iswascii = c.function_test('int', 'wint_t')
    iswblank = c.function_test('int', 'wint_t')
    iswcntrl = c.function_test('int', 'wint_t')
    iswctype = c.function_test('int', 'wint_t', 'wctype_t')
    iswdigit = c.function_test('int', 'wint_t')
    iswgraph = c.function_test('int', 'wint_t')
    iswhexnumber = c.function_test('int', 'wint_t')
    iswideogram = c.function_test('int', 'wint_t')
    iswlower = c.function_test('int', 'wint_t')
    iswnumber = c.function_test('int', 'wint_t')
    iswphonogram = c.function_test('int', 'wint_t')
    iswprint = c.function_test('int', 'wint_t')
    iswpunct = c.function_test('int', 'wint_t')
    iswrune = c.function_test('int', 'wint_t')
    iswspace = c.function_test('int', 'wint_t')
    iswspecial = c.function_test('int', 'wint_t')
    iswupper = c.function_test('int', 'wint_t')
    iswxdigit = c.function_test('int', 'wint_t')

class wordexp_h(c.Test):
    header = c.header_test('wordexp.h')
