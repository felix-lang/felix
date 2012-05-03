import fbuild.config.c as c
import fbuild.config.c.c90 as c90

# ------------------------------------------------------------------------------

class execinfo_h(c.Test):
    header = c.header_test('execinfo.h')

    backtrace = c.function_test('int', 'void**', 'int', test='''
        #include <execinfo.h>
        int main() {
            void* callstack[128];
            int frames = backtrace(callstack, 128);
            char** strs = backtrace_symbols(callstack, frames);
            return 0;
        }
        ''')
    backtrace_symbols = c.function_test('char**', 'void* const*', 'int',
        test=backtrace.test)
    backtrace_symbols_fd = c.function_test('void', 'void* const*', 'int', 'int')

class memory_h(c90.string_h):
    pass

class sys_dir_h(c.Test):
    header = c.header_test('sys/dir.h')

    DIR = c.type_test()

class sys_event_h(c.Test):
    header = c.header_test('sys/event.h')

    kqueue = c.function_test('int', 'void', test='''
        #include <sys/types.h>      // from the kqueue manpage
        #include <sys/event.h>      // kernel events
        #include <sys/time.h>       // timespec (kevent timeout)

        int main(int argc, char** argv) {
            int kq = kqueue();
            return (-1 == kq) ? 1 : 0;
        }
        ''')

class sys_ndir_h(c.Test):
    header = c.header_test('sys/ndir.h')

class sys_param_h(c.Test):
    header = c.header_test('sys/param.h')
