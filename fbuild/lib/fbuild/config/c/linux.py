import fbuild.config.c as c

# ------------------------------------------------------------------------------

class sys_epoll_h(c.Test):
    header = c.header_test('sys/epoll.h')

    EPOLLIN = c.macro_test()
    EPOLLPRI = c.macro_test()
    EPOLLOUT = c.macro_test()
    EPOLLRDNORM = c.macro_test()
    EPOLLRDBAND = c.macro_test()
    EPOLLWRNORM = c.macro_test()
    EPOLLWRBAND = c.macro_test()
    EPOLLMSG = c.macro_test()
    EPOLLERR = c.macro_test()
    EPOLLHUP = c.macro_test()
    EPOLLONESHOT = c.macro_test()
    EPOLLET = c.macro_test()
    EPOLL_CTL_ADD = c.macro_test()
    EPOLL_CTL_DEL = c.macro_test()
    EPOLL_CTL_MOD = c.macro_test()
    epoll_data_t = c.type_test()
    epoll_event = c.struct_test(
        ('uint32_t', 'events'),
        ('epoll_data_t', 'data'))
    epoll_create = c.function_test('int', 'int', test='''
        #include <sys/epoll.h>
        int main(int argc, char** argv) {
            int efd = epoll_create(20);
            return (-1 == efd) ? 1 : 0;
        }
        ''')
    epoll_ctl = c.function_test('int', 'int', 'int', 'int', 'struct epoll_event*', test='''
        #include <stdio.h>
        #include <sys/epoll.h>
        int main(int argc, char** argv) {
            int efd = epoll_create(20);
            struct epoll_event event;
            return epoll_ctl(efd, EPOLL_CTL_ADD, fileno(stdin), &event) == 0 ? 0 : 1;
        }
        ''')
    epoll_wait = c.function_test('int', 'int', 'struct epoll_event*', 'int', 'int')
