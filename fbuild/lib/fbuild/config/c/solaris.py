import fbuild.config.c as c

# ------------------------------------------------------------------------------

class port_h(c.Test):
    header = c.header_test('port.h')

    port_create = c.function_test('int', 'void', test='''
        #include <port.h>
        int main(int argc, char** argv) {
            int port = port_create();
            if (port < 0) { return 1; }
            if (close(port) < 0) { return 1; }
            return 0;
        }
        ''')
