import fbuild.config.c as c

# ------------------------------------------------------------------------------

class libgen_h(c.Test):
    header = c.header_test('libgen.h')

    basename = c.function_test('char*', 'char*')
    dirname = c.function_test('char*', 'char*')

class stdlib_h(c.Test):
    header = c.header_test('stdlib.h')

    ecvt = c.function_test('char*', 'double', 'int', 'int*', 'int*', test='''
        #include <stdlib.h>
        int main() {
            int decpt;
            int sign;
            char* s = ecvt(0.0, 1, &decpt, &sign);
            return s[0] == '0' && s[1] == '\\0' ? 0 : 1;
        }
        ''')
    fcvt = c.function_test('char*', 'double', 'int', 'int*', 'int*', test='''
        #include <stdlib.h>
        int main() {
            int decpt;
            int sign;
            char* s = fcvt(0.0, 1, &decpt, &sign);
            return s[0] == '0' && s[1] == '\\0' ? 0 : 1;
        }
        ''')
    gcvt = c.function_test('char*', 'double', 'int', 'char*', test='''
        #include <stdlib.h>
        int main() {
            char s[50] = {0};
            int decpt;
            int sign;
            gcvt(0.0, 1, s);
            return s[0] == '0' && s[1] == '\\0' ? 0 : 1;
        }
        ''')
    mktemp = c.function_test('char*', 'char*', test='''
        #include <stdlib.h>
        int main() {
            char s[] = "XXXXXX";
            return mktemp(s) == NULL ? 1 : 0;
        }
        ''')

class strings_h(c.Test):
    header = c.header_test('strings.h')

    bcmp = c.function_test('int', 'const void*', 'const void*', 'size_t')
    bcopy = c.function_test('void', 'const void*', 'void*', 'size_t',
        default_args=(0, 0, 0))
    bzero = c.function_test('void', 'void*', 'size_t')

class unistd_h(c.Test):
    header = c.header_test('unistd.h')

    brk = c.function_test('void*', 'void*')
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
    getwd = c.function_test('char*', 'char*', test='''
        #include <unistd.h>
        #include <sys/param.h>
        int main() {
            char arg[MAXPATHLEN];
            char* res = getwd(arg);
            return 0;
        }
        ''')
    sbrk = c.function_test('void*', 'intptr_t')
    ttyslot = c.function_test('int', 'void')
