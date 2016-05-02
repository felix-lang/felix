import fbuild.config.c as c

# ------------------------------------------------------------------------------

class extensions(c.Test):
    builtin_expect = c.function_test('long', 'long', 'long',
        name='__builtin_expect',
        test='int main() { if(__builtin_expect(1,1)); return 0; }')

    @c.cacheproperty
    def named_registers_x86(self):
        return self.builder.check_run('''
            #include <stdio.h>
            register void *sp __asm__ ("esp");

            int main() {
                printf("Sp = %p\\n", sp);
                return 0;
            }
            ''', 'checking for named x86 registers')

    @c.cacheproperty
    def named_registers_x86_64(self):
        return self.builder.check_run('''
            #include <stdio.h>
            register void *sp __asm__ ("rsp");

            int main() {
                printf("Sp = %p\\n", sp);
                return 0;
            }
            ''', 'checking for named x86_64 registers')

    @c.cacheproperty
    def computed_gotos(self):
        return self.builder.check_run('''
            int main(int argc, char** argv) {
                void *label = &&label2;
                goto *label;
            label1:
                return 1;
            label2:
                return 0;
            }
        ''', 'checking for computed gotos')

    @c.cacheproperty
    def asm_labels(self):
        return self.builder.check_run('''
            int main(int argc, char** argv) {
                void *label = &&label2;
                __asm__(".global fred");
                __asm__("fred:");
                __asm__(""::"g"(&&label1));
                goto *label;
            label1:
                return 1;
            label2:
                return 0;
            }
        ''', 'checking for asm labels')

class getopt_h(c.Test):
    header = c.header_test('getopt.h')

    getopt = c.function_test('int', 'int', 'char**', 'char*', test='''
        #include <getopt.h>
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
    getopt_long = c.function_test('int', 'int', 'char**', 'char*', 'struct option*', 'int',
        test='''
        #include <getopt.h>
        static struct option longopts[] = {
            { "foo", no_argument, NULL, 'f' }
        };

        int main(int argc, char** argv) {
            int ch, ret = 0;
            while ((ch = getopt_long(argc, argv, "b", longopts, NULL)) != -1) {
                switch (ch) {
                case 'b':
                case 'f':
                    break;
                default:
                    ret = 1;
                }
            }

            return ret;
        }
        ''')
    getopt_long_only = c.function_test('int', 'int', 'char**', 'char*', 'struct option*', 'int',
        test='''
        #include <getopt.h>
        static struct option longopts[] = {
            { "foo", no_argument, NULL, 'f' }
        };

        int main(int argc, char** argv) {
            int ch, ret = 0;
            while ((ch = getopt_long_only(argc, argv, "b", longopts, NULL)) != -1) {
                switch (ch) {
                case 'b':
                case 'f':
                    break;
                default:
                    ret = 1;
                }
            }

            return ret;
        }
        ''')
