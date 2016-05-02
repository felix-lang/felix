import fbuild
import fbuild.config.c as c

# ------------------------------------------------------------------------------

class arch(c.Test):
    @c.cacheproperty
    def little_endian(self):
        code = '''
            #include <stdio.h>

            enum enum_t {e_tag};
            typedef void (*fp_t)(void);

            union endian_t {
                unsigned long x;
                unsigned char y[sizeof(unsigned long)];
            } endian;

            int main(int argc, char** argv) {
                endian.x = 1ul;
                printf("%d\\n", endian.y[0]);
                return 0;
            }
        '''

        self.ctx.logger.check('checking endian')
        try:
            stdout = self.builder.tempfile_run(code)[0]
        except fbuild.ExecutionError:
            self.ctx.logger.failed()
            return None
        else:
            little_endian = int(stdout) == 1
            if little_endian:
                self.ctx.logger.passed('little endian')
            else:
                self.ctx.logger.passed('big endian')

            return little_endian
