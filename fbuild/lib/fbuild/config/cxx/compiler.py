import fbuild.config.c as c
import fbuild.config.cxx as cxx

# ------------------------------------------------------------------------------

class bugs(cxx.Test):
    @c.cacheproperty
    def class_member_intialization(self):
        return self.builder.check_compile('''
            struct X {
                static const int i = 1;
            };

            int main(int argc, char** argv) {
                return 0;
            }
        ''', 'checking class member initialization')
