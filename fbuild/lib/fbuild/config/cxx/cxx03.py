import fbuild.config.c.c90 as c90
import fbuild.config.cxx as cxx

# ------------------------------------------------------------------------------

class types(c90.types):
    bool = cxx.int_type_test()

class iterator(cxx.Test):
    header = cxx.header_test('iterator')
    namespace = 'std'

    iterator = cxx.template_test(test_types=['int', 'int', 'int'])

class cmath(cxx.Test):
    header = cxx.header_test('cmath')
    namespace = 'std'

    HUGE_VAL = cxx.macro_test()
    acos = cxx.function_test('double', 'double')
    asin = cxx.function_test('double', 'double')
    atan = cxx.function_test('double', 'double')
    atan2 = cxx.function_test('double', 'double', 'double')
    ceil = cxx.function_test('double', 'double')
    cos = cxx.function_test('double', 'double')
    cosh = cxx.function_test('double', 'double')
    exp = cxx.function_test('double', 'double')
    fabs = cxx.function_test('double', 'double')
    floor = cxx.function_test('double', 'double')
    fmod = cxx.function_test('double', 'double', 'double')
    frexp = cxx.function_test('double', 'double', 'int*', test='''
        #include <cmath>
        int main() {
            int d0;
            double d1 = std::frexp(0., &d0);
            return 0;
        }
        ''')
    ldexp = cxx.function_test('double', 'double', 'int')
    log = cxx.function_test('double', 'double')
    log10 = cxx.function_test('double', 'double')
    modf = cxx.function_test('double', 'double', 'double*', test='''
        #include <cmath>
        int main() {
            double d0;
            double d1 = std::modf(0., &d0);
            return 0;
        }
        ''')
    pow = cxx.function_test('double', 'double', 'double')
    sin = cxx.function_test('double', 'double')
    sinh = cxx.function_test('double', 'double')
    sqrt = cxx.function_test('double', 'double')
    tan = cxx.function_test('double', 'double')
    tanh = cxx.function_test('double', 'double')
