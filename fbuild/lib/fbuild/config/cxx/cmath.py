"""This module extends the C++ 2003 cmath header with common extensions."""

import fbuild.config.c.stdlib
import fbuild.config.cxx as cxx
import fbuild.config.cxx.cxx03 as cxx03

# ------------------------------------------------------------------------------

class std_cmath(cxx.Test):
    header = cxx.header_test('cmath')
    namespace = 'std'

    # These are usually redefined from a macro into a templated function.
    fpclassify = cxx.function_test('int', 'double')
    isfinite = cxx.function_test('int', 'double')
    isinf = cxx.function_test('int', 'double')
    isnan = cxx.function_test('int', 'double')
    isnormal = cxx.function_test('int', 'double')
    signbit = cxx.function_test('int', 'double')
    isgreater = cxx.function_test('int', 'double')
    isgreaterequal = cxx.function_test('int', 'double')
    isless = cxx.function_test('int', 'double')
    islessequal = cxx.function_test('int', 'double')
    islessgreater = cxx.function_test('int', 'double')
    isunordered = cxx.function_test('int', 'double')

class cmath(cxx03.cmath, std_cmath, fbuild.config.c.stdlib.math_h):
    pass
