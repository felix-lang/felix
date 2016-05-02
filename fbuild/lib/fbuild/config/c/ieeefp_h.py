"""ieeefp.h is a nonstandardized but common header that implements floating
point functions."""

import fbuild.config.c as c

# ------------------------------------------------------------------------------

class ieeefp_h(c.Test):
    header = c.header_test('ieeefp.h')

    fp_rnd = c.type_test()
    fpclass_t = c.type_test()

    FP_CLEAR = c.macro_test()
    FP_DISABLE = c.macro_test()
    FP_ENABLE = c.macro_test()
    FP_SET = c.macro_test()
    FP_X_DNML = c.macro_test()
    FP_X_DZ = c.macro_test()
    FP_X_DZ = c.macro_test()
    FP_X_IMP = c.macro_test()
    FP_X_IMP = c.macro_test()
    FP_X_INV = c.macro_test()
    FP_X_INV = c.macro_test()
    FP_X_OFL = c.macro_test()
    FP_X_OFL = c.macro_test()
    FP_X_UFL = c.macro_test()
    FP_X_UFL = c.macro_test()
    fp_except = c.macro_test()

    finite = c.function_test('int', 'double')
    finitef = c.function_test('int', 'float')
    finitel = c.function_test('int', 'long double')
    fpclass = c.function_test('fpclass', 'double')
    fpgetmask = c.function_test('fp_except', 'void')
    fpgetround = c.function_test('fp_rnd', 'void')
    fpgetsticky = c.function_test('fp_except', 'void')
    fpsetmask = c.function_test('fp_except', 'fp_except')
    fpsetround = c.function_test('fp_rnd', 'fp_rnd')
    fpsetsticky = c.function_test('fp_except', 'fp_except')
    isinf = c.function_test('int', 'double')
    isinff = c.function_test('int', 'float')
    isinfl = c.function_test('int', 'long double')
    isnan = c.function_test('int', 'double')
    isnand = c.function_test('int', 'double')
    isnanf = c.function_test('int', 'float')
    isnanl = c.function_test('int', 'long double')
    unordered = c.function_test('int', 'double', 'double')
