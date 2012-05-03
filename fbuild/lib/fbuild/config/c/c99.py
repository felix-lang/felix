import fbuild.config.c as c
import fbuild.config.c.c90 as c90

# ------------------------------------------------------------------------------

class types(c90.types):
    _Bool = c.int_type_test()
    float__Complex = c.type_test(name='float _Complex')
    double__Complex = c.type_test(name='double _Complex')
    long_double__Complex = c.type_test(name='long double _Complex')
    float__Imaginary = c.type_test(name='float _Imaginary')
    double__Imaginary = c.type_test(name='double _Imaginary')
    long_double__Imaginary = c.type_test(name='long double _Imaginary')

# ------------------------------------------------------------------------------

# assert.h didn't change in c99
assert_h = c90.assert_h

# ------------------------------------------------------------------------------

class complex_h(c.Test):
    header = c.header_test('complex.h')

    complex = c.macro_test()
    _Complex_I = c.macro_test()
    imaginary = c.macro_test()
    _Imaginary_I = c.macro_test()
    I = c.macro_test()

    float_complex = c.type_test(name='float complex')
    double_complex = c.type_test(name='double complex')
    long_double_complex = c.type_test(name='long double complex')

    float_imaginary = c.type_test(name='float imaginary')
    double_imaginary = c.type_test(name='double imaginary')
    long_double_imaginary = c.type_test(name='long double imaginary')

    cacos = c.function_test('double complex', 'double complex')
    cacosf = c.function_test('float complex', 'float complex')
    cacosl = c.function_test('long double complex', 'long double complex')
    casin = c.function_test('double complex', 'double complex')
    casinf = c.function_test('float complex', 'float complex')
    casinl = c.function_test('long double complex', 'long double complex')
    catan = c.function_test('double complex', 'double complex')
    catanf = c.function_test('float complex', 'float complex')
    catanl = c.function_test('long double complex', 'long double complex')
    ccos = c.function_test('double complex', 'double complex')
    ccosf = c.function_test('float complex', 'float complex')
    ccosl = c.function_test('long double complex', 'long double complex')
    csin = c.function_test('double complex', 'double complex')
    csinf = c.function_test('float complex', 'float complex')
    csinl = c.function_test('long double complex', 'long double complex')
    ctan = c.function_test('double complex', 'double complex')
    ctanf = c.function_test('float complex', 'float complex')
    ctanl = c.function_test('long double complex', 'long double complex')
    cacosh = c.function_test('double complex', 'double complex')
    cacoshf = c.function_test('float complex', 'float complex')
    cacoshl = c.function_test('long double complex', 'long double complex')
    casinh = c.function_test('double complex', 'double complex')
    casinhf = c.function_test('float complex', 'float complex')
    casinhl = c.function_test('long double complex', 'long double complex')
    catanh = c.function_test('double complex', 'double complex')
    catanhf = c.function_test('float complex', 'float complex')
    catanhl = c.function_test('long double complex', 'long double complex')
    ccosh = c.function_test('double complex', 'double complex')
    ccoshf = c.function_test('float complex', 'float complex')
    ccoshl = c.function_test('long double complex', 'long double complex')
    csinh = c.function_test('double complex', 'double complex')
    csinhf = c.function_test('float complex', 'float complex')
    csinhl = c.function_test('long double complex', 'long double complex')
    ctanh = c.function_test('double complex', 'double complex')
    ctanhf = c.function_test('float complex', 'float complex')
    ctanhl = c.function_test('long double complex', 'long double complex')
    cexp = c.function_test('double complex', 'double complex')
    cexpf = c.function_test('float complex', 'float complex')
    cexpl = c.function_test('long double complex', 'long double complex')
    clog = c.function_test('double complex', 'double complex')
    clogf = c.function_test('float complex', 'float complex')
    clogl = c.function_test('long double complex', 'long double complex')
    cabs = c.function_test('double', 'double complex')
    cabsf = c.function_test('float', 'float complex')
    cabsl = c.function_test('long double', 'long double complex')
    cpow = c.function_test('double complex', 'double complex', 'double complex')
    cpowf = c.function_test('float complex', 'float complex', 'float complex')
    cpowl = c.function_test('long double complex', 'long double complex', 'long double complex')
    csqrt = c.function_test('double complex', 'double complex')
    csqrtf = c.function_test('float complex', 'float complex')
    csqrtl = c.function_test('long double complex', 'long double complex')
    carg = c.function_test('double', 'double complex')
    cargf = c.function_test('float', 'float complex')
    cargl = c.function_test('long double', 'long double complex')
    cimag = c.function_test('double', 'double complex')
    cimagf = c.function_test('float', 'float complex')
    cimagl = c.function_test('long double', 'long double complex')
    conj = c.function_test('double complex', 'double complex')
    conjf = c.function_test('float complex', 'float complex')
    conjl = c.function_test('long double complex', 'long double complex')
    cproj = c.function_test('double complex', 'double complex')
    cprojf = c.function_test('float complex', 'float complex')
    cprojl = c.function_test('long double complex', 'long double complex')
    creal = c.function_test('double', 'double complex')
    crealf = c.function_test('float', 'float complex')
    creall = c.function_test('long double', 'long double complex')

# ------------------------------------------------------------------------------

class ctype_h(c90.ctype_h):
    isblank = c.function_test('int', 'int')

# ------------------------------------------------------------------------------

class errno_h(c90.errno_h):
    EILSEQ = c.macro_test()

# ------------------------------------------------------------------------------

class fenv_h(c.Test):
    header = c.header_test('fenv.h')

    fenv_t = c.type_test()
    fexcept_t = c.type_test()
    FE_DIVBYZERO = c.macro_test()
    FE_INEXACT = c.macro_test()
    FE_INVALID = c.macro_test()
    FE_OVERFLOW = c.macro_test()
    FE_UNDERFLOW = c.macro_test()
    FE_ALL_EXCEPT = c.macro_test()
    FE_DOWNWARD = c.macro_test()
    FE_TONEAREST = c.macro_test()
    FE_TOWARDZERO = c.macro_test()
    FE_UPWARD = c.macro_test()
    FE_DFL_ENV = c.macro_test()
    feclearexcept = c.function_test('int', 'int')
    fegetexceptflag = c.function_test('int', 'fexcept_t*', 'int', test='''
        #include <fenv.h>
        int main() {
            fexcept_t f;
            return fegetexceptflag(&f, 0);
        }
        ''')
    feraiseexcept = c.function_test('int', 'int')
    fesetexceptflag = c.function_test('int', 'const fexcept_t*', 'int', test='''
        #include <fenv.h>
        int main() {
            fexcept_t f;
            return fesetexceptflag(&f, 0);
        }
        ''')
    fetestexcept = c.function_test('int', 'int')
    fegetround = c.function_test('int', 'void')
    fesetround = c.function_test('int', 'int')
    fegetenv = c.function_test('int', 'fenv_t*')
    feholdexcept = c.function_test('int', 'fenv_t*')
    fesetenv = c.function_test('int', 'const fenv_t*')
    feupdateenv = c.function_test('int', 'const fenv_t*', test='''
        #include <fenv.h>
        int main() {
            fenv_t f;
            return feupdateenv(&f);
        }
        ''')

# ------------------------------------------------------------------------------

class float_h(c90.float_h):
    DECIMAL_DIG = c.macro_test()
    FLT_EVAL_METHOD = c.macro_test()

# ------------------------------------------------------------------------------

class inttypes_h(c.Test):
    header = c.header_test('inttypes.h')

    imaxdiv_t = c.type_test()
    PRId8 = c.macro_test()
    PRId16 = c.macro_test()
    PRId32 = c.macro_test()
    PRId64 = c.macro_test()
    PRIdLEAST8 = c.macro_test()
    PRIdLEAST16 = c.macro_test()
    PRIdLEAST32 = c.macro_test()
    PRIdLEAST64 = c.macro_test()
    PRIdFAST8 = c.macro_test()
    PRIdFAST16 = c.macro_test()
    PRIdFAST32 = c.macro_test()
    PRIdFAST64 = c.macro_test()
    PRIdMAX = c.macro_test()
    PRIdPTR = c.macro_test()
    PRIi8 = c.macro_test()
    PRIi16 = c.macro_test()
    PRIi32 = c.macro_test()
    PRIi64 = c.macro_test()
    PRIiLEAST8 = c.macro_test()
    PRIiLEAST16 = c.macro_test()
    PRIiLEAST32 = c.macro_test()
    PRIiLEAST64 = c.macro_test()
    PRIiFAST8 = c.macro_test()
    PRIiFAST16 = c.macro_test()
    PRIiFAST32 = c.macro_test()
    PRIiFAST64 = c.macro_test()
    PRIiMAX = c.macro_test()
    PRIiPTR = c.macro_test()
    PRIo8 = c.macro_test()
    PRIo16 = c.macro_test()
    PRIo32 = c.macro_test()
    PRIo64 = c.macro_test()
    PRIoLEAST8 = c.macro_test()
    PRIoLEAST16 = c.macro_test()
    PRIoLEAST32 = c.macro_test()
    PRIoLEAST64 = c.macro_test()
    PRIoFAST8 = c.macro_test()
    PRIoFAST16 = c.macro_test()
    PRIoFAST32 = c.macro_test()
    PRIoFAST64 = c.macro_test()
    PRIoMAX = c.macro_test()
    PRIoPTR = c.macro_test()
    PRIu8 = c.macro_test()
    PRIu16 = c.macro_test()
    PRIu32 = c.macro_test()
    PRIu64 = c.macro_test()
    PRIuLEAST8 = c.macro_test()
    PRIuLEAST16 = c.macro_test()
    PRIuLEAST32 = c.macro_test()
    PRIuLEAST64 = c.macro_test()
    PRIuFAST8 = c.macro_test()
    PRIuFAST16 = c.macro_test()
    PRIuFAST32 = c.macro_test()
    PRIuFAST64 = c.macro_test()
    PRIuMAX = c.macro_test()
    PRIuPTR = c.macro_test()
    PRIx8 = c.macro_test()
    PRIx16 = c.macro_test()
    PRIx32 = c.macro_test()
    PRIx64 = c.macro_test()
    PRIxLEAST8 = c.macro_test()
    PRIxLEAST16 = c.macro_test()
    PRIxLEAST32 = c.macro_test()
    PRIxLEAST64 = c.macro_test()
    PRIxFAST8 = c.macro_test()
    PRIxFAST16 = c.macro_test()
    PRIxFAST32 = c.macro_test()
    PRIxFAST64 = c.macro_test()
    PRIxMAX = c.macro_test()
    PRIxPTR = c.macro_test()
    PRIX8 = c.macro_test()
    PRIX16 = c.macro_test()
    PRIX32 = c.macro_test()
    PRIX64 = c.macro_test()
    PRIXLEAST8 = c.macro_test()
    PRIXLEAST16 = c.macro_test()
    PRIXLEAST32 = c.macro_test()
    PRIXLEAST64 = c.macro_test()
    PRIXFAST8 = c.macro_test()
    PRIXFAST16 = c.macro_test()
    PRIXFAST32 = c.macro_test()
    PRIXFAST64 = c.macro_test()
    PRIXMAX = c.macro_test()
    PRIXPTR = c.macro_test()
    SCNd8 = c.macro_test()
    SCNd16 = c.macro_test()
    SCNd32 = c.macro_test()
    SCNd64 = c.macro_test()
    SCNdLEAST8 = c.macro_test()
    SCNdLEAST16 = c.macro_test()
    SCNdLEAST32 = c.macro_test()
    SCNdLEAST64 = c.macro_test()
    SCNdFAST8 = c.macro_test()
    SCNdFAST16 = c.macro_test()
    SCNdFAST32 = c.macro_test()
    SCNdFAST64 = c.macro_test()
    SCNdMAX = c.macro_test()
    SCNdPTR = c.macro_test()
    SCNi8 = c.macro_test()
    SCNi16 = c.macro_test()
    SCNi32 = c.macro_test()
    SCNi64 = c.macro_test()
    SCNiLEAST8 = c.macro_test()
    SCNiLEAST16 = c.macro_test()
    SCNiLEAST32 = c.macro_test()
    SCNiLEAST64 = c.macro_test()
    SCNiFAST8 = c.macro_test()
    SCNiFAST16 = c.macro_test()
    SCNiFAST32 = c.macro_test()
    SCNiFAST64 = c.macro_test()
    SCNiMAX = c.macro_test()
    SCNiPTR = c.macro_test()
    SCNo8 = c.macro_test()
    SCNo16 = c.macro_test()
    SCNo32 = c.macro_test()
    SCNo64 = c.macro_test()
    SCNoLEAST8 = c.macro_test()
    SCNoLEAST16 = c.macro_test()
    SCNoLEAST32 = c.macro_test()
    SCNoLEAST64 = c.macro_test()
    SCNoFAST8 = c.macro_test()
    SCNoFAST16 = c.macro_test()
    SCNoFAST32 = c.macro_test()
    SCNoFAST64 = c.macro_test()
    SCNoMAX = c.macro_test()
    SCNoPTR = c.macro_test()
    SCNu8 = c.macro_test()
    SCNu16 = c.macro_test()
    SCNu32 = c.macro_test()
    SCNu64 = c.macro_test()
    SCNuLEAST8 = c.macro_test()
    SCNuLEAST16 = c.macro_test()
    SCNuLEAST32 = c.macro_test()
    SCNuLEAST64 = c.macro_test()
    SCNuFAST8 = c.macro_test()
    SCNuFAST16 = c.macro_test()
    SCNuFAST32 = c.macro_test()
    SCNuFAST64 = c.macro_test()
    SCNuMAX = c.macro_test()
    SCNuPTR = c.macro_test()
    SCNx8 = c.macro_test()
    SCNx16 = c.macro_test()
    SCNx32 = c.macro_test()
    SCNx64 = c.macro_test()
    SCNxLEAST8 = c.macro_test()
    SCNxLEAST16 = c.macro_test()
    SCNxLEAST32 = c.macro_test()
    SCNxLEAST64 = c.macro_test()
    SCNxFAST8 = c.macro_test()
    SCNxFAST16 = c.macro_test()
    SCNxFAST32 = c.macro_test()
    SCNxFAST64 = c.macro_test()
    SCNxMAX = c.macro_test()
    SCNxPTR = c.macro_test()
    imaxabs = c.function_test('intmax_t', 'intmax_t')
    imaxdiv = c.function_test('imaxdiv_t', 'intmax_t', 'intmax_t',
        default_args=(4, 2))
    strtoimax = c.function_test('intmax_t', 'const char*', 'char**', 'int')
    strtoumax = c.function_test('uintmax_t', 'const char*', 'char**', 'int')
    wcstoimax = c.function_test('intmax_t', 'const wchar_t*', 'wchar_t**', 'int', test='''
        #include <wchar.h>
        #include <inttypes.h>
        int main() {
            wchar_t* s1 = L"15";
            wchar_t* s2 = L"abc";
            wchar_t* endp;
            intmax_t d = wcstoimax(s1, &endp, 8);
            if (s1 != endp && *endp == L'\\0' && d == 13) {
                d = wcstoimax(s2, &endp, 10);
                return s1 == endp || *endp != '\\0' ? 0 : 1;
            }
            return 1;
        }
        ''')
    wcstoumax = c.function_test('uintmax_t', 'const wchar_t*', 'wchar_t**', 'int', test='''
        #include <wchar.h>
        #include <inttypes.h>
        int main() {
            wchar_t* s1 = L"15";
            wchar_t* s2 = L"abc";
            wchar_t* endp;
            uintmax_t d = wcstoumax(s1, &endp, 8);
            if (s1 != endp && *endp == L'\\0' && d == 13) {
                d = wcstoumax(s2, &endp, 10);
                return s1 == endp || *endp != '\\0' ? 0 : 1;
            }
            return 1;
        }
        ''')

# ------------------------------------------------------------------------------

class iso646_h(c.Test):
    header = c.header_test('iso646.h')

    and_ = c.macro_test(name='and')
    and_eq = c.macro_test()
    bitand = c.macro_test()
    bitor = c.macro_test()
    compl = c.macro_test()
    not_ = c.macro_test(name='not')
    not_eq = c.macro_test()
    or_ = c.macro_test(name='or')
    or_eq = c.macro_test()
    xor = c.macro_test()
    xor_eq = c.macro_test()

# ------------------------------------------------------------------------------

class limits_h(c90.limits_h):
    LLONG_MAX = c.macro_test()
    LLONG_MIN = c.macro_test()
    ULLONG_MAX = c.macro_test()

# ------------------------------------------------------------------------------

# locale.h didn't change in c99
locale_h = c90.locale_h

# ------------------------------------------------------------------------------

class math_h(c90.math_h):
    float_t = c.type_test()
    double_t = c.type_test()
    HUGE_VALF = c.macro_test()
    HUGE_VALL = c.macro_test()
    INFINITY = c.macro_test()
    NAN = c.macro_test()
    FP_INFINITE = c.macro_test()
    FP_NAN = c.macro_test()
    FP_NORMAL = c.macro_test()
    FP_SUBNORMAL = c.macro_test()
    FP_ZERO = c.macro_test()
    FP_FAST_FMA = c.macro_test()
    FP_FAST_FMAF = c.macro_test()
    FP_FAST_FMAL = c.macro_test()
    FP_ILOGB0 = c.macro_test()
    FP_ILOGBNAN = c.macro_test()
    MATH_ERRNO = c.macro_test()
    MATH_ERREXCEPT = c.macro_test()
    math_errhandling = c.macro_test()
    fpclassify = c.macro_test()
    isfinite = c.macro_test()
    isinf = c.macro_test()
    isnan = c.macro_test()
    isnormal = c.macro_test()
    signbit = c.macro_test()
    acosf = c.function_test('float', 'float')
    acosl = c.function_test('long double', 'long double')
    asinf = c.function_test('float', 'float')
    asinl = c.function_test('long double', 'long double')
    atanf = c.function_test('float', 'float')
    atanl = c.function_test('long double', 'long double')
    atan2f = c.function_test('float', 'float', 'float')
    atan2l = c.function_test('long double', 'long double', 'long double')
    cosf = c.function_test('float', 'float')
    cosl = c.function_test('long double', 'long double')
    sinf = c.function_test('float', 'float')
    sinl = c.function_test('long double', 'long double')
    tanf = c.function_test('float', 'float')
    tanl = c.function_test('long double', 'long double')
    acosh = c.function_test('double', 'double')
    acoshf = c.function_test('float', 'float')
    acoshl = c.function_test('long double', 'long double')
    asinh = c.function_test('double', 'double')
    asinhf = c.function_test('float', 'float')
    asinhl = c.function_test('long double', 'long double')
    atanh = c.function_test('double', 'double')
    atanhf = c.function_test('float', 'float')
    atanhl = c.function_test('long double', 'long double')
    coshf = c.function_test('float', 'float')
    coshl = c.function_test('long double', 'long double')
    sinhf = c.function_test('float', 'float')
    sinhl = c.function_test('long double', 'long double')
    tanhf = c.function_test('float', 'float')
    tanhl = c.function_test('long double', 'long double')
    expf = c.function_test('float', 'float')
    expl = c.function_test('long double', 'long double')
    exp2 = c.function_test('double', 'double')
    exp2f = c.function_test('float', 'float')
    exp2l = c.function_test('long double', 'long double')
    expm1 = c.function_test('double', 'double')
    expm1f = c.function_test('float', 'float')
    expm1l = c.function_test('long double', 'long double')
    frexpf = c.function_test('float', 'float', 'int*', test='''
        #include <math.h>
        int main() {
            int d0;
            float d1 = frexpf(0.f, &d0);
            return 0;
        }
        ''')
    frexpl = c.function_test('long double', 'long double', 'int*', test='''
        #include <math.h>
        int main() {
            int d0;
            long double d1 = frexpl(0.l, &d0);
            return 0;
        }
        ''')
    ilogb = c.function_test('int', 'double')
    ilogbf = c.function_test('int', 'float')
    ilogbl = c.function_test('int', 'long double')
    ldexpf = c.function_test('float', 'float', 'int')
    ldexpl = c.function_test('long double', 'long double', 'int')
    logf = c.function_test('float', 'float')
    logl = c.function_test('long double', 'long double')
    log10f = c.function_test('float', 'float')
    log10l = c.function_test('long double', 'long double')
    log1p = c.function_test('double', 'double')
    log1pf = c.function_test('float', 'float')
    log1pl = c.function_test('long double', 'long double')
    log2 = c.function_test('double', 'double')
    log2f = c.function_test('float', 'float')
    log2l = c.function_test('long double', 'long double')
    logb = c.function_test('double', 'double')
    logbf = c.function_test('float', 'float')
    logbl = c.function_test('long double', 'long double')
    modff = c.function_test('float', 'float', 'float*', test='''
        #include <math.h>
        int main() {
            float d0;
            float d1 = modff(0.f, &d0);
            return 0;
        }
        ''')
    modfl = c.function_test('long double', 'long double', 'long double*', test='''
        #include <math.h>
        int main() {
            long double d0;
            long double d1 = modfl(0.l, &d0);
            return 0;
        }
        ''')
    scalbn = c.function_test('double', 'double', 'int')
    scalbnf = c.function_test('float', 'float', 'int')
    scalbnl = c.function_test('long double', 'long double', 'int')
    scalbln = c.function_test('double', 'double', 'long int')
    scalblnf = c.function_test('float', 'float', 'long int')
    scalblnl = c.function_test('long double', 'long double', 'long int')
    cbrt = c.function_test('double', 'double')
    cbrtf = c.function_test('float', 'float')
    cbrtl = c.function_test('long double', 'long double')
    fabsf = c.function_test('float', 'float')
    fabsl = c.function_test('long double', 'long double')
    hypot = c.function_test('double', 'double', 'double')
    hypotf = c.function_test('float', 'float', 'float')
    hypotl = c.function_test('long double', 'long double', 'long double')
    powf = c.function_test('float', 'float', 'float')
    powl = c.function_test('long double', 'long double', 'long double')
    sqrtf = c.function_test('float', 'float')
    sqrtl = c.function_test('long double', 'long double')
    erf = c.function_test('double', 'double')
    erff = c.function_test('float', 'float')
    erfl = c.function_test('long double', 'long double')
    erfc = c.function_test('double', 'double')
    erfcf = c.function_test('float', 'float')
    erfcl = c.function_test('long double', 'long double')
    lgamma = c.function_test('double', 'double')
    lgammaf = c.function_test('float', 'float')
    lgammal = c.function_test('long double', 'long double')
    tgamma = c.function_test('double', 'double')
    tgammaf = c.function_test('float', 'float')
    tgammal = c.function_test('long double', 'long double')
    ceilf = c.function_test('float', 'float')
    ceill = c.function_test('long double', 'long double')
    floorf = c.function_test('float', 'float')
    floorl = c.function_test('long double', 'long double')
    nearbyint = c.function_test('double', 'double')
    nearbyintf = c.function_test('float', 'float')
    nearbyintl = c.function_test('long double', 'long double')
    rint = c.function_test('double', 'double')
    rintf = c.function_test('float', 'float')
    rintl = c.function_test('long double', 'long double')
    lrint = c.function_test('long int', 'double')
    lrintf = c.function_test('long int', 'float')
    lrintl = c.function_test('long int', 'long double')
    llrint = c.function_test('long long int', 'double')
    llrintf = c.function_test('long long int', 'float')
    llrintl = c.function_test('long long int', 'long double')
    round = c.function_test('double', 'double')
    roundf = c.function_test('float', 'float')
    roundl = c.function_test('long double', 'long double')
    lround = c.function_test('long int', 'double')
    lroundf = c.function_test('long int', 'float')
    lroundl = c.function_test('long int', 'long double')
    llround = c.function_test('long long int', 'double')
    llroundf = c.function_test('long long int', 'float')
    llroundl = c.function_test('long long int', 'long double')
    trunc = c.function_test('double', 'double')
    truncf = c.function_test('float', 'float')
    truncl = c.function_test('long double', 'long double')
    fmodf = c.function_test('float', 'float', 'float')
    fmodl = c.function_test('long double', 'long double', 'long double')
    remainder = c.function_test('double', 'double', 'double')
    remainderf = c.function_test('float', 'float', 'float')
    remainderl = c.function_test('long double', 'long double', 'long double')
    remquo = c.function_test('double', 'double', 'double', 'int*', test='''
        #include <math.h>
        int main() {
            int d0;
            double d1 = remquo(0., 0., &d0);
            return 0;
        }
        ''')
    remquof = c.function_test('float', 'float', 'float', 'int*', test='''
        #include <math.h>
        int main() {
            int d0;
            float d1 = remquof(0.f, 0.f, &d0);
            return 0;
        }

        ''')
    remquol = c.function_test('long double', 'long double', 'long double', 'int*', test='''
        #include <math.h>
        int main() {
            int d0;
            long double d1 = remquol(0.l, 0.l, &d0);
            return 0;
        }
        ''')
    copysign = c.function_test('double', 'double', 'double')
    copysignf = c.function_test('float', 'float', 'float')
    copysignl = c.function_test('long double', 'long double', 'long double')
    nan = c.function_test('double', 'const char*')
    nanf = c.function_test('float', 'const char*')
    nanl = c.function_test('long double', 'const char*')
    nextafter = c.function_test('double', 'double', 'double')
    nextafterf = c.function_test('float', 'float', 'float')
    nextafterl = c.function_test('long double', 'long double', 'long double')
    nexttoward = c.function_test('double', 'double', 'long double')
    nexttowardf = c.function_test('float', 'float', 'long double')
    nexttowardl = c.function_test('long double', 'long double', 'long double')
    fdim = c.function_test('double', 'double', 'double')
    fdimf = c.function_test('float', 'float', 'float')
    fdiml = c.function_test('long double', 'long double', 'long double')
    fmax = c.function_test('double', 'double', 'double')
    fmaxf = c.function_test('float', 'float', 'float')
    fmaxl = c.function_test('long double', 'long double', 'long double')
    fmin = c.function_test('double', 'double', 'double')
    fminf = c.function_test('float', 'float', 'float')
    fminl = c.function_test('long double', 'long double', 'long double')
    fma = c.function_test('double', 'double', 'double', 'double')
    fmaf = c.function_test('float', 'float', 'float', 'float')
    fmal = c.function_test('long double', 'long double', 'long double', 'long double')
    isgreater = c.macro_test()
    isgreaterequal = c.macro_test()
    isless = c.macro_test()
    islessequal = c.macro_test()
    islessgreater = c.macro_test()
    isunordered = c.macro_test()

# ------------------------------------------------------------------------------

# setjmp.h didn't change in c99
setjmp_h = c90.setjmp_h

# ------------------------------------------------------------------------------

# signal.h didn't change in c99
signal_h = c90.signal_h

# ------------------------------------------------------------------------------

class stdarg_h(c90.stdarg_h):
    va_copy = c.function_test('void', 'va_list', 'va_list')

# ------------------------------------------------------------------------------

class stdbool_h(c.Test):
    header = c.header_test('stdbool.h')

    bool = c.int_type_test()
    true = c.macro_test()
    false = c.macro_test()
    bool_true_false_are_defined = c.macro_test(name='__bool_true_false_are_defined')

# ------------------------------------------------------------------------------

# stddef.h didn't change in c99
stddef_h = c90.stddef_h

# ------------------------------------------------------------------------------

class stdint_h(c.Test):
    header = c.header_test('stdint.h')

    int8_t = c.int_type_test()
    int16_t = c.int_type_test()
    int32_t = c.int_type_test()
    int64_t = c.int_type_test()
    uint8_t = c.int_type_test()
    uint16_t = c.int_type_test()
    uint32_t = c.int_type_test()
    uint64_t = c.int_type_test()
    int_least8_t = c.int_type_test()
    int_least16_t = c.int_type_test()
    int_least32_t = c.int_type_test()
    int_least64_t = c.int_type_test()
    uint_least8_t = c.int_type_test()
    uint_least16_t = c.int_type_test()
    uint_least32_t = c.int_type_test()
    uint_least64_t = c.int_type_test()
    int_fast8_t = c.int_type_test()
    int_fast16_t = c.int_type_test()
    int_fast32_t = c.int_type_test()
    int_fast64_t = c.int_type_test()
    uint_fast8_t = c.int_type_test()
    uint_fast16_t = c.int_type_test()
    uint_fast32_t = c.int_type_test()
    uint_fast64_t = c.int_type_test()
    intptr_t = c.int_type_test()
    uintptr_t = c.int_type_test()
    intmax_t = c.int_type_test()
    uintmax_t = c.int_type_test()
    INT8_MIN = c.macro_test()
    INT16_MIN = c.macro_test()
    INT32_MIN = c.macro_test()
    INT64_MIN = c.macro_test()
    UINT8_MAX = c.macro_test()
    UINT16_MAX = c.macro_test()
    UINT32_MAX = c.macro_test()
    UINT64_MAX = c.macro_test()
    INT_LEAST8_MIN = c.macro_test()
    INT_LEAST16_MIN = c.macro_test()
    INT_LEAST32_MIN = c.macro_test()
    INT_LEAST64_MAX = c.macro_test()
    UINT_LEAST8_MAX = c.macro_test()
    UINT_LEAST16_MAX = c.macro_test()
    UINT_LEAST32_MAX = c.macro_test()
    UINT_LEAST64_MAX = c.macro_test()
    INT_FAST8_MIN = c.macro_test()
    INT_FAST16_MIN = c.macro_test()
    INT_FAST32_MIN = c.macro_test()
    INT_FAST64_MIN = c.macro_test()
    UINT_FAST8_MAX = c.macro_test()
    UINT_FAST16_MAX = c.macro_test()
    UINT_FAST32_MAX = c.macro_test()
    UINT_FAST64_MAX = c.macro_test()
    INTPTR_MIN = c.macro_test()
    INTPTR_MAX = c.macro_test()
    UINTPTR_MAX = c.macro_test()
    INTMAX_MIN = c.macro_test()
    INTMAX_MAX = c.macro_test()
    UINTMAX_MAX = c.macro_test()
    PTRDIFF_MIN = c.macro_test()
    PTRDIFF_MAX = c.macro_test()
    SIG_ATOMIC_MIN = c.macro_test()
    SIG_ATOMIC_MAX = c.macro_test()
    SIZE_MAX = c.macro_test()
    WCHAR_MIN = c.macro_test()
    WCHAR_MAX = c.macro_test()
    WINT_MIN = c.macro_test()
    WINT_MAX = c.macro_test()
    INT8_C = c.macro_test()
    INT16_C = c.macro_test()
    INT32_C = c.macro_test()
    INT64_C = c.macro_test()
    UINT8_C = c.macro_test()
    UINT16_C = c.macro_test()
    UINT32_C = c.macro_test()
    UINT64_C = c.macro_test()
    INTMAX_C = c.macro_test()
    UINTMAX_C = c.macro_test()

# ------------------------------------------------------------------------------

class stdio_h(c90.stdio_h):
    snprintf = c.function_test('int', 'char*', 'size_t', test='''
        #include <stdio.h>
        int main() {
            char s[50] = {0};
            return snprintf(s, 50, "%d %d", 5, 6) &&
                s[0] == '5' &&
                s[1] == ' ' &&
                s[2] == '6' &&
                s[3] == '\\0' ? 0 : 1;
        }
        ''')
    vfscanf = c.function_test('int', 'FILE*', 'const char*', 'va_list', test='''
        #include <stdarg.h>
        #include <stdio.h>
        #ifdef _WIN32
        #include <windows.h>
        #endif

        int f(char* s, ...) {
            int rc;
            va_list ap;
            va_start(ap, s);
            rc = vfscanf(stdin, s, ap);
            va_end(ap);
            return rc;
        }
        int main() {
            int x = 0, y = 0;
        #ifdef _WIN32
            /* Turn off the windows error box */
            DWORD dwMode = SetErrorMode(SEM_NOGPFAULTERRORBOX);
            SetErrorMode(dwMode | SEM_NOGPFAULTERRORBOX);
        #endif
            return f("%d %d", &x, &y) && x == 5 && y == 6 ? 0 : 1;
        }
        ''', stdin=b'5 6')
    vsnprintf = c.function_test('int', 'char*', 'size_t', 'char*', 'va_list', test='''
        #include <stdarg.h>
        #include <stdio.h>
        int f(char* s, ...) {
            int rc;
            va_list ap;
            va_start(ap, s);
            rc = vsnprintf(s, 50, "%d %d", ap);
            va_end(ap);
            return rc;
        }
        int main() {
            char s[50] = {0};
            return f(s, 5, 6) &&
                s[0] == '5' &&
                s[1] == ' ' &&
                s[2] == '6' &&
                s[3] == '\\0' ? 0 : 1;
        }
        ''')
    vscanf = c.function_test('const char*', 'char*', 'va_list', test='''
        #include <stdarg.h>
        #include <stdio.h>
        #ifdef _WIN32
        #include <windows.h>
        #endif

        int f(char* s, ...) {
            int rc;
            va_list ap;
            va_start(ap, s);
            rc = vscanf(s, ap);
            va_end(ap);
            return rc;
        }
        int main() {
            int x = 0, y = 0;
        #ifdef _WIN32
            /* Turn off the windows error box */
            DWORD dwMode = SetErrorMode(SEM_NOGPFAULTERRORBOX);
            SetErrorMode(dwMode | SEM_NOGPFAULTERRORBOX);
        #endif
            return f("%d %d", &x, &y) && x == 5 && y == 6 ? 0 : 1;
        }
        ''', stdin=b"5 6")
    vsscanf = c.function_test('const char*', 'char*', 'va_list', test='''
        #include <stdarg.h>
        #include <stdio.h>
        int f(char* s, ...) {
            int rc;
            va_list ap;
            va_start(ap, s);
            rc = vsscanf("5 6", s, ap);
            va_end(ap);
            return rc;
        }
        int main() {
            int x = 0, y = 0;
            return f("%d %d", &x, &y) && x == 5 && y == 6 ? 0 : 1;
        }
        ''')

# ------------------------------------------------------------------------------

class stdlib_h(c90.stdlib_h):
    lldiv_t = c.type_test()
    atoll = c.function_test('long long int', 'const char*', test='''
        #include <stdlib.h>
        int main() {
            return atoll("1234") == 1234LL ? 0 : 1;
        }
        ''')
    strtod = c.function_test('double', 'const char*', 'char**', test='''
        #include <stdlib.h>
        int main() {
            char* s1 = "15";
            char* s2 = "abc";
            char* endp;
            double d = strtod(s1, &endp);
            if (s1 != endp && *endp == '\\0' && d == 15.0) {
                d = strtod(s2, &endp);
                return s1 == endp || *endp != '\\0' ? 0 : 1;
            }
            return 1;
        }
        ''')
    strtof = c.function_test('float', 'const char*', 'char**', test='''
        #include <stdlib.h>
        int main() {
            char* s1 = "15";
            char* s2 = "abc";
            char* endp;
            float d = strtof(s1, &endp);
            if (s1 != endp && *endp == '\\0' && d == 15.0f) {
                d = strtof(s2, &endp);
                return s1 == endp || *endp != '\\0' ? 0 : 1;
            }
            return 1;
        }
        ''')
    strtold = c.function_test('long double', 'const char*', 'char**', test='''
        #include <stdlib.h>
        int main() {
            char* s1 = "15";
            char* s2 = "abc";
            char* endp;
            long double d = strtold(s1, &endp);
            if (s1 != endp && *endp == '\\0' && d == 15.0l) {
                d = strtold(s2, &endp);
                return s1 == endp || *endp != '\\0' ? 0 : 1;
            }
            return 1;
        }
        ''')
    strtoll = c.function_test('long long int', 'const char*', 'char**', 'int', test='''
        #include <stdlib.h>
        int main() {
            char* s1 = "15";
            char* s2 = "abc";
            char* endp;
            long long int d = strtoll(s1, &endp, 8);
            if (s1 != endp && *endp == '\\0' && d == 13l) {
                d = strtoll(s2, &endp, 8);
                return s1 == endp || *endp != '\\0' ? 0 : 1;
            }
            return 1;
        }
        ''')
    strtoull = c.function_test('unsigned long long int', 'const char*', 'char**', 'int', test='''
        #include <stdlib.h>
        int main() {
            char* s1 = "15";
            char* s2 = "abc";
            char* endp;
            unsigned long long int d = strtoull(s1, &endp, 8);
            if (s1 != endp && *endp == '\\0' && d == 13ul) {
                d = strtoull(s2, &endp, 8);
                return s1 == endp || *endp != '\\0' ? 0 : 1;
            }
            return 1;
        }
        ''')
    _Exit = c.function_test('void', 'int', test='''
        #include <stdlib.h>
        void f() { exit(1); }
        int main() {
            _Exit(0);
            return 1;
        }
        ''')
    llabs = c.function_test('long long int', 'long long int')
    lldiv = c.function_test('lldiv_t', 'long long int', 'long long int',
        default_args=(4, 2))

# ------------------------------------------------------------------------------

# string.h didn't change in c99
string_h = c90.string_h

# ------------------------------------------------------------------------------

class tgmath_h(c.Test):
    header = c.header_test('tgmath.h')

    acos = c.macro_test()
    asin = c.macro_test()
    atan = c.macro_test()
    acosh = c.macro_test()
    asinh = c.macro_test()
    atanh = c.macro_test()
    cos = c.macro_test()
    sin = c.macro_test()
    tan = c.macro_test()
    cosh = c.macro_test()
    sinh = c.macro_test()
    tanh = c.macro_test()
    exp = c.macro_test()
    log = c.macro_test()
    pow = c.macro_test()
    sqrt = c.macro_test()
    fabs = c.macro_test()
    atan2 = c.macro_test()
    cbrt = c.macro_test()
    ceil = c.macro_test()
    copysign = c.macro_test()
    erf = c.macro_test()
    erfc = c.macro_test()
    exp2 = c.macro_test()
    expm1 = c.macro_test()
    fdim = c.macro_test()
    floor = c.macro_test()
    fma = c.macro_test()
    fmax = c.macro_test()
    fmin = c.macro_test()
    fmod = c.macro_test()
    frexp = c.macro_test()
    hypot = c.macro_test()
    ilogb = c.macro_test()
    ldexp = c.macro_test()
    lgamma = c.macro_test()
    llrint = c.macro_test()
    llround = c.macro_test()
    log10 = c.macro_test()
    log1p = c.macro_test()
    log2 = c.macro_test()
    logb = c.macro_test()
    lrint = c.macro_test()
    lround = c.macro_test()
    nearbyint = c.macro_test()
    nextafter = c.macro_test()
    nexttoward = c.macro_test()
    remainder = c.macro_test()
    remquo = c.macro_test()
    rint = c.macro_test()
    round = c.macro_test()
    scalbn = c.macro_test()
    scalbln = c.macro_test()
    tgamma = c.macro_test()
    trunc = c.macro_test()
    carg = c.macro_test()
    cimag = c.macro_test()
    conj = c.macro_test()
    cproj = c.macro_test()
    creal = c.macro_test()

# ------------------------------------------------------------------------------

class time_h(c90.time_h):
    CLOCKS_PER_SEC = c.macro_test()
    difftime = c.function_test('double', 'time_t', 'time_t')

# ------------------------------------------------------------------------------

class wchar_h(c.Test):
    header = c.header_test('wchar.h')

    wchar_t = c.int_type_test()
    size_t = c.int_type_test()
    mbstate_t = c.type_test()
    wint_t = c.int_type_test()
    tm = c.struct_test()
    NULL = c.macro_test()
    WCHAR_MAX = c.macro_test()
    WCHAR_MIN = c.macro_test()
    WEOF = c.macro_test()
    fwprintf = c.function_test('int', 'FILE*', 'const wchar_t*', test='''
        #include <stdio.h>
        #include <wchar.h>
        int main() {
            return fwprintf(stdout, L"%d %d", 5, 6) ? 0 : 1;
        }
        ''', stdout=b'5 6')
    fwscanf = c.function_test('int', 'FILE*', 'const wchar_t*', test='''
        #include <stdio.h>
        #include <wchar.h>
        int main() {
            int x = 0, y = 0;
            return fwscanf(stdin, L"%d %d", &x, &y) &&
                x == 5 &&
                y == 6 ? 0 : 1;
        }
        ''', stdin=b'5 6')
    swprintf = c.function_test('int', 'wchar_t*', 'size_t', 'const wchar_t*', test='''
        #include <wchar.h>
        int main() {
            wchar_t s[50];
            return swprintf(s, 50, L"%d %d", 5, 6) &&
                s[0] == L'5' &&
                s[1] == L' ' &&
                s[2] == L'6' &&
                s[3] == L'\\0' ? 0 : 1;
        }
        ''')
    swscanf = c.function_test('int', 'const wchar_t*', 'const wchar_t*', test='''
        #include <wchar.h>
        int main() {
            int x = 0, y = 0;
            return swscanf(L"5 6", L"%d %d", &x, &y) &&
                x == 5 &&
                y == 6 ? 0 : 1;
        }
        ''')
    vfwprintf = c.function_test('int', 'FILE*', 'const wchar_t*', 'va_list', test='''
        #include <stdarg.h>
        #include <stdio.h>
        #include <wchar.h>
        int f(wchar_t* s, ...) {
            va_list ap;
            va_start(ap, s);
            return vfwprintf(stdout, s, ap);
        }
        int main() {
            return f(L"%d %d", 5, 6) ? 0 : 1;
        }
        ''', stdout=b'5 6')
    vfwscanf = c.function_test('int', 'FILE*', 'const wchar_t*', 'va_list', test='''
        #include <stdarg.h>
        #include <stdio.h>
        #include <wchar.h>
        #ifdef _WIN32
        #include <windows.h>
        #endif

        int f(wchar_t* s, ...) {
            va_list ap;
            va_start(ap, s);
            return vfwscanf(stdin, s, ap);
        }
        int main() {
            int x = 0, y = 0;
        #ifdef _WIN32
            /* Turn off the windows error box */
            DWORD dwMode = SetErrorMode(SEM_NOGPFAULTERRORBOX);
            SetErrorMode(dwMode | SEM_NOGPFAULTERRORBOX);
        #endif
            return f(L"%d %d", &x, &y) &&
                x == 5 &&
                y == 6 ? 0 : 1;
        }
        ''', stdin=b'5 6')
    vswprintf = c.function_test('int', 'wchar_t*', 'size_t', 'const wchar_t*', 'va_list', test='''
        #include <stdarg.h>
        #include <wchar.h>
        #ifdef _WIN32
        #include <windows.h>
        #endif

        int f(wchar_t* s, ...) {
            va_list ap;
            va_start(ap, s);
            return vswprintf(s, 50, L"%d %d", ap);
        }
        int main() {
            wchar_t s[50] = {0};
        #ifdef _WIN32
            /* Turn off the windows error box */
            DWORD dwMode = SetErrorMode(SEM_NOGPFAULTERRORBOX);
            SetErrorMode(dwMode | SEM_NOGPFAULTERRORBOX);
        #endif
            return f(s, 5, 6) &&
                s[0] == L'5' &&
                s[1] == L' ' &&
                s[2] == L'6' &&
                s[3] == L'\\0' ? 0 : 1;
        }
        ''')
    vswscanf = c.function_test('int', 'const wchar_t*', 'const wchar_t*', 'va_list', test='''
        #include <stdarg.h>
        #include <wchar.h>
        int f(wchar_t* s, ...) {
            va_list ap;
            va_start(ap, s);
            return vswscanf(L"5 6", s, ap);
        }
        int main() {
            int x = 0, y = 0;
            return f(L"%d %d", &x, &y) && x == 5 && y == 6 ? 0 : 1;
            return 0;
        }
        ''')
    vwprintf = c.function_test('int', 'const wchar_t*', 'va_list', test='''
        #include <stdarg.h>
        #include <wchar.h>
        int f(char* s, ...) {
            va_list ap;
            va_start(ap, s);
            return vwprintf(s, ap);
        }
        int main() {
            return f(L"%d %d", 5, 6) ? 0 : 1;
        }
        ''', stdout=b'5 6')
    vwscanf = c.function_test('int', 'const wchar_t*', 'va_list', test='''
        #include <stdarg.h>
        #include <wchar.h>
        #ifdef _WIN32
        #include <windows.h>
        #endif

        int f(const wchar_t* s, ...) {
            va_list ap;
            va_start(ap, s);
            return vwscanf(s, ap);
        }
        int main() {
            int x = 0, y = 0;
        #ifdef _WIN32
            /* Turn off the windows error box */
            DWORD dwMode = SetErrorMode(SEM_NOGPFAULTERRORBOX);
            SetErrorMode(dwMode | SEM_NOGPFAULTERRORBOX);
        #endif
            return f(L"%d %d", &x, &y) && x == 5 && y == 6 ? 0 : 1;
        }
        ''', stdin=b'5 6')
    wprintf = c.function_test('int', 'const wchar_t*', test='''
        #include <wchar.h>
        int main() {
            return wprintf(L"%d %d", 5, 6) ? 0 : 1;
        }
        ''', stdout=b'5 6')
    wscanf = c.function_test('int', 'const wchar_t*', test='''
        #include <wchar.h>
        int main() {
            int x = 0, y = 0;
            return wscanf(L"%d %d", &x, &y) &&
                x == 5 &&
                y == 6 ? 0 : 1;
        }
        ''', stdin=b'5 6')
    fgetwc = c.function_test('wint_t', 'FILE*', test='''
        #include <stdio.h>
        #include <wchar.h>
        int main() {
            return fgetwc(stdin) == L'5' ? 0 : 1;
        }
        ''', stdin=b'5')
    fgetws = c.function_test('wchar_t*', 'wchar_t*', 'int', 'FILE*', test='''
        #include <stdio.h>
        #include <wchar.h>
        int main() {
            wchar_t s[50] = {0};
            return fgetws(s, 4, stdin) &&
                s[0] == L'5' &&
                s[1] == L' ' &&
                s[2] == L'6' &&
                s[3] == L'\\0' ? 0 : 1;
        }
        ''', stdin=b'5 6')
    fputwc = c.function_test('wint_t', 'wchar_t', 'FILE*', test='''
        #include <stdio.h>
        #include <wchar.h>
        int main() {
            return fputwc(L'5', stdout) == L'5' ? 0 : 1;
        }
        ''', stdout=b'5')
    fputws = c.function_test('int', 'const wchar_t*', 'FILE*', test='''
        #include <stdio.h>
        #include <wchar.h>
        int main() {
            return fputws(L"5 6", stdout) ? 0 : 1;
        }
        ''', stdout=b'5 6')
    fwide = c.function_test('FILE*', 'int', 'int', test='''
        #include <stdio.h>
        #include <wchar.h>
        int main() {
            return fwide(stdout, 0) == 0 ? 0 : 1;
        }
        ''')
    getwc = c.function_test('wint_t', 'FILE*', test='''
        #include <stdio.h>
        #include <wchar.h>
        int main() {
            return getwc(stdin) == 'c' ? 0 : 1;
        }
        ''', stdin=b'c')
    getwchar = c.function_test('wint_t', 'void', test='''
        #include <wchar.h>
        int main() {
            return getwchar() == 'c' ? 0 : 1;
        }
        ''', stdin=b'c')
    putwc = c.function_test('wint_t', 'wchar_t', 'FILE*', test='''
        #include <stdio.h>
        #include <wchar.h>
        int main() {
            return putwc(L'5', stdout) == L'5' ? 0 : 1;
        }
    ''', stdout=b'5')
    putwchar = c.function_test('wint_t', 'wchar_t', test='''
        #include <wchar.h>
        int main() {
            return putwchar(L'5') == L'5' ? 0 : 1;
        }
        ''', stdout=b'5')
    ungetwc = c.function_test('wint_t', 'wint_t', 'FILE*', test='''
        #include <stdio.h>
        #include <wchar.h>
        int main() {
            if (ungetwc(L'5', stdin) != L'5') return 1;
            return getwc(stdin) == L'5' ? 0 : 1;
        }
        ''')
    wcstod = c.function_test('double', 'const wchar_t*', 'wchar_t**', test='''
        #include <wchar.h>
        int main() {
            wchar_t* s1 = L"0.5";
            wchar_t* s2 = L"abc";
            wchar_t* endp;
            double d = wcstod(s1, &endp);
            if (s1 != endp && *endp == L'\\0' && d == 0.5) {
                d = wcstod(s2, &endp);
                return s1 == endp || *endp != '\\0' ? 0 : 1;
            }
            return 1;
        }
        ''')
    wcstof = c.function_test('float', 'const wchar_t*', 'wchar_t**', test='''
        #include <wchar.h>
        int main() {
            wchar_t* s1 = L"0.5";
            wchar_t* s2 = L"abc";
            wchar_t* endp;
            float d = wcstof(s1, &endp);
            if (s1 != endp && *endp == L'\\0' && d == 0.5f) {
                d = wcstof(s2, &endp);
                return s1 == endp || *endp != '\\0' ? 0 : 1;
            }
            return 1;
        }
        ''')
    wcstold = c.function_test('long double', 'const wchar_t*', 'wchar_t**', test='''
        #include <wchar.h>
        int main() {
            wchar_t* s1 = L"0.5";
            wchar_t* s2 = L"abc";
            wchar_t* endp;
            long double d = wcstold(s1, &endp);
            if (s1 != endp && *endp == L'\\0' && d == 0.5l) {
                d = wcstold(s2, &endp);
                return s1 == endp || *endp != '\\0' ? 0 : 1;
            }
            return 1;
        }
        ''')
    wcstol = c.function_test('long int', 'const wchar_t*', 'wchar_t**', 'int', test='''
        #include <wchar.h>
        int main() {
            wchar_t* s1 = L"15";
            wchar_t* s2 = L"abc";
            wchar_t* endp;
            long int d = wcstol(s1, &endp, 8);
            if (s1 != endp && *endp == L'\\0' && d == 13) {
                d = wcstol(s2, &endp, 8);
                return s1 == endp || *endp != '\\0' ? 0 : 1;
            }
            return 1;
        }
        ''')
    wcstoll = c.function_test('long long int', 'const wchar_t*', 'wchar_t**', 'int', test='''
        #include <wchar.h>
        int main() {
            wchar_t* s1 = L"15";
            wchar_t* s2 = L"abc";
            wchar_t* endp;
            long long int d = wcstoll(s1, &endp, 8);
            if (s1 != endp && *endp == L'\\0' && d == 13l) {
                d = wcstoll(s2, &endp, 8);
                return s1 == endp || *endp != '\\0' ? 0 : 1;
            }
            return 1;
        }
        ''')
    wcstoul = c.function_test('unsigned long int', 'const wchar_t*', 'wchar_t**', 'int', test='''
        #include <wchar.h>
        int main() {
            wchar_t* s1 = L"15";
            wchar_t* s2 = L"abc";
            wchar_t* endp;
            unsigned long int d = wcstoul(s1, &endp, 8);
            if (s1 != endp && *endp == L'\\0' && d == 13ul) {
                d = wcstoul(s2, &endp, 8);
                return s1 == endp || *endp != '\\0' ? 0 : 1;
            }
            return 1;
        }
        ''')
    wcstoull = c.function_test('unsigned long long int', 'const wchar_t*', 'wchar_t**', 'int', test='''
        #include <wchar.h>
        int main() {
            wchar_t* s1 = L"15";
            wchar_t* s2 = L"abc";
            wchar_t* endp;
            unsigned long long int d = wcstoull(s1, &endp, 8);
            if (s1 != endp && *endp == L'\\0' && d == 13ull) {
                d = wcstoull(s2, &endp, 8);
                return s1 == endp || *endp != '\\0' ? 0 : 1;
            }
            return 1;
        }
        ''')
    wcscpy = c.function_test('wchar_t*', 'wchar_t*', 'const wchar_t*', test='''
        #include <wchar.h>
        int main() {
            wchar_t s[] = {L'a', L'b', L'c', L'd', L'e', L'f', L'\\0'};
            return wcscpy(s, L"1234") == s &&
                s[0] == L'1' &&
                s[1] == L'2' &&
                s[2] == L'3' &&
                s[3] == L'4' &&
                s[4] == L'\\0' &&
                s[5] == L'f' &&
                s[6] == L'\\0' ? 0 : 1;
        }
        ''')
    wcsncpy = c.function_test('wchar_t*', 'wchar_t*', 'const wchar_t*', 'size_t', test='''
        #include <wchar.h>
        int main() {
            wchar_t s[] = {L'a', L'b', L'c', L'd', L'e', L'f', L'\\0'};
            return wcsncpy(s, L"1234", 4) == s &&
                s[0] == L'1' &&
                s[1] == L'2' &&
                s[2] == L'3' &&
                s[3] == L'4' &&
                s[4] == L'e' &&
                s[5] == L'f' &&
                s[6] == L'\\0' ? 0 : 1;
        }
        ''')
    wmemcpy = c.function_test('wchar_t*', 'wchar_t*', 'const wchar_t*', 'size_t', test='''
        #include <wchar.h>
        int main() {
            wchar_t s[] = {L'a', L'a', L'a', L'a', L'a', L'a'};
            return wmemcpy(s, L"1234", 4) == s &&
                s[0] == L'1' &&
                s[1] == L'2' &&
                s[2] == L'3' &&
                s[3] == L'4' &&
                s[4] == L'a' &&
                s[5] == L'a' ? 0 : 1;
        }
        ''')
    wmemmove = c.function_test('wchar_t*', 'wchar_t*', 'const wchar_t*', 'size_t', test='''
        #include <wchar.h>
        int main() {
            wchar_t s[] = {L'a', L'a', L'a', L'a', L'a', L'a'};
            return wmemmove(s, L"1234", 4) == s &&
                s[0] == L'1' &&
                s[1] == L'2' &&
                s[2] == L'3' &&
                s[3] == L'4' &&
                s[4] == L'a' &&
                s[5] == L'a' ? 0 : 1;
        }
        ''')
    wcscat = c.function_test('wchar_t*', 'wchar_t*', 'const wchar_t*', test='''
        #include <wchar.h>
        int main() {
            wchar_t s[] = {L'1', L' ', L'2', L'\\0', L'\\0', L'\\0', L'\\0'};
            return wcscat(s, L"5 6") == s &&
                s[0] == L'1' &&
                s[1] == L' ' &&
                s[2] == L'2' &&
                s[3] == L'5' &&
                s[4] == L' ' &&
                s[5] == L'6' &&
                s[6] == L'\\0' ? 0 : 1;
        }
        ''')
    wcsncat = c.function_test('wchar_t*', 'wchar_t*', 'const wchar_t*', 'size_t', test='''
        #include <wchar.h>
        int main() {
            wchar_t s[] = {L'1', L' ', L'2', L'\\0', L'\\0', L'\\0', L'\\0'};
            return wcsncat(s, L"5 6", 2) == s &&
                s[0] == L'1' &&
                s[1] == L' ' &&
                s[2] == L'2' &&
                s[3] == L'5' &&
                s[4] == L' ' &&
                s[5] == L'\\0' ? 0 : 1;
        }
        ''')
    wcscmp = c.function_test('int', 'const wchar_t*', 'const wchar_t*', test='''
        #include <wchar.h>
        int main() {
            return
                  wcscmp(L"1 2\\0 3", L"1 2\\0 4") &&
                 !wcscmp(L"1 2\\0 3", L"2 2\\0 4");
        }
        ''')
    wcscoll = c.function_test('int', 'const wchar_t*', 'const wchar_t*', test='''
        #include <wchar.h>
        int main() {
            return
                  wcscoll("1 2\\0 3", "1 2\\0 4") &&
                 !wcscoll("1 2\\0 3", "2 2\\0 4");
        }
        ''')
    wcsncmp = c.function_test('int', 'const wchar_t*', 'const wchar_t*', 'size_t', test='''
        #include <wchar.h>
        int main() {
            return
                  wcsncmp(L"1 2 3", L"1 2 4", 4) &&
                 !wcsncmp(L"1 2 3", L"2 2 4", 4);
        }
        ''')
    wcsxfrm = c.function_test('size_t', 'wchar_t*', 'const wchar_t*', 'size_t', test='''
        #include <string.h>
        int main() {
            return wcsxfrm(NULL, L"abcd", 0) == 4 ? 0 : 1;
        }
        ''')
    wmemcmp = c.function_test('int', 'const wchar_t*', 'const wchar_t*', 'size_t', test='''
        #include <string.h>
        int main() {
            return
                 wmemcmp(L"1 2\\0 3", L"1 2\\0 3", 6) &&
                !wmemcmp(L"1 2\\0 3", L"2 2\\0 3", 6);
        }
        ''')
    wcschr = c.function_test('wchar_t*', 'const wchar_t*', 'wchar_t', test='''
        #include <wchar.h>
        int main() {
            wchar_t* s = L"abcdc";
            return
                wcschr(s, L'c') == &s[2] &&
                wcschr(s, L'f') == NULL ? 0 : 1;
        }
        ''')
    wcscspn = c.function_test('size_t', 'const wchar_t*', 'const wchar_t*', test='''
        #include <wchar.h>
        int main() {
            return wcscspn(L"abc123", L"1234567890") == 3 ? 0 : 1;
        }
        ''')
    wcspbrk = c.function_test('wchar_t*', 'const wchar_t*', 'const wchar_t*', test='''
        #include <wchar.h>
        int main() {
            wchar_t s[] = L"abcde";
            return
                wcspbrk(s, L"bd") == &s[1] &&
                wcspbrk(s, L"xd") == &s[3] &&
                wcspbrk(s, L"xy") == NULL ? 0 : 1;
        }
        ''')
    wcsrchr = c.function_test('wchar_t*', 'const wchar_t*', 'wchar_t', test='''
        #include <wchar.h>
        int main() {
            wchar_t* s = L"abcdc";
            return
                wcsrchr(s, L'c') == &s[4] &&
                wcsrchr(s, L'f') == NULL ? 0 : 1;
        }
        ''')
    wcsspn = c.function_test('size_t', 'const wchar_t*', 'const wchar_t*', test='''
        #include <wchar.h>
        int main() {
            return wcsspn(L"123abc", L"1234567890") == 3 ? 0 : 1;
        }
        ''')
    wcsstr = c.function_test('wchar_t*', 'const wchar_t*', 'const wchar_t*', test='''
        #include <wchar.h>
        int main() {
            wchar_t* s = L"abcdef";
            return
                wcsstr(s, L"cde") == &s[2] &&
                wcsstr(s, L"cdf") == NULL ? 0 : 1;
        }
        ''')
    wcstok = c.function_test('wchar_t*', 'wchar_t*', 'const wchar_t*', 'wchar_t**', test='''
        #include <wchar.h>
        int main() {
            wchar_t *last, s[50] = L"a,b,c";
            wchar_t* p = wcstok(s, L",", &last);
            return p && p[0] == L'a' && p[1] == L'\\0' ? 0 : 1;
        }
        ''')
    wmemchr = c.function_test('wchar_t*', 'const wchar_t*', 'wchar_t', 'size_t', test='''
        #include <wchar.h>
        int main() {
            wchar_t* s = L"ab\\0dc";
            return
                wmemchr(s, L'\\0', 5) == &s[2] &&
                wmemchr(s, L'f', 5) == NULL ? 0 : 1;
        }
        ''')
    wcslen = c.function_test('size_t', 'const wchar_t*', test='''
        #include <wchar.h>
        int main() {
            return wcslen(L"abcde") == 5 ? 0 : 1;
        }
        ''')
    wmemset = c.function_test('wchar_t*', 'wchar_t*', 'wchar_t', 'size_t', test='''
        #include <wchar.h>
        int main() {
            wchar_t s[50] = {0};
            int i;
            if (wmemset(s, L'b', 50) != s) return 1;
            for (i = 0; i < 50; ++i) if (s[i] != L'b') return 1;
            return 0;
        }
        ''')
    wcsftime = c.function_test('size_t', 'wchar_t*', 'size_t', 'const wchar_t*', 'const struct tm*', test='''
        #include <time.h>
        #include <wchar.h>
        int main() {
            struct tm t = { 0 };
            wchar_t s[50] = {0};
            return wcsftime(s, 50, L" ", &t) == 1 ? 0 : 1;
        }
        ''')
    btowc = c.function_test('wint_t', 'int')
    wctob = c.function_test('int', 'wint_t')
    mbsinit = c.function_test('int', 'const mbstate_t*')
    mbrlen = c.function_test('size_t', 'const char*', 'size_t', 'mbstate_t*',
        default_args=('""', 0, 'NULL'))
    mbrtowc = c.function_test('size_t', 'wchar_t*', 'const char*', 'size_t', 'mbstate_t*', test='''
        #include <wchar.h>
        int main() {
            wchar_t s[50];
            return mbrtowc(s, "5", 50, NULL) == 1 && s[0] == L'5' ? 0 : 1;
        }
        ''')
    wcrtomb = c.function_test('size_t', 'char*', 'wchar_t', 'mbstate_t*')
    mbsrtowcs = c.function_test('size_t', 'wchar_t*', 'const char**', 'size_t', 'mbstate_t*', test='''
        #include <wchar.h>
        int main() {
            char* s1 = "abcd";
            wchar_t s2[50] = {0};
            return mbsrtowcs(s2, &s1, 4, NULL) == 4 &&
                s2[0] == L'a' &&
                s2[1] == L'b' &&
                s2[2] == L'c' &&
                s2[3] == L'd' ? 0 : 1;
        }
        ''')
    wcsrtombs = c.function_test('size_t', 'char*', 'const wchar_t**', 'size_t', 'mbstate_t*', test='''
        #include <wchar.h>
        int main() {
            wchar_t* s1 = L"abcd";
            char s2[50] = {0};
            return wcsrtombs(s2, &s1, 4, NULL) == 4 &&
                s2[0] == 'a' &&
                s2[1] == 'b' &&
                s2[2] == 'c' &&
                s2[3] == 'd' ? 0 : 1;
        }
        ''')

# ------------------------------------------------------------------------------

class wctype_h(c.Test):
    header = c.header_test('wctype.h')

    wint_t = c.type_test()
    wctrans_t = c.type_test()
    wctype_t = c.type_test()
    WEOF = c.macro_test()
    iswalnum = c.function_test('int', 'wint_t')
    iswalpha = c.function_test('int', 'wint_t')
    iswblank = c.function_test('int', 'wint_t')
    iswcntrl = c.function_test('int', 'wint_t')
    iswdigit = c.function_test('int', 'wint_t')
    iswgraph = c.function_test('int', 'wint_t')
    iswlower = c.function_test('int', 'wint_t')
    iswprint = c.function_test('int', 'wint_t')
    iswpunct = c.function_test('int', 'wint_t')
    iswspace = c.function_test('int', 'wint_t')
    iswupper = c.function_test('int', 'wint_t')
    iswxdigit = c.function_test('int', 'wint_t')
    iswctype = c.function_test('int', 'wint_t', 'wctype_t')
    wctype = c.function_test('wctype_t', 'const char*')
    towlower = c.function_test('wint_t', 'wint_t')
    towupper = c.function_test('wint_t', 'wint_t')
    towctrans = c.function_test('wint_t', 'wint_t', 'wctrans_t')
    wctrans = c.function_test('wctrans_t', 'const char*')
