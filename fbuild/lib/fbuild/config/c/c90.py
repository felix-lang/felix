import tempfile

import fbuild
import fbuild.config as config
import fbuild.config.c as c
import fbuild.db
import fbuild.temp

# ------------------------------------------------------------------------------

class types(c.Test):
    # We put char last as the signedness is platform dependent.
    signed_char = c.int_type_test(name='signed char')
    unsigned_char = c.int_type_test(name='unsigned char')
    char = c.int_type_test()

    short = c.int_type_test()
    signed_short = c.int_type_test(name='signed short')
    unsigned_short = c.int_type_test(name='unsigned short')

    int = c.int_type_test()
    signed_int = c.int_type_test(name='signed int')
    unsigned_int = c.int_type_test(name='unsigned int')

    long = c.int_type_test()
    signed_long = c.int_type_test(name='signed long')
    unsigned_long = c.int_type_test(name='unsigned long')

    long_long = c.int_type_test(name='long long')
    signed_long_long = c.int_type_test(name='signed long long')
    unsigned_long_long = c.int_type_test(name='unsigned long long')

    float = c.type_test()
    double = c.type_test()
    long_double = c.type_test(name='long double')

    voidp = c.type_test(name='void*')
    enum = c.type_test(test='''
        #include <stddef.h>
        #include <stdio.h>

        typedef enum enum_t {tag} type;
        struct TEST { char c; type mem; };
        int main() {
            printf("%d\\n", (int)offsetof(struct TEST, mem));
            printf("%d\\n", (int)sizeof(type));
            return 0;
        }
        ''')

    def structural_alias(self, ctype):
        if ctype is None:
            return None

        for name, type_ in self.int_types():
            if type_ is None:
                continue

            if isinstance(ctype, c.IntType):
                if type_ == ctype:
                    return name
            elif type_.size == ctype.size and \
                    type_.alignment == ctype.alignment:
                return name
        return None

    @fbuild.db.cachemethod
    def conversion_map(self, *args, **kwargs):
        type_pairs = [(name1, name2)
            for name1, type1 in self.int_types() if type1 is not None
            for name2, type2 in self.int_types() if type2 is not None]

        lines = []
        for t1, t2 in type_pairs:
            lines.append(
                'printf("%%d %%d\\n", '
                '(int)sizeof((%(t1)s)0 + (%(t2)s)0), '
                '(%(t1)s)~3 + (%(t2)s)1 < (%(t1)s)0 + (%(t2)s)0);' %
                {'t1': t1, 't2': t2})

        code = '''
        #include <stdio.h>
        int main(int argc, char** argv) {
            %s
            return 0;
        }
        ''' % '\n'.join(lines)

        try:
            stdout, stderr = self.builder.tempfile_run(code, *args, **kwargs)
        except fbuild.ExecutionError:
            raise fbuild.ConfigFailed('failed to detect type conversions')

        lookup = {(t.size, t.signed): self.structural_alias(t)
            for n, t in self.int_types() if t is not None}

        d = {}
        for line, (t1, t2) in zip(
                stdout.decode('utf-8').split('\n'),
                type_pairs):
            size, sign = line.split()
            d[(t1, t2)] = lookup[(int(size), int(sign) == 1)]

        return d

# ------------------------------------------------------------------------------

class assert_h(c.Test):
    header = c.header_test('assert.h')

    @c.cacheproperty
    def assert_(self):
        self.ctx.logger.check("checking assert in 'assert.h'")

        if not self.builder.try_run('''
                #include <assert.h>
                int main() {
                    assert(1);
                    return 0;
                }
                '''):
            self.ctx.logger.failed()
            return None

        if self.builder.try_run('''
                #include <assert.h>
                #ifdef _WIN32
                #include <windows.h>
                #endif

                int main() {
                #ifdef _WIN32
                    /* Turn off the windows error box */
                    DWORD dwMode = SetErrorMode(SEM_NOGPFAULTERRORBOX);
                    SetErrorMode(dwMode | SEM_NOGPFAULTERRORBOX);
                #endif
                    assert(0);
                    return 0;
                }
                '''):
            self.ctx.logger.failed()
            return None

        if not self.builder.try_run('''
                #include <assert.h>
                int main() {
                    assert(0);
                    return 0;
                }
                ''', ckwargs={'macros': ['NDEBUG']}):
            self.ctx.logger.failed()
            return None

        self.ctx.logger.passed()
        return c.Macro()

# ------------------------------------------------------------------------------

class ctype_h(c.Test):
    header = c.header_test('ctype.h')

    isalnum = c.function_test('int', 'int')
    isalpha = c.function_test('int', 'int')
    iscntrl = c.function_test('int', 'int')
    isdigit = c.function_test('int', 'int')
    isgraph = c.function_test('int', 'int')
    islower = c.function_test('int', 'int')
    isprint = c.function_test('int', 'int')
    ispunct = c.function_test('int', 'int')
    isspace = c.function_test('int', 'int')
    isupper = c.function_test('int', 'int')
    isxdigit = c.function_test('int', 'int')
    tolower = c.function_test('int', 'int')
    toupper = c.function_test('int', 'int')

# -----------------------------------------------------------------------------

class errno_h(c.Test):
    header = c.header_test('errno.h')

    EDOM = c.macro_test()
    ERANGE = c.macro_test()
    errno = c.variable_test()

# -----------------------------------------------------------------------------

class float_h(c.Test):
    header = c.header_test('float.h')

    DBL_DIG = c.macro_test()
    DBL_EPSILON = c.macro_test()
    DBL_MANT_DIG = c.macro_test()
    DBL_MAX = c.macro_test()
    DBL_MAX_10_EXP = c.macro_test()
    DBL_MAX_EXP = c.macro_test()
    DBL_MIN = c.macro_test()
    DBL_MIN_10_EXP = c.macro_test()
    DBL_MIN_EXP = c.macro_test()
    FLT_DIG = c.macro_test()
    FLT_EPSILON = c.macro_test()
    FLT_MANT_DIG = c.macro_test()
    FLT_MAX = c.macro_test()
    FLT_MAX_10_EXP = c.macro_test()
    FLT_MAX_EXP = c.macro_test()
    FLT_MIN = c.macro_test()
    FLT_MIN_10_EXP = c.macro_test()
    FLT_MIN_EXP = c.macro_test()
    FLT_RADIX = c.macro_test()
    FLT_ROUNDS = c.macro_test()
    LDBL_DIG = c.macro_test()
    LDBL_EPSILON = c.macro_test()
    LDBL_MANT_DIG = c.macro_test()
    LDBL_MAX = c.macro_test()
    LDBL_MAX_10_EXP = c.macro_test()
    LDBL_MAX_EXP = c.macro_test()
    LDBL_MIN = c.macro_test()
    LDBL_MIN_10_EXP = c.macro_test()
    LDBL_MIN_EXP = c.macro_test()

# -----------------------------------------------------------------------------

class limits_h(c.Test):
    header = c.header_test('limits.h')

    CHAR_BIT = c.macro_test()
    CHAR_MAX = c.macro_test()
    CHAR_MIN = c.macro_test()
    INT_MAX = c.macro_test()
    INT_MIN = c.macro_test()
    LONG_MAX = c.macro_test()
    LONG_MIN = c.macro_test()
    MB_LEN_MAX = c.macro_test()
    SCHAR_MAX = c.macro_test()
    SCHAR_MIN = c.macro_test()
    SHRT_MAX = c.macro_test()
    SHRT_MIN = c.macro_test()
    UCHAR_MAX = c.macro_test()
    UINT_MAX = c.macro_test()
    ULONG_MAX = c.macro_test()
    USHRT_MAX = c.macro_test()

# -----------------------------------------------------------------------------

class locale_h(c.Test):
    header = c.header_test('locale.h')

    LC_ALL = c.macro_test()
    LC_COLLATE = c.macro_test()
    LC_CTYPE = c.macro_test()
    LC_MONETARY = c.macro_test()
    LC_NUMERIC = c.macro_test()
    LC_TIME = c.macro_test()
    NULL = c.macro_test()
    lconv = c.struct_test(
         ('char*', 'decimal_point'),
         ('char*', 'thousands_sep'),
         ('char*', 'grouping'),
         ('char*', 'int_curr_symbol'),
         ('char*', 'currency_symbol'),
         ('char*', 'mon_decimal_point'),
         ('char*', 'mon_thousands_sep'),
         ('char*', 'mon_grouping'),
         ('char*', 'positive_sign'),
         ('char*', 'negative_sign'),
         ('char', 'int_frac_digits'),
         ('char', 'frac_digits'),
         ('char', 'p_cs_precedes'),
         ('char', 'p_sep_by_space'),
         ('char', 'n_cs_precedes'),
         ('char', 'n_sep_by_space'),
         ('char', 'p_sign_posn'),
         ('char', 'n_sign_posn'))
    setlocale = c.function_test('char*', 'int', 'const char*')
    localeconv = c.function_test('struct lconv*', 'void')

# -----------------------------------------------------------------------------

class math_h(c.Test):
    header = c.header_test('math.h')

    HUGE_VAL = c.macro_test()
    acos = c.function_test('double', 'double')
    asin = c.function_test('double', 'double')
    atan = c.function_test('double', 'double')
    atan2 = c.function_test('double', 'double', 'double')
    cos = c.function_test('double', 'double')
    sin = c.function_test('double', 'double')
    tan = c.function_test('double', 'double')
    cosh = c.function_test('double', 'double')
    sinh = c.function_test('double', 'double')
    tanh = c.function_test('double', 'double')
    exp = c.function_test('double', 'double')
    frexp = c.function_test('double', 'double', 'int*', test='''
        #include <math.h>
        int main() {
            int d0;
            double d1 = frexp(0., &d0);
            return 0;
        }
        ''')
    ldexp = c.function_test('double', 'double', 'int')
    log = c.function_test('double', 'double')
    log10 = c.function_test('double', 'double')
    modf = c.function_test('double', 'double', 'double*', test='''
        #include <math.h>
        int main() {
            double d0;
            double d1 = modf(0., &d0);
            return 0;
        }
        ''')
    pow = c.function_test('double', 'double', 'double')
    sqrt = c.function_test('double', 'double')
    ceil = c.function_test('double', 'double')
    fabs = c.function_test('double', 'double')
    floor = c.function_test('double', 'double')
    fmod = c.function_test('double', 'double', 'double')

# -----------------------------------------------------------------------------

class setjmp_h(c.Test):
    header = c.header_test('setjmp.h')

    jmp_buf = c.type_test()

    longjmp = c.function_test('void', 'jmp_buf', 'int', test='''
        #include <setjmp.h>
        int main() {
            jmp_buf env;
            int i = setjmp(env);
            if (i == 2) return 0;
            longjmp(env, 2);
            return 2;
        }
        ''')

    @property
    def setjmp(self):
        if self.longjmp:
            return c.Function('int', 'jmp_buf')

# -----------------------------------------------------------------------------

class signal_h(c.Test):
    header = c.header_test('signal.h')

    sig_atomic_t = c.type_test()
    SIG_DFL = c.macro_test()
    SIG_ERR = c.macro_test()
    SIG_IGN = c.macro_test()
    SIGABRT = c.macro_test()
    SIGFPE = c.macro_test()
    SIGILL = c.macro_test()
    SIGINT = c.macro_test()
    SIGSEGV = c.macro_test()
    SIGTERM = c.macro_test()
    signal = c.function_test(
        c.Function('void', 'int'), 'int', c.Function('void', 'int'),
        test='''
        #include <signal.h>
        void foo(int x) {}
        int main() {
            void (*f)(int) = signal(SIGTERM, foo);
            return 0;
        }
        ''')
    raise_ = c.function_test('int', 'int', name='raise', test='''
        #include <signal.h>
        int main() {
            signal(SIGTERM, SIG_IGN);
            return raise(SIGTERM) == 0 ? 0 : 1;
        }
        ''')

# -----------------------------------------------------------------------------

class stdarg_h(c.Test):
    header = c.header_test('stdarg.h')

    va_list = c.type_test()
    va_start = c.macro_test(test='''
        #include <stdarg.h>
        int f(int x, ...) {
            int res = 0;
            va_list ap;
            va_start(ap, x);
            res = x == 1 &&
                va_arg(ap, int) == 3 &&
                va_arg(ap, int) == 'a' &&
                va_arg(ap, double) == 4.5;
            va_end(ap);
            return res;
        }
        int main() {
            return f(1, 3, 'a', 4.5) == 1 ? 0 : 1;
        }
        ''')
    va_arg = c.macro_test(test=va_list.test)
    va_end = c.macro_test(test=va_list.test)

# -----------------------------------------------------------------------------

class stddef_h(c.Test):
    header = c.header_test('stddef.h')

    NULL = c.macro_test()
    offsetof = c.macro_test()
    ptrdiff_t = c.int_type_test()
    size_t = c.int_type_test()
    wchar_t = c.int_type_test()

# -----------------------------------------------------------------------------

class stdio_h(c.Test):
    header = c.header_test('stdio.h')

    _IOFBF = c.macro_test()
    _IOLBF = c.macro_test()
    _IONBF = c.macro_test()
    BUFSIZ = c.macro_test()
    EOF = c.macro_test()
    FILE = c.type_test()
    FILENAME_MAX = c.macro_test()
    FOPEN_MAX = c.macro_test()
    fpos_t = c.type_test()
    L_tmpnam = c.macro_test()
    NULL = c.macro_test()
    SEEK_CUR = c.macro_test()
    SEEK_END = c.macro_test()
    SEEK_SET = c.macro_test()
    size_t = c.int_type_test()
    stderr = c.variable_test()
    stdin = c.variable_test()
    stdout = c.variable_test()
    TMP_MAX = c.macro_test()
    remove = c.function_test('int', 'const char*', default_args=('""',))
    rename = c.function_test('int', 'const char*', 'const char*')
    tmpfile = c.function_test('FILE*', 'void')
    tmpnam = c.function_test('char*', 'char*', default_args=('NULL',))

    @c.cacheproperty
    def fclose(self):
        if self.fopen:
            return c.Function('int', 'FILE*')

    fflush = c.function_test('int', 'FILE*', test='''
        #include <stdio.h>
        int main() {
            return fflush(stdout) == 0 ? 0 : 1;
        }
        ''')

    @c.cacheproperty
    def fopen(self):
        with tempfile.NamedTemporaryFile() as f:
            test = '''
                #include <errno.h>
                #include <stdio.h>
                int main() {
                    FILE* f;
                    fpos_t p;
                    if (!(f = fopen("%s", "r"))) return 1;
                    if (ftell(f) != 0) return 1;
                    if (fgetpos(f, &p) != 0) return 1;
                    if (fseek(f, 1L, SEEK_CUR) != 0) return 1;
                    if (ftell(f) != 1) return 1;
                    if (fsetpos(f, &p) != 0) return 1;
                    if (ftell(f) != 0) return 1;
                    if (fflush(NULL) != 0) return 1;
                    if (!(f = freopen(NULL, "r", f))) return 1;
                    errno = 0;
                    rewind(f);
                    if (errno) return 1;
                    if (ferror(f) != 0) return 1;
                    if (feof(f) != 0) return 1;
                    if (fclose(f) != 0) return 1;
                    return 0;
                }
                ''' % f.name

            if self.builder.check_run(test, "checking fopen in 'stdio.h'"):
                return c.Function('FILE*', 'const char*', 'const char*')

    @c.cacheproperty
    def freopen(self):
        if self.fopen:
            return c.Function('FILE*', 'const char*', 'const char*', 'FILE*')

    setbuf = c.function_test('void', 'FILE*', 'char*', test='''
        #include <stdio.h>
        int main() {
            setbuf(stdout, "");
            return 0;
        }
        ''')
    setvbuf = c.function_test('int', 'FILE*', 'char*', 'int', 'size_t', test='''
        #include <stdio.h>
        int main() {
            setvbuf(stdout, "", _IONBF, 0);
            return 0;
        }
        ''')
    fprintf = c.function_test('int', 'FILE*', 'const char*', test='''
        #include <stdio.h>
        int main() {
            return fprintf(stdout, "%d %d", 5, 6) ? 0 : 1;
        }
        ''', stdout=b'5 6')
    fscanf = c.function_test('int', 'FILE*', 'const char*', test='''
        #include <stdio.h>
        int main() {
            int x = 0, y = 0;
            return fscanf(stdin, "%d %d", &x, &y) &&
                x == 5 &&
                y == 6 ? 0 : 1;
        }
        ''', stdin=b'5 6')
    printf = c.function_test('int', 'const char*', test='''
        #include <stdio.h>
        int main() {
            return printf("%d %d", 5, 6) ? 0 : 1;
        }
        ''', stdout=b'5 6')
    scanf = c.function_test('int', 'const char*', test='''
        #include <stdio.h>
        int main() {
            int x = 0, y = 0;
            return scanf("%d %d", &x, &y) &&
                x == 5 &&
                y == 6 ? 0 : 1;
        }
        ''', stdin=b'5 6')
    sprintf = c.function_test('int', 'char*', 'const char*', test='''
        #include <stdio.h>
        int main() {
            char s[50];
            return sprintf(s, "%d%d", 5, 6) &&
                s[0] == '5' &&
                s[1] == '6' ? 0 : 1;
        }
        ''')
    sscanf = c.function_test('int', 'const char*', 'const char*', test='''
        #include <stdio.h>
        int main() {
            int x = 0, y = 0;
            return sscanf("5 6", "%d %d", &x, &y) &&
                x == 5 && y == 6 ? 0 : 1;
        }
        ''')
    vfprintf = c.function_test('int', 'FILE*', 'const char*', 'va_list', test='''
        #include <stdarg.h>
        #include <stdio.h>
        int f(char* s, ...) {
            int rc;
            va_list ap;
            va_start(ap, s);
            rc = vfprintf(stdout, s, ap);
            va_end(ap);
            return rc;
        }
        int main() {
            return f("%d %d", 5, 6) ? 0 : 1;
        }
        ''', stdout=b'5 6')
    vprintf = c.function_test('int', 'const char*', 'va_list', test='''
        #include <stdarg.h>
        #include <stdio.h>
        int f(char* s, ...) {
            int rc;
            va_list ap;
            va_start(ap, s);
            rc = vprintf(s, ap);
            va_end(ap);
            return rc;
        }
        int main() {
            return f("%d %d", 5, 6) ? 0 : 1;
        }
        ''', stdout=b'5 6')
    vsprintf = c.function_test('int', 'char*', 'const char*', 'va_list', test='''
        #include <stdarg.h>
        #include <stdio.h>
        int f(char* s, ...) {
            int rc;
            va_list ap;
            va_start(ap, s);
            rc = vsprintf(s, "%d %d", ap);
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
    fgetc = c.function_test('int', 'FILE*', test='''
        #include <stdio.h>
        int main() {
            return fgetc(stdin) == '5' ? 0 : 1;
        }
        ''', stdin=b'5')
    fgets = c.function_test('char*', 'char*', 'int', 'FILE*', test='''
        #include <stdio.h>
        int main() {
            char s[50] = {0};
            return fgets(s, 4, stdin) &&
                s[0] == '5' &&
                s[1] == ' ' &&
                s[2] == '6' &&
                s[3] == '\\0' ? 0 : 1;
        }
        ''', stdin=b'5 6')
    fputc = c.function_test('int', 'int', 'FILE*', test='''
        #include <stdio.h>
        int main() {
            return fputc('5', stdout) == '5' ? 0 : 1;
        }
        ''', stdout=b'5')
    fputs = c.function_test('int', 'const char*', 'FILE*', test='''
        #include <stdio.h>
        int main() {
            return fputs("5 6", stdout) ? 0 : 1;
        }
        ''', stdout=b'5 6')
    getc = c.function_test('int', 'FILE*', test='''
        #include <stdio.h>
        int main() {
            return getc(stdin) == '5' ? 0 : 1;
        }
        ''',
        stdin=b'5')
    getchar = c.function_test('int', 'void', test='''
        #include <stdio.h>
        int main() {
            return getchar() == '5' ? 0 : 1;
        }
        ''', stdin=b'5')
    gets = c.function_test('char*', 'char*', test='''
        #include <stdio.h>
        int main() {
            char s[50] = {0};
            return s == gets(s) &&
                s[0] == '5' &&
                s[1] == ' ' &&
                s[2] == '6' &&
                s[3] == '\\0' ? 0 : 1;
        }
        ''', stdin=b'5 6')
    putc = c.function_test('int', 'int', 'FILE*', test='''
        #include <stdio.h>
        int main() {
            return putc('5', stdout) == '5' ? 0 : 1;
        }
        ''', stdout=b'5')
    putchar = c.function_test('int', 'int', test='''
        #include <stdio.h>
        int main() {
            return putchar('5') == '5' ? 0 : 1;
        }
        ''', stdout=b'5')
    puts = c.function_test('int', 'const char*', test='''
        #include <stdio.h>
        int main() {
            return puts("5 6") ? 0 : 1;
        }
        ''', stdout=b'5 6\n')
    ungetc = c.function_test('int', 'int', 'FILE*', test='''
        #include <stdio.h>
        int main() {
            if (ungetc('5', stdin) != '5') return 1;
            return getc(stdin) == '5' ? 0 : 1;
        }
        ''')
    fread = c.function_test('size_t', 'void*', 'size_t', 'size_t', 'FILE*', test='''
        #include <stdio.h>
        int main() {
            char s[50] = {0};
            return fread(s, sizeof(char), 3, stdin) == 3 &&
                s[0] == '5' &&
                s[1] == ' ' &&
                s[2] == '6' ? 0 : 1;
        }
        ''', stdin=b'5 6')
    fwrite = c.function_test('size_t', 'const void*', 'size_t', 'size_t', 'FILE*', test='''
        #include <stdio.h>
        int main() {
            return fwrite("5 6", sizeof(char), 3, stdout) == 3 ? 0 : 1;
        }
        ''', stdout=b'5 6')

    @c.cacheproperty
    def fgetpos(self):
        if self.fopen:
            return c.Function('int', 'FILE*', 'fpos_t*', 'fgetpos')

    @c.cacheproperty
    def fseek(self):
        if self.fopen:
            return c.Function('int', 'FILE*', 'long int', 'int', 'fseek')

    @c.cacheproperty
    def fsetpos(self):
        if self.fopen:
            return c.Function('int', 'FILE*', 'const fpos_t*', 'fsetpos')

    @c.cacheproperty
    def ftell(self):
        if self.fopen:
            return c.Function('long int', 'FILE*', 'ftell')

    @c.cacheproperty
    def rewind(self):
        if self.fopen:
            return c.Function('void', 'FILE*', 'rewind')

    clearerr = c.function_test('void', 'FILE*', test='''
        #include <stdio.h>
        int main() {
            clearerr(stdout);
            return 0;
        }
        ''')

    @property
    def feof(self):
        if self.fopen:
            return c.function_test('int', 'FILE*')

    @property
    def ferror(self):
        if self.fopen:
            return c.Function('int', 'FILE*')

    perror = c.function_test('void', 'const char*', default_args=('""',))

# -----------------------------------------------------------------------------

class stdlib_h(c.Test):
    header = c.header_test('stdlib.h')

    EXIT_FAILURE = c.macro_test()
    EXIT_SUCCESS = c.macro_test()
    MB_CUR_MAX = c.macro_test()
    NULL = c.macro_test()
    RAND_MAX = c.macro_test()
    div_t = c.type_test()
    ldiv_t = c.type_test()
    size_t = c.int_type_test()
    wchar_t = c.int_type_test()
    atof = c.function_test('double', 'const char*', test='''
        #include <stdlib.h>
        int main() {
            return atof("0.5") == 0.5 ? 0 : 1;
        }
        ''')
    atoi = c.function_test('int', 'const char*', test='''
        #include <stdlib.h>
        int main() {
            return atoi("1234") == 1234 ? 0 : 1;
        }
        ''')
    atol = c.function_test('long int', 'const char*', test='''
        #include <stdlib.h>
        int main() {
            return atol("1234") == 1234L ? 0 : 1;
        }
        ''')
    strtod = c.function_test('double', 'const char*', 'char**', test='''
        #include <stdlib.h>
        int main() {
            char* s1 = "0.5";
            char* s2 = "abc";
            char* endp;
            double d = strtod(s1, &endp);
            if (s1 != endp && *endp == '\\0' && d == 0.5) {
                d = strtod(s2, &endp);
                return s1 == endp || *endp != '\\0' ? 0 : 1;
            }
            return 1;
        }
        ''')
    strtol = c.function_test('long int', 'const char*', 'char**', 'int', test='''
        #include <stdlib.h>
        int main() {
            char* s1 = "15";
            char* s2 = "abc";
            char* endp;
            long int d = strtol(s1, &endp, 8);
            if (s1 != endp && *endp == '\\0' && d == 13l) {
                d = strtol(s2, &endp, 8);
                return s1 == endp || *endp != '\\0' ? 0 : 1;
            }
            return 1;
        }
        ''')
    strtoul = c.function_test('unsigned long int', 'const char*', 'char**', 'int', test='''
        #include <stdlib.h>
        int main() {
            char* s1 = "15";
            char* s2 = "abc";
            char* endp;
            unsigned long int d = strtoul(s1, &endp, 8);
            if (s1 != endp && *endp == '\\0' && d == 13ul) {
                d = strtoul(s2, &endp, 8);
                return s1 == endp || *endp != '\\0' ? 0 : 1;
            }
            return 1;
        }
        ''')
    rand = c.function_test('int', 'void')
    srand = c.function_test('void', 'unsigned int')
    calloc = c.function_test('void*', 'size_t', 'size_t')
    free = c.function_test('void', 'void* ptr', test='''
        #include <stdlib.h>
        int main() {
            void* p = malloc(5);
            if (!p) return 1;
            free(p);
            return 0;
        }
        ''')
    malloc = c.function_test('void*', 'size_t', test=free.test)
    realloc = c.function_test('void*', 'void*', 'size_t', test='''
        #include <stdlib.h>
        int main() {
            void* p = malloc(5);
            if (!p) return 1;
            p = realloc(p, 6);
            if (!p) return 1;
            free(p);
            return 0;
        }
        ''')

    @c.cacheproperty
    def abort(self):
        self.ctx.logger.check("checking abort in 'stdlib.h'")

        with self.builder.tempfile('''
                #include <stdlib.h>
                #ifdef _WIN32
                #include <windows.h>
                #endif

                int main() {
                #ifdef _WIN32
                    /* Turn off the windows error box */
                    DWORD dwMode = SetErrorMode(SEM_NOGPFAULTERRORBOX);
                    SetErrorMode(dwMode | SEM_NOGPFAULTERRORBOX);
                #endif
                    abort();
                    return 0;
                }
                ''') as src:
            dst = src.parent / 'temp'
            try:
                obj = self.builder.uncached_compile(src, quieter=1)
                exe = self.builder.uncached_link_exe(dst, [obj], quieter=1)
            except fbuild.ExecutionError:
                pass
            else:
                try:
                    self.builder.ctx.execute([exe], quieter=1)
                except fbuild.ExecutionError as e:
                    self.ctx.logger.passed()
                    return c.Function('void', 'void')

        self.ctx.logger.failed()

    atexit = c.function_test('int', 'void (*f)(void)', test='''
        #include <stdio.h>
        #include <stdlib.h>
        void f() { printf("passed"); }
        int main() {
            atexit(&f);
            return 0;
        }
        ''', stdout=b'passed')

    exit = c.function_test('void', 'int', test='''
        #include <stdlib.h>
        int main() {
            exit(0);
            return 1;
        }
        ''')
    getenv = c.function_test('char*', 'const char*', default_args=('""',))
    system = c.function_test('int', 'const char*', default_args=('NULL',))
    bsearch = c.function_test(
        'void*',
        'const void*',
        'const void*',
        'size_t',
        'size_t',
        'int (*f)(const void*, const void*)',
        test='''
        #include <stdlib.h>
        int f(const void* a, const void* b) { return *(int*)a - *(int*)b; }
        int main() {
            int a[] = {0, 1, 2, 3, 4};
            int b = 3;
            int* c = (int*)bsearch(&b, a, 5, sizeof(a[0]), &f);
            return c && c == &a[3] && *c == 3 ? 0 : 1;
        }
        ''')
    qsort = c.function_test('void', 'void*', 'size_t', 'size_t', 'int (*f)(const void*, const void*)', test='''
        #include <stdlib.h>
        int f(const void* a, const void* b) { return *(int*)a - *(int*)b; }
        int main() {
            int a[] = {4, 3, 2, 1, 0};
            qsort(a, 5, sizeof(a[0]), f);
            return
                a[0] == 0 &&
                a[1] == 1 &&
                a[2] == 2 &&
                a[3] == 3 &&
                a[4] == 4 ? 0 : 1;
        }
        ''')
    abs = c.function_test('int', 'int')
    div = c.function_test('div_t', 'int', 'int', default_args=(4, 2))
    labs = c.function_test('long int', 'long int')
    ldiv = c.function_test('ldiv_t', 'long int', 'long int', default_args=(4, 2))
    mblen = c.function_test('int', 'const char*', 'size_t', default_args=('""', 0))
    mbtowc = c.function_test('int', 'wchar_t*', 'const char*', 'size_t', test='''
        #include <stdlib.h>
        int main() {
            wchar_t s[50];
            return mbtowc(s, "5", 50) == 1 && s[0] == L'5' ? 0 : 1;
        }
        ''')
    wctomb = c.function_test('int', 'char*', 'wchar_t')
    mbstowcs = c.function_test('size_t', 'wchar_t*', 'const char*', 'size_t', test='''
        #include <stdlib.h>
        int main() {
            wchar_t s[50];
            return mbstowcs(s, "5 6", 50) == 3 &&
                s[0] == L'5' &&
                s[1] == L' ' &&
                s[2] == L'6' &&
                s[3] == L'\\0' ? 0 : 1;
        }
        ''')
    wcstombs = c.function_test('size_t', 'char*', 'const wchar_t*', 'size_t', test='''
        #include <stdlib.h>
        int main() {
            char s[50];
            return wcstombs(s, L"5 6", 50) == 3 &&
                s[0] == '5' &&
                s[1] == ' ' &&
                s[2] == '6' &&
                s[3] == '\\0' ? 0 : 1;
        }
        ''')

# -----------------------------------------------------------------------------

class string_h(c.Test):
    header = c.header_test('string.h')

    NULL = c.macro_test()
    size_t = c.int_type_test()
    memcpy = c.function_test('void*', 'void*', 'const void*', 'size_t', test='''
        #include <string.h>
        int main() {
            char s[] = {'a', 'a', 'a', 'a', 'a', 'a'};
            return memcpy(s, "1234", 4) == s &&
                s[0] == '1' &&
                s[1] == '2' &&
                s[2] == '3' &&
                s[3] == '4' &&
                s[4] == 'a' &&
                s[5] == 'a' ? 0 : 1;
        }
        ''')
    memmove = c.function_test('void*', 'void*', 'const void*', 'size_t', test='''
        #include <string.h>
        int main() {
            char s[] = {'a', 'a', 'a', 'a', 'a', 'a'};
            return memmove(s, "1234", 4) == s &&
                s[0] == '1' &&
                s[1] == '2' &&
                s[2] == '3' &&
                s[3] == '4' &&
                s[4] == 'a' &&
                s[5] == 'a' ? 0 : 1;
        }
        ''')
    strcpy = c.function_test('char*', 'char*', 'const char*', test='''
        #include <string.h>
        int main() {
            char s[] = {'a', 'b', 'c', 'd', 'e', 'f', '\\0'};
            return strcpy(s, "1234") == s &&
                s[0] == '1' &&
                s[1] == '2' &&
                s[2] == '3' &&
                s[3] == '4' &&
                s[4] == '\\0' &&
                s[5] == 'f' &&
                s[6] == '\\0' ? 0 : 1;
        }
        ''')
    strncpy = c.function_test('char*', 'char*', 'const char*', 'size_t', test='''
        #include <string.h>
        int main() {
            char s[] = {'a', 'b', 'c', 'd', 'e', 'f', '\\0'};
            return strncpy(s, "1234", 4) == s &&
                s[0] == '1' &&
                s[1] == '2' &&
                s[2] == '3' &&
                s[3] == '4' &&
                s[4] == 'e' &&
                s[5] == 'f' &&
                s[6] == '\\0' ? 0 : 1;
        }
        ''')
    strcat = c.function_test('char*', 'char*', 'const char*', test='''
        #include <string.h>
        int main() {
            char s[] = {'1', ' ', '2', '\\0', '\\0', '\\0', '\\0'};
            return strcat(s, "5 6") == s &&
                s[0] == '1' &&
                s[1] == ' ' &&
                s[2] == '2' &&
                s[3] == '5' &&
                s[4] == ' ' &&
                s[5] == '6' &&
                s[6] == '\\0' ? 0 : 1;
        }
        ''')
    strncat = c.function_test('char*', 'char*', 'const char*', 'size_t', test='''
        #include <string.h>
        int main() {
            char s[] = {'1', ' ', '2', '\\0', '\\0', '\\0', '\\0'};
            return strncat(s, "5 6", 2) == s &&
                s[0] == '1' &&
                s[1] == ' ' &&
                s[2] == '2' &&
                s[3] == '5' &&
                s[4] == ' ' &&
                s[5] == '\\0' ? 0 : 1;
        }
        ''')
    memcmp = c.function_test('int', 'const void*', 'const void*', 'size_t', test='''
        #include <string.h>
        int main() {
            return
                 memcmp("1 2\\0 3", "1 2\\0 3", 6) &&
                !memcmp("1 2\\0 3", "2 2\\0 3", 6);
        }
        ''')
    strcmp = c.function_test('int', 'const char*', 'const char*', test='''
        #include <string.h>
        int main() {
            return
                  strcmp("1 2\\0 3", "1 2\\0 4") &&
                 !strcmp("1 2\\0 3", "2 2\\0 4");
        }
        ''')
    strcoll = c.function_test('int', 'const char*', 'const char*', test='''
        #include <string.h>
        int main() {
            return
                  strcoll("1 2\\0 3", "1 2\\0 4") &&
                 !strcoll("1 2\\0 3", "2 2\\0 4");
        }
        ''')
    strncmp = c.function_test('int', 'const char*', 'const char*', 'size_t', test='''
        #include <string.h>
        int main() {
            return
                  strncmp("1 2 3", "1 2 4", 4) &&
                 !strncmp("1 2 3", "2 2 4", 4);
        }
        ''')
    strxfrm = c.function_test('size_t', 'char*', 'const char*', 'size_t', test='''
        #include <string.h>
        int main() {
            return strxfrm(NULL, "abcd", 0) == 4 ? 0 : 1;
        }
        ''')
    memchr = c.function_test('void*', 'const void*', 'int', 'size_t', test='''
        #include <string.h>
        int main() {
            char* s = "ab\\0dc";
            return
                memchr(s, '\\0', 5) == &s[2] &&
                memchr(s, 'f', 5) == NULL ? 0 : 1;
        }
        ''')
    strchr = c.function_test('char*', 'const char*', 'int', test='''
        #include <string.h>
        int main() {
            char* s = "abcdc";
            return
                strchr(s, 'c') == &s[2] &&
                strchr(s, 'f') == NULL ? 0 : 1;
        }
        ''')
    strcspn = c.function_test('size_t', 'const char*', 'const char*', test='''
        #include <string.h>
        int main() {
            return strcspn("abc123", "1234567890") == 3 ? 0 : 1;
        }
        ''')
    strpbrk = c.function_test('char*', 'const char*', 'const char*', test='''
        #include <string.h>
        int main() {
            char s[] = "abcde";
            return
                strpbrk(s, "bd") == &s[1] &&
                strpbrk(s, "xd") == &s[3] &&
                strpbrk(s, "xy") == NULL ? 0 : 1;
        }
        ''')
    strrchr = c.function_test('char*', 'const char*', 'int', test='''
        #include <string.h>
        int main() {
            char* s = "abcdc";
            return
                strrchr(s, 'c') == &s[4] &&
                strrchr(s, 'f') == NULL ? 0 : 1;
        }
        ''')
    strspn = c.function_test('size_t', 'const char*', 'const char*', test='''
        #include <string.h>
        int main() {
            return strspn("123abc", "1234567890") == 3 ? 0 : 1;
        }
        ''')
    strstr = c.function_test('char*', 'const char*', 'const char*', test='''
        #include <string.h>
        int main() {
            char* s = "abcdef";
            return
                strstr(s, "cde") == &s[2] &&
                strstr(s, "cdf") == NULL ? 0 : 1;
        }
        ''')
    strtok = c.function_test('char*', 'char*', 'const char*', test='''
        #include <string.h>
        int main() {
            char s[] = "a,b,c";
            char* p = strtok(s, ",");
            return p && p[0] == 'a' && p[1] == '\\0' ? 0 : 1;
        }
        ''')
    memset = c.function_test('void*', 'void*', 'int', 'size_t', test='''
        #include <string.h>
        int main() {
            char s[50] = {0};
            int i;
            if (memset(s, 'b', 50) != s) return 1;
            for (i = 0; i < 50; ++i) if (s[i] != 'b') return 1;
            return 0;
        }
        ''')
    strerror = c.function_test('char*', 'int')
    strlen = c.function_test('size_t', 'const char*', test='''
        #include <string.h>
        int main() {
            return strlen("abcde") == 5 ? 0 : 1;
        }
        ''')

# -----------------------------------------------------------------------------

class time_h(c.Test):
    header = c.header_test('time.h')

    CLK_TCK = c.macro_test()
    NULL = c.macro_test()
    clock_t = c.int_type_test()
    time_t = c.int_type_test()
    size_t = c.int_type_test()
    tm = c.struct_test(
        ('int', 'tm_sec'),
        ('int', 'tm_min'),
        ('int', 'tm_hour'),
        ('int', 'tm_mday'),
        ('int', 'tm_mon'),
        ('int', 'tm_year'),
        ('int', 'tm_wday'),
        ('int', 'tm_yday'),
        ('int', 'tm_isdst'))
    clock = c.function_test('clock_t', 'void')
    difftime = c.function_test('double', 'time_t', 'time_t')
    mktime = c.function_test('time_t', 'struct tm*', test='''
        #include <time.h>
        int main() {
            struct tm a;
            time_t b = mktime(&a);
            return 0;
        }
        ''')
    time = c.function_test('time_t', 'time_t*', default_args=('NULL',))
    asctime = c.function_test('char*', 'const struct tm*', test='''
        #include <time.h>
        int main() {
            struct tm t = { 0, 0, 0, 1, 0, 70, 4, 0, 0 };
            char* s = asctime(&t);
            return 0;
        }
        ''')
    ctime = c.function_test('char*', 'const time_t*', test='''
        #include <time.h>
        int main() {
            time_t t = 0;
            char* s = ctime(&t);
            return 0;
        }
        ''')
    gmtime = c.function_test('struct tm*', 'const time_t*', test='''
        #include <time.h>
        int main() {
            time_t t = 0;
            struct tm* tm = gmtime(&t);
            return 0;
        }
        ''')
    localtime = c.function_test('struct tm*', 'const time_t*', test='''
        #include <time.h>
        int main() {
            time_t t = 0;
            struct tm* tm = localtime(&t);
            return 0;
        }
        ''')
    strftime = c.function_test('size_t', 'char*', 'size_t', 'const char*', 'const struct tm*', test='''
        #include <time.h>
        int main() {
            struct tm t = { 0, 0, 0, 1, 0, 70, 4, 0, 0 };
            char s[50] = {0};
            return strftime(s, 50, " ", &t) == 1 ? 0 : 1;
        }
        ''')
