import fbuild.config.c as c

# ------------------------------------------------------------------------------

class omp_h(c.Test):
    header = c.header_test('omp.h')

    omp_get_thread_num = c.function_test('int', 'void')
