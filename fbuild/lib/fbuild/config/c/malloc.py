"""Provides tests for nonstandard malloc-related headers and functionality."""

import fbuild.config.c as c

# ------------------------------------------------------------------------------

class malloc_h(c.Test):
    header = c.header_test('malloc.h')

    alloca = c.function_test('void*', 'size_t')
    malloc = c.function_test('void*', 'size_t')

class malloc_malloc_h(c.Test):
    header = c.header_test('malloc/malloc.h')

    malloc_zone_statistics = c.function_test(
        'void', 'malloc_zone_t*', 'malloc_statistics_t*',
        test='''
        #include <malloc/malloc.h>
        int main() {
            malloc_statistics_t stats;
            malloc_zone_statistics(malloc_default_zone(), &stats);
            return 0;
        }
        ''')

class alloca_h(c.Test):
    header = c.header_test('alloca.h')

    alloca = c.function_test('void*', 'size_t')
