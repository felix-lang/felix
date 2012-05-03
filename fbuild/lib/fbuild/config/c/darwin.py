import fbuild.config.c as c

# ------------------------------------------------------------------------------

class mach_mach_h(c.Test):
    header = c.header_test('mach/mach.h')

class mach_o_dyld_h(c.Test):
    header = c.header_test('mach-o/dyld.h')
