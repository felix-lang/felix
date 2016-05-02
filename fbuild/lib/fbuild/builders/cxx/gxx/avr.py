import fbuild.builders.ar.avr
import fbuild.builders.c.gcc
import fbuild.builders.c.gcc.avr

# ------------------------------------------------------------------------------

def make_gxx(exe=None, default_exes=['avr-g++'], **kwargs):
    return fbuild.builders.c.gcc.avr.make_gcc(exe, default_exes, **kwargs)

# ------------------------------------------------------------------------------

@fbuild.db.caches
def static(*args, make_gcc=make_gcc, src_suffix='.cc', **kwargs):
    return fbuild.builders.c.gcc.static(*args,
        make_gcc=make_gcc,
        make_lib_linker=fbuild.builders.ar.avr.Ar,
        src_suffix=src_suffix,
        **kwargs)

@fbuild.db.caches
def shared(*args, make_gcc=make_gcc, src_suffix='.cc', **kwargs):
    return fbuild.builders.c.gcc.shared(*args,
        make_gcc=make_gcc,
        src_suffix=src_suffix,
        **kwargs)
