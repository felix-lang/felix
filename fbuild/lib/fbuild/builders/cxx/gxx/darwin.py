from fbuild.builders.c.gcc import darwin
from fbuild.builders.cxx import gxx

# ------------------------------------------------------------------------------

def static(*args, **kwargs):
    return gxx.static(*args, **kwargs)

def shared(*args, src_suffix='.cc', **kwargs):
    return darwin.shared(*args,
        make_cc=gxx.make_cxx,
        src_suffix=src_suffix,
        **kwargs)
