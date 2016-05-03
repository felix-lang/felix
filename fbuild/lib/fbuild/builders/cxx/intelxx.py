from fbuild.builders.c import intel
from fbuild.builders.cxx import gxx

# ------------------------------------------------------------------------------

def make_cxx(ctx, exe=None, default_exes=['icpc'], **kwargs):
    return intel.make_cc(ctx, exe, default_exes, **kwargs)

# ------------------------------------------------------------------------------

def static(*args, make_cxx=make_cxx, src_suffix='.cc', **kwargs):
    return intel.static(*args,
        make_cc=make_cxx,
        src_suffix=src_suffix,
        **kwargs)

def shared(*args, make_cxx=make_cxx, src_suffix='.cc', **kwargs):
    return intel.shared(*args,
        make_cc=make_cxx,
        src_suffix=src_suffix,
        **kwargs)
