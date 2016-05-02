from fbuild.builders.c.clang import darwin
from fbuild.builders.cxx import clangxx

# ------------------------------------------------------------------------------

def static(*args, **kwargs):
    return clangxx.static(*args, **kwargs)

def shared(*args, src_suffix='.cc', **kwargs):
    return darwin.shared(*args,
        make_cc=clangxx.make_cxx,
        src_suffix=src_suffix,
        **kwargs)
