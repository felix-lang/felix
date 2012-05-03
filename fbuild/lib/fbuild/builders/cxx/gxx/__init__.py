import fbuild.builders.c.gcc
import fbuild.db

# ------------------------------------------------------------------------------

def make_cxx(ctx, exe=None, default_exes=['g++', 'c++'], **kwargs):
    return fbuild.builders.c.gcc.make_cc(ctx, exe, default_exes, **kwargs)

# ------------------------------------------------------------------------------

@fbuild.db.caches
def static(*args, make_cxx=make_cxx, src_suffix='.cc', **kwargs):
    return fbuild.builders.c.gcc.static(*args,
        make_cc=make_cxx,
        src_suffix=src_suffix,
        **kwargs)

@fbuild.db.caches
def shared(*args, make_cxx=make_cxx, src_suffix='.cc', **kwargs):
    return fbuild.builders.c.gcc.shared(*args,
        make_cc=make_cxx,
        src_suffix=src_suffix,
        **kwargs)
