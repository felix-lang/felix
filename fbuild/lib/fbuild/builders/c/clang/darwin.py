import fbuild.builders.c.clang as clang

# ------------------------------------------------------------------------------

def static(*args, **kwargs):
    return clang.static(*args, **kwargs)

def shared(*args, lib_link_flags=['-dynamiclib'], **kwargs):
    return clang.shared(*args, lib_link_flags=lib_link_flags, **kwargs)
