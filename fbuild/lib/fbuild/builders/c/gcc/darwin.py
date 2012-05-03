import fbuild.builders.c.gcc as gcc

# ------------------------------------------------------------------------------

def static(*args, **kwargs):
    return gcc.static(*args, **kwargs)

def shared(*args, lib_link_flags=['-dynamiclib'], **kwargs):
    return gcc.shared(*args, lib_link_flags=lib_link_flags, **kwargs)
