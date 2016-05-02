from .. import gcc

# -----------------------------------------------------------------------------

def config_static(*args,
        obj_suffix='.obj',
        lib_prefix='',
        lib_suffix='.lib',
        exe_suffix='.exe',
        **kwargs):
    return gcc.config_static(
        obj_suffix=obj_suffix,
        lib_prefix=lib_prefix,
        lib_suffix=lib_suffix,
        exe_suffix=exe_suffix,
        *args, **kwargs)

def config_shared(*args,
        obj_suffix='.obj',
        lib_prefix='',
        lib_suffix='.dll',
        exe_suffix='.exe',
        **kwargs):
    return gcc.config_static(
        obj_suffix=obj_suffix,
        lib_prefix=lib_prefix,
        lib_suffix=lib_suffix,
        exe_suffix=exe_suffix,
        *args, **kwargs)

def config(*args,
        config_static=config_static,
        config_shared=config_shared,
        **kwargs):
    return gcc.config(
        config_static=config_static,
        config_shared=config_shared,
        *args, **kwargs)
