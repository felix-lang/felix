import fbuild.builders.c.msvc
import fbuild.db

# ------------------------------------------------------------------------------

@fbuild.db.caches
def static(*args, src_suffix='.cc', **kwargs):
    return fbuild.builders.c.msvc.static(*args, src_suffix=src_suffix, **kwargs)

@fbuild.db.caches
def shared(*args, src_suffix='.cc', **kwargs):
    return fbuild.builders.c.msvc.shared(*args, src_suffix=src_suffix, **kwargs)
