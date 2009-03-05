from itertools import chain

import fbuild
import fbuild.db
from fbuild.path import Path

# ------------------------------------------------------------------------------

@fbuild.db.caches
def copy_to(dstdir, srcs:fbuild.db.SRCS) -> fbuild.db.DSTS:
    dstdir.makedirs()

    dsts = []

    for src in srcs:
        src = Path(src)
        dst = dstdir / src.name
        fbuild.logger.check(' * copy', '%s -> %s' % (src, dst),
            color='yellow')
        src.copy(dst)
        dsts.append(dst)

    return dsts

def copy_hpps_to_rtl(*hpps):
    return copy_to(fbuild.buildroot / 'lib/rtl', tuple(hpps))

def copy_flxs_to_lib(flxs):
    return copy_to(fbuild.buildroot / 'lib', tuple(flxs))

def copy_fpc_to_config(fpcs):
    return copy_to(fbuild.buildroot / 'config', tuple(fpcs))

@fbuild.db.caches
def move_to(dstdir, srcs:fbuild.db.SRCS) -> fbuild.db.DSTS:
    dstdir.makedirs()

    dsts = []

    for src in srcs:
        src = Path(src)
        dst = dstdir / src.name
        fbuild.logger.check(' * move', '%s -> %s' % (src, dst),
            color='yellow')
        src.move(dst)
        dsts.append(dst)

    return dsts

# ------------------------------------------------------------------------------

def build_c_static_lib(phase, dst, *args, macros=[], **kwargs):
    macros = list(chain(macros, ['FLX_STATIC_LINK']))
    return phase.c.static.build_lib(dst + '_static', *args,
        macros=macros, **kwargs)

def build_c_shared_lib(phase, dst, *args, **kwargs):
    lib = phase.c.shared.build_lib(dst + '_dynamic', *args, **kwargs)

    if 'windows' in phase.platform:
        copy_to(fbuild.buildroot / 'bin', (lib,))

    return lib

# ------------------------------------------------------------------------------

def build_cxx_static_lib(phase, dst, *args, macros=[], **kwargs):
    macros = list(chain(macros, ['FLX_STATIC_LINK']))
    return phase.cxx.static.build_lib(dst + '_static', *args,
        macros=macros, **kwargs)

def build_cxx_shared_lib(phase, dst, *args, **kwargs):
    lib = phase.cxx.shared.build_lib(dst + '_dynamic', *args, **kwargs)

    if 'windows' in phase.platform:
        copy_to(fbuild.buildroot / 'bin', (lib,))

    return lib
