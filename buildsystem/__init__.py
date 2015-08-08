import os, shutil
from itertools import chain

import fbuild
import fbuild.db
from fbuild.path import Path

# ------------------------------------------------------------------------------

@fbuild.db.caches
def copy_to(ctx, dstdir, srcs:fbuild.db.SRCS) -> fbuild.db.DSTS:
    #print("__init__.copy_to, srcs="+str(srcs)+", dst=" + str(dstdir))
    dstdir.makedirs()

    dsts = []

    for src in srcs:
        src = Path(src)
        dst = dstdir / src.name
        ctx.logger.check(' * copy', '%s -> %s' % (src, dst), color='yellow')
        try:
            src.copy(dst)
        except shutil.SameFileError:
            pass
        dsts.append(dst)

    return dsts

@fbuild.db.caches
def copy_dir_to(ctx, dstdir, srcdir, *, pattern=None) -> fbuild.db.DSTS:
    #print("Copy dir to: from srcdir = " + 
    #  str(srcdir) + ", pattern=" + str(pattern) +
    #  ", to " + str(dstdir))
    srcdir = Path(srcdir)

    srcs = []
    dsts = []

    for src in srcdir.find(pattern=pattern, include_dirs=False):
        dst = src.removeroot(srcdir+os.sep).addroot(dstdir)
        dst.parent.makedirs()

        srcs.append(src)
        dsts.append(dst)

        ctx.logger.check(' * copy', '%s -> %s' % (src, dst), color='yellow')
        try:
            src.copy(dst)
        except shutil.SameFileError:
            pass

    ctx.db.add_external_dependencies_to_call(srcs=srcs)

    return dsts


def copy_hpps_to_rtl(ctx, *hpps):
    #print("COPY HPPS TO RTL")
    return copy_to(ctx, ctx.buildroot / 'share/lib/rtl', tuple(hpps))

def copy_flxs_to_lib(ctx, flxs):
    print("COPY FLXS TO LIB")
    return copy_to(ctx, ctx.buildroot / 'share/lib', tuple(flxs))

def copy_flxs_to_libstd(ctx, flxs):
    print("COPY FLXS TO LIBSTD")
    return copy_to(ctx, ctx.buildroot / 'share/lib/std', tuple(flxs))

def copy_flxs_to_libstd_posix(ctx, flxs):
    print("COPY FLXS TO LIBSTDPOSIX")
    return copy_to(ctx, ctx.buildroot / 'share/lib/std/posix', tuple(flxs))

def copy_flxs_to_libstl(ctx, flxs):
    print("COPY FLXS TO LIBSTL")
    return copy_to(ctx, ctx.buildroot / 'share/lib/stl', tuple(flxs))

def copy_fpc_to_config(ctx, fpcs):
    print("COPY FPCS to CONFIG")
    return copy_to(ctx, ctx.buildroot / 'host/config', tuple(fpcs))

@fbuild.db.caches
def move_to(ctx, dstdir, srcs:fbuild.db.SRCS) -> fbuild.db.DSTS:
    print("MOVETO")
    dstdir.makedirs()

    dsts = []

    for src in srcs:
        src = Path(src)
        dst = dstdir / src.name
        ctx.logger.check(' * move', '%s -> %s' % (src, dst), color='yellow')
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
        copy_to(phase.ctx, phase.ctx.buildroot / 'host/bin', (lib,))

    return lib

# ------------------------------------------------------------------------------

def build_cxx_static_lib(phase, dst, *args, macros=[], **kwargs):
    macros = list(chain(macros, ['FLX_STATIC_LINK']))
    return phase.cxx.static.build_lib(dst + '_static', *args,
        macros=macros, **kwargs)

def build_cxx_shared_lib(phase, dst, *args, **kwargs):
    lib = phase.cxx.shared.build_lib(dst + '_dynamic', *args, **kwargs)

    if 'windows' in phase.platform:
        copy_to(phase.ctx, phase.ctx.buildroot / 'host/bin', (lib,))

    return lib
