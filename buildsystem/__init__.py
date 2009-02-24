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
