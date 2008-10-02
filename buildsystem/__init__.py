import fbuild
from fbuild.path import Path

# -----------------------------------------------------------------------------

def copy_to(dstdir, srcs):
    dstdir.make_dirs()

    for src in srcs:
        src = Path(src)
        dst = dstdir / src.name
        if dst.is_dirty(src):
            fbuild.logger.check(' * copy', '%s -> %s' % (src, dst),
                color='yellow')
            src.copy(dst)

def copy_hpps_to_rtl(*hpps):
    return copy_to(fbuild.buildroot / 'lib/rtl', hpps)

def copy_flxs_to_lib(flxs):
    return copy_to(fbuild.buildroot / 'lib', flxs)
