import os
import re

import fbuild.db
from fbuild.path import Path

# ------------------------------------------------------------------------------

def _copy_or_move(ctx, src, dst, function, function_name):
    """
    Helper function to simplify copies and moves.
    """

    src = Path(src)
    dst = Path(dst).addroot(ctx.buildroot)

    if not dst.exists():
        # if dst ends with the separator, treat it like a directory
        if dst.endswith(os.sep):
            dst.makedirs()
            dst = dst / src.name
        else:
            dst.parent.makedirs()
    elif dst.isdir():
        # If the dst is a directory, we're just copying that file into that
        # directory.
        dst = dst / src.name

    ctx.logger.check(' * %s' % function_name, '%s -> %s' % (src, dst),
        color='yellow')
    function(src, dst)

    return dst

# ------------------------------------------------------------------------------

@fbuild.db.caches
def copy(ctx, src:fbuild.db.SRC, dst) -> fbuild.db.DST:
    """
    Copy a file to a different location. Creates the directory if it does not
    exist.
    """

    return _copy_or_move(ctx, src, dst, Path.copy, 'copy')


@fbuild.db.caches
def copy_regex(ctx, *, srcdir, dstdir, src_pattern, dst_pattern,
        exclude_pattern=None,
        include_dirs=True) -> fbuild.db.DSTS:
    """
    Recursively copies the files from the srcdir to the dstdir using the
    src_pattern and dst_pattern to choose and rename the files.

    >>> ctx = fbuild.context.make_default_context()
    >>> copy_regex(ctx, 'src', 'dst', r'(.*\.c)', r'foo-\1')
    """

    srcdir = Path(srcdir)
    dstdir = Path(dstdir).addroot(ctx.buildroot)

    srcs = []
    dsts = []
    for src in srcdir.find(include_dirs=include_dirs):
        # Filter out any files we're ignoring.
        if exclude_pattern is not None and re.search(exclude_pattern, src):
            continue

        dst, nsub = re.subn(
            src_pattern,
            dst_pattern,
            src[len(srcdir + os.sep):])

        if nsub > 0:
            dst = dstdir / Path(dst)
            ctx.logger.check(' * copy', '%s -> %s' % (src, dst), color='yellow')

            dst.parent.makedirs()
            src.copy(dst)

            srcs.append(src)
            dsts.append(dst)

    if srcs or dsts:
        ctx.db.add_external_dependencies_to_call(srcs=srcs, dsts=dsts)

    return dsts

# ------------------------------------------------------------------------------

@fbuild.db.caches
def move(ctx, src:fbuild.db.SRC, dst) -> fbuild.db.DST:
    """
    Move a file to a different location. Creates the directory if it does not
    exist.
    """

    return _copy_or_move(ctx, src, dst, Path.move, 'move')
