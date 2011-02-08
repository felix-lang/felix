import fbuild
import fbuild.db
from fbuild.builders.file import copy, copy_regex
from fbuild.functools import call
from fbuild.path import Path
from fbuild.record import Record

import buildsystem

# ------------------------------------------------------------------------------

@fbuild.db.caches
def build_judytables(ctx, tablegen, dst) -> fbuild.db.DST:
    """Create the judytable generator executable."""

    # Make sure the directory exists.
    dst.parent.makedirs()

    # We have to run the tablegen from the working directory to get the files
    # generated in the right place.
    ctx.execute(tablegen.abspath(),
        msg1=tablegen.name,
        msg2=dst,
        cwd=dst.parent,
        color='yellow')

    return dst

# ------------------------------------------------------------------------------

def _build_objs(ctx, builder, dstname):
    """
    Build the object files for Judy1 or JudyL. Unfortunately the judy build
    process is a little complicated because the same underlying code is used for
    bit arrays and word arrays. The only distinguishing feature is if the macro
    JUDY1 or JUDYL is defined. This function abstracts the handling of this
    distinction.
    """

    path = Path('src/judy/src')
    includes = [path, path / 'JudyCommon', path / dstname]

    types = call('fbuild.builders.c.std.config_types', ctx, builder)

    macros = [dstname.upper()]
    if types['void*']['size'] == 8:
        macros.append('JU_64BIT')
    else:
        macros.append('JU_32BIT')

    # First, copy all the common files into the Judy* directory.
    srcs = []
    srcs.extend(copy_regex(ctx,
        srcdir=path / 'JudyCommon',
        dstdir=path / dstname,
        src_pattern=r'^Judy(.*\.c)',
        dst_pattern=r'%s\1' % dstname,
        exclude_pattern=
            r'('
            r'JudyMalloc.c|'
            r'JudyByCount.c|'
            r'JudyPrevNext.c|'
            r'JudyPrevNextEmpty.c|'
            r'JudyTables.c|'
            r'JudyPrintJP.c)'))

    # Create the tablegen.
    tablegen = builder.build_exe(path / dstname / dstname + 'TableGen',
        [copy(ctx,
            src=path / 'JudyCommon/JudyTables.c',
            dst=path / dstname / dstname + 'TablesGen.c')],
        includes=includes,
        macros=macros)

    # Create the table source.
    srcs.append(build_judytables(ctx, tablegen,
        ctx.buildroot / path / dstname / dstname + 'Tables.c'))
    # Compile the objects.

    objs = []
    objs.extend(builder.build_objects(srcs,
        includes=includes,
        macros=macros))

    objs.extend((
        builder.compile(
            path / 'JudyCommon/JudyGet.c',
            dst=path / dstname / 'j__udyGet.c',
            includes=includes,
            macros=macros + ['JUDYGETINLINE']),
        builder.compile(
            path / 'JudyCommon/JudyPrevNext.c',
            dst=path / dstname / dstname + 'Next.c',
            includes=includes,
            macros=macros + ['JUDYNEXT']),
        builder.compile(
            path / 'JudyCommon/JudyPrevNextEmpty.c',
            dst=path / dstname / dstname + 'NextEmpty.c',
            includes=includes,
            macros=macros + ['JUDYNEXT']),
        builder.compile(
            path / 'JudyCommon/JudyPrevNext.c',
            dst=path / dstname / dstname + 'Prev.c',
            includes=includes,
            macros=macros + ['JUDYPREV']),
        builder.compile(
            path / 'JudyCommon/JudyPrevNextEmpty.c',
            dst=path / dstname / dstname + 'PrevEmpty.c',
            includes=includes,
            macros=macros + ['JUDYPREV']),
        builder.compile(
            path / 'JudyCommon/JudyByCount.c',
            path / dstname / dstname + 'ByCount.c',
            includes=includes,
            macros=macros + ['NOSMARTJBB', 'NOSMARTJBU', 'NOSMARTJLB']),
    ))

    return objs


def build_runtime(phase):
    """
    Builds the judy runtime library, and returns the static and shared
    library versions.
    """

    path = Path('src/judy/src')

    # Copy the header into the runtime library.
    buildsystem.copy_to(phase.ctx, phase.ctx.buildroot / 'lib/rtl', [
        path / 'Judy.h'])

    srcs = [
        path / 'JudyCommon/JudyMalloc.c',
        path / 'JudySL/JudySL.c',
        path / 'JudyHS/JudyHS.c']

    return Record(
        static=buildsystem.build_c_static_lib(phase, 'lib/rtl/judy',
            srcs=srcs,
            objs=\
                _build_objs(phase.ctx, phase.c.static, 'Judy1') +
                _build_objs(phase.ctx, phase.c.static, 'JudyL'),
            includes=[path, path / 'JudyCommon']),
        shared=buildsystem.build_c_shared_lib(phase, 'lib/rtl/judy',
            srcs=srcs,
            objs=
                _build_objs(phase.ctx, phase.c.shared, 'Judy1') +
                _build_objs(phase.ctx, phase.c.shared, 'JudyL'),
            includes=[path, path / 'JudyCommon']))

# ------------------------------------------------------------------------------

def build_flx(phase):
    return buildsystem.copy_flxs_to_lib(phase.ctx,
        Path('src/judy/*.flx').glob())
