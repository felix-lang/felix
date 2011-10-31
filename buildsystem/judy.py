import fbuild
import fbuild.db
from fbuild.builders.file import copy, copy_regex
from fbuild.path import Path
from fbuild.record import Record

import buildsystem
from buildsystem.config import config_call

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

def _build_objs(host_phase, target_phase, builder, dstname):
    """
    Build the object files for Judy1 or JudyL. Unfortunately the judy build
    process is a little complicated because the same underlying code is used for
    bit arrays and word arrays. The only distinguishing feature is if the macro
    JUDY1 or JUDYL is defined. This function abstracts the handling of this
    distinction.
    """

    path = Path('src/judy/src')
    includes = [path, path / 'JudyCommon', path / dstname]

    types = config_call('fbuild.config.c.c99.types',
        target_phase.platform, builder)

    macros = [dstname.upper()]
    if types.voidp.size == 8:
        macros.append('JU_64BIT')
    else:
        macros.append('JU_32BIT')

    kwargs = {}
    if 'iphone' in target_phase.platform:
        kwargs['machine_flags'] = ['32']

    # First, copy all the common files into the Judy* directory.
    srcs = []
    srcs.extend(copy_regex(target_phase.ctx,
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
    tablegen = host_phase.c.static.build_exe(
        path / dstname / dstname + 'TableGen',
        [copy(target_phase.ctx,
            src=path / 'JudyCommon/JudyTables.c',
            dst=path / dstname / dstname + 'TablesGen.c')],
        includes=includes,
        macros=macros,
        ckwargs=kwargs,
        lkwargs=kwargs)

    # Create the table source.
    srcs.append(build_judytables(target_phase.ctx, tablegen,
        target_phase.ctx.buildroot / path / dstname / dstname + 'Tables.c'))
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


def build_runtime(host_phase, target_phase):
    """
    Builds the judy runtime library, and returns the static and shared
    library versions.
    """

    path = Path('src/judy/src')

    # Copy the header into the runtime library.
    buildsystem.copy_to(target_phase.ctx,
        target_phase.ctx.buildroot / 'lib/rtl',
        [path / 'Judy.h'])

    types = config_call('fbuild.config.c.c99.types',
        target_phase.platform, target_phase.c.static)

    if types.voidp.size == 8:
        macros = ['JU_64BIT']
    else:
        macros = ['JU_32BIT']

    srcs = [
        path / 'JudyCommon/JudyMalloc.c',
        path / 'JudySL/JudySL.c',
        path / 'JudyHS/JudyHS.c']

    static = buildsystem.build_c_static_lib(target_phase, 'lib/rtl/judy',
        srcs=srcs,
        objs=
            _build_objs(host_phase, target_phase, target_phase.c.static, 'Judy1') +
            _build_objs(host_phase, target_phase, target_phase.c.static, 'JudyL'),
        macros=macros,
        includes=[path, path / 'JudyCommon'])

    shared = buildsystem.build_c_shared_lib(target_phase, 'lib/rtl/judy',
        srcs=srcs,
        objs=
            _build_objs(host_phase, target_phase, target_phase.c.shared, 'Judy1') +
            _build_objs(host_phase, target_phase, target_phase.c.shared, 'JudyL'),
        macros=macros,
        includes=[path, path / 'JudyCommon'])

    return Record(static=static, shared=shared)

# ------------------------------------------------------------------------------

def build_flx(phase):
    return buildsystem.copy_flxs_to_lib(phase.ctx,
        Path('src/judy/*.flx').glob())
