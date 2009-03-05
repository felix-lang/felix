import fbuild
from fbuild.functools import call
from fbuild.path import Path
from fbuild.record import Record

import buildsystem

# ------------------------------------------------------------------------------

def build_runtime(phase, host=True):
    buildsystem.copy_hpps_to_rtl(
        fbuild.buildroot / 'config/target/flx_faio_config.hpp',
        fbuild.buildroot / 'config/target/flx_elk_config.hpp',
        fbuild.buildroot / 'config/host/flx_host_elk_config.hpp',
        fbuild.buildroot / 'config/target/flx_target_elk_config.hpp',
        'src/smbase/sm_array.h',
        'src/smbase/sm_objpool.h',
        'src/smbase/sm_sobjlist.h',
        'src/smbase/sm_trdelete.h',
        'src/smbase/sm_voidlist.h',
        'src/smbase/sm_macros.h',
        'src/smbase/sm_srcloc.h',
        'src/smbase/sm_typ.h',
        'src/smbase/sm_xassert.h',
        'src/smbase/sm_objlist.h',
        'src/smbase/sm_str.h',
        'src/elkhound/elk_lexerint.h',
        'src/elkhound/elk_glrconfig.h',
        'src/elkhound/elk_parsetables.h',
        'src/elkhound/elk_glr.h',
        'src/elkhound/elk_rcptr.h',
        'src/elkhound/elk_useract.h',
    )

    dst = fbuild.buildroot / 'lib/rtl/elk'
    srcs = Path.globall(
        'src/smbase/sm_malloc_stub.cpp',
        'src/smbase/sm_nonport.cpp',
        'src/smbase/sm_autofile.cpp',
        'src/smbase/sm_bflatten.cpp',
        'src/smbase/sm_bit2d.cpp',
        'src/smbase/sm_bitarray.cpp',
        'src/smbase/sm_boxprint.cpp',
        'src/smbase/sm_breaker.cpp',
        'src/smbase/sm_crc.cpp',
        'src/smbase/sm_datablok.cpp',
        'src/smbase/sm_flatten.cpp',
        'src/smbase/sm_growbuf.cpp',
        'src/smbase/sm_gprintf.cpp',
        'src/smbase/sm_hashline.cpp',
        'src/smbase/sm_hashtbl.cpp',
        'src/smbase/sm_missing.cpp',
        'src/smbase/sm_point.cpp',
        'src/smbase/sm_pprint.cpp',
        'src/smbase/sm_strdict.cpp',
        'src/smbase/sm_strhash.cpp',
        'src/smbase/sm_stringset.cpp',
        'src/smbase/sm_strtokp.cpp',
        'src/smbase/sm_strutil.cpp',
        'src/smbase/sm_svdict.cpp',
        'src/smbase/sm_vdtllist.cpp',
        'src/smbase/sm_vptrmap.cpp',
        'src/smbase/sm_warn.cpp',
        'src/smbase/sm_srcloc.cpp',
        'src/smbase/sm_syserr.cpp',
        'src/smbase/sm_str.cpp',
        'src/smbase/sm_trace.cpp',
        'src/smbase/sm_trdelete.cpp',
        'src/smbase/sm_voidlist.cpp',
        'src/smbase/sm_exc.cpp',

        'src/elkhound/elk_glr.cpp',
        'src/elkhound/elk_parsetables.cpp',
        'src/elkhound/elk_useract.cpp',
        'src/elkhound/elk_ptreenode.cpp',
        'src/elkhound/elk_ptreeact.cpp',
    )
    includes = [
        fbuild.buildroot / 'config/host',
        fbuild.buildroot / 'config/target',
        'src/smbase',
    ]

    if host:
        dst += '_host'
        macros = ['BUILD_ELK', 'HOST_BUILD']
    else:
        macros = ['BUILD_ELK', 'TARGET_BUILD']

    return Record(
        static=buildsystem.build_cxx_static_lib(phase, dst, srcs,
            includes=includes,
            macros=macros),
        shared=buildsystem.build_cxx_shared_lib(phase, dst, srcs,
            includes=includes,
            macros=macros))

def build_exe(phase):
    srcs = Path.globall(
        'src/ast/ast_gramlex.cpp',
        'src/ast/ast_ccsstr.cpp',
        'src/ast/ast_reporterr.cpp',
        'src/ast/ast_embedded.cpp',
        'src/ast/ast_asthelp.cpp',
        'src/ast/ast_strtable.cpp',
        'src/ast/ast_locstr.cpp',

        'src/elkhound/elk_asockind.cpp',
        'src/elkhound/elk_grammar.cpp',
        'src/elkhound/elk_emitcode.cpp',
        'src/elkhound/elk_mlsstr.cpp',
        'src/elkhound/elk_genml.cpp',
        'src/elkhound/elk_gramast.ast.gen.cpp',
        'src/elkhound/elk_gramlex.yy.cpp',
        'src/elkhound/elk_grampar.cpp',
        'src/elkhound/elk_grampar.tab.cpp',
        'src/elkhound/elk_gramexpl.cpp',
        'src/elkhound/elk_gramanl.cpp',
        'src/elkhound/elk_gramanl.cxx',
    )

    return phase.cxx.static.build_exe(
        dst=fbuild.buildroot / 'bin/flx_elkhound',
        srcs=srcs,
        includes=[
            fbuild.buildroot / 'config/host',
            fbuild.buildroot / 'config/target',
            'src/smbase',
            'src/ast',
        ],
        libs=[call(build_runtime, phase, host=True).static],
        macros=['HOST_BUILD', 'FLX_STATIC_LINK'],
    )
