import fbuild
import fbuild.packages.cxx as cxx
from fbuild.record import Record

# -----------------------------------------------------------------------------

def build(host, target):
    smbase_srcs = [
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
    ]

    ast_srcs = [
        'src/ast/ast_gramlex.cpp',
        'src/ast/ast_ccsstr.cpp',
        'src/ast/ast_reporterr.cpp',
        'src/ast/ast_embedded.cpp',
        'src/ast/ast_asthelp.cpp',
        'src/ast/ast_strtable.cpp',
        'src/ast/ast_locstr.cpp',
    ]

    elkhound_srcs = [
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
    ]

    elkrtl_srcs = [
        'src/elkhound/elk_glr.cpp',
        'src/elkhound/elk_parsetables.cpp',
        'src/elkhound/elk_useract.cpp',
        'src/elkhound/elk_ptreenode.cpp',
        'src/elkhound/elk_ptreeact.cpp',
    ]

    srcs = []
    srcs.extend(smbase_srcs)
    srcs.extend(ast_srcs)
    srcs.extend(elkhound_srcs)
    srcs.extend(elkrtl_srcs)
    srcs.append('src/elkhound/elk_gramanl.cpp')

    exe = cxx.Executable(fbuild.buildroot / 'bin/flx_elkhound',
        srcs + ['src/elkhound/elk_gramanl.cxx'],
        includes=[
            fbuild.buildroot / 'config/host',
            fbuild.buildroot / 'config/target',
            'src/smbase',
            'src/ast',
        ],
        macros=['HOST_BUILD'],
        builder=target.cxx,
    )

    for hpp in (
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
            'src/elkhound/elk_useract.h'):
        fbuild.packages.Copy(fbuild.buildroot / 'lib/rtl', hpp).build()

    lib = cxx.SharedLibrary(fbuild.buildroot / 'lib/rtl/elk_dynamic',
        elkrtl_srcs + smbase_srcs,
        includes=[
            fbuild.buildroot / 'config/host',
            fbuild.buildroot / 'config/target',
            'src/smbase',
        ],
        macros=['TARGET_BUILD'],
        builder=target.cxx,
    )

    return Record(exe=exe, lib=lib)
