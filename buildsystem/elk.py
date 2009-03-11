import fbuild
from fbuild.functools import call
from fbuild.path import Path
from fbuild.record import Record

import buildsystem

# ------------------------------------------------------------------------------

def build_runtime(phase):
    buildsystem.copy_hpps_to_rtl(
        fbuild.buildroot / 'config/target/flx_faio_config.hpp',
        fbuild.buildroot / 'config/target/flx_elk_config.hpp',
        fbuild.buildroot / 'config/host/flx_host_elk_config.hpp',
        fbuild.buildroot / 'config/target/flx_target_elk_config.hpp',
        'src/elkhound/elk_glr.h',
        'src/elkhound/elk_glrconfig.h',
        'src/elkhound/elk_lexerint.h',
        'src/elkhound/elk_parsetables.h',
        'src/elkhound/elk_rcptr.h',
        'src/elkhound/elk_useract.h',
        'src/smbase/sm_array.h',
        'src/smbase/sm_macros.h',
        'src/smbase/sm_objlist.h',
        'src/smbase/sm_objpool.h',
        'src/smbase/sm_sobjlist.h',
        'src/smbase/sm_srcloc.h',
        'src/smbase/sm_str.h',
        'src/smbase/sm_trdelete.h',
        'src/smbase/sm_typ.h',
        'src/smbase/sm_voidlist.h',
        'src/smbase/sm_xassert.h',
    )

    dst = fbuild.buildroot / 'lib/rtl/elk'
    srcs = Path.globall(
        'src/elkhound/elk_glr.cpp',
        'src/elkhound/elk_parsetables.cpp',
        'src/elkhound/elk_useract.cpp',
        'src/elkhound/elk_ptreenode.cpp',
        'src/elkhound/elk_ptreeact.cpp',
        'src/smbase/*.cpp',
        exclude=[
            'src/smbase/sm_mysig.cpp',
            'src/smbase/sm_tsobjlist.cpp',
        ])
    includes = [
        fbuild.buildroot / 'config/host',
        fbuild.buildroot / 'config/target',
        'src/smbase',
    ]

    macros = ['BUILD_ELK', 'TARGET_BUILD']

    return Record(
        static=buildsystem.build_cxx_static_lib(phase, dst, srcs,
            includes=includes,
            macros=macros + ['FLX_STATIC_LINK']),
        shared=buildsystem.build_cxx_shared_lib(phase, dst, srcs,
            includes=includes,
            macros=macros))

def build_exe(phase):
    srcs = Path.globall(
        'src/ast/*.cpp',
        'src/elkhound/*.cpp',
        'src/elkhound/*.cxx',
        'src/smbase/*.cpp',
        exclude=[
            'src/ast/ast_ast.ast.cpp',
            'src/ast/ast_towner.cpp',
            'src/smbase/sm_mysig.cpp',
            'src/smbase/sm_tsobjlist.cpp',
        ])

    return phase.cxx.static.build_exe(
        dst=fbuild.buildroot / 'bin/flx_elkhound',
        srcs=srcs,
        includes=[
            fbuild.buildroot / 'config/host',
            fbuild.buildroot / 'config/target',
            'src/smbase',
            'src/ast',
        ],
        macros=['HOST_BUILD', 'FLX_STATIC_LINK'],
    )
