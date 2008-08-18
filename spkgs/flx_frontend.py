caml_modules = [
    'src/compiler/frontend/flx_dlst',
    'src/compiler/frontend/flx_unify',
    'src/compiler/frontend/flx_beta',
    'src/compiler/frontend/flx_generic',
    'src/compiler/frontend/flx_tpat',
    'src/compiler/frontend/flx_tconstraint',
    'src/compiler/frontend/flx_overload',
    'src/compiler/frontend/flx_lookup',
    'src/compiler/frontend/flx_mbind',
    'src/compiler/frontend/flx_treg',
    'src/compiler/frontend/flx_use',
    'src/compiler/frontend/flx_prop',
    'src/compiler/frontend/flx_cexpr',
    'src/compiler/frontend/flx_symtab',
    'src/compiler/frontend/flx_child',
    'src/compiler/frontend/flx_bexe',
    'src/compiler/frontend/flx_dfa',
    'src/compiler/frontend/flx_bbind',
    'src/compiler/frontend/flx_strabs',
    'src/compiler/frontend/flx_typeclass',
    'src/compiler/frontend/flx_axiom',
    'src/compiler/frontend/flx_label',
    'src/compiler/frontend/flx_cflow',
    'src/compiler/frontend/flx_call',
    'src/compiler/frontend/flx_passign',
    'src/compiler/frontend/flx_tailit',
    'src/compiler/frontend/flx_reparent',
    'src/compiler/frontend/flx_spexes',
    'src/compiler/frontend/flx_foldvars',
    'src/compiler/frontend/flx_args',
    'src/compiler/frontend/flx_uncurry',
    'src/compiler/frontend/flx_stack_calls',
    'src/compiler/frontend/flx_mkproc',
    'src/compiler/frontend/flx_reduce',
    'src/compiler/frontend/flx_mono',
    'src/compiler/frontend/flx_mkcls',
    'src/compiler/frontend/flx_global',
    'src/compiler/frontend/flx_inst',
    'src/compiler/frontend/flx_terminate',
    'src/compiler/frontend/flx_inline',
]

caml_include_paths = [
    'src/compiler/flx_core',
    'src/compiler/flx_misc',
    'src/compiler/flxcclib',
    'src/compiler/inria_re',
]

caml_provide_lib = 'src/compiler/frontend/flx_frontend'

pkg_requires = [
    'flx_core',
    'flx_misc',
    'flxcc_util',
    'inria_re',
]

weaver_directory = 'doc/flx/flx_compiler/'
