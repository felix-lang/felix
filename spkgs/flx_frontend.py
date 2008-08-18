caml_modules = [
    'src/compiler/frontend/flx_treg',
    'src/compiler/frontend/flx_use',
    'src/compiler/frontend/flx_prop',
    'src/compiler/frontend/flx_child',
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
    'src/compiler/frontend/flx_inline',
]

caml_include_paths = [
    'src/compiler/flx_core',
    'src/compiler/flx_misc',
    'src/compiler/flx_bind',
    'src/compiler/flxcclib',
    'src/compiler/inria_re',
]

caml_provide_lib = 'src/compiler/frontend/flx_frontend'

pkg_requires = [
    'flx_core',
    'flx_misc',
    'flx_bind',
    'flxcc_util',
    'inria_re',
]

weaver_directory = 'doc/flx/flx_compiler/'
