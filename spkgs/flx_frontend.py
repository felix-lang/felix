caml_modules = [
    'src/compiler/flx_frontend/flx_treg',
    'src/compiler/flx_frontend/flx_use',
    'src/compiler/flx_frontend/flx_prop',
    'src/compiler/flx_frontend/flx_child',
    'src/compiler/flx_frontend/flx_strabs',
    'src/compiler/flx_frontend/flx_typeclass',
    'src/compiler/flx_frontend/flx_axiom',
    'src/compiler/flx_frontend/flx_label',
    'src/compiler/flx_frontend/flx_cflow',
    'src/compiler/flx_frontend/flx_call',
    'src/compiler/flx_frontend/flx_passign',
    'src/compiler/flx_frontend/flx_tailit',
    'src/compiler/flx_frontend/flx_reparent',
    'src/compiler/flx_frontend/flx_spexes',
    'src/compiler/flx_frontend/flx_foldvars',
    'src/compiler/flx_frontend/flx_args',
    'src/compiler/flx_frontend/flx_uncurry',
    'src/compiler/flx_frontend/flx_stack_calls',
    'src/compiler/flx_frontend/flx_mkproc',
    'src/compiler/flx_frontend/flx_reduce',
    'src/compiler/flx_frontend/flx_mono',
    'src/compiler/flx_frontend/flx_mkcls',
    'src/compiler/flx_frontend/flx_global',
    'src/compiler/flx_frontend/flx_inst',
    'src/compiler/flx_frontend/flx_inline',
]

caml_include_paths = [
    'src/compiler/flx_core',
    'src/compiler/flx_misc',
    'src/compiler/flx_bind',
    'src/compiler/flxcclib',
    'src/compiler/inria_re',
]

caml_provide_lib = 'src/compiler/flx_frontend/flx_frontend'

pkg_requires = [
    'flx_core',
    'flx_misc',
    'flx_bind',
    'flxcc_util',
    'inria_re',
]

weaver_directory = 'doc/flx/flx_compiler/'
