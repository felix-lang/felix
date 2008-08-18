caml_modules = [
    'src/compiler/backend/flx_backend_config',
    'src/compiler/backend/flx_name',
    'src/compiler/backend/flx_csubst',
    'src/compiler/backend/flx_tgen',
    'src/compiler/backend/flx_display',
    'src/compiler/backend/flx_ogen',
    'src/compiler/backend/flx_regen',
    'src/compiler/backend/flx_unravel',
    'src/compiler/backend/flx_pgen',
    'src/compiler/backend/flx_egen',
    'src/compiler/backend/flx_ctorgen',
    'src/compiler/backend/flx_elkgen',
    'src/compiler/backend/flx_why',
    'src/compiler/backend/flx_gen',
    'src/compiler/backend/flx_flxopt',
]

caml_include_paths = [
    'src/compiler/flx_misc',
    'src/compiler/frontend',
    'src/compiler/flxcclib',
]

caml_provide_lib = 'src/compiler/backend/flx_backend'

pkg_requires = [
    'flx_misc',
    'flx_frontend',
    'flxcc_util',
]

weaver_directory = 'doc/flx/flx_compiler/'
