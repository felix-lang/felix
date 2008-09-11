iscr_source = ['flx.pak']

caml_modules = [
    'src/compiler/flx_backend/flx_backend_config',
    'src/compiler/flx_backend/flx_name',
    'src/compiler/flx_backend/flx_csubst',
    'src/compiler/flx_backend/flx_tgen',
    'src/compiler/flx_backend/flx_display',
    'src/compiler/flx_backend/flx_ogen',
    'src/compiler/flx_backend/flx_regen',
    'src/compiler/flx_backend/flx_unravel',
    'src/compiler/flx_backend/flx_pgen',
    'src/compiler/flx_backend/flx_egen',
    'src/compiler/flx_backend/flx_ctorgen',
    'src/compiler/flx_backend/flx_elkgen',
    'src/compiler/flx_backend/flx_why',
    'src/compiler/flx_backend/flx_gen',
]

caml_include_paths = [
    'src/compiler/flx_core',
    'src/compiler/flx_misc',
    'src/compiler/flx_bind',
    'src/compiler/flxcclib',
    'src/compiler/flx_frontend',
]

caml_provide_lib = 'src/compiler/flx_backend/flx_backend'

pkg_requires = [
    'flx_core',
    'flx_frontend',
    'flx_misc',
    'flx_bind',
    'flxcc_util',
]

weaver_directory = 'doc/flx/flx_compiler/'
