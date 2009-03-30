caml_modules = [
    'src/compiler/flx_bind/flx_generic',
    'src/compiler/flx_bind/flx_tpat',
    'src/compiler/flx_bind/flx_tconstraint',
    'src/compiler/flx_bind/flx_overload',
    'src/compiler/flx_bind/flx_lookup',
    'src/compiler/flx_bind/flx_mbind',
    'src/compiler/flx_bind/flx_bexe',
    'src/compiler/flx_bind/flx_bbind',
    'src/compiler/flx_bind/flx_symtab',
]

caml_include_paths = [
    'src/compiler/flx_core',
    'src/compiler/flx_misc',
    'src/compiler/inria_re',
]

caml_provide_lib = 'src/compiler/flx_bind/flx_bind'

pkg_requires = [
    'flx_core',
    'flx_misc',
    'inria_re',
]
