iscr_source = ['flx.pak']

caml_modules = [
    'src/compiler/flx_lex/flx_token',
    'src/compiler/flx_lex/flx_prelex',
    'src/compiler/flx_lex/flx_tok',
    'src/compiler/flx_lex/flx_charset',
    'src/compiler/flx_lex/flx_pdoc',
    'src/compiler/flx_lex/flx_preparse',
    'src/compiler/flx_lex/flx_id',
]

caml_include_paths = [
    'src/compiler/dyp/dyplib',
    'src/compiler/flx_core',
    'src/compiler/flx_misc',
    'src/compiler/flx_version',
    'src/compiler/ocs',
    'src/compiler/sex',
]

caml_provide_lib = 'src/compiler/flx_lex/flx_lex'

pkg_requires = [
    'dypgen',
    'flx_core',
    'flx_misc',
    'flx_version',
    'ocs',
    'sex',
]
