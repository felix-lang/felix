iscr_source = ['flx.pak']

caml_modules = [
    'src/compiler/flx_parse/flx_sex2flx',
    'src/compiler/flx_parse/flx_parse',
]

caml_include_paths = [
    'src/compiler/dyp/dyplib',
    'src/compiler/flx_core',
    'src/compiler/flx_lex',
    'src/compiler/flx_misc',
    'src/compiler/flx_version',
    'src/compiler/ocs',
    'src/compiler/sex',
]

caml_provide_lib = 'src/compiler/flx_parse/flx_parse'

pkg_requires = [
    'dypgen',
    'flx_core',
    'flx_lex',
    'flx_misc',
    'flx_version',
    'ocs',
    'sex',
]
