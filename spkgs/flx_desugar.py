iscr_source = ['flx.pak']

caml_modules = [
    'src/compiler/flx_desugar/flx_pat',
    'src/compiler/flx_desugar/flx_constfld',
    'src/compiler/flx_desugar/flx_macro',
    'src/compiler/flx_desugar/flx_colns',
    'src/compiler/flx_desugar/flx_ciltoflx',
    'src/compiler/flx_desugar/flx_cformat',
    'src/compiler/flx_desugar/flx_tcdoc',
    'src/compiler/flx_desugar/flx_desugar',
]

caml_include_paths = [
    'src/compiler/cil/ocamlutil',
    'src/compiler/cil/src',
    'src/compiler/cil/src/frontc',
    'src/compiler/dyp/dyplib',
    'src/compiler/flx_core',
    'src/compiler/flx_lex',
    'src/compiler/flx_misc',
    'src/compiler/flx_parse',
    'src/compiler/flx_version',
    'src/compiler/flxcclib',
    'src/compiler/ocs',
    'src/compiler/sex',
]

caml_provide_lib = 'src/compiler/flx_desugar/flx_desugar'

pkg_requires = [
    'cil',
    'dypgen',
    'flx_core',
    'flx_lex',
    'flx_misc',
    'flx_parse',
    'flx_version',
    'flxcc_util',
    'ocs',
    'sex',
]
