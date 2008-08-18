iscr_source = ['flx.pak']

caml_exes = [
    'src/compiler/drivers/flxl',
    'src/compiler/drivers/flxp',
    'src/compiler/drivers/flxm',
    'src/compiler/drivers/flxd',
    'src/compiler/drivers/flxb',
    'src/compiler/drivers/flxg',
    'src/compiler/drivers/stub',
]

caml_require_libs = [
    'str',
    'nums',
    'unix',
    'flx_version',
    'ocslib',
    'sexlib',
    'dyplib',
    'flx_misc',
    'cillib',
    'flxcclib',
    'inria_re',
    'flx_core',
    'flx_lex',
    'flx_parse',
    'flx_frontend',
    'flx_backend',
    'flx_version_hook',
]

caml_include_paths = [
    'src/compiler/frontend',
    'src/compiler/backend',
    'src/compiler/inria_re',
    'src/compiler/ocs',
    'src/compiler/sex',
    'src/compiler/cil',
    'src/compiler/cil/ocamlutil',
    'src/compiler/cil/src',
    'src/compiler/cil/src/frontc',
    'src/compiler/dyp/dyplib',
    'src/compiler/flxcclib',
    'src/compiler/flx_core',
    'src/compiler/flx_lex',
    'src/compiler/flx_parse',
    'src/compiler/flx_misc',
    'src/compiler/flx_version',
    'src/compiler/flx_version_hook'
]

pkg_requires = [
    'flx_frontend',
    'flx_backend',
]

weaver_directory = 'doc/flx/flx_compiler/'
