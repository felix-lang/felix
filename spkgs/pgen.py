caml_include_paths = ['src/compiler/dyp/dyplib']
caml_lexes = ['src/compiler/dyp/generators/pgen/pgen_lexer']
caml_implementations=[
  'src/compiler/dyp/generators/pgen/pgen_parser_param',
  'src/compiler/dyp/generators/pgen/pgen_lexer'
]
caml_provide_lib = 'src/compiler/dyp/generators/pgen/pgen'
caml_require_libs = ['dyplib','pgen']
caml_exes = ['src/compiler/dyp/generators/pgen/pgen']
weaver_directory = 'doc/dypgen/'
pkg_requires = ['dyplib']
