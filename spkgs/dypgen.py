caml_lexes = [
  'src/compiler/dyp/generators/dypgen/dypgen_lexer',
  'src/compiler/dyp/generators/dypgen/insert_linenum'
]

caml_pgenparses = ['src/compiler/dyp/generators/dypgen/dypgen_parser']
caml_interfaces =[
  'src/compiler/dyp/generators/dypgen/parse_tree',
  'src/compiler/dyp/generators/dypgen/dypgen_parser',
]
caml_implementations=[
  'src/compiler/dyp/generators/dypgen/argument',
  'src/compiler/dyp/generators/dypgen/dypgen_parser',
  'src/compiler/dyp/generators/dypgen/dypgen_lexer',
  'src/compiler/dyp/generators/dypgen/insert_linenum',
]
caml_include_paths = ['src/compiler/dyp/dyplib']
caml_provide_lib = 'src/compiler/dyp/generators/dypgen/dypgen'
caml_require_libs = ['dyplib','dypgen']
caml_exes = ['src/compiler/dyp/generators/dypgen/dypgen']
pkg_requires = ['dyplib','pgen']
weaver_directory = 'doc/dypgen/'
