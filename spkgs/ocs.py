caml_modules = [
  'src/compiler/ocs/ocs_vartable',
  'src/compiler/ocs/ocs_error',
  'src/compiler/ocs/ocs_port',
  'src/compiler/ocs/ocs_types',
  'src/compiler/ocs/ocs_sym',
  'src/compiler/ocs/ocs_env',
  'src/compiler/ocs/ocs_char',
  'src/compiler/ocs/ocs_numaux',
  'src/compiler/ocs/ocs_complex',
  'src/compiler/ocs/ocs_num',
  'src/compiler/ocs/ocs_numstr',
  'src/compiler/ocs/ocs_lex',
  'src/compiler/ocs/ocs_misc',
  'src/compiler/ocs/ocs_read',
  'src/compiler/ocs/ocs_print',
  'src/compiler/ocs/ocs_eval',
  'src/compiler/ocs/ocs_list',
  'src/compiler/ocs/ocs_compile',
  'src/compiler/ocs/ocs_macro',
  'src/compiler/ocs/ocs_io',
  'src/compiler/ocs/ocs_prim',
  'src/compiler/ocs/ocs_string',
  'src/compiler/ocs/ocs_vector',
  'src/compiler/ocs/ocs_contin',
  'src/compiler/ocs/ocs_top',
]

caml_provide_lib = 'src/compiler/ocs/ocslib'
caml_require_libs = ['nums','unix','ocslib']
caml_exes = ['src/compiler/ocs/ocs_main']
weaver_directory='doc/ocs'
pkg_requires = []
tmpdir = ['ocs']
