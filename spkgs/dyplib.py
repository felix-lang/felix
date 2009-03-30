caml_interfaces = [
#  'src/compiler/dyp/dyplib/sig',
  'src/compiler/dyp/dyplib/dyp',
]

caml_implementations = [
  'src/compiler/dyp/dyplib/priority_by_relation',
  'src/compiler/dyp/dyplib/automaton',
  'src/compiler/dyp/dyplib/gs',
#  'src/compiler/dyp/dyplib/parser',
  'src/compiler/dyp/dyplib/dyp',
]

#caml_pack = [
#  ("Dyp",'src/compiler/dyp/dyplib/dyp',[
#    'src/compiler/dyp/dyplib/sig',
#    'src/compiler/dyp/dyplib/gs',
#    'src/compiler/dyp/dyplib/priority_by_relation',
#    'src/compiler/dyp/dyplib/automaton',
#    'src/compiler/dyp/dyplib/parser'
#  ])
#]

caml_provide_lib = 'src/compiler/dyp/dyplib/dyplib'
weaver_directory = 'doc/dypgen/'
