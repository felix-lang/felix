import fbuild
from fbuild.path import Path
from fbuild.record import Record

# ------------------------------------------------------------------------------

def build_lib(ocaml):
    path = Path('src/compiler/ocs/src')

    return ocaml.build_lib(path/'ocs',
        list((path/'*.ml{,i}').glob(exclude='ocs_main.ml')),
        external_libs=['nums', 'unix'])

def build_exe(ocaml):
    path = Path('src/compiler/ocs/src')

    return ocaml.build_exe(path/'ocs', [path/'ocs_main.ml'],
        libs=[build_lib(ocaml)],
        external_libs=['nums', 'unix'])
