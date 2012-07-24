import fbuild
from fbuild.path import Path
from fbuild.record import Record

# ------------------------------------------------------------------------------

def build_lib(phase):
    path = Path('src/compiler/ocs/src')

    return phase.ocaml.build_lib(path/'ocs',
        list((path/'*.ml{,i}').glob(exclude=path/'ocs_main.ml')),
        external_libs=['nums', 'unix'])

def build_exe(phase):
    path = Path('src/compiler/ocs/src')

    return phase.ocaml.build_exe(path/'ocs', [path/'ocs_main.ml'],
        libs=[build_lib(phase)],
        external_libs=['nums', 'unix'])
