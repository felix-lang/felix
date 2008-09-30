import fbuild
import fbuild.packages.ocaml as ocaml
from fbuild.path import Path
from fbuild.record import Record

# -----------------------------------------------------------------------------

def build(builder=None):
    ocs = Record()

    path = Path('src/compiler/ocs')

    ocs.lib = ocaml.Library(path/'ocs',
        list((path/'*.ml{,i}').glob(exclude='ocs_main.ml')),
        libs=['nums', 'unix'],
        builder=builder)

    ocs.exe = ocaml.Executable(path/'ocs', [path/'ocs_main.ml'],
        libs=['unix', 'nums', ocs.lib],
        builder=builder)

    return ocs
