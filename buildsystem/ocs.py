import fbuild
import fbuild.packages.ocaml as ocaml

def build(env, builder=None):
    ocs = fbuild.Record()

    path = fbuild.Path('src/compiler/ocs')

    ocs.lib = ocaml.Library(path/'ocs',
        list((path/'*.ml{,i}').glob(exclude='ocs_main.ml')),
        libs=['nums', 'unix'],
        builder=builder)

    ocs.exe = ocaml.Executable(path/'ocs', [path/'ocs_main.ml'],
        libs=['unix', 'nums', ocs.lib],
        builder=builder)

    return ocs
