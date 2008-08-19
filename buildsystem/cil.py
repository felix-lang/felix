import fbuild
import fbuild.packages.ocaml as ocaml

def build(env, builder=None):
    path = fbuild.Path('src', 'compiler', 'cil')

    return ocaml.Library(path / 'cil', [
            list((path / 'src/*.ml{,i}').glob(
                exclude=path/'src'/'{main,libmaincil,testcil}.*')),
            ocaml.Ocamllex(path / 'src/formatlex.mll'),
            ocaml.Ocamlyacc(path / 'src/formatparse.mly'),

            path / 'ocamlutil/*.ml{,i}',
            path / 'src/frontc/*.ml{,i}',
            ocaml.Ocamllex(path / 'src/frontc/clexer.mll'),
            ocaml.Ocamlyacc(path / 'src/frontc/cparser.mly'),
            fbuild.buildroot / path / '*.ml{,i}',
        ],
        libs=['str'],
        builder=builder,
    )
