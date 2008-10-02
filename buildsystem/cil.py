import fbuild
from fbuild.path import Path

# -----------------------------------------------------------------------------

def build(ocaml):
    path = Path('src', 'compiler', 'cil')

    return ocaml.builder.build_lib(path / 'cil', [
            list((path / 'src/*.ml{,i}').glob(
                exclude=path/'src'/'{main,libmaincil,testcil}.*')),
            ocaml.ocamllex(path / 'src/formatlex.mll'),
            ocaml.ocamlyacc(path / 'src/formatparse.mly'),

            path / 'ocamlutil/*.ml{,i}',
            path / 'src/frontc/*.ml{,i}',
            ocaml.ocamllex(path / 'src/frontc/clexer.mll'),
            ocaml.ocamlyacc(path / 'src/frontc/cparser.mly'),
            fbuild.buildroot / path / '*.ml{,i}',
        ],
        libs=['str'],
    )
