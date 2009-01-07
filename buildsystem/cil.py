import fbuild
from fbuild.path import Path

# ------------------------------------------------------------------------------

def build(ocaml):
    path = Path('src', 'compiler', 'cil')

    return ocaml.ocaml.build_lib(path / 'cil', Path.globall(
            Path.glob(path / 'src/*.ml{,i}',
                exclude=path/'src/{main,libmaincil,testcil}.*'),
            ocaml.ocamllex(path / 'src/formatlex.mll'),
            ocaml.ocamlyacc(path / 'src/formatparse.mly'),

            path / 'ocamlutil/*.ml{,i}',
            path / 'src/frontc/*.ml{,i}',
            ocaml.ocamllex(path / 'src/frontc/clexer.mll'),
            ocaml.ocamlyacc(path / 'src/frontc/cparser.mly'),
            fbuild.buildroot / path / '*.ml{,i}'),
        external_libs=['str'],
    )
