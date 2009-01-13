import fbuild
from fbuild.path import Path

# ------------------------------------------------------------------------------

def build(ocaml, ocamllex, ocamlyacc):
    path = Path('src', 'compiler', 'cil')

    return ocaml.build_lib(path / 'cil', Path.globall(
            Path.glob(path / 'src/*.ml{,i}',
                exclude=path/'src/{main,libmaincil,testcil}.*'),
            ocamllex(path / 'src/formatlex.mll'),
            ocamlyacc(path / 'src/formatparse.mly'),

            path / 'ocamlutil/*.ml{,i}',
            path / 'src/frontc/*.ml{,i}',
            ocamllex(path / 'src/frontc/clexer.mll'),
            ocamlyacc(path / 'src/frontc/cparser.mly'),
            fbuild.buildroot / path / '*.ml{,i}'),
        external_libs=['str'],
    )
