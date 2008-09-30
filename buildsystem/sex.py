import fbuild
import fbuild.packages.ocaml as ocaml
from fbuild.path import Path

# -----------------------------------------------------------------------------

def build(builder=None):
    from buildsystem.dyp import Dypgen

    path = Path('src', 'compiler', 'sex')
    return ocaml.Library(path/'sex', [
            path/'*.ml{,i}',
            Dypgen(path/'sex_parse.dyp'),
            ocaml.Ocamllex(path/'sex_lex.mll'),
        ],
        libs=[
            fbuild.env.run('buildsystem.ocs.build', builder).lib,
            fbuild.env.run('buildsystem.dyp.build_dyplib', builder),
        ],
        builder=builder)
