import fbuild
import fbuild.packages.ocaml as ocaml

def build(env, builder=None):
    from buildsystem.dyp import Dypgen

    path = fbuild.Path('src', 'compiler', 'sex')
    return ocaml.Library(path/'sex', [
            path/'*.ml{,i}',
            Dypgen(path/'sex_parse.dyp'),
            ocaml.Ocamllex(path/'sex_lex.mll'),
        ],
        libs=[
            env.config('buildsystem.ocs.build', builder).lib,
            env.config('buildsystem.dyp.build_dyplib', builder),
        ],
        builder=builder)
