import fbuild
from fbuild.packages import Copy
import fbuild.packages.ocaml as ocaml

from buildsystem.dyp import Dypgen

def build_flx_misc(env, builder):
    path = fbuild.Path('src/compiler/flx_misc')
    return ocaml.Library(path/'flx_misc',
        srcs=[path / '*.ml{,i}'],
        libs=['nums', 'str', 'unix'],
        builder=builder)

def build_flx_core(env, builder):
    path = fbuild.Path('src/compiler/flx_core')
    return ocaml.Library(path / 'flx_core',
        srcs=[path / '*.ml{,i}'],
        libs=[
            'nums',
            env.config(build_flx_misc, builder),
            env.config('buildsystem.ocs.build', builder).lib,
        ],
        builder=builder)

def build_flx_version(env, builder):
    path = fbuild.buildroot / 'src/compiler/flx_version'
    return ocaml.Library(path / 'flx_version',
        srcs=[path / '*.ml{,i}'],
        builder=builder)

def build_flx_version_hook(env, builder):
    path = fbuild.buildroot / 'src/compiler/flx_version_hook'
    return ocaml.Library(path / 'flx_version_hook',
        srcs=[path / '*.ml{,i}'],
        libs=[env.config(build_flx_version, builder)],
        builder=builder)

def build_flx_lex(env, builder):
    path = fbuild.Path('src/compiler/flx_lex')
    return ocaml.Library(path/'flx_lex',
        srcs=[
            path / '*.ml{,i}',
            fbuild.buildroot/ path / '*.ml{,i}',
            ocaml.Ocamllex(fbuild.buildroot / path / 'flx_lex.mll'),
            Dypgen(fbuild.buildroot / path / 'flx_preparse.dyp',
                flags=['--prio-pt', '--pv-obj', '--noemit-token-type']),
        ],
        libs=[
            env.config('buildsystem.dyp.build_dyplib', builder),
            env.config('buildsystem.ocs.build', builder).lib,
            env.config('buildsystem.sex.build', builder),
            env.config(build_flx_misc, builder),
            env.config(build_flx_core, builder),
            env.config(build_flx_version, builder),
        ],
        builder=builder)

def build_flx_parse(env, builder):
    path = fbuild.Path('src/compiler/flx_parse')
    return ocaml.Library(path/'flx_parse',
        srcs=[
            path / '*.ml{,i}',
            Dypgen(fbuild.buildroot / path / 'flx_parse.dyp',
                flags=['--prio-pt', '--pv-obj', '--noemit-token-type']),
        ],
        libs=[
            env.config('buildsystem.dyp.build_dyplib', builder),
            env.config('buildsystem.ocs.build', builder).lib,
            env.config('buildsystem.sex.build', builder),
            env.config(build_flx_misc, builder),
            env.config(build_flx_core, builder),
            env.config(build_flx_version, builder),
            env.config(build_flx_lex, builder),
        ],
        builder=builder)

def build_flxcclib(env, builder):
    path = fbuild.buildroot / 'src/compiler/flxcclib'
    return ocaml.Library(path / 'flxcclib',
        srcs=[path / '*.ml{,i}'],
        libs=[
            env.config('buildsystem.cil.build', builder),
            env.config(build_flx_core, builder),
        ],
        builder=builder)

def build_flx_desugar(env, builder):
    path = fbuild.Path('src/compiler/flx_desugar')
    return ocaml.Library(path / 'flx_desugar',
        srcs=[path / '*.ml{,i}'],
        libs=[
            'nums',
            'unix',
            env.config('buildsystem.dyp.build_dyplib', builder),
            env.config('buildsystem.ocs.build', builder).lib,
            env.config('buildsystem.sex.build', builder),
            env.config('buildsystem.cil.build', builder),
            env.config(build_flx_misc, builder),
            env.config(build_flx_core, builder),
            env.config(build_flx_version, builder),
            env.config(build_flx_lex, builder),
            env.config(build_flx_parse, builder),
            env.config(build_flxcclib, builder),
        ],
        builder=builder)

def build_inria_re(env, builder):
    path = fbuild.Path('src/compiler/inria_re')
    return ocaml.Library(path / 'inria_re', [path / '*.ml{,i}'],
        builder=builder)

def build_flx_bind(env, builder):
    path = fbuild.Path('src/compiler/flx_bind')
    return ocaml.Library(path / 'flx_bind', [path / '*.ml{,i}'],
        libs=[
            'nums',
            env.config(build_flx_misc, builder),
            env.config(build_flx_core, builder),
            env.config(build_inria_re, builder),
        ],
        builder=builder)

def build_flx_frontend(env, builder):
    path = fbuild.Path('src/compiler/flx_frontend')
    return ocaml.Library(path / 'flx_frontend', [path / '*.ml{,i}'],
        libs=[
            env.config(build_flx_misc, builder),
            env.config(build_flx_core, builder),
            env.config(build_flx_bind, builder),
        ],
        builder=builder)

def build_flx_backend(env, builder):
    path = fbuild.Path('src/compiler/flx_backend')
    return ocaml.Library(path / 'flx_backend', [
            path / '*.ml{,i}',
            fbuild.buildroot / path / '*.ml{,i}',
        ],
        libs=[
            'nums',
            env.config(build_flx_misc, builder),
            env.config(build_flx_core, builder),
            env.config(build_flx_bind, builder),
            env.config(build_flxcclib, builder),
            env.config(build_flx_frontend, builder),
        ],
        builder=builder)

def build_flx_drivers(env, builder):
    path = fbuild.Path('src', 'compiler', 'drivers')

    lib = ocaml.Library(path / 'flx_driver', [
            path / 'flx_terminate.ml{,i}',
            path / 'flx_flxopt.ml{,i}',
        ],
        libs=[
            env.config(build_flx_misc, builder),
            env.config(build_flx_core, builder),
            env.config(build_flxcclib, builder),
        ],
        builder=builder)

    flxl = ocaml.Executable(path / 'flxl', [path / 'flxl.ml'],
        libs=[
            lib,
            env.config(build_flx_misc, builder),
            env.config(build_flx_core, builder),
            env.config(build_flx_version, builder),
            env.config(build_flx_version_hook, builder),
            env.config(build_flx_lex, builder),
            env.config(build_flx_desugar, builder),
        ],
        builder=builder)

    flxp = ocaml.Executable(path / 'flxp', [path / 'flxp.ml'],
        libs=[
            lib,
            env.config(build_flx_misc, builder),
            env.config(build_flx_core, builder),
            env.config(build_flx_version, builder),
            env.config(build_flx_version_hook, builder),
            env.config(build_flx_lex, builder),
            env.config(build_flx_parse, builder),
            env.config(build_flx_desugar, builder),
        ],
        builder=builder)

    flxm = ocaml.Executable(path / 'flxm', [path / 'flxm.ml'],
        libs=[
            lib,
            env.config(build_flx_misc, builder),
            env.config(build_flx_core, builder),
            env.config(build_flx_version, builder),
            env.config(build_flx_version_hook, builder),
            env.config(build_flx_parse, builder),
            env.config(build_flx_desugar, builder),
        ],
        builder=builder)

    flxd = ocaml.Executable(path / 'flxd', [path / 'flxd.ml'],
        libs=[
            lib,
            env.config(build_flx_misc, builder),
            env.config(build_flx_core, builder),
            env.config(build_flx_version, builder),
            env.config(build_flx_version_hook, builder),
            env.config(build_flx_desugar, builder),
        ],
        builder=builder)

    flxb = ocaml.Executable(path / 'flxb', [path / 'flxb.ml'],
        libs=[
            lib,
            env.config(build_flx_misc, builder),
            env.config(build_flx_core, builder),
            env.config(build_flx_version, builder),
            env.config(build_flx_version_hook, builder),
            env.config(build_flx_parse, builder),
            env.config(build_flx_desugar, builder),
            env.config(build_flx_bind, builder),
            env.config(build_flx_frontend, builder),
        ],
        builder=builder)

    flxg = ocaml.Executable(path / 'flxg', [path / 'flxg.ml'],
        libs=[
            lib,
            env.config(build_flx_misc, builder),
            env.config(build_flx_core, builder),
            env.config(build_flx_version, builder),
            env.config(build_flx_version_hook, builder),
            env.config(build_flxcclib, builder),
            env.config(build_flx_desugar, builder),
            env.config(build_flx_bind, builder),
            env.config(build_flx_frontend, builder),
            env.config(build_flx_backend, builder),
        ],
        builder=builder)

    return fbuild.Record(
        flxl=Copy(fbuild.buildroot / 'bin/flxl', flxl),
        flxp=Copy(fbuild.buildroot / 'bin/flxp', flxp),
        flxm=Copy(fbuild.buildroot / 'bin/flxm', flxm),
        flxd=Copy(fbuild.buildroot / 'bin/flxd', flxd),
        flxb=Copy(fbuild.buildroot / 'bin/flxb', flxb),
        flxg=Copy(fbuild.buildroot / 'bin/flxg', flxg),
    )
