import fbuild
from fbuild.functools import call
from fbuild.path import Path
from fbuild.record import Record

# ------------------------------------------------------------------------------

def build_flx_version(phase):
    path = Path ('src/compiler/flx_version')
    return phase.ocaml.build_lib(path / 'flx_version',
        srcs=Path.glob(path / '*.ml{,i}'))

def build_flx_misc(phase):
    path = Path('src/compiler/flx_misc')
    return phase.ocaml.build_lib(path / 'flx_misc',
        srcs=Path.glob(path / '*.ml{,i}'),
        libs=[build_flx_version(phase)],
        external_libs=['nums', 'str', 'unix'])

def build_flx_version_hook(phase):
    path = phase.ctx.buildroot / 'src/compiler/flx_version_hook'
    return phase.ocaml.build_lib(path / 'flx_version_hook',
        srcs=Path.glob(path / '*.ml{,i}'),
        libs=[build_flx_version(phase)])

def build_flx_lex(phase):
    path = Path('src/compiler/flx_lex')
    dypgen = call('buildsystem.dypgen.build_exe', phase)
    return phase.ocaml.build_lib(path/'flx_lex',
        srcs=Path.globall(path / '*.ml{,i}'),
        libs=[
            call('buildsystem.dypgen.build_lib', phase),
            call('buildsystem.ocs.build_lib', phase),
            call('buildsystem.sex.build', phase),
            build_flx_version(phase)])

def build_flx_parse(phase):
    path = Path('src/compiler/flx_parse')
    dypgen = call('buildsystem.dypgen.build_exe', phase)
    return phase.ocaml.build_lib(path/'flx_parse',
        srcs=Path.globall(
            path / '*.ml{,i}',
            dypgen(path / 'flx_parse.dyp',
                flags=['--no-undef-nt', '--pv-obj', '--noemit-token-type'])),
        libs=[
            call('buildsystem.dypgen.build_lib', phase),
            call('buildsystem.ocs.build_lib', phase),
            call('buildsystem.sex.build', phase),
            build_flx_version(phase),
            build_flx_lex(phase)])

def build_flx_file(phase):
    path = Path('src/compiler/flx_file')
    return phase.ocaml.build_lib(path/'flx_file',
        srcs=Path.globall( path / '*.ml{,i}'),
        libs=[
            call('buildsystem.dypgen.build_lib', phase),
            call('buildsystem.ocs.build_lib', phase),
            call('buildsystem.sex.build', phase),
            build_flx_version(phase),
            build_flx_misc(phase),
            build_flx_lex(phase),
            build_flx_parse(phase),
            ])


def build_flx_core(phase):
    path = Path('src/compiler/flx_core')
    return phase.ocaml.build_lib(path / 'flx_core',
        srcs=Path.glob(path / '*.ml{,i}'),
        libs=[
            call('buildsystem.dypgen.build_lib', phase),
            call('buildsystem.ocs.build_lib', phase),
            build_flx_lex(phase),
            build_flx_parse(phase),
            build_flx_misc(phase),
            ],
        external_libs=['nums'])

def build_flx_desugar(phase):
    path = Path('src/compiler/flx_desugar')

    return phase.ocaml.build_lib(path / 'flx_desugar',
        srcs=Path.glob(path / '*.ml{,i}'),
        libs=[
            call('buildsystem.dypgen.build_lib', phase),
            call('buildsystem.ocs.build_lib', phase),
            call('buildsystem.sex.build', phase),
            build_flx_lex(phase),
            build_flx_parse(phase),
            build_flx_file(phase),
            build_flx_misc(phase),
            build_flx_core(phase),
            build_flx_version(phase),
            ],
        external_libs=['nums', 'unix'])

def build_flx_bind(phase):
    path = Path('src/compiler/flx_bind')
    return phase.ocaml.build_lib(path / 'flx_bind',
        srcs=Path.glob(path / '*.ml{,i}'),
        libs=[
            build_flx_lex(phase),
            build_flx_misc(phase),
            build_flx_core(phase),
            build_flx_desugar(phase)],
        external_libs=['nums'])

def build_flx_frontend(phase):
    path = Path('src/compiler/flx_frontend')
    return phase.ocaml.build_lib(path / 'flx_frontend',
        srcs=Path.glob(path / '*.ml{,i}'),
        libs=[
            build_flx_lex(phase),
            build_flx_misc(phase),
            build_flx_core(phase)])

def build_flx_opt(phase):
    path = Path('src/compiler/flx_opt')
    return phase.ocaml.build_lib(path / 'flx_opt',
        srcs=Path.glob(path / '*.ml{,i}'),
        libs=[
            build_flx_lex(phase),
            build_flx_misc(phase),
            build_flx_core(phase),
            build_flx_frontend(phase)])

def build_flx_lower(phase):
    path = Path('src/compiler/flx_lower')
    return phase.ocaml.build_lib(path / 'flx_lower',
        srcs=Path.glob(path / '*.ml{,i}'),
        libs=[
            build_flx_lex(phase),
            build_flx_misc(phase),
            build_flx_core(phase),
            build_flx_frontend(phase)])

def build_flx_backend(phase):
    path = Path('src/compiler/flx_backend')
    return phase.ocaml.build_lib(path / 'flx_backend',
        srcs=Path.glob(path / '*.ml{,i}'),
        libs=[
            build_flx_lex(phase),
            build_flx_misc(phase),
            build_flx_core(phase)])

def build_flx_cpp_backend(phase):
    path = Path('src/compiler/flx_cpp_backend')
    return phase.ocaml.build_lib(path / 'flx_cpp_backend',
        srcs=Path.globall(
            path / '*.ml{,i}',),
        libs=[
            build_flx_lex(phase),
            build_flx_misc(phase),
            build_flx_core(phase),
            build_flx_frontend(phase),
            build_flx_backend(phase)],
        external_libs=['nums'])

def build_flx_drivers(ctx, phase):
    libs = [
        call('buildsystem.ocs.build_lib', phase),
        call('buildsystem.sex.build', phase),
        call('buildsystem.dypgen.build_lib', phase),
        build_flx_version(phase),
        build_flx_lex(phase),
        build_flx_parse(phase),
        build_flx_misc(phase),
        build_flx_file(phase),
        build_flx_core(phase),
        build_flx_desugar(phase),
        build_flx_bind(phase),
        build_flx_frontend(phase),
        build_flx_opt(phase),
        build_flx_lower(phase),
        build_flx_backend(phase),
        build_flx_cpp_backend(phase),
        build_flx_version_hook(phase),
        ]

    external_libs = ['nums', 'unix', 'str']

    flxg = phase.ocaml.build_exe('host/bin/flxg',
        Path.glob(phase.ctx.buildroot/'src/compiler/flxg/*.ml{,i}'),
        libs=libs,
        external_libs=external_libs)

    return Record(
        flxg=flxg,
    )
