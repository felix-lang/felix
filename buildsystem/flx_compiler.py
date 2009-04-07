import fbuild
from fbuild.functools import call
from fbuild.path import Path
from fbuild.record import Record

# ------------------------------------------------------------------------------

def build_flx_misc(ocaml):
    path = Path('src/compiler/flx_misc')
    return ocaml.build_lib(path / 'flx_misc',
        srcs=Path.glob(path / '*.ml{,i}'),
        external_libs=['nums', 'str', 'unix'])

def build_flx_core(ocaml):
    path = Path('src/compiler/flx_core')
    return ocaml.build_lib(path / 'flx_core',
        srcs=Path.glob(path / '*.ml{,i}'),
        libs=[
            build_flx_misc(ocaml),
            call('buildsystem.ocs.build_lib', ocaml)],
        external_libs=['nums'])

def build_flx_version(ocaml):
    path = fbuild.buildroot / 'src/compiler/flx_version'
    return ocaml.build_lib(path / 'flx_version',
        srcs=Path.glob(path / '*.ml{,i}'))

def build_flx_version_hook(ocaml):
    path = fbuild.buildroot / 'src/compiler/flx_version_hook'
    return ocaml.build_lib(path / 'flx_version_hook',
        srcs=Path.glob(path / '*.ml{,i}'),
        libs=[build_flx_version(ocaml)])

def build_flx_lex(ocaml, ocamllex):
    path = Path('src/compiler/flx_lex')
    dypgen = call('buildsystem.dypgen.build_exe', ocaml, ocamllex)
    return ocaml.build_lib(path/'flx_lex',
        srcs=Path.globall(
            path / '*.ml{,i}',
            ocamllex(path / 'flx_lex.mll'),
            dypgen(path / 'flx_preparse.dyp',
                flags=['--no-undef-nt', '--pv-obj', '--noemit-token-type'])),
        libs=[
            call('buildsystem.dypgen.build_lib', ocaml),
            call('buildsystem.ocs.build_lib', ocaml),
            call('buildsystem.sex.build', ocaml, ocamllex),
            build_flx_misc(ocaml),
            build_flx_core(ocaml),
            build_flx_version(ocaml)])

def build_flx_parse(ocaml, ocamllex):
    path = Path('src/compiler/flx_parse')
    dypgen = call('buildsystem.dypgen.build_exe', ocaml, ocamllex)
    return ocaml.build_lib(path/'flx_parse',
        srcs=Path.globall(
            path / '*.ml{,i}',
            dypgen(path / 'flx_parse.dyp',
                flags=['--no-undef-nt', '--pv-obj', '--noemit-token-type'])),
        libs=[
            call('buildsystem.dypgen.build_lib', ocaml),
            call('buildsystem.ocs.build_lib', ocaml),
            call('buildsystem.sex.build', ocaml, ocamllex),
            build_flx_misc(ocaml),
            build_flx_core(ocaml),
            build_flx_version(ocaml),
            build_flx_lex(ocaml, ocamllex)])

def build_flxcclib(ocaml, ocamllex, ocamlyacc):
    path = Path('src/compiler/flxcclib')
    return ocaml.build_lib(path / 'flxcclib',
        srcs=Path.glob(path / '*.ml{,i}'),
        includes=[fbuild.buildroot / 'src/compiler/cil/src'],
        libs=[
            call('buildsystem.cil.build', ocaml, ocamllex, ocamlyacc),
            build_flx_core(ocaml)])

def build_flx_desugar(ocaml, ocamllex, ocamlyacc):
    path = Path('src/compiler/flx_desugar')

    return ocaml.build_lib(path / 'flx_desugar',
        srcs=Path.glob(path / '*.ml{,i}'),
        includes=[
            fbuild.buildroot / 'src/compiler/cil/ocamlutil',
            fbuild.buildroot / 'src/compiler/cil/src',
            fbuild.buildroot / 'src/compiler/cil/src/frontc'],
        libs=[
            call('buildsystem.dypgen.build_lib', ocaml),
            call('buildsystem.ocs.build_lib', ocaml),
            call('buildsystem.sex.build', ocaml, ocamllex),
            call('buildsystem.cil.build', ocaml, ocamllex, ocamlyacc),
            build_flx_misc(ocaml),
            build_flx_core(ocaml),
            build_flx_version(ocaml),
            build_flx_lex(ocaml, ocamllex),
            build_flx_parse(ocaml, ocamllex),
            build_flxcclib(ocaml, ocamllex, ocamlyacc)],
        external_libs=['nums', 'unix'])

def build_inria_re(ocaml):
    path = Path('src/compiler/inria_re')
    return ocaml.build_lib(path / 'inria_re',
        srcs=Path.glob(path / '*.ml{,i}'))

def build_flx_bind(ocaml):
    path = Path('src/compiler/flx_bind')
    return ocaml.build_lib(path / 'flx_bind',
        srcs=Path.glob(path / '*.ml{,i}'),
        libs=[
            build_flx_misc(ocaml),
            build_flx_core(ocaml),
            build_inria_re(ocaml)],
        external_libs=['nums'])

def build_flx_frontend(ocaml):
    path = Path('src/compiler/flx_frontend')
    return ocaml.build_lib(path / 'flx_frontend',
        srcs=Path.glob(path / '*.ml{,i}'),
        libs=[
            build_flx_misc(ocaml),
            build_flx_core(ocaml),
            build_flx_bind(ocaml)])

def build_flx_backend(ocaml, ocamllex, ocamlyacc):
    path = Path('src/compiler/flx_backend')
    return ocaml.build_lib(path / 'flx_backend',
        srcs=Path.globall(
            path / '*.ml{,i}',
            fbuild.buildroot / path / '*.ml{,i}'),
        libs=[
            build_flx_misc(ocaml),
            build_flx_core(ocaml),
            build_flx_bind(ocaml),
            build_flxcclib(ocaml, ocamllex, ocamlyacc),
            build_flx_frontend(ocaml)],
        external_libs=['nums'])

def build_flx_drivers(ocaml, ocamllex, ocamlyacc):
    path = Path('src', 'compiler', 'drivers')

    lib = ocaml.build_lib(path / 'flx_driver',
        Path.globall(
            path / 'flx_terminate.ml{,i}',
            path / 'flx_flxopt.ml{,i}'),
        libs=[
            build_flx_misc(ocaml),
            build_flx_core(ocaml),
            build_flxcclib(ocaml, ocamllex, ocamlyacc)])

    libs = [
        call('buildsystem.ocs.build_lib', ocaml),
        call('buildsystem.sex.build', ocaml, ocamllex),
        call('buildsystem.dypgen.build_lib', ocaml),
        call('buildsystem.cil.build', ocaml, ocamllex, ocamlyacc),
        build_flx_misc(ocaml),
        build_flx_core(ocaml),
        build_flx_version(ocaml),
        build_flx_version_hook(ocaml),
        build_flx_lex(ocaml, ocamllex),
        build_flx_parse(ocaml, ocamllex),
        build_flxcclib(ocaml, ocamllex, ocamlyacc),
        build_flx_desugar(ocaml, ocamllex, ocamlyacc),
        build_inria_re(ocaml),
        build_flx_bind(ocaml),
        build_flx_frontend(ocaml),
        build_flx_backend(ocaml, ocamllex, ocamlyacc),
        lib]

    external_libs = ['nums', 'unix', 'str']

    flxl = ocaml.build_exe(fbuild.buildroot / 'bin/flxl',
        [path / 'flxl.ml'], libs=libs, external_libs=external_libs)

    flxp = ocaml.build_exe(fbuild.buildroot / 'bin/flxp',
        [path / 'flxp.ml'], libs=libs, external_libs=external_libs)

    flxm = ocaml.build_exe(fbuild.buildroot / 'bin/flxm',
        [path / 'flxm.ml'], libs=libs, external_libs=external_libs)

    flxd = ocaml.build_exe(fbuild.buildroot / 'bin/flxd',
        [path / 'flxd.ml'], libs=libs, external_libs=external_libs)

    flxb = ocaml.build_exe(fbuild.buildroot / 'bin/flxb',
        [path / 'flxb.ml'], libs=libs, external_libs=external_libs)

    flxg = ocaml.build_exe(fbuild.buildroot / 'bin/flxg',
        [path / 'flxg.ml'], libs=libs, external_libs=external_libs)

    return Record(
        flxl=flxl,
        flxp=flxp,
        flxm=flxm,
        flxd=flxd,
        flxb=flxb,
        flxg=flxg,
    )
