import fbuild
import fbuild.db
from fbuild.functools import call
from fbuild.path import Path

# ------------------------------------------------------------------------------

class Builder(fbuild.db.PersistentObject):
    def __init__(self, exe):
        self.exe = exe

    @fbuild.db.cachemethod
    def __call__(self, src:fbuild.db.SRC, *,
            buildroot=fbuild.buildroot,
            flags=[]) -> fbuild.db.DSTS:
        # first, copy the src file into the buildroot
        src_buildroot = src.addroot(buildroot)
        dsts = (
            src_buildroot.replaceext('.ml'),
            src_buildroot.replaceext('.mli'),
        )

        if src != src_buildroot:
            src_buildroot.parent.makedirs()
            src.copy(src_buildroot)
            src = src_buildroot

        cmd = [self.exe]
        cmd.extend(flags)
        cmd.append(src)

        fbuild.execute(cmd, self.exe.name, '%s -> %s' % (src, ' '.join(dsts)),
            color='yellow')

        return dsts

# ------------------------------------------------------------------------------

def build_lib(ocaml):
    path = fbuild.buildroot/'src/compiler/dyp/dyplib'
    return ocaml.ocaml.build_lib(path/'dyp', Path.glob(path/'*.ml{,i}'))

def build_pgen(ocaml):
    path = fbuild.buildroot/'src/compiler/dyp/generators/pgen'
    exe = ocaml.ocaml.build_exe(path/'pgen', Path.globall(
            path/'*.ml{,i}',
            ocaml.ocamllex(path/'pgen_lexer.mll')),
        libs=[call(build_lib, ocaml)])

    return Builder(exe)

def build_dypgen(ocaml):
    path = fbuild.buildroot/'src/compiler/dyp/generators/dypgen'

    pgen = call(build_pgen, ocaml)
    exe = ocaml.ocaml.build_exe(path/'dypgen', Path.globall(
            path/'*.ml{,i}',
            ocaml.ocamllex(path/'dypgen_lexer.mll'),
            ocaml.ocamllex(path/'insert_linenum.mll'),
            pgen(path/'dypgen_parser.dyp')),
        libs=[call(build_lib, ocaml)])

    return Builder(exe)
