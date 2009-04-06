import fbuild
import fbuild.db
from fbuild.path import Path

# ------------------------------------------------------------------------------

class Builder(fbuild.db.PersistentObject):
    def __init__(self, exe):
        self.exe = exe

    @fbuild.db.cachemethod
    def __call__(self, src:fbuild.db.SRC, *,
            buildroot=fbuild.buildroot,
            flags=[]) -> fbuild.db.DST:
        # first, copy the src file into the buildroot
        src_buildroot = src.addroot(buildroot)
        dst = src_buildroot.replaceext('.ml')

        if src != src_buildroot:
            src_buildroot.parent.makedirs()
            src.copy(src_buildroot)
            src = src_buildroot

        cmd = [self.exe, '--no-mli']
        cmd.extend(flags)
        cmd.append(src)

        fbuild.execute(cmd, self.exe.name, '%s -> %s' % (src, dst),
            color='yellow')

        return dst

# ------------------------------------------------------------------------------

def build_lib(ocaml):
    path = Path('src/compiler/dypgen/dyplib')
    return ocaml.build_lib(path/'dyp', Path.glob(path/'*.ml{,i}'))

def build_exe(ocaml, ocamllex):
    path = Path('src/compiler/dypgen/dypgen')
    exe = ocaml.build_exe(path/'dypgen', Path.globall(
        path/'*.ml{,i}',
        ocamllex(path/'dypgen_lexer.mll'),
        ocamllex(path/'extract_type.mll'),
        ocamllex(path/'insert_linenum.mll')),
        libs=[build_lib(ocaml)])

    return Builder(exe)
