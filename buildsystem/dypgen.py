import fbuild
import fbuild.db
from fbuild.path import Path

# ------------------------------------------------------------------------------

class Builder(fbuild.db.PersistentObject):
    def __init__(self, ctx, exe):
        super().__init__(ctx)

        self.exe = exe

    @fbuild.db.cachemethod
    def __call__(self, src:fbuild.db.SRC, *,
            buildroot=None,
            flags=[]) -> fbuild.db.DST:
        buildroot = buildroot or self.ctx.buildroot

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

        self.ctx.execute(cmd, self.exe.name, '%s -> %s' % (src, dst),
            color='yellow')

        return dst

# ------------------------------------------------------------------------------

def build_lib(phase):
    path = Path('src/compiler/dypgen/dyplib')
    return phase.ocaml.build_lib(path/'dyp', Path.glob(path/'*.ml{,i}'))

def build_exe(phase):
    path = Path('src/compiler/dypgen/dypgen')
    exe = phase.ocaml.build_exe(path/'dypgen', Path.globall(
        path/'*.ml{,i}',
        phase.ocamllex(path/'dypgen_lexer.mll'),
        phase.ocamllex(path/'extract_type.mll'),
        phase.ocamllex(path/'insert_linenum.mll')),
        libs=[build_lib(phase)])

    return Builder(phase.ctx, exe)
