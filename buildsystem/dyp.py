import fbuild
import fbuild.packages
import fbuild.packages.ocaml as ocaml

# -----------------------------------------------------------------------------

class Builder:
    def __init__(self, exe):
        self.exe = exe

    def __call__(self, src, *, buildroot=fbuild.buildroot, flags=[]):
        # first, copy the src file into the buildroot
        src_buildroot = src.replace_root(buildroot)

        if src != src_buildroot:
            src_buildroot.parent.make_dirs()
            src.copy(src_buildroot)
            src = src_buildroot

        dst = src.replace_ext('.ml')
        cmd = [self.exe]
        cmd.extend(flags)
        cmd.append(src)

        fbuild.execute(cmd, self.exe.name, '%s -> %s' % (src, dst),
            color='yellow')

        return (dst, dst + 'i')

# -----------------------------------------------------------------------------

def build_dyplib(env, builder=None):
    path = fbuild.buildroot/'src/compiler/dyp/dyplib'
    return ocaml.Library(path/'dyp', [path/'*.ml{,i}'], builder=builder)

def build_pgen(env, builder=None):
    path = fbuild.buildroot/'src/compiler/dyp/generators/pgen'
    return ocaml.Executable(path/'pgen', [
            path/'*.ml{,i}',
            ocaml.Ocamllex(path/'pgen_lexer.mll'),
        ],
        libs=[env.config(build_dyplib, builder)],
        builder=builder)

def build_dypgen(env, builder=None):
    path = fbuild.buildroot/'src/compiler/dyp/generators/dypgen'
    return ocaml.Executable(path/'dypgen', [
            path/'*.ml{,i}',
            ocaml.Ocamllex(path/'dypgen_lexer.mll'),
            ocaml.Ocamllex(path/'insert_linenum.mll'),
            Pgen(path/'dypgen_parser.dyp'),
        ],
        libs=[env.config(build_dyplib, builder)],
        builder=builder)

# -----------------------------------------------------------------------------

class Pgen(fbuild.packages.SimplePackage):
    default_config = 'buildsystem.dyp.build_pgen'

    def dependencies(self, env):
        return (self.target, self.config(env))

    def command(self, env, *args, **kwargs):
        return Builder(self.config(env).build(env))(*args, **kwargs)

class Dypgen(fbuild.packages.SimplePackage):
    default_config = 'buildsystem.dyp.build_dypgen'

    def dependencies(self, env):
        return (self.target, self.config(env))

    def command(self, env, *args, **kwargs):
        return Builder(self.config(env).build(env))(*args, **kwargs)
