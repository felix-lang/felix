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
        dsts = (
            src_buildroot.replace_ext('.ml'),
            src_buildroot.replace_ext('.mli'),
        )

        for dst in dsts:
            if dst.is_dirty((src,)):
                break
        else:
            return dsts

        if src != src_buildroot:
            src_buildroot.parent.make_dirs()
            src.copy(src_buildroot)
            src = src_buildroot

        cmd = [self.exe]
        cmd.extend(flags)
        cmd.append(src)

        fbuild.execute(cmd, self.exe.name, '%s -> %s' % (src, ' '.join(dsts)),
            color='yellow')

        return dsts

# -----------------------------------------------------------------------------

def build_dyplib(builder=None):
    path = fbuild.buildroot/'src/compiler/dyp/dyplib'
    return ocaml.Library(path/'dyp', [path/'*.ml{,i}'], builder=builder)

def build_pgen(builder=None):
    path = fbuild.buildroot/'src/compiler/dyp/generators/pgen'
    return fbuild.packages.BuilderWrapper(Builder,
        ocaml.Executable(path/'pgen',
            srcs=[
                path/'*.ml{,i}',
                ocaml.Ocamllex(path/'pgen_lexer.mll'),
            ],
            libs=[fbuild.env.run(build_dyplib, builder)],
            builder=builder,
        )
    )

def build_dypgen(builder=None):
    path = fbuild.buildroot/'src/compiler/dyp/generators/dypgen'
    return fbuild.packages.BuilderWrapper(Builder,
        ocaml.Executable(
            dst=path/'dypgen',
            srcs=[
                path/'*.ml{,i}',
                ocaml.Ocamllex(path/'dypgen_lexer.mll'),
                ocaml.Ocamllex(path/'insert_linenum.mll'),
                Pgen(path/'dypgen_parser.dyp'),
            ],
            libs=[fbuild.env.run(build_dyplib, builder)],
            builder=builder,
        )
    )

# -----------------------------------------------------------------------------

class Pgen(fbuild.packages.SimplePackage):
    default_config = 'buildsystem.dyp.build_pgen'

    def dependencies(self):
        return (self.target, self.config)

    def command(self, *args, **kwargs):
        return self.config.build()(*args, **kwargs)

class Dypgen(fbuild.packages.SimplePackage):
    default_config = 'buildsystem.dyp.build_dypgen'

    def dependencies(self):
        return (self.target, self.config)

    def command(self, *args, **kwargs):
        return self.config.build()(*args, **kwargs)
