import fbuild

# ------------------------------------------------------------------------------

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

# ------------------------------------------------------------------------------

def build_lib(ocaml):
    path = fbuild.buildroot/'src/compiler/dyp/dyplib'
    return ocaml.builder.build_lib(path/'dyp', [path/'*.ml{,i}'])

def build_pgen(ocaml):
    path = fbuild.buildroot/'src/compiler/dyp/generators/pgen'
    exe = ocaml.builder.build_exe(path/'pgen', [
            path/'*.ml{,i}',
            ocaml.ocamllex(path/'pgen_lexer.mll'),
        ],
        libs=[fbuild.env.run(build_lib, ocaml)])

    return Builder(exe)

def build_dypgen(ocaml):
    path = fbuild.buildroot/'src/compiler/dyp/generators/dypgen'

    pgen = fbuild.env.run(build_pgen, ocaml)
    exe = ocaml.builder.build_exe(path/'dypgen', [
            path/'*.ml{,i}',
            ocaml.ocamllex(path/'dypgen_lexer.mll'),
            ocaml.ocamllex(path/'insert_linenum.mll'),
            pgen(path/'dypgen_parser.dyp'),
        ],
        libs=[fbuild.env.run(build_lib, ocaml)])

    return Builder(exe)
