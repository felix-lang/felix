import os
import shutil

import config
from fbuild.flxbuild.process import Process
from fbuild.flxbuild.flxutil import unix2native

def copy_mli2ml(pkg, pkgdict, *args):
    for f in pkgdict.get('caml_raw_interfaces', []):
        f = unix2native(f)
        src = f+'.mli'
        dst = f+'.ml'

        print 'copying file', src, '->', dst
        shutil.copyfile(src, dst)


def build_grammar(pkg, pkgdict, *args):
    LEXS = pkgdict.get("caml_lexes", [])
    PARSES = pkgdict.get("caml_parses", [])
    MODULES = pkgdict.get("caml_modules", [])

    for module in MODULES:
        mll = config.HOST_OCAML.find_in_src_dir(module + '.mll')
        if os.path.exists(mll) or os.path.exists(os.path.join('build', mll)):
            LEXS.append(module)

        mly = config.HOST_OCAML.find_in_src_dir(module + '.mly')
        if os.path.exists(mly) or os.path.exists(os.path.join('build', mly)):
            PARSES.append(module)

    if not (LEXS or PARSES):
        return

    print "CAML BUILDING GRAMMAR", pkg

    if LEXS:
        config.HOST_OCAML.gen_lexer(LEXS,
            outdir='build',
        )

    if PARSES:
        config.HOST_OCAML.gen_parser(PARSES,
            outdir='build',
        )


class build_modules(Process):
    def runme(self, pkg, pkgdict, *args):
        MODULES = pkgdict.get("caml_modules", [])
        INTERFACES = pkgdict.get("caml_interfaces", [])
        IMPLEMENTATIONS = pkgdict.get("caml_implementations", [])
        PACKS = pkgdict.get("caml_pack", [])
        INCLUDES = pkgdict.get("caml_include_paths", [])

        if not (MODULES or INTERFACES or IMPLEMENTATIONS):
            return

        print "CAML COMPILING", pkg

        kwds = dict(
            outdir='build',
            include_paths=[os.path.join('build', i) for i in INCLUDES],
            packs=PACKS,
            debug=self.debug,
            profile='profile' in self.options,
            optimise='optimise_felix' in self.options,
        )

        for module in MODULES:
            mli = config.HOST_OCAML.find_in_src_dir(module + '.mli')
            if os.path.exists(mli) or os.path.exists(os.path.join('build', mli)):
                config.HOST_OCAML.compile_interface([module], **kwds)

            ml  = config.HOST_OCAML.find_in_src_dir(module + '.ml')
            if os.path.exists(ml) or os.path.exists(os.path.join('build', ml)):
                config.HOST_OCAML.compile_module([module],
                    bytecode='bytecode' in self.options,
                    **kwds)

        config.HOST_OCAML.compile_interface(INTERFACES, **kwds)

        config.HOST_OCAML.compile_module(IMPLEMENTATIONS,
            bytecode='bytecode' in self.options,
            **kwds)


class build_libs(Process):
    def runme(self, pkg, pkgdict, *args):
        IMPLEMENTATIONS = [os.path.join('build', f)
            for f in pkgdict.get("caml_implementations", [])]
        lib = pkgdict.get('caml_provide_lib', os.path.join('src', pkg + 'lib'))

        MODULES = [os.path.join('build', f)
            for f in pkgdict.get("caml_modules", [])]

        if 'bytecode' in self.options or not config.HOST_OCAML.options.NATIVE_CODE_COMPILER:
            MODULES = [f for f in MODULES if os.path.exists(f + '.cmo')]
        else:
            MODULES = [f for f in MODULES if os.path.exists(f + '.cmx')]

        if not MODULES + IMPLEMENTATIONS:
            return

        print "CAML CREATING LIBRARY", lib
        config.HOST_OCAML.link_lib(MODULES + IMPLEMENTATIONS, lib,
            bytecode='bytecode' in self.options,
            outdir='build',
        )


class build_exes(Process):
    def runme(self, pkg, pkgdict, *args):
        EXES = pkgdict.get("caml_exes", [])
        OLIBRARIES = pkgdict.get("caml_require_libs", [])
        INCLUDES = pkgdict.get("caml_include_paths", [])

        if not EXES:
            return

        print "CAML LINKING EXECUTABLES"

        kwds = dict(
            outdir='build',
            bytecode='bytecode' in self.options,
            include_paths=[os.path.join('build', i) for i in INCLUDES],
        )

        output_exes = []
        for exe in EXES:
            config.HOST_OCAML.compile_module([exe], **kwds)

            src = config.HOST_OCAML.link_exe(
                [os.path.join('build', exe)],
                os.path.splitext(exe)[0] + config.HOST_OCAML.options.EXT_EXE,
                libs=OLIBRARIES,
                **kwds)

            dst = os.path.join('bin', os.path.basename(src))

            if not self.quiet: print 'copying file', src, '->', dst
            shutil.copy(src, dst)
            output_exes.append(dst)
