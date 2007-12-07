import os
import shutil

import config
from fbuild.flxbuild.process import Process
from fbuild.flxbuild.flxutil import unix2native

class copy_mli2ml(Process):
    def runme(self, pkg, pkgdict, *args):
        for f in pkgdict.get('caml_raw_interfaces', []):
            f = unix2native(f)
            src = f+'.mli'
            dst = f+'.ml'
            if not self.quiet: print 'copying file', src, '->', dst
            shutil.copyfile(src, dst)


class build_grammar(Process):
    def runme(self, pkg, pkgdict, *args):
        LEXS = pkgdict.get("caml_lexes", [])
        PARSES = pkgdict.get("caml_parses", [])

        if not (LEXS or PARSES):
            return

        print "CAML BUILDING GRAMMAR", pkg

        if LEXS:
            config.HOST_OCAML.gen_lexer(LEXS,
                    outdir='build')

        if PARSES:
            config.HOST_OCAML.gen_parser(PARSES,
                    outdir='build')


class build_modules(Process):
    def runme(self, pkg, pkgdict, *args):
        INTERFACES = pkgdict.get("caml_interfaces", [])
        IMPLEMENTATIONS = pkgdict.get("caml_implementations", [])
        PACKS = pkgdict.get("caml_pack", [])
        INCLUDES = pkgdict.get("caml_include_paths", [])

        if not (INTERFACES or IMPLEMENTATIONS):
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

        config.HOST_OCAML.compile_interface(INTERFACES, **kwds)

        config.HOST_OCAML.compile_module(IMPLEMENTATIONS,
            bytecode='bytecode' in self.options,
            **kwds)


class build_libs(Process):
    def runme(self, pkg, pkgdict, *args):
        IMPLEMENTATIONS = [os.path.join('build', f)
            for f in pkgdict.get("caml_implementations", [])]
        lib = pkgdict.get('caml_provide_lib', os.path.join('src', pkg + 'lib'))

        if not IMPLEMENTATIONS:
            return

        print "CAML CREATING LIBRARY", lib
        config.HOST_OCAML.link_lib(IMPLEMENTATIONS, lib,
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
