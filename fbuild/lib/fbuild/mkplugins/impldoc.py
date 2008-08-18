import os

from fbuild.flxbuild.process import Process
from fbuild.flxbuild.flxutil import mkdirs, ExecutionError, MissingFile

import config

class impldoc(Process):
    help = 'make the ocaml compiler documentation'

    def runme(self, pkg, pkgdict, *args):
        modules = pkgdict.get('caml_modules', [])
        interfaces = pkgdict.get('caml_interfaces', [])
        includes = pkgdict.get('caml_include_paths', [])

        if not (modules or interfaces):
            return

        print "GENERATING OCAMLDOC", pkg

        mlis = []

        for module in interfaces + modules:
            for extension in '.mli', '.ml':
                f = config.HOST_OCAML.find_in_src_dir(module + extension)
                if os.path.exists(f):
                    mlis.append(f)
                    break
                elif os.path.exists(os.path.join('build', f)):
                    mlis.append(os.path.join('build', f))
                    break
                else:
                    print 'ignoring:', module
            else:
                raise MissingFile(module)

        if not mlis:
            return

        try:
            config.HOST_OCAML.ocamldoc(mlis,
                outdir=os.path.join('doc', 'impldoc', pkg),
                include_paths=[os.path.join('build', i) for i in includes])
        except ExecutionError:
            pass # well ocamldoc is full of bugs ..
