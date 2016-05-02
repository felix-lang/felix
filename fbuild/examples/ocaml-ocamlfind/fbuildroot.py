import fbuild.builders.ocaml.ocamlfind
from fbuild.path import Path

def build(ctx):
    ocaml = fbuild.builders.ocaml.ocamlfind.Ocaml(ctx,
        packages=['unix'])

    libb = ocaml.ocamlc.build_lib('libb', Path.glob('b*.ml{,i}'),
        packages=['num'])
    liba = ocaml.ocamlc.build_lib('liba', Path.glob('a*.ml{,i}'), libs=[libb])
    exe  = ocaml.ocamlc.build_exe('exe.byte', ['exe.ml'],
        libs=[libb, liba],
        packages=['num'])

    ctx.logger.log(' * running %s:' % exe)
    ctx.execute([exe])

    libb = ocaml.ocamlopt.build_lib('libb', Path.glob('b*.ml{,i}'),
        packages=['num'])
    liba = ocaml.ocamlopt.build_lib('liba', Path.glob('a*.ml{,i}'), libs=[libb])
    exe  = ocaml.ocamlopt.build_exe('exe.native', ['exe.ml'],
        libs=[libb, liba],
        packages=['num'])

    ctx.logger.log(' * running %s:' % exe)
    ctx.execute([exe])

    # We can also build bytecode and native libraries at the same time.
    libb = ocaml.build_lib('libb', Path.glob('b*.ml{,i}'),
        packages=['num'])
    liba = ocaml.build_lib('liba', Path.glob('a*.ml{,i}'), libs=[libb])
    exe  = ocaml.build_exe('exe', ['exe.ml'],
        libs=[libb, liba],
        packages=['num']).bytecode
