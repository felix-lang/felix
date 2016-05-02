import fbuild.builders.ocaml.batteries
from fbuild.path import Path

def build(ctx):
    ocaml = fbuild.builders.ocaml.batteries.Ocaml(ctx)

    lib = ocaml.ocamlc.build_lib('lib', Path.glob('lib*.ml{,i}'),
        packages=['sexplib.syntax'])
    print('here', lib)
    exe = ocaml.ocamlc.build_exe('exe.byte', ['exe.ml'], libs=[lib])

    ctx.logger.log(' * running %s:' % exe)
    ctx.execute([exe])

    lib = ocaml.ocamlopt.build_lib('lib', Path.glob('lib*.ml{,i}'),
        packages=['sexplib.syntax'])
    exe = ocaml.ocamlopt.build_exe('exe.byte', ['exe.ml'], libs=[lib])

    ctx.logger.log(' * running %s:' % exe)
    ctx.execute([exe])

    # We can also build bytecode and native libraries at the same time.
    lib = ocaml.build_lib('lib', Path.glob('lib*.ml{,i}'),
        packages=['sexplib.syntax'])
    exe = ocaml.build_exe('exe', ['exe.ml'], libs=[lib]).bytecode
