import fbuild.builders.ocaml.ocamlfind as ocamlfind

# ------------------------------------------------------------------------------

class Ocamldep(ocamlfind.Ocamldep):
    def __init__(self, *args, ocamlfind_cmd='batteries/ocamldep', **kwargs):
        super().__init__(*args, ocamlfind_cmd=ocamlfind_cmd, **kwargs)

class Ocamlc(ocamlfind.Ocamlc):
    def __init__(self, *args,
            ocamlfind_cmd='batteries/ocamlc',
            make_ocamldep=Ocamldep,
            **kwargs):
        super().__init__(*args,
            ocamlfind_cmd=ocamlfind_cmd,
            make_ocamldep=make_ocamldep,
            **kwargs)

class Ocamlcp(ocamlfind.Ocamlcp):
    def __init__(self, *args,
            ocamlfind_cmd='batteries/ocamlcp',
            make_ocamldep=Ocamldep,
            **kwargs):
        super().__init__(*args,
            ocamlfind_cmd=ocamlfind_cmd,
            make_ocamldep=make_ocamldep,
            **kwargs)

class Ocamlopt(ocamlfind.Ocamlopt):
    def __init__(self, *args,
            ocamlfind_cmd='batteries/ocamlopt',
            make_ocamldep=Ocamldep,
            make_ocamlc=Ocamlc,
            **kwargs):
        super().__init__(*args,
            ocamlfind_cmd=ocamlfind_cmd,
            make_ocamldep=make_ocamldep,
            make_ocamlc=make_ocamlc,
            **kwargs)

class Ocaml(ocamlfind.Ocaml):
    def __init__(self, *args,
            make_ocamldep=Ocamldep,
            make_ocamlc=Ocamlc,
            make_ocamlcp=Ocamlcp,
            make_ocamlopt=Ocamlopt,
            **kwargs):
        super().__init__(*args,
            make_ocamldep=make_ocamldep,
            make_ocamlc=make_ocamlc,
            make_ocamlcp=make_ocamlcp,
            make_ocamlopt=make_ocamlopt,
            **kwargs)
