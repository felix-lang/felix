
all:
	python ../../interscript/bin/iscr.py sex.pak
	ocamllex sex_lex.mll
	../../bin/dypgen sex_parse.dyp
	rm sex_parse.mli
	ocamlopt  -c sex_types.ml
	ocamlopt  -c sex_print.ml
	ocamlopt  -c sex_map.ml
	ocamlopt -I ../../dypgen/dyplib -c sex_parse.ml
	ocamlopt -I ../../dypgen/dyplib -c sex_lex.ml
	ocamlopt -a -o sex.cmxa sex_types.cmx sex_print.cmx sex_map.cmx sex_parse.cmx sex_lex.cmx 
	ocamlopt -c sex.ml
	ocamlopt -o sex ../../dypgen/dyplib/dyplib.cmxa sex.cmxa sex.cmx
clean:
	rm -f *.a *.o *.cmi *.cmx *.cmxa *.ml *.mli *.dyp *.mll *.cache sex tmp.tmp *.sex

test:
	./sex test.sex

