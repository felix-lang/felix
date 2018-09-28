all:
	python ../../interscript/bin/iscr.py sex.pak
	ocamllex sex_lex.mll
	../../bin/dypgen sex_parse.dyp
	rm sex_parse.mli
	ocamlopt -c sex_types.ml
	ocamlopt -c sex_print.ml
	ocamlopt -c sex_map.ml
	ocamlopt -c -I ../../ocs ocs2sex.ml
	ocamlopt -c -I ../../dypgen/dyplib sex_parse.ml
	ocamlopt -c -I ../../dypgen/dyplib sex_lex.ml
	ocamlopt -a -o sexlib.cmxa sex_types.cmx sex_print.cmx sex_map.cmx ocs2sex.cmx sex_parse.cmx sex_lex.cmx
	ocamlopt -c -I ../../src sex.ml
	ocamlopt -o sex nums.cmxa ../../dypgen/dyplib/dyplib.cmxa ../../ocs/ocslib.cmxa ../../src/flxlib.cmxa sexlib.cmxa sex.cmx

clean:
	rm -f *.a *.o *.cmi *.cmx *.cmxa *.cache sex tmp.tmp

test:
	./sex test.sex
