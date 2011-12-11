all: calc

calc: calc_parser.dyp expr.dyp calc.ml
	../../dypgen/dypgen --cpp-options "-w" --ocamlc "-I ../../dyplib" calc_parser.dyp
	ocamlc -c -I ../../dyplib calc_parser.mli
	ocamlc -c -I ../../dyplib calc_parser.ml
	ocamlc -c -I ../../dyplib calc.ml
	ocamlc -o calc -I ../../dyplib dyp.cma calc_parser.cmo calc.cmo

clean:
	rm -f *.cm[iox] *~ .*~ *.o *parser.ml *parser.mli *.cpp.dyp *_temp.ml *.extract_type calc
