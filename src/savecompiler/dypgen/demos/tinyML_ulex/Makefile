SOURCES = argument.ml parse_tree.ml tinyML_parser.dyp tinyML_lexer.ml tinyML.ml
REP = -I ../../dyplib
CAMLC = ocamlfind ocamlc -package ulex $(REP) -rectypes
CAMLOPT = ocamlfind ocamlopt -package ulex $(REP) -rectypes
DYPGEN = ../../dypgen/dypgen --ocamlc "-I ../../dyplib -rectypes"
LIBS=dyp.cma ulexing.cma

all: tinyML

opt: tinyML.opt

SOURCES1 = $(SOURCES:.dyp=.ml)
OBJS = $(SOURCES1:.ml=.cmo)
OPTOBJS = $(SOURCES1:.ml=.cmx)

tinyML: $(OBJS)
	$(CAMLC) -o tinyML $(LIBS) $(OBJS)

tinyML.opt: $(OPTOBJS)
	$(CAMLOPT) -o tinyML.opt $(LIBS:.cma=.cmxa) $(OPTOBJS)

.SUFFIXES: .ml .mli .cmo .cmi .cmx .dyp

.ml.cmo:
	$(CAMLC) -c $<

.ml.cmx:
	$(CAMLOPT) -c $<

.mli.cmi:
	$(CAMLC) -c $<

.dyp.ml:
	$(DYPGEN) $<
	$(CAMLC) -c $*.mli

clean:
	rm -f *.cm[iox] *~ .*~ *.o
	rm -f tinyML tinyML.opt *.extract_type *_temp.ml
	rm -f *parser.ml *parser.mli

tinyML_lexer.cmo:tinyML_parser.cmi
	$(CAMLC) -c -syntax camlp4o tinyML_lexer.ml

tinyML_lexer.cmx:tinyML_parser.cmi
	$(CAMLOPT) -c -syntax camlp4o tinyML_lexer.ml
