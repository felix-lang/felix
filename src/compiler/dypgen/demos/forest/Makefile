SOURCES = forest_parser.dyp forest.ml
REP = -I ../../dyplib
CAMLC = ocamlc $(REP)
DYPGEN = ../../dypgen/dypgen --ocamlc "-I ../../dyplib"
LIBS=dyp.cma

all: forest

SOURCES1 = $(SOURCES:.mll=.ml)
SOURCES2 = $(SOURCES1:.dyp=.ml)
OBJS = $(SOURCES2:.ml=.cmo)

forest: parse_tree.cmi $(OBJS)
	$(CAMLC) -o forest $(LIBS) $(OBJS)

.SUFFIXES: .ml .mli .cmo .cmi .dyp

.ml.cmo:
	$(CAMLC) -c $<

.mli.cmi:
	$(CAMLC) -c $<

.dyp.ml:
	$(DYPGEN) $<
	$(CAMLC) -c $*.mli

clean:
	rm -f *.cm[iox] *~ .*~ *.o
	rm -f forest
	rm -f *.extract_type *_temp.ml
	rm -f *parser.ml *parser.mli
