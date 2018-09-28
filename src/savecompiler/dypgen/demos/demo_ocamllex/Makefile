SOURCES = parse_tree.ml parser.dyp lexer.mll demo.ml
REP = -I ../../dyplib
CAMLC = ocamlc $(REP)
DYPGEN = ../../dypgen/dypgen --no-undef-nt --ocamlc "-I ../../dyplib"
LIBS=dyp.cma

all: tinyML

SOURCES1 = $(SOURCES:.mll=.ml)
SOURCES2 = $(SOURCES1:.dyp=.ml)
OBJS = $(SOURCES2:.ml=.cmo)

tinyML: $(OBJS)
	$(CAMLC) -o demo $(LIBS) $(OBJS)

.SUFFIXES: .ml .mli .cmo .cmi .mll .dyp

.ml.cmo:
	$(CAMLC) -c $<

.mli.cmi:
	$(CAMLC) -c $<

.mll.ml:
	ocamllex $<

.dyp.ml:
	$(DYPGEN) $<
	$(CAMLC) -c $*.mli

clean:
	rm -f *.cm[iox] *~ .*~ *.o
	rm -f demo
	rm -f *lexer.ml *.extract_type *_temp.ml
	rm -f *parser.ml *parser.mli
