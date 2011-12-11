SOURCES = calc_parser.dyp calc_lexer.mll calc.ml
REP = -I ../../dyplib
CAMLC = ocamlc $(REP)
DYPGEN = ../../dypgen/dypgen --ocamlc "-I ../../dyplib"
LIBS=dyp.cma

all: calc

SOURCES1 = $(SOURCES:.mll=.ml)
SOURCES2 = $(SOURCES1:.dyp=.ml)
OBJS = $(SOURCES2:.ml=.cmo)

calc: $(OBJS)
	$(CAMLC) -o calc $(LIBS) $(OBJS)

.SUFFIXES: .ml .cmo .mll .dyp

.ml.cmo:
	$(CAMLC) -c $<

.mll.ml:
	ocamllex $<

.dyp.ml:
	$(DYPGEN) $<
	$(CAMLC) -c $*.mli

clean:
	rm -f *.cm[iox] *~ .*~ *.o
	rm -f calc
	rm -f *lexer.ml *.extract_type *_temp.ml
	rm -f *parser.ml *parser.mli
