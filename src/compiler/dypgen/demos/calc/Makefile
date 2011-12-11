SOURCES = calc_parser.dyp calc.ml
REP = -I ../../dyplib
CAMLC = ocamlc -g $(REP)
DYPGEN = ../../dypgen/dypgen --pv-obj --ocamlc "-I ../../dyplib"
LIBS=dyp.cma

all: calc

SOURCES1 = $(SOURCES:.mll=.ml)
SOURCES2 = $(SOURCES1:.dyp=.ml)
OBJS = $(SOURCES2:.ml=.cmo)

calc: $(OBJS)
	$(CAMLC) -o calc $(LIBS) $(OBJS)

.SUFFIXES: .ml .cmo .dyp

.ml.cmo:
	$(CAMLC) -c $<

.dyp.ml:
	$(DYPGEN) $<
	$(CAMLC) -c $*.mli

clean:
	rm -f *.cm[iox] *~ .*~ *.o
	rm -f calc
	rm -f *.extract_type *_temp.ml
	rm -f *parser.ml *parser.mli
