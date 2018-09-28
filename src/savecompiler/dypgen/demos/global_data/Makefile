SOURCES = global_data_parser.dyp global_data.ml
REP = -I ../../dyplib
CAMLC = ocamlc $(REP)
DYPGEN = ../../dypgen/dypgen --ocamlc "-I ../../dyplib"
LIBS=dyp.cma

all: global_data

SOURCES1 = $(SOURCES:.mll=.ml)
SOURCES2 = $(SOURCES1:.dyp=.ml)
OBJS = $(SOURCES2:.ml=.cmo)

global_data: $(OBJS)
	$(CAMLC) -o global_data $(LIBS) $(OBJS)

.SUFFIXES: .ml .cmo .dyp

.ml.cmo:
	$(CAMLC) -c $<

.dyp.ml:
	$(DYPGEN) $<
	$(CAMLC) -c $*.mli

clean:
	rm -f *.cm[iox] *~ .*~ *.o
	rm -f global_data
	rm -f *.extract_type *.extract_type *_temp.ml
	rm -f *parser.ml *parser.mli
