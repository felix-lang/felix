SOURCES = sharp_parser.dyp sharp.ml
REP = -I ../../dyplib
CAMLC = ocamlc -rectypes $(REP)
DYPGEN = ../../dypgen/dypgen --ocamlc "-I ../../dyplib -rectypes"
LIBS=dyp.cma

all: sharp

SOURCES1 = $(SOURCES:.mll=.ml)
SOURCES2 = $(SOURCES1:.dyp=.ml)
OBJS = $(SOURCES2:.ml=.cmo)

sharp: $(OBJS)
	$(CAMLC) -o sharp $(LIBS) $(OBJS)

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
	rm -f sharp
	rm -f *.extract_type *.extract_type *_temp.ml
	rm -f *parser.ml *parser.mli
