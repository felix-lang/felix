SOURCES = parse_tree.ml local_data_parser.dyp local_data.ml
REP = -I ../../dyplib
CAMLC = ocamlc -rectypes $(REP)
DYPGEN = ../../dypgen/dypgen --ocamlc "-I ../../dyplib -rectypes"
LIBS=dyp.cma

all: local_data

SOURCES2 = $(SOURCES:.dyp=.ml)
OBJS = $(SOURCES2:.ml=.cmo)

local_data: $(OBJS)
	$(CAMLC) -o local_data $(LIBS) $(OBJS)

.SUFFIXES: .ml .cmo .dyp

.ml.cmo:
	$(CAMLC) -c $<

.dyp.ml:
	$(DYPGEN) $<
	$(CAMLC) -c $*.mli

clean:
	rm -f *.cm[iox] *~ .*~ *.o
	rm -f local_data
	rm -f *.extract_type *_temp.ml
	rm -f *parser.ml *parser.mli
