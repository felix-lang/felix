SOURCES = parse_tree.ml merge_tp_parser.dyp merge_tp.ml
REP = -I ../../dyplib
CAMLC = ocamlc $(REP)
DYPGEN = ../../dypgen/dypgen --ocamlc "-I ../../dyplib"
LIBS=dyp.cma

all: merge_tp

SOURCES2 = $(SOURCES:.dyp=.ml)
OBJS = $(SOURCES2:.ml=.cmo)

merge_tp: $(OBJS)
	$(CAMLC) -o merge_tp $(LIBS) $(OBJS)

.SUFFIXES: .ml .cmo .dyp

.ml.cmo:
	$(CAMLC) -c $<

.dyp.ml:
	$(DYPGEN) $<
	$(CAMLC) -c $*.mli

clean:
	rm -f *.cm[iox] *~ .*~ *.o
	rm -f merge_tp
	rm -f *.extract_type *_temp.ml
	rm -f *parser.ml *parser.mli
