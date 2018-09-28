include ../../Makefile.config
SOURCES = argument.ml parse_tree.ml tinyML_parser.dyp tinyML.ml
REP = -I ../../dyplib
CAMLC = ocamlc -g -rectypes $(REP)
DYPGEN = ../../dypgen/dypgen --no-mli --pv-obj --merge-warning --command "ocamlc -I ../../dyplib -rectypes -i -impl tinyML_parser_temp.ml > tinyML_parser.extract_type"
LIBS=dyp.cma

all: tinyML

opt: tinyML.opt

prof: tinyML.p

SOURCES1 = $(SOURCES:.mll=.ml)
SOURCES2 = $(SOURCES1:.dyp=.ml)
OBJS = $(SOURCES2:.ml=.cmo)
OPTOBJS = $(SOURCES2:.ml=.cmx)
PROFOBJS = $(SOURCES2:.ml=.p.cmx)

tinyML: $(OBJS)
	$(CAMLC) -o tinyML $(LIBS) $(OBJS)

ifdef CAMLOPT
tinyML.opt: $(OPTOBJS)
	$(CAMLOPT) -rectypes $(REP) -o tinyML.opt $(LIBS:.cma=.cmxa) $(OPTOBJS)
tinyML.p: $(PROFOBJS)
	$(CAMLOPT) -rectypes -p $(REP) -o tinyML.p $(LIBS:.cma=.p.cmxa) $(PROFOBJS)
else
tinyML.opt:
tinyML.p:
endif

.SUFFIXES: .ml .mli .cmo .cmi .cmx .p.cmx .dyp

.ml.cmo:
	$(CAMLC) -c $<

.mli.cmi:
	$(CAMLC) -c $<

.ml.cmx:
	$(CAMLOPT) -rectypes $(REP) -c $<

.ml.p.cmx:
	$(CAMLOPT) -rectypes $(REP) -p -c -o $*.p.cmx $<

.dyp.ml:
	$(DYPGEN) $<
#	$(CAMLC) -c $*.mli

clean:
	rm -f *.cm[iox] *.p.cmx *~ .*~ *.o
	rm -f tinyML tinyML.opt tinyML.p
	rm -f *.extract_type *_temp.ml
	rm -f *parser.ml #*parser.mli
	rm -f list_syntax
