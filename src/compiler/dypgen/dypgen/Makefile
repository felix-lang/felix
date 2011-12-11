include ../Makefile.config
SOURCES = argument.ml extract_type.mll dypgen_parser.ml dypgen_lexer.mll insert_linenum.mll dypgen.ml
REP = -I ../dyplib
CAMLC = ocamlc $(REP)
DYPGEN = ./dypgen --ocamlc "-rectypes -I ../dyplib"
LIBS=dyp.cma

all: dypgen dypgen.opt

SOURCES1 = $(SOURCES:.mll=.ml)
SOURCES2 = $(SOURCES1:.dyp=.ml)
OBJS = $(SOURCES2:.ml=.cmo)
OPTOBJS = $(SOURCES2:.ml=.cmx)

dypgen: parse_tree.cmi dypgen_parser.cmi $(OBJS)
	$(CAMLC) -o dypgen $(LIBS) $(OBJS)

ifdef CAMLOPT
dypgen.opt: parse_tree.cmi dypgen_parser.cmi $(OPTOBJS)
	$(CAMLOPT) $(REP) -o dypgen.opt $(LIBS:.cma=.cmxa) $(OPTOBJS)
else
dypgen.opt:
endif

.SUFFIXES: .ml .mli .cmo .cmi .cmx .mll

.ml.cmo:
	$(CAMLC) -c $<

.mli.cmi:
	$(CAMLC) -c $<

.ml.cmx:
	$(CAMLOPT) $(REP) -c $<

.mll.ml:
	ocamllex $<

clean:
	rm -f *.cm[iox] *~ .*~ *.o
	rm -f dypgen dypgen.opt sortie*
	rm -f *lexer.ml dypgen_parser.extract_type dypgen_parser_temp.ml

parser: parse_tree.cmi
	$(DYPGEN) dypgen_parser.dyp

clean_parser:
	rm -f dypgen_parser.ml dypgen_parser.mli
