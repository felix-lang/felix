include ../Makefile.config

REP=
CAMLC=ocamlc
OCAMLFIND=ocamlfind

all: dyp opt prof

dyp: dyp.cmi priority_by_relation.cmo automaton.cmo dyplex.cmo dyp.cmo
	$(CAMLC) -a -o dyp.cma priority_by_relation.cmo automaton.cmo dyplex.cmo dyp.cmo

ifdef CAMLOPT
opt: dyp.cmi priority_by_relation.cmx automaton.cmx dyplex.cmx dyp.cmx
	$(CAMLOPT) -a -o dyp.cmxa priority_by_relation.cmx automaton.cmx dyplex.cmx dyp.cmx
prof: dyp.cmi priority_by_relation.p.cmx automaton.p.cmx dyplex.p.cmx dyp.p.cmx
	$(CAMLOPT) -a -o dyp.p.cmxa priority_by_relation.p.cmx automaton.p.cmx dyplex.p.cmx dyp.p.cmx
else
opt:
prof:
endif

install: uninstall
	$(OCAMLFIND) install dyp META dyp.cma dyp.cmxa dyp.a dyp.cmi

uninstall:
	$(OCAMLFIND) remove dyp

install2: install_dyp install_opt
	cp dyp.cmi $(DYPGENLIBDIR)

install_dyp:
	mkdir -p $(DYPGENLIBDIR)
	cp dyp.cma $(DYPGENLIBDIR)

ifdef CAMLOPT
install_opt:
	mkdir -p $(DYPGENLIBDIR)
	cp dyp.cmxa $(DYPGENLIBDIR)
	cp dyp.a $(DYPGENLIBDIR)
else
install_opt:
endif

ifdef CAMLOPT
.SUFFIXES: .ml .mli .cmo .cmi .cmx .p.cmx
else
.SUFFIXES: .ml .mli .cmo .cmi
endif

.ml.cmo:
	$(CAMLC) $(REP) -c $<

.mli.cmi:
	$(CAMLC) $(REP) -c $<

ifdef CAMLOPT
.ml.cmx:
	$(CAMLOPT) $(REP) -c $<
.ml.p.cmx:
	$(CAMLOPT) $(REP) -p -c -o $*.p.cmx $<
endif

clean:
	rm -f *.cm[iox] *~ .*~ *.o *.a *.cma *.cmxa
