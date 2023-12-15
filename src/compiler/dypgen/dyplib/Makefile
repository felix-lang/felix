include ../Makefile.config

REP=
CAMLC=ocamlc
OCAMLFIND=ocamlfind

all: dyp opt 

dyp: dyp.cmi priority_by_relation.cmo automaton.cmo dyplex.cmo dyp.cmo
	$(CAMLC) -a -o dyp.cma priority_by_relation.cmo automaton.cmo dyplex.cmo dyp.cmo

ifdef CAMLOPT
opt: dyp.cmi priority_by_relation.cmx automaton.cmx dyplex.cmx dyp.cmx
	$(CAMLOPT) -a -o dyp.cmxa priority_by_relation.cmx automaton.cmx dyplex.cmx dyp.cmx
else
opt:
endif

install: uninstall
	install -d $(DYPGENLIBDIR)
	$(OCAMLFIND) install -destdir $(DYPGENLIBDIR) dyp META dyp.cma dyp.cmxa dyp.a dyp.cmi
	chmod a-x $(DYPGENLIBDIR)/dyp/META #why is this needed ?

uninstall:
	- $(OCAMLFIND) remove -destdir $(DYPGENLIBDIR) dyp

install2: install_dyp install_opt
	install -D --mode=644 dyp.cmi $(DYPGENLIBDIR)/dyp

install_dyp:
	install -D --mode=644 dyp.cma $(DYPGENLIBDIR)/dyp

ifdef CAMLOPT
install_opt:
	install -D --mode=644 dyp.cmxa $(DYPGENLIBDIR)/dyp
	install -D --mode=644 dyp.a $(DYPGENLIBDIR)/dyp
else
install_opt:
endif

ifdef CAMLOPT
.SUFFIXES: .ml .mli .cmo .cmi .cmx 
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
endif

clean:
	rm -f *.cm[iox] *~ .*~ *.o *.a *.cma *.cmxa
