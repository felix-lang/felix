DIRS=calc calc_cpp calc_ext calc_nested_rule calc_ocamllex calc_pattern demo demo_ocamllex forest global_data local_data local_data_early_action merge_times_plus position position_ocamllex position_token_list sharp tinyML

include Makefile.config

dyp:
	cd dyplib; $(MAKE)
	cd dypgen; $(MAKE)

all:
	$(MAKE) dypgen
	$(MAKE) examples

examples:
	for i in $(DIRS); do \
		(cd demos/$$i; $(MAKE) all); \
	done

clean_examples:
	for i in $(DIRS) tinyML_ulex; do \
		(cd demos/$$i; $(MAKE) clean); \
	done

#install with ocaml-findlib
install: install_opt
	install -D --mode=755 dypgen/dypgen $(BINDIR)
	install -D --mode=755 dyp2gram.pl $(BINDIR)/dyp2gram
	install -D --mode=644 doc/dypgen.1 $(MANDIR)/dypgen.1
	install -D --mode=644 doc/dypgen.1 $(MANDIR)/dypgen.opt.1
	install -D --mode=644 doc/dypgen.1 $(MANDIR)/dyp2gram.1
	cd dyplib; $(MAKE) install

#install without ocaml-findlib
install2: install_opt
	install -D --mode=755 dypgen/dypgen $(BINDIR)
	install -D --mode=755 dyp2gram.pl $(BINDIR)/dyp2gram
	install -D --mode=644 doc/dypgen.1 $(MANDIR)/dypgen.1
	install -D --mode=644 doc/dypgen.1 $(MANDIR)/dypgen.opt.1
	install -D --mode=644 doc/dypgen.1 $(MANDIR)/dyp2gram.1
	cd dyplib; $(MAKE) install2

ifdef CAMLOPT
install_opt:
	mkdir -p $(BINDIR)
	cp dypgen/dypgen.opt $(BINDIR)
else
install_opt:
endif

uninstall:
	rm -rf $(BINDIR)/dypgen
	rm -rf $(BINDIR)/dypgen.opt
	rm -rf $(BINDIR)/dyp2gram.pl
	cd dyplib; $(MAKE) uninstall

uninstall2:
	rm -rf $(BINDIR)/dypgen
	rm -rf $(BINDIR)/dypgen.opt
	rm -rf $(BINDIR)/dyp2gram.pl
	cd $(DYPGENLIBDIR); rm -rf dyp.cmi dyp.cma dyp.cmxa dyp.a

clean:
	$(MAKE) clean_examples
	cd dyplib; $(MAKE) clean
	cd dypgen; $(MAKE) clean
	rm -f *~ dypgen-doc.log dypgen-doc.aux dypgen-doc.toc
