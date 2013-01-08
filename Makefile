all: build test

# Build directory structure:
#
# ${BUILDROOT}: the default build location, optimised
# build/debug:   debug symbols, optimisation off
#
# build32, build64: so you can build "the other" word size version
#   if your platform can build it, to check code is portable
#

# 
# default build
#

VERSION = 1.1.7dev
DISTDIR ?= ./build/dist
BUILDROOT ?= build/release
DEBUGBUILDROOT ?= build/debug

help:
	# Makefile help
	# FELIX VERSION  ${VERSION}
	# DISTDIR  ${DISTDIR}
	# BUILDROOT  ${BUILDROOT}
	# 
	# Make Targets, USERS:
	#   build: primary build default release target ${BUILDROOT}
	#   test: run regression test suite
	#   install: install release to install point 
	#     default install point: /usr/local/lib/felix/felix-version
	#
	# Make Targets, DEVELOPERS:
	#   gendoc: generate docs (developers only)
	#
	# Params:
	#   BUILDROOT: directory to build into, default build/release
	#   FBUILD_PARAMS: parameters to fbuild, default none
	#     fbuild/fbuild-light --help for options 

build: user-build

dev-build: fbuild gendoc

user-build: fbuild doc
  
#
# Core integrated build
#
fbuild:
	python3 fbuild/fbuild-light build --buildroot=${BUILDROOT} $(FBUILD_PARAMS)

#
# regression test on release image
# 
test:
	python3 fbuild/fbuild-light test --buildroot=${BUILDROOT} $(FBUILD_PARAMS)


#
# debug build
#
fbuild-debug:
	python3 fbuild/fbuild-light -g build --buildroot=${DEBUGBUILDROOT} $(FBUILD_PARAMS)

test-debug:
	python3 fbuild/fbuild-light -g test --buildroot=${DEBUGBUILDROOT} $(FBUILD_PARAMS)

#
#
# Install default build into /usr/local/lib/felix/version/
#
install:
	sudo ${BUILDROOT}/bin/flx --test=${BUILDROOT} --install 
	sudo ${BUILDROOT}/bin/flx --test=${BUILDROOT} --install-bin
	sudo rm -rf $(HOME)/.felix/cache
	sudo rm -f /usr/local/lib/felix/felix-latest
	sudo ln -s /usr/local/lib/felix/felix-$(VERSION) /usr/local/lib/felix/felix-latest
	echo 'println ("installed "+ Version::felix_version);' > install-done.flx
	flx install-done
	rm install-done.*
	sudo chown $(USER) $(HOME)/.felix
	flx_libcontents --html > tmp1.html
	flx_libindex --html > tmp2.html
	flx_gramdoc --html > tmp3.html
	sudo cp tmp1.html /usr/local/lib/felix/felix-latest/web/flx_libcontents.html
	sudo cp tmp2.html /usr/local/lib/felix/felix-latest/web/flx_libindex.html
	sudo cp tmp3.html /usr/local/lib/felix/felix-latest/web/flx_gramdoc.html
	rm tmp1.html tmp2.html tmp3.html


#
# Install binaries on felix-lang.org
# (felix-lang.org maintainer only)
#
install-felix-lang.org:
	-sudo stop felixweb
	sudo ${BUILDROOT}/bin/flx --test=${BUILDROOT} --install 
	sudo ${BUILDROOT}/bin/flx --test=${BUILDROOT} --install-bin
	sudo rm -rf $(HOME)/.felix/cache
	echo 'println ("installed "+ Version::felix_version);' > install-done.flx
	flx install-done
	rm install-done.*
	flx_libcontents --html > tmp1.html
	flx_libindex --html > tmp2.html
	flx_gramdoc --html > tmp3.html
	sudo cp tmp1.html /usr/local/lib/felix/felix-latest/web/flx_libcontents.html
	sudo cp tmp2.html /usr/local/lib/felix/felix-latest/web/flx_libindex.html
	sudo cp tmp3.html /usr/local/lib/felix/felix-latest/web/flx_gramdoc.html
	rm tmp1.html tmp2.html tmp3.html
	sudo start felixweb

#
# Finalise a release??
# (Felix release manager only)
#
release:
	git tag v`flx --version`
	git commit v`flx --version`
	git push
	fbuild/fbuild-light configure build doc dist
	sudo ${BUILDROOT}/bin/flx --test=${BUILDROOT} --install
	sudo ${BUILDROOT}/bin/flx --test=${BUILDROOT} --install-bin
	echo "Restart webservers now"
	echo "Upgrade buildsystem/version.py now and rebuild"


#
# Make distribution image for ArchLinux
# ArchLinux packager only
#
make-dist:
	rm -rf $(DISTDIR)
	./${BUILDROOT}/bin/flx --test=${BUILDROOT} --dist=$(DISTDIR)
	rm -rf $(HOME)/.felix/cache
	echo 'println ("installed "+ Version::felix_version);' > $(DISTDIR)/install-done.flx
	./${BUILDROOT}/bin/flx --test=$(DISTDIR)/lib/felix/felix-$(VERSION) $(DISTDIR)/install-done.flx
	echo "export LD_LIBRARY_PATH=$(DISTDIR)/lib:$(DISTDIR)/lib/felix/felix-$(VERSION)/lib/rtl">$(DISTDIR)/build-idx.sh
	echo "$(DISTDIR)/bin/flx_libcontents --html > $(DISTDIR)/tmp1.html">>$(DISTDIR)/build-idx.sh
	echo "$(DISTDIR)/bin/flx_libindex --html > $(DISTDIR)/tmp2.html">>$(DISTDIR)/build-idx.sh
	echo "$(DISTDIR)/bin/flx_gramdoc --html > $(DISTDIR)/tmp3.html">>$(DISTDIR)/build-idx.sh
	sh $(DISTDIR)/build-idx.sh
	cp $(DISTDIR)/tmp1.html $(DISTDIR)/lib/felix/felix-$(VERSION)/web/flx_libcontents.html
	cp $(DISTDIR)/tmp2.html $(DISTDIR)/lib/felix/felix-$(VERSION)/web/flx_libindex.html
	cp $(DISTDIR)/tmp3.html $(DISTDIR)/lib/felix/felix-$(VERSION)/web/flx_gramdoc.html
	rm -f $(DISTDIR)/tmp1.html $(DISTDIR)/tmp2.html $(DISTDIR)/tmp3.html $(DISTDIR)/build-idx.sh $(DISTDIR)/install-done.flx  $(DISTDIR)/install-done.so


# 
# Quick install plugins
# Plugin developers only
#
install-plugins:
	sudo cp ${BUILDROOT}/shlib/* /usr/local/lib/

install-website:
	sudo cp -r ${BUILDROOT}/web/* /usr/local/lib/felix/felix-latest/web


#
# Helper for checking new syntax
# Grammar developers only
#
syntax:
	rm -f ${BUILDROOT}/lib/grammar/*
	cp src/lib/grammar/* ${BUILDROOT}/lib/grammar
	rm *.par2

#
# Documentation
#
doc: copy-doc 

# Copy docs from repo src to release image
copy-doc: 
	${BUILDROOT}/bin/flx_cp src/web '(.*\.fdoc)' '${BUILDROOT}/web/$${1}'
	${BUILDROOT}/bin/flx_cp src/web '(.*\.(png|jpg|gif))' '${BUILDROOT}/web/$${1}'
	${BUILDROOT}/bin/flx_cp src/web '(.*\.html)' '${BUILDROOT}/web/$${1}'
	${BUILDROOT}/bin/flx_cp src/ '(.*\.html)' '${BUILDROOT}/$${1}'

gendoc: gen-doc copy-doc check-tut

# upgrade tutorial indices in repo src
# must be done prior to copy-doc
# muut be done after primary build
# results should be committed to repo.
# Shouldn't be required on client build because the results
# should already have been committed to the repo.
gen-doc:
	${BUILDROOT}/bin/mktutindex tut Tutorial tutorial.fdoc
	${BUILDROOT}/bin/mktutindex fibres Fibres tutorial.fdoc
	${BUILDROOT}/bin/mktutindex objects Objects tutorial.fdoc
	${BUILDROOT}/bin/mktutindex polymorphism Polymorphism tutorial.fdoc
	${BUILDROOT}/bin/mktutindex pattern Patterns tutorial.fdoc
	${BUILDROOT}/bin/mktutindex literals Literals tutorial.fdoc
	${BUILDROOT}/bin/mktutindex cbind "C Binding" tutorial.fdoc
	${BUILDROOT}/bin/mktutindex streams Streams tutorial.fdoc
	${BUILDROOT}/bin/mktutindex array "Arrays" tutorial.fdoc
	${BUILDROOT}/bin/mktutindex garray "Generalised Arrays" tutorial.fdoc
	${BUILDROOT}/bin/mktutindex uparse "Universal Parser" uparse.fdoc
	${BUILDROOT}/bin/mktutindex nutut/intro/intro "Ground Up" ../../tutorial.fdoc

# Checks correctness of tutorial in release image
# must be done after copy-doc
# must be done after primary build
check-tut:
	${BUILDROOT}/bin/flx_tangle --inoutdir=${BUILDROOT}/web/nutut/intro/ '.*'
	for  i in ${BUILDROOT}/web/nutut/intro/*.flx; \
	do \
		j=$$(echo $$i | sed s/.flx//); \
		echo $$j; \
		${BUILDROOT}/bin/flx --test=${BUILDROOT} --stdout=$$j.output $$j; \
		diff -N $$j.expect $$j.output; \
	done

# optional build of compiler docs
# targets repository
# Don't run by default because ocamldoc is a bit buggy
ocamldoc:
	mkdir -p parsedoc
	ocamldoc -d parsedoc -html \
		-I ${BUILDROOT}/src/compiler/flx_version \
		-I ${BUILDROOT}/src/compiler/ocs/src \
		-I ${BUILDROOT}/src/compiler/dypgen/dyplib \
		-I ${BUILDROOT}/src/compiler/sex \
		-I ${BUILDROOT}/src/compiler/flx_lex \
		-I ${BUILDROOT}/src/compiler/flx_parse \
		-I ${BUILDROOT}/src/compiler/flx_parse \
		-I ${BUILDROOT}/src/compiler/flx_misc \
		-I ${BUILDROOT}/src/compiler/flx_file \
		src/compiler/flx_version/*.mli \
		src/compiler/flx_version/*.ml \
		src/compiler/sex/*.mli \
		src/compiler/sex/*.ml \
		src/compiler/flx_lex/*.mli \
		src/compiler/flx_lex/*.ml \
		src/compiler/flx_parse/*.ml \
		src/compiler/flx_parse/*.mli \
		src/compiler/flx_file/*.mli \
		src/compiler/flx_file/*.ml \
		src/compiler/flx_misc/*.mli \
		src/compiler/flx_misc/*.ml 


.PHONY : build32 build64 build test32 test64 test  
.PHONY : build32-debug build64-debug build-debug test32-debug test64-debug test-debug 
.PHONY : doc install websites-linux  release install-bin 
.PHONY : copy-doc gen-doc check-tut gendoc fbuild

