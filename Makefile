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

VERSION = 1.1.9dev
DISTDIR ?= ./build/dist
INSTALLDIR ?= /usr/local/lib/felix/felix-$(VERSION)
FBUILDROOT ?= build
BUILDROOT ?= ${FBUILDROOT}/release
DEBUGBUILDROOT ?= ${FBUILDROOT}/debug
PYTHON ?= python3
# If running as root skip sudo
ifeq ($(USER),root)
SUDO=
else
SUDO=sudo
endif

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
	#   FBUILDROOT: directory to build into, default build
	#   FBUILD_PARAMS: parameters to fbuild, default none
	#     fbuild/fbuild-light --help for options 

build: user-build

dev-build: fbuild gendoc

user-build: fbuild doc copy-src
  
#
# Core integrated build
#
fbuild:
	$(PYTHON) fbuild/fbuild-light build --buildroot=${FBUILDROOT} $(FBUILD_PARAMS)

#
# regression test on release image
# 
test:
	$(PYTHON) fbuild/fbuild-light test --buildroot=${FBUILDROOT} $(FBUILD_PARAMS)

#
#
# Install default build into /usr/local/lib/felix/version/
#

install:
	${SUDO} rm -rf /usr/local/lib/felix/felix-${VERSION}
	${SUDO} ${BUILDROOT}/host/bin/flx_cp ${BUILDROOT}/host '(.*)' '/usr/local/lib/felix/felix-${VERSION}/host/$${1}'
	${SUDO} ${BUILDROOT}/host/bin/flx_cp ${BUILDROOT}/share '(.*)' '/usr/local/lib/felix/felix-${VERSION}/share/$${1}'
	${SUDO} ${BUILDROOT}/host/bin/flx_cp ${BUILDROOT} '(VERSION)' '/usr/local/lib/felix/felix-${VERSION}/$${1}'
	${SUDO} ${BUILDROOT}/host/bin/flx_cp ${BUILDROOT}/host/bin '(flx)' '/usr/local/bin/$${1}'
	${BUILDROOT}/host/bin/flx_cp src/ '(.*\.(c|cxx|cpp|h|hpp|flx|flxh|fdoc|fpc|ml|mli))' 'usr/local/lib/felix/felix-${VERSION}/share/src/$${1}'
	${SUDO} rm -rf $(HOME)/.felix/cache
	${SUDO} rm -f /usr/local/lib/felix/felix-latest
	${SUDO} ln -s /usr/local/lib/felix/felix-$(VERSION) /usr/local/lib/felix/felix-latest
	echo 'println ("installed "+ Version::felix_version);' > install-done.flx
	flx --clean install-done
	rm install-done.*
	${SUDO} chown $(USER) $(HOME)/.felix

#
# Install binaries on felix-lang.org
# (felix-lang.org maintainer only)
#
install-felix-lang.org:
	-sudo stop felixweb
	make install
	sudo start felixweb

#
# Make distribution image for ArchLinux
# ArchLinux packager only
#
## FIXME: needs to conform to new layout
make-dist:
	rm -rf $(DISTDIR)
	./${BUILDROOT}/host/bin/flx --test=${BUILDROOT} --dist=$(DISTDIR)
	./${BUILDROOT}/host/bin/flx_cp ${BUILDROOT}/speed '(.*)' '${DISTDIR}/speed/$${1}'
	rm -rf $(HOME)/.felix/cache
	echo 'println ("installed "+ Version::felix_version);' > $(DISTDIR)/install-done.flx
	./${BUILDROOT}/host/bin/flx --clean --test=$(DISTDIR)/lib/felix/felix-$(VERSION) $(DISTDIR)/install-done.flx
	rm -f $(DISTDIR)/install-done.flx  $(DISTDIR)/install-done.so


install-website:
	${SUDO} cp -r ${BUILDROOT}/share/web/* /usr/local/lib/felix/felix-latest/share/web


#
# Helper for checking new syntax
# Grammar developers only
#
syntax:
	rm -f ${BUILDROOT}/share/lib/grammar/*
	cp src/lib/grammar/* ${BUILDROOT}/share/lib/grammar

#
# Speedway
# Developer only
#
speed:
	-rm -rf result.tmp
	sh speed/perf.sh 2>>result.tmp
	${BUILDROOT}/host/bin/flx_gengraph

upgrade-test:
	${BUILDROOT}/host/bin/flx_tangle --indir=src/test --outdir=test

gentest:
	rm -rf test
	mkdir test
	mkdir test/test-data
	cp src/test/test-data/* test/test-data
	${BUILDROOT}/host/bin/flx_tangle --indir=src/test --outdir=test
	git add test
#
# Documentation
#
doc: copy-doc 

# Copy docs from repo src to release image
copy-doc: 
	${BUILDROOT}/host/bin/flx_cp src/web '(.*\.fdoc)' '${BUILDROOT}/share/web/$${1}'
	${BUILDROOT}/host/bin/flx_cp src/web '(.*\.(png|jpg|gif))' '${BUILDROOT}/share/web/$${1}'
	${BUILDROOT}/host/bin/flx_cp src/web '(.*\.html)' '${BUILDROOT}/share/web/$${1}'
	${BUILDROOT}/host/bin/flx_cp src/ '(index\.html)' '${BUILDROOT}/share/$${1}'
	${BUILDROOT}/host/bin/flx_cp speed/ '(.*\.(c|ml|cc|flx|ada|hs|svg))' '${BUILDROOT}/share/speed/$${1}'
	${BUILDROOT}/host/bin/flx_cp speed/ '(.*/expect)' '${BUILDROOT}/share/speed/$${1}'
	
gendoc: gen-doc copy-doc check-tut

# upgrade tutorial indices in repo src
# must be done prior to copy-doc
# muut be done after primary build
# results should be committed to repo.
# Shouldn't be required on client build because the results
# should already have been committed to the repo.
gen-doc:
	${BUILDROOT}/host/bin/flx_mktutindex tut Tutorial tutorial.fdoc
	${BUILDROOT}/host/bin/flx_mktutindex fibres Fibres tutorial.fdoc
	${BUILDROOT}/host/bin/flx_mktutindex objects Objects tutorial.fdoc
	${BUILDROOT}/host/bin/flx_mktutindex polymorphism Polymorphism tutorial.fdoc
	${BUILDROOT}/host/bin/flx_mktutindex pattern Patterns tutorial.fdoc
	${BUILDROOT}/host/bin/flx_mktutindex literals Literals tutorial.fdoc
	${BUILDROOT}/host/bin/flx_mktutindex cbind "C Binding" tutorial.fdoc
	${BUILDROOT}/host/bin/flx_mktutindex streams Streams tutorial.fdoc
	${BUILDROOT}/host/bin/flx_mktutindex array "Arrays" tutorial.fdoc
	${BUILDROOT}/host/bin/flx_mktutindex garray "Generalised Arrays" tutorial.fdoc
	${BUILDROOT}/host/bin/flx_mktutindex uparse "Universal Parser" uparse.fdoc
	${BUILDROOT}/host/bin/flx_mktutindex nutut/intro/intro "Ground Up" ../../tutorial.fdoc
	# Build reference docs. Note this requires plugins and RTL to be installed
	# on (DY)LD_LIBRARY_PATH. Won't work otherwise.
	env LD_LIBRARY_PATH=${BUILDROOT}/host/lib/rtl ${BUILDROOT}/host/bin/flx_libcontents --html > src/web/flx_libcontents.html
	env LD_LIBRARY_PATH=${BUILDROOT}/host/lib/rtl ${BUILDROOT}/host/bin/flx_libindex --html > src/web/flx_libindex.html
	env LD_LIBRARY_PATH=${BUILDROOT}/host/lib/rtl ${BUILDROOT}/host/bin/flx_gramdoc --html > src/web/flx_gramdoc.html


# Checks correctness of tutorial in release image
# must be done after copy-doc
# must be done after primary build
check-tut:
	${BUILDROOT}/host/bin/flx_tangle --inoutdir=${BUILDROOT}/share/web/nutut/intro/ '.*'
	for  i in ${BUILDROOT}/web/nutut/intro/*.flx; \
	do \
		j=$$(echo $$i | sed s/.flx//); \
		echo $$j; \
		${BUILDROOT}/host/bin/flx --test=${BUILDROOT} --stdout=$$j.output $$j; \
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

${BUILDROOT}/host/bin/scoop: demos/scoop/bin/scoop.flx ${BUILDROOT}/lib/std/felix/pkgtool_base.flx ${BUILDROOT}/lib/std/felix/pkgtool.flx
	@${BUILDROOT}/host/bin/flx --inline=1 --test=${BUILDROOT} demos/scoop/setup build  --test=${BUILDROOT} --build-dir=demos/scoop 2> /dev/null
	@${BUILDROOT}/host/bin/flx --inline=1 --test=${BUILDROOT} demos/scoop/setup install  --test=${BUILDROOT} --build-dir=demos/scoop 2> /dev/null
	@${BUILDROOT}/host/bin/flx --inline=1 --test=${BUILDROOT} demos/scoop/setup clean  --test=${BUILDROOT} --build-dir=demos/scoop 2> /dev/null

scoop: ${BUILDROOT}/host/bin/scoop
	@echo "Scoop Package Manager"

install-scoop: ${BUILDROOT}/host/bin/scoop
	@echo "Installing scoop binary in /usr/local/bin"
	@${SUDO} cp ${BUILDROOT}/host/bin/scoop /usr/local/bin
	@${SUDO} ${BUILDROOT}/host/bin/flx_cp src/lib/std/felix '(pkgtool.*\.(flx))' '/usr/local/lib/felix/felix-latest/lib/std/felix/$${0}' --verbose

tarball:
	tar -vzcf felix_${VERSION}_`uname`_tarball.tar.gz Makefile  \
		${BUILDROOT}/index.html \
		${BUILDROOT}/VERSION \
		${BUILDROOT}/host \
		${BUILDROOT}/share

rtlbuild:
	# ##############################################
	# build a new Felix system: build/trial
	# using the existing one: build/release
	# ##############################################
	#  
	# ##############################################
	# clean out build target
	# ##############################################
	# 
	rm -rf build/trial
	rm -rf trial-tmp
	# 
	# ##############################################
	# create some essential directories
	# ##############################################
	# 
	mkdir build/trial
	mkdir build/trial/host
	mkdir build/trial/host/bin
	#
	# ##############################################
	# copy the configuration (later use a config program)
	# ##############################################
	#
	build/release/host/bin/flx_cp 'build/release/host/config' '(.*)' 'build/trial/host/config/$${1}'
	build/release/host/bin/flx_cp 'build/release/host/lib' '(.*\.(h|hpp|flx|flxh))' 'build/trial/host/lib/$${1}'
	#
	# ##############################################
	# copy the whole repository source
	# ##############################################
	#
	build/release/host/bin/flx_cp 'src' '(.*\.(h|hpp|ml|c|cpp|cxx|cc|flx|flxh|fdoc|js|html|svg|files))' 'build/trial/share/src/$${1}'
	#
	#
	# ##############################################
	# copy the Felix library
	# ##############################################
	#
	build/release/host/bin/flx_cp 'build/trial/share/src/lib' '(.*\.(flx|flxh|fdoc|files))' 'build/trial/share/lib/$${1}'
	build/release/host/bin/flx_cp 'build/release/share/lib/std' '(version.flx)' 'build/trial/share/lib/std/$${1}'
	#
	# ##############################################
	# Build the rtl
	# ##############################################
	#
	LD_LIBRARY_PATH=build/release/host/lib/rtl build/release/host/bin/flx --test=build/release --static -o build/trial/host/bin/flx_build_rtl_demo build/trial/share/src/tools/flx_build_rtl_demo build/trial/host/config build_flx_rtl_clang_osx build/trial . trial-tmp
	#
	# ##############################################
	# Copy the compiler
	# ##############################################
	#
	build/release/host/bin/flx_cp build/release/host/bin flxg build/trial/host/bin/flxg

trial-plugins:
	#
	# ##############################################
	# Build the plugins: dynamic library
	# ##############################################
	#
	build/release/host/bin/flx --test=build/trial -c -ox build/trial/host/lib/rtl/cpp2html build/trial/share/lib/plugins/cpp2html
	build/release/host/bin/flx --test=build/trial -c -ox build/trial/host/lib/rtl/fdoc2html build/trial/share/lib/plugins/fdoc2html
	build/release/host/bin/flx --test=build/trial -c -ox build/trial/host/lib/rtl/fdoc_button build/trial/share/lib/plugins/fdoc_button 
	build/release/host/bin/flx --test=build/trial -c -ox build/trial/host/lib/rtl/fdoc_fileseq build/trial/share/lib/plugins/fdoc_fileseq
	build/release/host/bin/flx --test=build/trial -c -ox build/trial/host/lib/rtl/fdoc_heading build/trial/share/lib/plugins/fdoc_heading
	build/release/host/bin/flx --test=build/trial -c -ox build/trial/host/lib/rtl/fdoc_paragraph build/trial/share/lib/plugins/fdoc_paragraph
	build/release/host/bin/flx --test=build/trial -c -ox build/trial/host/lib/rtl/fdoc_scanner build/trial/share/lib/plugins/fdoc_scanner
	build/release/host/bin/flx --test=build/trial -c -ox build/trial/host/lib/rtl/fdoc_slideshow build/trial/share/lib/plugins/fdoc_slideshow
	build/release/host/bin/flx --test=build/trial -c -ox build/trial/host/lib/rtl/flx2html build/trial/share/lib/plugins/flx2html
	build/release/host/bin/flx --test=build/trial -c -ox build/trial/host/lib/rtl/fpc2html build/trial/share/lib/plugins/fpc2html
	build/release/host/bin/flx --test=build/trial -c -ox build/trial/host/lib/rtl/ocaml2html build/trial/share/lib/plugins/ocaml2html
	build/release/host/bin/flx --test=build/trial -c -ox build/trial/host/lib/rtl/py2html build/trial/share/lib/plugins/py2html
	build/release/host/bin/flx --test=build/trial -c -ox build/trial/host/lib/rtl/toolchain_clang_linux build/trial/share/lib/plugins/toolchain_clang_linux
	build/release/host/bin/flx --test=build/trial -c -ox build/trial/host/lib/rtl/toolchain_clang_osx build/trial/share/lib/plugins/toolchain_clang_osx
	build/release/host/bin/flx --test=build/trial -c -ox build/trial/host/lib/rtl/toolchain_gcc_linux build/trial/share/lib/plugins/toolchain_gcc_linux
	build/release/host/bin/flx --test=build/trial -c -ox build/trial/host/lib/rtl/toolhcain_gcc_osx build/trial/share/lib/plugins/toolchain_gcc_osx
	#
	#
	# ##############################################
	# Build the plugins: dynamic object
	# ##############################################
	#
	build/release/host/bin/flx --test=build/trial -c --nolink -ox build/trial/host/lib/rtl/cpp2html build/trial/share/lib/plugins/cpp2html
	build/release/host/bin/flx --test=build/trial -c --nolink -ox build/trial/host/lib/rtl/fdoc2html build/trial/share/lib/plugins/fdoc2html
	build/release/host/bin/flx --test=build/trial -c --nolink -ox build/trial/host/lib/rtl/fdoc_button build/trial/share/lib/plugins/fdoc_button 
	build/release/host/bin/flx --test=build/trial -c --nolink -ox build/trial/host/lib/rtl/fdoc_fileseq build/trial/share/lib/plugins/fdoc_fileseq
	build/release/host/bin/flx --test=build/trial -c --nolink -ox build/trial/host/lib/rtl/fdoc_heading build/trial/share/lib/plugins/fdoc_heading
	build/release/host/bin/flx --test=build/trial -c --nolink -ox build/trial/host/lib/rtl/fdoc_paragraph build/trial/share/lib/plugins/fdoc_paragraph
	build/release/host/bin/flx --test=build/trial -c --nolink -ox build/trial/host/lib/rtl/fdoc_scanner build/trial/share/lib/plugins/fdoc_scanner
	build/release/host/bin/flx --test=build/trial -c --nolink -ox build/trial/host/lib/rtl/fdoc_slideshow build/trial/share/lib/plugins/fdoc_slideshow
	build/release/host/bin/flx --test=build/trial -c --nolink -ox build/trial/host/lib/rtl/flx2html build/trial/share/lib/plugins/flx2html
	build/release/host/bin/flx --test=build/trial -c --nolink -ox build/trial/host/lib/rtl/fpc2html build/trial/share/lib/plugins/fpc2html
	build/release/host/bin/flx --test=build/trial -c --nolink -ox build/trial/host/lib/rtl/ocaml2html build/trial/share/lib/plugins/ocaml2html
	build/release/host/bin/flx --test=build/trial -c --nolink -ox build/trial/host/lib/rtl/py2html build/trial/share/lib/plugins/py2html
	build/release/host/bin/flx --test=build/trial -c --nolink -ox build/trial/host/lib/rtl/toolchain_clang_linux build/trial/share/lib/plugins/toolchain_clang_linux
	build/release/host/bin/flx --test=build/trial -c --nolink -ox build/trial/host/lib/rtl/toolchain_clang_osx build/trial/share/lib/plugins/toolchain_clang_osx
	build/release/host/bin/flx --test=build/trial -c --nolink -ox build/trial/host/lib/rtl/toolchain_gcc_linux build/trial/share/lib/plugins/toolchain_gcc_linux
	build/release/host/bin/flx --test=build/trial -c --nolink -ox build/trial/host/lib/rtl/toolhcain_gcc_osx build/trial/share/lib/plugins/toolchain_gcc_osx
	#
	# ##############################################
	# Build the plugins: static object
	# ##############################################
	#
	build/release/host/bin/flx --test=build/trial --static -c --nolink -ox build/trial/host/lib/rtl/cpp2html build/trial/share/lib/plugins/cpp2html
	build/release/host/bin/flx --test=build/trial --static -c --nolink -ox build/trial/host/lib/rtl/fdoc2html build/trial/share/lib/plugins/fdoc2html
	build/release/host/bin/flx --test=build/trial --static -c --nolink -ox build/trial/host/lib/rtl/fdoc_button build/trial/share/lib/plugins/fdoc_button 
	build/release/host/bin/flx --test=build/trial --static -c --nolink -ox build/trial/host/lib/rtl/fdoc_fileseq build/trial/share/lib/plugins/fdoc_fileseq
	build/release/host/bin/flx --test=build/trial --static -c --nolink -ox build/trial/host/lib/rtl/fdoc_heading build/trial/share/lib/plugins/fdoc_heading
	build/release/host/bin/flx --test=build/trial --static -c --nolink -ox build/trial/host/lib/rtl/fdoc_paragraph build/trial/share/lib/plugins/fdoc_paragraph
	build/release/host/bin/flx --test=build/trial --static -c --nolink -ox build/trial/host/lib/rtl/fdoc_scanner build/trial/share/lib/plugins/fdoc_scanner
	build/release/host/bin/flx --test=build/trial --static -c --nolink -ox build/trial/host/lib/rtl/fdoc_slideshow build/trial/share/lib/plugins/fdoc_slideshow
	build/release/host/bin/flx --test=build/trial --static -c --nolink -ox build/trial/host/lib/rtl/flx2html build/trial/share/lib/plugins/flx2html
	build/release/host/bin/flx --test=build/trial --static -c --nolink -ox build/trial/host/lib/rtl/fpc2html build/trial/share/lib/plugins/fpc2html
	build/release/host/bin/flx --test=build/trial --static -c --nolink -ox build/trial/host/lib/rtl/ocaml2html build/trial/share/lib/plugins/ocaml2html
	build/release/host/bin/flx --test=build/trial --static -c --nolink -ox build/trial/host/lib/rtl/py2html build/trial/share/lib/plugins/py2html
	build/release/host/bin/flx --test=build/trial --static -c --nolink -ox build/trial/host/lib/rtl/toolchain_clang_linux build/trial/share/lib/plugins/toolchain_clang_linux
	build/release/host/bin/flx --test=build/trial --static -c --nolink -ox build/trial/host/lib/rtl/toolchain_clang_osx build/trial/share/lib/plugins/toolchain_clang_osx
	build/release/host/bin/flx --test=build/trial --static -c --nolink -ox build/trial/host/lib/rtl/toolchain_gcc_linux build/trial/share/lib/plugins/toolchain_gcc_linux
	build/release/host/bin/flx --test=build/trial --static -c --nolink -ox build/trial/host/lib/rtl/toolhcain_gcc_osx build/trial/share/lib/plugins/toolchain_gcc_osx



.PHONY : build32 build64 build test32 test64 test  
.PHONY : build32-debug build64-debug build-debug test32-debug test64-debug test-debug 
.PHONY : doc install websites-linux  release install-bin 
.PHONY : copy-doc gen-doc check-tut gendoc fbuild speed tarball
.PHONY : rtlbuild copy-src

