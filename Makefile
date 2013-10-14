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

user-build: fbuild
  
#
# Core integrated build
#
fbuild:
	$(PYTHON) fbuild/fbuild-light build --buildroot=${FBUILDROOT} $(FBUILD_PARAMS)

#
# regression test on release image
# 
test:
	build/release/host/bin/flx --test=build/release --expect --indir=test/regress/rt --regex='.*\.flx'

cleantest:
	build/release/host/bin/flx --clean
	build/release/host/bin/flx --test=build/release --expect --indir=test/regress/rt --regex='.*\.flx'

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
	${SUDO} cp -r src/web/* /usr/local/lib/felix/felix-latest/share/src/web


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
	# Build reference docs. Note this requires plugins.
	DYLD_LIBRARY_PATH=${BUILDROOT}/host/lib/rtl ${BUILDROOT}/host/bin/flx_libcontents --html > src/web/flx_libcontents.html
	DYLD_LIBRARY_PATH=${BUILDROOT}/host/lib/rtl ${BUILDROOT}/host/bin/flx_libindex --html > src/web/flx_libindex.html
	DYLD_LIBRARY_PATH=${BUILDROOT}/host/lib/rtl ${BUILDROOT}/host/bin/flx_gramdoc --html > src/web/flx_gramdoc.html


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
		${BUILDROOT}/host \
		${BUILDROOT}/share

post-tarball:
	scp felix_${VERSION}_`uname`_tarball.tar.gz \
		skaller@felix-lang.org:/usr/local/lib/felix/tarballs/felix_${VERSION}_`uname`_tarball.tar.gz

#--------------------------------------------------
# NEW BUILD ROUTINES
#--------------------------------------------------

#--------------------------------------------------
# FALLBACK BUILD: use already built binary versions
# of the tools, flx_build_flxg and flx_build_rtl
# located in build/release/host/bin
#
# Use these when you managed to clear the cache and
# the build screws up rebuilding the build tools.
#
# Normally only fallack-copy and fallback-flxg will
# be needed, due to screwing up the flxg compiler,
# and then needing it to build the tool that builds
# that compiler.
#
# There's an extra fallback for that: you can fallback
# to the *installed* tools as well.
#
# If NONE of that works you'll have to fallback all the
# way to the boostrap Python build (make build)
#--------------------------------------------------

recovery-fallback-flxg:
	# building flxg
	/usr/local/lib/felix/felix-latest/host/bin/flx_build_flxg
	cp tmp-dir/flxg build/release/host/bin

recovery-fallback-copy:
	# copying ./src to build/release/src
	/usr/local/lib/felix/felix-latest/host/bin/flx_build_rtl \
		--repo=.\
		--target-dir=build/release \
		--target-bin=host \
		--copy-repo 

fallback-flxg:
	# building flxg
	build/release/host/bin/flx_build_flxg
	cp tmp-dir/flxg build/release/host/bin

fallback-copy:
	# copying ./src to build/release/src
	build/release/host/bin/flx_build_rtl \
		--repo=.\
		--target-dir=build/release \
		--target-bin=host \
		--copy-repo 

fallback-rtl:
	# rebuild rtl
	build/release/host/bin/flx_build_rtl \
		--repo=.\
		--target-dir=build/release \
		--target-bin=host \
		--build-rtl

fallback-plugins:
	# rebuild plugins
	build/release/host/bin/flx_build_rtl \
		--target-dir=build/release \
		--target-bin=host \
		--build-plugins

fallback-tools:
	# rebuild tools
	build/release/host/bin/flx_build_rtl \
		--target-dir=build/release \
		--target-bin=host \
		--build-tools

fallback-flx:
	# rebuild flx
	build/release/host/bin/flx_build_rtl \
		--target-dir=build/release \
		--target-bin=host \
		--build-flx

fallback-lib: fallback-copy
	# copy files from src to lib
	build/release/host/bin/flx_build_rtl \
		--target-dir=build/release \
		--target-bin=host \
		--copy-library




flxg:
	# building flxg
	build/release/host/bin/flx --test=build/release src/tools/flx_build_flxg
	cp tmp-dir/flxg build/release/host/bin

copy:
	# copying ./src to build/release/src
	build/release/host/bin/flx --test=build/release  src/tools/flx_build_rtl \
		--repo=.\
		--target-dir=build/release \
		--target-bin=host \
		--copy-repo 

rtl:
	# rebuild rtl
	build/release/host/bin/flx --test=build/release  src/tools/flx_build_rtl \
		--repo=.\
		--target-dir=build/release \
		--target-bin=host \
		--build-rtl

plugins:
	# rebuild plugins
	build/release/host/bin/flx --test=build/release  src/tools/flx_build_rtl \
		--target-dir=build/release \
		--target-bin=host \
		--build-plugins

tools:
	# rebuild tools
	build/release/host/bin/flx --test=build/release  src/tools/flx_build_rtl \
		--target-dir=build/release \
		--target-bin=host \
		--build-tools

flx:
	# rebuild flx
	build/release/host/bin/flx --test=build/release  src/tools/flx_build_rtl \
		--target-dir=build/release \
		--target-bin=host \
		--build-flx

lib: copy
	# copy files from src to lib
	build/release/host/bin/flx --test=build/release  src/tools/flx_build_rtl \
		--target-dir=build/release \
		--target-bin=host \
		--copy-library




bootstrap:
	rm -rf tmp-dir
	build/release/host/bin/flx --test=build/release src/tools/flx_build_flxg
	rm -rf build/trial
	mkdir build/trial
	mkdir build/trial/host
	mkdir build/trial/host/bin
	cp tmp-dir/flxg build/trial/host/bin
	build/release/host/bin/flx --test=build/release  src/tools/flx_build_rtl \
		--repo=.\
		--target-dir=build/trial \
		--target-bin=host \
		--source-dir=build/release \
		--source-bin=host \
		--copy-repo \
		--copy-pkg-db \
		--copy-config-headers \
		--copy-version \
		--copy-library \
		--build-rtl \
		--build-plugins \
		--build-flx \
		--build-tools
	build/trial/host/bin/flx --test=build/trial --clean
	build/trial/host/bin/flx --test=build/trial --expect --indir=test/regress/rt --regex='.*\.flx'
	rm -rf build/release
	mv build/trial build/release

hosttools:
	# rebuild Felix code executables and plugins "in place"
	# switch from clang to gcc just so we can check it all works
	# This target should handle all changes to Felix code
	# in the repository.
	build/release/host/bin/flx --test=build/release  src/tools/flx_build_rtl \
		--repo=. \
		--target-dir=build/release \
		--target-bin=host \
		--source-dir=build/release \
		--source-bin=host \
		--pkg=build_flx_rtl_gcc_osx \
		--copy-repo \
		--copy-library \
		--build-plugins \
		--build-flx \
		--build-tools

gccosxtarget:
	# This Make target is used to make a Felix target 'gccosx'
	# in the build/release directory. It's a target for OSX
	# which uses gcc instead of the default clang used by 'host'
	#
	# Because this is a target OF build/release we can use the
	# existing share directory, so no need to copy the repo src
	# or the library. However we have to copy flxg.
	build/release/host/bin/flx --test=build/release  src/tools/flx_build_rtl \
		--repo=build/release/share \
		--target-dir=build/release \
		--target-bin=gccosx \
		--source-dir=build/release \
		--source-bin=host \
		--pkg=build_flx_rtl_gcc_osx \
		--clean-target-bin-dir \
		--copy-compiler \
		--copy-pkg-db \
		--copy-config-headers \
		--copy-version \
		--build-rtl \
		--build-plugins \
		--build-flx \
		--build-tools
	build/release/gccosx/bin/flx --test=build/release --target=gccosx --clean
	build/release/gccosx/bin/flx --test=build/release --target=gccosx  --expect --indir=test/regress/rt --regex='.*\.flx'

sdltest:
	build/release/host/bin/flx --test=build/release --force -c -od demos/sdl demos/sdl/edit_buffer
	build/release/host/bin/flx --test=build/release --force -c -od demos/sdl demos/sdl/edit_display
	build/release/host/bin/flx --test=build/release --force -c -od demos/sdl demos/sdl/edit_controller
	DYLD_LIBRARY_PATH=demos/sdl build/release/host/bin/flx --test=build/release --force -od demos/sdl demos/sdl/sdltest


weblink:
	build/release/host/bin/flx --test=build/release -c --nolink --static -ox build/release/host/lib/rtl/webserver src/tools/webserver.flx 
	build/release/host/bin/flx --test=build/release -c --static -ox build/release/host/bin/weblink \
		build/release/host/lib/rtl/fdoc_heading.o \
		build/release/host/lib/rtl/fdoc_button.o \
		build/release/host/lib/rtl/fdoc_fileseq.o \
		build/release/host/lib/rtl/fdoc_paragraph.o \
		build/release/host/lib/rtl/fdoc_scanner.o \
		build/release/host/lib/rtl/fdoc_slideshow.o \
		build/release/host/lib/rtl/fdoc2html.o \
		build/release/host/lib/rtl/flx2html.o \
		build/release/host/lib/rtl/py2html.o \
		build/release/host/lib/rtl/cpp2html.o \
		build/release/host/lib/rtl/ocaml2html.o \
		build/release/host/lib/rtl/fpc2html.o \
		build/release/host/lib/rtl/webserver.o \
		src/tools/weblink.flx


.PHONY : build32 build64 build test32 test64 test  
.PHONY : build32-debug build64-debug build-debug test32-debug test64-debug test-debug 
.PHONY : doc install websites-linux  release install-bin 
.PHONY : copy-doc gen-doc check-tut gendoc fbuild speed tarball
.PHONY : weblink flx tools plugins rtl copy lib
.PHONY : sdltest
