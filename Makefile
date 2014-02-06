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

VERSION = 1.1.10
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

# Choose one: Linux or OSX
# LPATH = LD_LIBRARY_PATH or, LPATH = DYLD_LIBRARY_PATH
platform := $(shell uname -s)
ifeq ($(platform), Linux)
	LPATH = LD_LIBRARY_PATH
else
	ifeq ($(platform), Darwin)
		LPATH = DYLD_LIBRARY_PATH
	endif
endif
ifndef LPATH
	$(error Unrecognized kernel name -- Unable to detect setting for LPATH)
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

build: user-build slow-flxg rebuild

dev-build: fbuild gendoc

user-build: fbuild
	cp ${BUILDROOT}/host/bin/bootflx ${BUILDROOT}/host/bin/flx
	${BUILDROOT}/host/bin/flx --test=${BUILDROOT} -c -od ${BUILDROOT}/host/lib/rtl src/lib/plugins/flx_plugin

#
# Core integrated build
#
fbuild:
	#
	# ============================================================
	#
	# BOOTSTRAPPING FELIX
	#
	#   See build/release/fbuild.log for full transcript
	#
	# ============================================================
	#
	$(PYTHON) fbuild/fbuild-light build --buildroot=${FBUILDROOT} $(FBUILD_PARAMS)

#
# regression test on release image
#
test:
	mkdir -p test
	${BUILDROOT}/host/bin/flx_tangle --indir=${BUILDROOT}/share/src/test --outdir=test
	${BUILDROOT}/host/bin/flx --test=${BUILDROOT} --usage=prototype --expect --indir=test/regress/rt --regex='.*\.flx'

tut-check:
	mkdir -p web
	${BUILDROOT}/host/bin/flx_tangle --indir=src/web --outdir=web
	${BUILDROOT}/host/bin/flx --test=${BUILDROOT} --usage=prototype --expect --input --indir=web --regex='.*\.flx'


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
	${SUDO} ${BUILDROOT}/host/bin/flx_cp src/ '(.*\.(c|cxx|cpp|h|hpp|flx|flxh|fdoc|fpc|ml|mli))' '/usr/local/lib/felix/felix-${VERSION}/share/src/$${1}'
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
	flx src/tools/flx_gengraph

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

gen-doc:
	${BUILDROOT}/host/bin/flx_mktutindex src/web/tut tutorial.fdoc
	# Build reference docs. Note this requires plugins.
	${LPATH}=${BUILDROOT}/host/lib/rtl ${BUILDROOT}/host/bin/flx_libcontents --html > src/web/ref/flx_libcontents.html
	${LPATH}=${BUILDROOT}/host/lib/rtl ${BUILDROOT}/host/bin/flx_libindex --html > src/web/ref/flx_libindex.html
	${LPATH}=${BUILDROOT}/host/lib/rtl ${BUILDROOT}/host/bin/flx_gramdoc --html > src/web/ref/flx_gramdoc.html


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
		${BUILDROOT}/share \
		src

post-tarball:
	scp felix_${VERSION}_`uname`_tarball.tar.gz \
		skaller@felix-lang.org:/usr/local/lib/felix/tarballs/felix_${VERSION}_`uname`_tarball.tar.gz

#--------------------------------------------------
# NEW BUILD ROUTINES
#--------------------------------------------------

#
slow-flxg:
	# =========================================================
	# building flxg
	# =========================================================
	build/release/host/bin/flx --test=build/release src/tools/flx_build_flxg
	cp tmp-dir/flxg build/release/host/bin

flxg:
	# =========================================================
	# building flxg
	# =========================================================
	build/release/host/bin/flx_build_flxg
	cp tmp-dir/flxg build/release/host/bin

copy:
	# =========================================================
	# copying ./src to build/release/share/src
	# =========================================================
	build/release/host/bin/flx_build_prep \
		--repo=.\
		--target-dir=build/release \
		--target-bin=host \
		--copy-repo

rtl:
	# =========================================================
	# rebuild rtl
	# =========================================================
	${LPATH}=${BUILDROOT}/host/lib/rtl build/release/host/bin/flx_build_rtl \
		--target-dir=build/release \
		--target-bin=host

web-plugins:
	# =========================================================
	# rebuild web plugins
	# =========================================================
	build/release/host/bin/flx --test=build/release  src/tools/flx_build_boot \
		--target-dir=build/release \
		--target-bin=host \
		--build-web-plugins

toolchain-plugins:
	# =========================================================
	# rebuild toolchain plugins
	# =========================================================
	build/release/host/bin/flx --test=build/release  src/tools/flx_build_boot \
		--target-dir=build/release \
		--target-bin=host \
		--build-toolchain-plugins

tools:
	# =========================================================
	# rebuild tools
	# =========================================================
	build/release/host/bin/flx --test=build/release  src/tools/flx_build_boot \
		--target-dir=build/release \
		--target-bin=host \
		--build-tools

flx:
	# =========================================================
	# rebuild flx
	# =========================================================
	build/release/host/bin/flx --test=build/release  src/tools/flx_build_boot \
		--target-dir=build/release \
		--target-bin=host \
		--build-flx

build-tools:
	# =========================================================
	# rebuild flx build tools
	# =========================================================
	build/release/host/bin/flx --test=build/release  src/tools/flx_build_boot \
		--target-dir=build/release \
		--target-bin=host \
		--build-flx-tools

lib: copy
	# =========================================================
	# copy files from src to lib
	# =========================================================
	build/release/host/bin/flx_build_prep \
		--target-dir=build/release \
		--target-bin=host \
		--copy-library

really-fast-rebuild:
	# =========================================================
	# rebuild everything from installed Felix except compiler
	# [Note: requires LPATH variable be set in Makefile!]
	# [Note: requires Felix be installed already!]
	# =========================================================
	${LPATH}=${INSTALLDIR}/host/lib/rtl ${INSTALLDIR}/host/bin/flx_build_prep \
		--repo=.\
		--source-dir=build/release \
		--source-bin=host \
		--target-dir=build/release \
		--target-bin=host \
		--copy-repo \
		--copy-library
	${LPATH}=${INSTALLDIR}/host/lib/rtl ${INSTALLDIR}/host/bin/flx_build_rtl \
		--target-dir=build/release \
		--target-bin=host
	${LPATH}=${INSTALLDIR}/host/lib/rtl ${INSTALLDIR}/host/bin/flx_build_boot \
		--target-dir=build/release \
		--target-bin=host \
		--build-all

fast-rebuild:
	# =========================================================
	# rebuild everything in-place except the compiler
	# [Note: requires LPATH variable be set in Makefile!]
	# =========================================================
	${LPATH}=build/release/host/lib/rtl build/release/host/bin/flx_build_prep \
		--repo=.\
		--source-dir=build/release \
		--source-bin=host \
		--target-dir=build/release \
		--target-bin=host \
		--copy-repo \
		--copy-library
	${LPATH}=build/release/host/lib/rtl build/release/host/bin/flx_build_rtl \
		--target-dir=build/release \
		--target-bin=host
	cp build/release/host/bin/flx_build_boot flx_build_boot
	${LPATH}=build/release/host/lib/rtl ./flx_build_boot \
		--target-dir=build/release \
		--target-bin=host \
		--build-all
	rm flx_build_boot

fast-rebuild-nortl:
	# =========================================================
	# rebuild everything in-place except the compiler and RTL
	# [Note: requires LPATH variable be set in Makefile!]
	# =========================================================
	${LPATH}=build/release/host/lib/rtl build/release/host/bin/flx_build_prep \
		--repo=.\
		--source-dir=build/release \
		--source-bin=host \
		--target-dir=build/release \
		--target-bin=host \
		--copy-repo \
		--copy-library
	${LPATH}=build/release/host/lib/rtl ./flx_build_boot \
		--target-dir=build/release \
		--target-bin=host \
		--build-all
	rm flx_build_boot


rebuild:
	# =========================================================
	# rebuild everything in-place except the compiler
	# [Note: Slow and messy. Requires "flx" be built in build/release]
	# [Builds build tools from repository using flx]
	# =========================================================
	build/release/host/bin/flx --test=build/release  src/tools/flx_build_prep \
		--repo=.\
		--source-dir=build/release \
		--source-bin=host \
		--target-dir=build/release \
		--target-bin=host \
		--copy-repo \
		--copy-library
	build/release/host/bin/flx --test=build/release  src/tools/flx_build_rtl \
		--target-dir=build/release \
		--target-bin=host
	cp build/release/host/bin/flx flx
	./flx --test=build/release  src/tools/flx_build_boot \
		--target-dir=build/release \
		--target-bin=host \
		--build-all
	rm flx

bootstrap:
	# =========================================================
	# Clean rebuild into separate directory build/trial
	# followed by overwrite of build/release if all tests pass.
	# [VERY SLOW!]
	# [Requires Ocaml]
	# [Requires flx already build in build/release]
	# =========================================================
	rm -rf tmp-dir trial-tmp build/trial
	build/release/host/bin/flx --test=build/release --clean
	build/release/host/bin/flx --test=build/release src/tools/flx_build_flxg
	mkdir build/trial
	mkdir build/trial/host
	mkdir build/trial/host/bin
	cp tmp-dir/flxg build/trial/host/bin
	build/release/host/bin/flx --test=build/release  src/tools/flx_build_prep \
		--repo=.\
		--target-dir=build/trial \
		--target-bin=host \
		--source-dir=build/release \
		--source-bin=host \
		--copy-repo \
		--copy-pkg-db \
		--copy-config-headers \
		--copy-version \
		--copy-library
	build/release/host/bin/flx --test=build/release  src/tools/flx_build_rtl \
		--target-dir=build/trial \
		--target-bin=host \
		--source-dir=build/release \
		--source-bin=host
	build/release/host/bin/flx --test=build/release  src/tools/flx_build_boot \
		--target-dir=build/trial \
		--target-bin=host \
		--source-dir=build/release \
		--source-bin=host \
		--build-all
	build/trial/host/bin/flx --test=build/trial --clean
	mkdir -p trial-test
	build/trial/host/bin/flx_tangle --indir=build/trial/share/src/test --outdir=trial-test
	build/trial/host/bin/flx --test=build/trial --usage=prototype --expect --indir=trial-test/regress/rt --regex='.*\.flx'
	rm -rf build/release
	mv build/trial build/release
	rm -rf trial-test

sdltest:
	build/release/host/bin/flx --test=build/release --force -c -od sdlbin demos/sdl/edit_buffer
	build/release/host/bin/flx --test=build/release --force -c -od sdlbin demos/sdl/edit_display
	build/release/host/bin/flx --test=build/release --force -c -od sdlbin demos/sdl/edit_controller
	${LPATH}=sdlbin build/release/host/bin/flx --test=build/release --force -od sdlbin demos/sdl/sdltest


.PHONY : build32 build64 build test32 test64 test
.PHONY : build32-debug build64-debug build-debug test32-debug test64-debug test-debug
.PHONY : doc install websites-linux  release install-bin
.PHONY : copy-doc gen-doc check-tut gendoc fbuild speed tarball
.PHONY : weblink flx tools web-plugins toolchain-plugins rtl copy lib
.PHONY : sdltest
