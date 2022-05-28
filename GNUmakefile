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

VERSION := $(shell python3 showversion.py)
DISTDIR ?= ./build/dist
PREFIX ?= /usr/local
EXECPREFIX ?= ${PREFIX}/bin
INSTALLROOT ?= ${PREFIX}/lib/felix
INSTALLDIR ?= ${INSTALLROOT}/felix-$(VERSION)
FBUILDROOT ?= build
BUILDROOT ?= ${FBUILDROOT}/release
BUILDBIN = ${BUILDROOT}/host/bin
DEBUGBUILDROOT ?= ${FBUILDROOT}/debug
PYTHON ?= python3
# If running as root skip sudo
ifeq ($(USER),root)
SUDO=
else
SUDO=sudo
endif

ifeq ($(FLX_BUILD_TOOLCHAIN_FAMILY),gcc)
FBUILD_PARAMS = --build-cc=gcc --build-cxx=g++
endif

ifeq ($(FLX_BUILD_TOOLCHAIN_FAMILY),clang)
FBUILD_PARAMS = --build-cc=clang --build-cxx=clang++
endif


# Choose one: Linux or OSX
# LPATH = LD_LIBRARY_PATH or, LPATH = DYLD_LIBRARY_PATH
platform := $(shell uname -s)
ifeq ($(platform), Linux)
	LPATH = LD_LIBRARY_PATH
ifeq ($(FLX_BUILD_TOOLCHAIN_FAMILY),clang)
   TOOLCHAIN=toolchain_clang_linux
else
   TOOLCHAIN=toolchain_gcc_linux
endif
else
ifeq ($(platform), Darwin)
		LPATH = DYLD_LIBRARY_PATH
ifeq ($(FLX_BUILD_TOOLCHAIN_FAMILY),gcc)
   TOOLCHAIN=toolchain_gcc_macosx
else
   TOOLCHAIN=toolchain_clang_macosx
endif
endif
endif
ifndef LPATH
$(warning Unrecognized kernel name -- Unable to detect setting for LPATH)
  LPATH = LD_LIBRARY_PATH
  TOOLCHAIN=toolchain_gcc_linux
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

showversion:
	echo Felix Version ${VERSION}

build: extract showversion configure bootstrap bootstrap-tools rebuild uproot

bootstrap: fbuild
	cp ${BUILDBIN}/bootflx ${BUILDBIN}/flx
	${BUILDROOT}/host/bin/flx --felix=build.fpc -c -od ${BUILDROOT}/host/lib/rtl ${BUILDROOT}/share/lib/plugins/flx_plugin

clean:
	rm -rf build trial
	git clean -fd
  
#
# Core integrated build
#

configure: extract
	#
	# ============================================================
	#
	# CONFIGURING FELIX
	#
	#   See ${BUILDROOT}/fbuild.log for full transcript
	#
	# ============================================================
	#
	$(PYTHON) fbuild/fbuild-light configure --buildroot=${FBUILDROOT} $(FBUILD_PARAMS)

do-fbuild:
	#
	# ============================================================
	#
	# BOOTSTRAPPING FELIX
	#
	#   See ${BUILDROOT}/fbuild.log for full transcript
	#
	# ============================================================
	#
	$(PYTHON) fbuild/fbuild-light build --buildroot=${FBUILDROOT} $(FBUILD_PARAMS)

clean-fbuild:
	rm -rf ${BUILDROOT}/exe*
	rm -rf ${BUILDROOT}/temp*
	rm -rf ${BUILDROOT}/obj*
	rm -rf ${BUILDROOT}/lib*
	rm -rf ${BUILDROOT}/__init__.py
	rm -rf ${BUILDROOT}/tut
	rm -rf ${BUILDROOT}/tutopt
	rm -rf ${BUILDROOT}/pyconfig
	rm -rf ${BUILDROOT}/cache

fbuild: do-fbuild clean-fbuild

packages:
	# =========================================================
	# generates source code from "src/packages" to various places
	# around the repo
	# =========================================================
	python3 src/tools/flx_iscr.py -q -d "src/packages" ${BUILDROOT}

extras:
	for file in extras/*.fdoc; do python3 src/tools/flx_iscr.py $$file ${BUILDROOT}; done


grammar:
	src/tools/flx_find_grammar_files.py ${BUILDROOT}

extract: packages grammar

fextract:
	flx_iscr ${PWD}/src/packages "--regex=.*\\.fdoc" build/release
	flx_find_grammar_files


bootstrap-tools:
	# =========================================================
	# build tools required for rebuild.
	# This is to be done right after building the bootstrap.
	# The tools are placed in the host.
	# =========================================================
	${BUILDBIN}/flx --felix=build.fpc --static -c -od ${BUILDBIN} src/tools/flx_build_flxg.flx
	${BUILDBIN}/flx --felix=build.fpc --static -c -od ${BUILDBIN} src/tools/flx_build_prep.flx
	${BUILDBIN}/flx --felix=build.fpc --static -c -od ${BUILDBIN} src/tools/flx_build_rtl.flx
	${BUILDBIN}/flx --felix=build.fpc --static -c -od ${BUILDBIN} src/tools/flx_build_boot.flx

flxg:
	# =========================================================
	# building flxg
	# =========================================================
	${BUILDBIN}/flx_build_flxg
	cp build/flxg-tmp/flxg ${BUILDBIN}

gcc_macosx:
	# =========================================================
	# prepare for gcc build on macosx
	# =========================================================
	${BUILDBIN}/flx_build_prep --target-dir=${BUILDROOT}		\
		--target-bin=gcc_macosx --source-dir=${BUILDROOT}	\
		--source-bin=host --clean-target-bin-dir --copy-compiler\
	       	--configure --compiler=gcc --os=macosx --bits=64	\
		--cxx-compiler=g++ --debug

	# =========================================================
	# build gcc rtl on macosx
	# =========================================================
	${LPATH}=${BUILDROOT}/host/lib/rtl flx_build_rtl --target-dir=${BUILDROOT} --target-bin=gcc_macosx

	# =========================================================
	# build Felix with gcc rtl on macosx
	# =========================================================
	${LPATH}=${BUILDROOT}/host/lib/rtl flx_build_boot --target-dir=${BUILDROOT} --target-bin=gcc_macosx --build-all


prep: 
	# =========================================================
	# prepare for host build
	# =========================================================
	${BUILDBIN}/flx_build_prep --target-dir=${BUILDROOT}		      \
		--target-bin=trial --source-dir=${BUILDROOT} --source-bin=host\
		--clean-target-bin-dir --copy-compiler --copy-pkg-db	      \
	       	--copy-config-headers --toolchain=${TOOLCHAIN} --debug

rtlbase:
	# =========================================================
	# rebuild rtl
	# =========================================================
	${LPATH}=${BUILDROOT}/host/lib/rtl ${BUILDBIN}/flx_build_rtl --target-dir=${BUILDROOT} --target-bin=trial


rtl: extract prep rtlbase

target: prep flxg rtlbase boot

boot:
	# =========================================================
	# rebuild flx build tools and plugins
	# =========================================================
	${LPATH}=${BUILDROOT}/host/lib/rtl ${BUILDBIN}/flx_build_boot --target-dir=${BUILDROOT} --target-bin=trial --build-all

rebuild: extract target uproot

uproot:
	rm -rf ${BUILDROOT}/host
	mv ${BUILDROOT}/trial ${BUILDROOT}/host

src:
	rm -rf ${BUILDROOT}/share/src
	mkdir -p ${BUILDROOT}/share/src
	cp -r src ${BUILDROOT}/share
	rm -rf ${BUILDROOT}/share/test
	mkdir -p ${BUILDROOT}/share/test
	cp -r src/test ${BUILDROOT}/share/test
	rm -rf ${BUILDROOT}/test

iphonesimulator:
	# prepare directory
	${BUILDBIN}/flx_build_prep --target-dir=${BUILDROOT} --target-bin=iphonesimulator --source-dir=${BUILDROOT} \
	       	--source-bin=host --clean-target-bin-dir --copy-compiler --copy-pkg-db \
	       	--copy-config-headers --toolchain=toolchain_iphonesimulator --debug
	rm -rf build/rtl-tmp
	# build rtl
	DYLD_LIBRARY_PATH=${BUILDROOT}/host/lib/rtl ${BUILDBIN}/flx_build_rtl \
		--target-dir=${BUILDROOT} --target-bin=iphonesimulator --static --noexes


iphoneos:
	# prepare directory
	${BUILDBIN}/flx_build_prep --target-dir=${BUILDROOT} --target-bin=iphoneos --source-dir=${BUILDROOT} \
	       	--source-bin=host --clean-target-bin-dir --copy-compiler --copy-pkg-db \
	       	--copy-config-headers --toolchain=toolchain_iphoneos --debug
	rm -rf build/rtl-tmp
	# build rtl
	DYLD_LIBRARY_PATH=${BUILDROOT}/host/lib/rtl ${BUILDBIN}/flx_build_rtl \
		--target-dir=${BUILDROOT} --target-bin=iphoneos --static --noexes


#
# regression test on release image
#
test-dir:
	mkdir -p ${BUILDROOT}/test
	${BUILDBIN}/flx_tangle --indir=${BUILDROOT}/share/src/test --outdir=${BUILDROOT}/test
	for file in src/test/regress/rt/*.fdoc; do ${BUILDBIN}/flx_iscr $$file ${BUILDROOT}/test; done

unit-dir:
	mkdir -p ${BUILDROOT}/test/unit/projection
	${BUILDBIN}/flx_tangle --indir=${BUILDROOT}/share/src/test/unit/projection \
      	      --outdir=${BUILDROOT}/test/unit/projection

perf-dir:
	mkdir -p ${BUILDROOT}/test/perf
	${BUILDBIN}/flx_tangle --indir=${BUILDROOT}/share/src/test/perf \
      	      --outdir=${BUILDROOT}/test/perf

tutopt-dir:
	mkdir -p ${BUILDROOT}/tutopt
	${BUILDBIN}/flx_tangle --indir=${BUILDROOT}/share/src/web/tutopt --outdir=${BUILDROOT}/test/tutopt
	for file in src/web/tutopt/*.fdoc; do [ -e "$file" ] || break; ${BUILDBIN}/flx_iscr $$file ${BUILDROOT}/test/tutopt; done

tut-dir:
	mkdir -p ${BUILDROOT}/tut
	${BUILDBIN}/flx_tangle --linenos --indir=src/web/tut --outdir=${BUILDROOT}/test/tut
	for file in src/web/tut/*.fdoc; do ${BUILDBIN}/flx_iscr $$file ${BUILDROOT}/test/tut; done

extras-check:
	-${BUILDBIN}/flx --felix=build.fpc --usage=prototype --expect --nonstop --indir=${BUILDROOT}/test/extras --regex='.*\.flx' ${BUILDROOT}/test
 
regress-check: test-dir
	# ============================================================
	#
	# RUNNING REGRESSION TESTS
	#
	# ============================================================
	-${BUILDBIN}/flx --felix=build.fpc --usage=prototype --expect --nonstop --indir=${BUILDROOT}/test/regress/rt --regex='.*\.flx' ${BUILDROOT}/test

unit-check: unit-dir
	# ============================================================
	#
	# RUNNING UNIT TESTS
	#
	# ============================================================
	-${BUILDBIN}/flx --felix=build.fpc --usage=prototype --expect --input \
     --nonstop --indir=${BUILDROOT}/test/unit/projection --regex='.*\.flx' ${BUILDROOT}/test/unit/projection

perf-check: perf-dir
	# ============================================================
	#
	# RUNNING PERFORMANCE TESTS
	#
	# ============================================================
	-${BUILDBIN}/flx --felix=build.fpc --usage=prototype --expect --input \
     --nonstop --indir=${BUILDROOT}/test/perf --regex='.*\.flx' ${BUILDROOT}/test/perf


tut-check: tut-dir
	# ============================================================
	#
	# CHECKING CORRECTNESS OF TUTORIAL EXAMPLES
	#
	# ============================================================
	-${BUILDBIN}/flx --felix=build.fpc --usage=prototype --expect --input --nonstop --indir=${BUILDROOT}/test/tut --regex='.*\.flx' ${BUILDROOT}/test/tut

tutopt-check: tutopt-dir
	#
	# ============================================================
	#
	# TESTING OPTIONAL COMPONENTS
	#
	# Tests are expected to fail if the relevant third party
	# support is not available or Felix is not properly configured
	# to use it.
	# ============================================================
	#
	-FLX_INSTALL_DIR=${BUILDROOT} ${BUILDBIN}/flx --felix=build.fpc \
		--usage=prototype --expect --input --nonstop \
		--indir=${BUILDROOT}/test/tutopt --regex='.*\.flx' ${BUILDROOT}/test/tutopt


test: unit-check regress-check tut-check perf-check tutopt-check extras-check
fresh-test: src test

install:
	mkdir -p ${PREFIX}'/bin'
	mkdir -p ${PREFIX}'/host'
	mkdir -p ${PREFIX}'/lib'
	mkdir -p ${PREFIX}'/share'
	rm -rf ${INSTALLDIR}
	${BUILDBIN}/flx_cp ${BUILDROOT}/host '(.*)' ${INSTALLDIR}'/host/$${1}'
	${BUILDBIN}/flx_cp ${BUILDROOT}/share '(.*)' ${INSTALLDIR}'/share/$${1}'
	${BUILDBIN}/flx_cp ${BUILDROOT} '(VERSION)' ${INSTALLDIR}'/$${1}'
	${BUILDBIN}/flx_cp ${BUILDBIN} '(flx)' ${EXECPREFIX}'/$${1}'
	${BUILDBIN}/flx_cp speed/ '(.*)' ${INSTALLDIR}'/speed/$${1}'

	rm -f ${INSTALLROOT}/felix-latest
	ln -s felix-${VERSION} ${INSTALLROOT}/felix-latest
	ln -sfn ${INSTALLROOT}/felix-latest/host/* ${PREFIX}/host/
	ln -sfn ${INSTALLROOT}/felix-latest/share/* ${PREFIX}/share/

macosxfpcs:
	build/release/host/bin/flx_build_prep --debug --target-dir=build/release --target-bin=host --configure --compiler=clang --os=macosx --bits=64

 
.PHONY : test extras bootstrap configure packages grammar
.PHONY : doc install websites-linux  release install-bin
.PHONY : copy-doc gen-doc gendoc fbuild speed tarball
.PHONY : weblink flx tools web-plugins toolchain-plugins rtl copy lib
.PHONY : sdltest src do-fbuild clean-fbuild uproot
