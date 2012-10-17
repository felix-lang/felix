all: build test doc 

# Build directory structure:
#
# build/release: the default build location, optimised
# build/debug:   debug symbols, optimisation off
#
# build32, build64: so you can build "the other" word size version
#   if your platform can build it, to check code is portable
#

# 
# default build
#
build:
	python3 fbuild/fbuild-light build

build-clang:
	python3 fbuild/fbuild-light build --build-cc=clang --build-cxx=clang++

test:
	python3 fbuild/fbuild-light test

test-clang:
	python3 fbuild/fbuild-light build --build-cc=clang --build-cxx=clang++ test


#
# debug build
#
build-debug:
	python3 fbuild/fbuild-light -g build

test-debug:
	python3 fbuild/fbuild-light -g test

#
# 32 bit build
#
build32:
	python3 fbuild/fbuild-light --c-flag=-m32 --buildroot=build32 build

test32:
	python3 fbuild/fbuild-light --c-flag=-m32 --buildroot=build32 test

#
# 32 bit debug build
#
build32-debug:
	python3 fbuild/fbuild-light -g --c-flag=-m32 --buildroot=build32 build

test32-debug:
	python3 fbuild/fbuild-light -g --c-flag=-m32 --buildroot=build32 test

#
# 64 bit build
#
build64:
	python3 fbuild/fbuild-light --c-flag=-m64 --buildroot=build64 build

test64:
	python3 fbuild/fbuild-light --c-flag=-m64 --buildroot=build64 test

#
# 64 bit debug build
build64-debug:
	python3 fbuild/fbuild-light -g --c-flag=-m64 --buildroot=build64 build

test64-debug:
	python3 fbuild/fbuild-light -g --c-flag=-m64 --buildroot=build64 test

#
# Documentation
#
doc:
	python3 fbuild/fbuild-light doc

doc-clang:
	python3 fbuild/fbuild-light build --build-cc=clang --build-cxx=clang++ doc

#
# Install default build into /usr/local/lib/felix/version/
#
install:
	sudo build/release/bin/flx --test=build/release --install 
	sudo build/release/bin/flx --test=build/release --install-bin
	sudo rm -rf $(HOME)/.felix/cache
	echo 'println ("installed "+ Version::felix_version);' > install-done.flx
	flx install-done
	rm install-done.*
	sudo chown $(USER) $(HOME)/.felix
	flx_libcontents --html > tmp1.html
	flx_libindex --html > tmp2.html
	sudo cp tmp1.html /usr/local/lib/felix/felix-latest/web/flx_libcontents.html
	sudo cp tmp2.html /usr/local/lib/felix/felix-latest/web/flx_libindex.html
	rm tmp1.html tmp2.html
#
# Install binaries on felix-lang.org
#
install-felix-lang.org:
	sudo stop felixweb
	sudo build/release/bin/flx --test=build/release --install 
	sudo build/release/bin/flx --test=build/release --install-bin
	sudo rm -rf $(HOME)/.felix/cache
	echo 'println ("installed "+ Version::felix_version);' > install-done.flx
	flx install-done
	rm install-done.*
	flx_libcontents --html > tmp1.html
	flx_libindex --html > tmp2.html
	sudo cp tmp1.html /usr/local/lib/felix/felix-latest/web/flx_libcontents.html
	sudo cp tmp2.html /usr/local/lib/felix/felix-latest/web/flx_libindex.html
	rm tmp1.html tmp2.html
	sudo start felixweb

#
# Finalise a release??
#
release:
	git tag v`flx --version`
	git commit v`flx --version`
	git push
	fbuild/fbuild-light configure build doc dist
	sudo build/release/bin/flx --test=build/release --install
	sudo build/release/bin/flx --test=build/release --install-bin
	echo "Restart webservers now"
	echo "Upgrade buildsystem/version.py now and rebuild"

#
# Helper for checking new syntax
#
syntax:
	rm -f build/release/lib/grammar/*
	cp src/lib/grammar/* build/release/lib/grammar
	rm *.par2

tutindex:
	build/release/bin/mktutindex tut Tutorial tutorial.fdoc
	build/release/bin/mktutindex objects Objects tutorial.fdoc
	build/release/bin/mktutindex polymorphism Polymorphism tutorial.fdoc
	build/release/bin/mktutindex pattern Patterns tutorial.fdoc
	build/release/bin/mktutindex literals Literals tutorial.fdoc
	build/release/bin/mktutindex cbind "C Binding" tutorial.fdoc
	build/release/bin/mktutindex streams Streams tutorial.fdoc
	build/release/bin/mktutindex garray "Generalised Arrays" tutorial.fdoc
	build/release/bin/mktutindex uparse "Universal Parser" uparse.fdoc

ocamldoc:
	mkdir -p parsedoc
	ocamldoc -d parsedoc -html \
		-I build/release/src/compiler/flx_version \
		-I build/release/src/compiler/ocs/src \
		-I build/release/src/compiler/dypgen/dyplib \
		-I build/release/src/compiler/sex \
		-I build/release/src/compiler/flx_lex \
		-I build/release/src/compiler/flx_parse \
		-I build/release/src/compiler/flx_parse \
		-I build/release/src/compiler/flx_misc \
		-I build/release/src/compiler/flx_file \
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

