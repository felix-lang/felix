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

test:
	python3 fbuild/fbuild-light test


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

#
# Install default build into /usr/local/lib/felix/version/
#
install:
	sudo build/release/bin/flx --test=build/release --install 


#
# Install binaries into /usr/local/bin
#
install-bin:
	sudo build/release/bin/flx --test=build/release --install-bin

#
# Install binaries on felix-lang.org
#
install-felix-lang.org:
	sudo stop felixweb
	sudo build/release/bin/flx --test=build/release --install-bin
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

.PHONY : build32 build64 build test32 test64 test 
.PHONY : build32-debug build64-debug build-debug test32-debug test64-debug test-debug 
.PHONY : doc install websites-linux  release install-bin 

