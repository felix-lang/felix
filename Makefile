all: build test doc 

build32:
	python3 fbuild/fbuild-light --c-flag=-m32 --buildroot=build32 build

build:
	python3 fbuild/fbuild-light --c-flag=-m64 --buildroot=build64 build

test32:
	python3 fbuild/fbuild-light --c-flag=-m32 --buildroot=build32 test

test:
	python3 fbuild/fbuild-light --c-flag=-m64 --buildroot=build64 test

doc:
	python3 fbuild/fbuild-light doc

install:
	sudo build/release/bin/flx --test=build64/release --install 

install-bin:
	sudo build/release/bin/flx --test=build64/release --install-bin

install-felix-lang.org:
	sudo stop felixweb
	sudo build/release/bin/flx --test=build64/release --install-bin
	sudo start felixweb

release:
	git tag v`flx --version`
	git commit v`flx --version`
	git push
	fbuild/fbuild-light configure build doc dist
	sudo build/release/bin/flx --test=build/release --install
	sudo build/release/bin/flx --test=build/release --install-bin
	echo "Restart webservers now"
	echo "Upgrade buildsystem/version.py now and rebuild"

.PHONY : build32 build test32 test doc install websites-linux  release install-bin 
