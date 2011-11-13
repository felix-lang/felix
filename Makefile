all: configure build test doc dist

configure:
	python3 fbuild/fbuild-light configure

build:
	python3 fbuild/fbuild-light build

test:
	python3 fbuild/fbuild-light test

doc:
	python3 fbuild/fbuild-light doc

dist:
	python3 fbuild/fbuild-light dist

install:
	sudo build/release/bin/flx --test=build/release --install 

install-bin:
	sudo build/release/bin/flx --test=build/release --install-bin

install-felix-lang.org:
	stop felixweb
	sudo build/release/bin/flx --test=build/release --install-bin
	start felixweb

release:
	git tag v`flx --version`
	git commit v`flx --version`
	git push
	fbuild/fbuild-light configure build doc dist
	sudo build/release/bin/flx --test=build/release --install
	sudo build/release/bin/flx --test=build/release --install-bin
	echo "Restart webservers now"
	echo "Upgrade buildsystem/version.py now and rebuild"

.PHONY : configure build test doc install websites-linux websites-osx release install-bin install-lib
