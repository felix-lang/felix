all: configure build test doc dist

configure:
	fbuild/fbuild-light configure

build:
	fbuild/fbuild-light build

test:
	fbuild/fbuild-light test

doc:
	fbuild/fbuild-light doc

dist:
	fbuild/fbuild-light dist

install:
	sudo build/release/bin/flx --test=build/release --install

install-bin:
	sudo build/release/bin/flx --test=build/release --install-bin

websites-linux:
	mk_daemon /usr/local/bin/webserver --port=1116
	sudo privbind -u skaller mk_daemon /usr/local/bin/webserver --port=80 --root=`flx --where`/web

websites-osx:
	mk_daemon /usr/local/bin/webserver --port=1116
	mk_daemon /usr/local/bin/webserver --port=80 --root=`flx --where`/web

release:
	git tag v`flx --version`
	git commit v`flx --version`
	git push
	fbuild/fbuild-light configure build doc dist
	sudo build/release/bin/flx --test=build/release --install
	sudo build/release/bin/flx --test=build/release --install-bin
	echo "Restart webservers now"
	echo "Upgrade buildsystem/version.py now and rebuild"

.PHONY : configure build test doc install websites-linux websites-osx release install-bin
	


