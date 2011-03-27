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
	sudo build/release/flx --test=build/release --install 
	echo Stop web servers now, then do make install-bin

install-bin:
	echo Make sure you have stopped webservers before doing this!
	sudo build/release/flx --test=build/release --install-bin
	echo Restart webservers now

websites-linux:
	mk_daemon /usr/local/bin/webserver --port=1116
	sudo mk_daemon /usr/bin/privbind -u felixweb /usr/local/bin/webserver --port=80 --root=/usr/local/lib/felix/felix-latest

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

.PHONY : configure build test doc install websites-linux websites-osx release install-bin install-lib
