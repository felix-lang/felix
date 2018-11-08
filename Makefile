# Windows 64 bit build for Windows 10 and Visual Studio 2015.
#
# Requirements:
#
# Python 3 must be on PATH
# Ocaml bin (and libs) must be on setup and the PATH
# MSVC compiler tools, SDK, and DLLS must setup and be on PATH
# build\release\host\bin and build\release\host\lib\rtl must be on PATH
#
# We currently make a debug build for 64 bit windows ONLY.

all: extract showversion env bootstrap tools target uproot test

showversion:
	python showversion.py

env:
	set PWD=$(MAKEDIR)

rebuild: extract copy tools target uproot

extract:
	python src\tools\flx_iscr.py -q -d src\packages build\release
	python src\tools\flx_find_grammar_files.py build\release

copy:
	flx_cp src ".*" build\release\share\src\$${0}

clean:
	cmd.exe /C mkdir build\crap
	cmd.exe /C mkdir build\rtl-tmp\crap
	cmd.exe /C mkdir build\flxg-tmp\crap
	cmd.exe /C rmdir /Q /S build\rtl-tmp
	cmd.exe /C rmdir /Q /S build\flxg-tmp
	cmd.exe /C rmdir /Q /S build

bootstrap:
	python fbuild\fbuild-light
	copy build\release\host\bin\bootflx.exe build\release\host\bin\flx.exe

tools:
	flx --felix=wbuild.fpc --static -c -od build\release\host\bin src\tools\flx_build_flxg.flx 
	flx --felix=wbuild.fpc --static -c -od build\release\host\bin src\tools\flx_build_prep.flx 
	flx --felix=wbuild.fpc --static -c -od build\release\host\bin src\tools\flx_build_rtl.flx 
	flx --felix=wbuild.fpc --static -c -od build\release\host\bin src\tools\flx_build_boot.flx 

target:
	flx_build_prep --target-dir=build\release --target-bin=win --source-dir=build\release \
	       	--source-bin=host --clean-target-bin-dir --copy-compiler --copy-pkg-db \
	       	--copy-config-headers --toolchain=toolchain_msvc_win --debug
	flx_build_flxg
	copy build\flxg-tmp\flxg build\release\win\bin\flxg.exe
	flx_build_rtl --target-dir=build\release --target-bin=win
	flx_build_boot --target-dir=build\release --target-bin=win --build-toolchain-plugins
	flx_build_boot --target-dir=build\release --target-bin=win --build-flx
	flx_build_boot --target-dir=build\release --target-bin=win --build-flx-tools
	flx_build_boot --target-dir=build\release --target-bin=win --build-tools
	flx_build_boot --target-dir=build\release --target-bin=win --build-web-plugins
	flx_build_boot --target-dir=build\release --target-bin=win --build-flx-web

uproot:
	cmd.exe /C rmdir /Q /S build\release\host
	cmd.exe /C move build\release\win build\release\host

mktestdir:
	cmd.exe /C if not exist build\release\test mkdir build\release\test

regress:
	flx_tangle --indir=build\release\share\src\test --outdir=build\release\test
	cmd.exe /C for /r %x in ("src\test\regress\rt\*.fdoc") do flx_iscr %x build\release\test
	flx --felix=wbuild.fpc --usage=prototype --expect --nonstop \
		--indir=build\release\test\regress\rt --regex=".*\.flx" build\release\test

tut:
	flx_tangle --indir=build\release\share\src\web\tut --outdir=build\release\test\tut
	python src\tools\flx_iscr.py -q -d src\web\tut build\release\test\tut
	flx --felix=wbuild.fpc --usage=prototype --expect --input --nonstop \
		--indir=build\release\test\tut --regex=".*\.flx" build\release\test\tut

unit:
	flx_tangle --indir=build\release\share\src\test\unit\projection --outdir=build\release\test\unit\projection
	cmd.exe /C for /r %x in ("src\test\unit\projection\*.fdoc") do flx_iscr %x build\release\test\unit\projection
	flx --felix=wbuild.fpc --usage=prototype --expect --nonstop \
		--indir=build\release\test\unit\projection --regex=".*\.flx" build\release\unit\projection


regress-test: copy mktestdir regress

test: copy mktestdir unit regress tut

guitest:
	flx --felix=wbuild.fpc --indir=src\web\tutopt\sdlgui --regex=".*\.fdoc"

install:
	installscript\wininstall.bat

