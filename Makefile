all: bootstrap tools target uproot

rebuild: tools target uproot

extract:
	python src\tools\flx_iscr.py -q -d src\packages\*.fdoc build\release
	python src\tools\flx_find_grammar_files.py build\release

clean:
	cmd.exe /C rmdir /Q /S build
	cmd.exe /C rmdir /Q /S trial-tmp

bootstrap:
	python fbuild\fbuild-light
	copy build\release\host\bin\bootflx.exe build\release\host\bin\flx.exe

tools:
	flx --felix=build.fpc --static -c -od build\release\host\bin src\tools\flx_build_prep.flx 
	flx --felix=build.fpc --static -c -od build\release\host\bin src\tools\flx_build_rtl.flx 
	flx --felix=build.fpc --static -c -od build\release\host\bin src\tools\flx_build_boot.flx 

target:
	flx_build_prep --target-dir=build\release --target-bin=win32 --source-dir=build\release --source-bin=host --clean-target-bin-dir --copy-compiler --copy-pkg-db --copy-config-headers --toolchain=toolchain_msvc_win32 --debug
	copy build\release\host\bin\flxg.exe build\release\win32\bin\flxg.exe
	flx_build_rtl --target-dir=build\release --target-bin=win32
	flx_build_boot --target-dir=build\release --target-bin=win32 --build-toolchain-plugins
	flx_build_boot --target-dir=build\release --target-bin=win32 --build-flx
	flx_build_boot --target-dir=build\release --target-bin=win32 --build-flx-tools
	flx_build_boot --target-dir=build\release --target-bin=win32 --build-tools
	flx_build_boot --target-dir=build\release --target-bin=win32 --build-web-plugins

uproot:
	cmd.exe /C rmdir /Q /S build\release\host
	cmd.exe /C move build\release\win32 build\release\host
