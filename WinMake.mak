all: bootstrap tools target 

clean:
	cmd.exe rmdir /Q /S build
	cmd.exe rmdir /Q /S trial-tmp

bootstrap:
	python fbuild\fbuild-light

tools:
	bootflx --felix=build.fpc --static -c -od build\release\host\bin src\tools\flx_build_prep.flx 
	bootflx --felix=build.fpc --static -c -od build\release\host\bin src\tools\flx_build_rtl.flx 
	bootflx --felix=build.fpc --static -c -od build\release\host\bin src\tools\flx_build_boot.flx 

target:
	flx_build_prep --target-dir=build\release --target-bin=win32 --source-dir=build\release --source-bin=host --clean-target-bin-dir --copy-compiler --copy-pkg-db --copy-config-headers --toolchain=toolchain_msvc_win32 --debug
	flx_build_rtl --target-dir=build\release --target-bin=win32
	flx_build_boot --target-bin=win32 --build-all

exit:
	call popp
	call popp
