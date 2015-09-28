call pushp build\release\host\lib\rtl
call pushp build\release\host\bin
rmdir /Q /S build
rmdir /Q /S trial-tmp
python fbuild\fbuild-light
bootflx --felix=build.fpc --static -c -od build\release\host\bin src\tools\flx_build_rtl.flx 
flx_build_rtl --target-dir=build\release --target-bin=host
call popp
call popp
