call pushp build\release\host\lib\rtl
call pushp build\release\host\bin
rmdir /Q /S build
rmdir /Q /S trial-tmp
set FLX_MIN_MEM=1000
set PWD=%cd%
python fbuild\fbuild-light
if %ERRORLEVEL% == 0 goto :prep
echo "ERROR in fbuild bootstrap"
goto :eos

:prep
echo BOOTSTRAP COMPLETE
echo PREPARING TARGET win32
bootflx --felix=build.fpc --static -c -od build\release\host\bin src\tools\flx_build_prep.flx 
if %ERRORLEVEL% == 0 goto :doprep
echo "ERROR compiling flx_build_prep"
goto :eos

:doprep
flx_build_prep --target-dir=build\release --target-bin=win32 --source-dir=build\release --source-bin=host --clean-target-bin-dir --copy-compiler --copy-pkg-db --copy-config-headers --toolchain=toolchain_msvc_win32 --debug
if %ERRORLEVEL% == 0 goto :rtl
echo "ERROR running flx_build_prep"
goto :eos

:rtl
echo BUILDING RTL
bootflx --felix=build.fpc --static -c -od build\release\host\bin src\tools\flx_build_rtl.flx 
if %ERRORLEVEL% == 0 goto :dortl
echo "ERROR compiling flx_build_rtl"
goto :eos

:dortl
flx_build_rtl --target-dir=build\release --target-bin=win32
if %ERRORLEVEL% == 0 goto :tools
echo "ERROR running flx_build_rtl"
goto :eos

:tools
echo BUILDING TOOLS
bootflx --felix=build.fpc --static -c -od build\release\host\bin src\tools\flx_build_boot.flx 
if %ERRORLEVEL% == 0 goto :dotools
echo "ERROR compiling flx_build_boot"
goto :eos

:dotools
flx_build_boot --target-bin=win32 --build-all
if %ERRORLEVEL% == 0 goto :finok
echo "ERROR running flx_build_boot"
goto :eos

:finok
echo BUILD COMPLTE OK!!

:eos
call popp
call popp
