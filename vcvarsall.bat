

@echo off
if "%1" == "" goto x86
if "%2" == "" goto check_platform
if not "%2" == "store" goto usage

:check_platform
if /i %1 == x86       goto x86
if /i %1 == amd64     goto amd64
if /i %1 == x64       goto amd64
if /i %1 == arm       goto arm
if /i %1 == x86_arm   goto x86_arm
if /i %1 == x86_amd64 goto x86_amd64
if /i %1 == amd64_x86 goto amd64_x86
if /i %1 == amd64_arm goto amd64_arm
goto usage

:x86
if not exist "%~dp0bin\vcvars32.bat" goto missing
call "%~dp0bin\vcvars32.bat" %2
goto :SetVisualStudioVersion

:amd64
if not exist "%~dp0bin\amd64\vcvars64.bat" goto missing
call "%~dp0bin\amd64\vcvars64.bat" %2
goto :SetVisualStudioVersion

:arm
if not exist "%~dp0bin\arm\vcvarsarm.bat" goto missing
call "%~dp0bin\arm\vcvarsarm.bat" %2
goto :SetVisualStudioVersion

:x86_amd64
if not exist "%~dp0bin\x86_amd64\vcvarsx86_amd64.bat" goto missing
call "%~dp0bin\x86_amd64\vcvarsx86_amd64.bat" %2
goto :SetVisualStudioVersion

:x86_arm
if not exist "%~dp0bin\x86_arm\vcvarsx86_arm.bat" goto missing
call "%~dp0bin\x86_arm\vcvarsx86_arm.bat" %2
goto :SetVisualStudioVersion

:amd64_x86
if not exist "%~dp0bin\amd64_x86\vcvarsamd64_x86.bat" goto missing
call "%~dp0bin\amd64_x86\vcvarsamd64_x86.bat" %2
goto :SetVisualStudioVersion

:amd64_arm
if not exist "%~dp0bin\amd64_arm\vcvarsamd64_arm.bat" goto missing
call "%~dp0bin\amd64_arm\vcvarsamd64_arm.bat" %2
goto :SetVisualStudioVersion

:SetVisualStudioVersion
set VisualStudioVersion=14.0
goto :eof

:usage
echo Error in script usage. The correct usage is:
echo     %0 [option]
echo   or
echo     %0 [option] store
echo where [option] is: x86 ^| amd64 ^| arm ^| x86_amd64 ^| x86_arm ^| amd64_x86 ^| amd64_arm
echo:
echo The store parameter sets environment variables to support
echo   store (rather than desktop) development.
echo:
echo For example:
echo     %0 x86_amd64
echo     %0 x86_arm store
goto :eof

:missing
echo The specified configuration type is missing.  The tools for the
echo configuration might not be installed.
goto :eof

