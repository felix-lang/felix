@echo off
SETLOCAL
SET RUNIT=1
SET DOFLXG=1
SET DOECHO=0
SET FID=%FLX_INSTALL_DIR%
SET STATIC=0

:DOARGS
IF "-c" EQU "%1" (
SET RUNIT=0
SHIFT
GOTO DOARGS
)

IF "--nofelix" EQU "%1" (
SET DOFLXG=0
SHIFT
GOTO DOARGS
)

IF "--echo" EQU "%1" (
echo on
SET DOECHO=1
SHIFT
GOTO DOARGS
)

IF "--test" EQU "%1" (
SET FID=.
SHIFT
GOTO DOARGS
)

IF "--static" EQU "%1" (
SET STATIC=1
SHIFT
GOTO DOARGS
)

"%FID%\BIN\FLXG" -I"%FID%\LIB" --import nugram.flxh --import:plat\\flx.flxh std %1 >"%1.LOG"
if ERRORLEVEL 1 GOTO ERROR
if %STATIC% EQU 0 (
%FID%\BIN\FLX_PKGCONFIG --path:%FID%\CONFIG --field:cflags @%1.resh >%1_cflags.txt
if ERRORLEVEL 1 GOTO ERROR
cl /nologo /MD /c /EHs /w  /I"%FID%\LIB\RTL" /I"%FID%\config\target" @%1_cflags.txt %1.cpp /Fo%1.obj >"%1.LOG"
if ERRORLEVEL 1 GOTO ERROR
%FID%\BIN\FLX_PKGCONFIG --path:%FID%\CONFIG --field:provides_dlib --field:requires_dlibs @%1.resh >%1_linkflags.txt
if ERRORLEVEL 1 GOTO ERROR
link /dll %1.obj /OUT:%1.dll @%1_linkflags.txt /LIBPATH:"%FID%\BIN" /DEFAULTLIB:flx_dynamic >"%1.LOG"
if ERRORLEVEL 1 GOTO ERROR
del %1.exp
del %1.lib
del %1_cflags.txt
del %1_linkflags.txt
IF %RUNIT% EQU 1 (
"%FID%\bin\flx_arun" %1.dll %2 %3 %4 %5 %6 %7 %8 %9
)
) else (
%FID%\BIN\FLX_PKGCONFIG --path:%FID%\CONFIG --field:cflags @%1.resh >%1_cflags.txt
if ERRORLEVEL 1 GOTO ERROR
cl /nologo /MT /c /EHs /w  /I"%FID%\LIB\RTL" /I"%FID%\config\target" /DFLX_STATIC_LINK @%1_cflags.txt %1.cpp /Fo%1.obj >"%1.LOG"
if ERRORLEVEL 1 GOTO ERROR
%FID%\BIN\FLX_PKGCONFIG --path:%FID%\CONFIG --field:provides_slib --field:requires_slibs @%1.resh >%1_linkflags.txt
link "%FID%\rtl\flx_arun_static.obj" %1.obj /OUT:%1.exe @%1_linkflags.txt /LIBPATH:"%FID%\RTL" /DEFAULTLIB:flx_async_static /DEFAULTLIB:faio_static /DEFAULTLIB:demux_static /DEFAULTLIB:flx_pthread_static /DEFAULTLIB:flx_static >"%1.LOG"
if ERRORLEVEL 1 GOTO ERROR
del %1_cflags.txt
del %1_linkflags.txt
%1 %2 %3 %4 %5 %6 %7 %8 %9
)
GOTO FINISHED
:ERROR
ECHO SOME KIND OF ERROR OCCURED, rerun with --echo
type %1.LOG
EXIT /B 1
:FINISHED
ENDLOCAL

