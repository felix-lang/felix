:: configure_fbuild.cmd
::
:: Provide a usable Windows environment for fbuild.

:: Erase the build directory here.

@echo off

del /Q /S .\build > nul 2>&1

echo Preparing to fbuild...

:: Configure %PATH%

set "PFPATH=C:\Program Files"

set "SYSTEM_PATH=C:\WINDOWS\system32;C:\WINDOWS;C:\WINDOWS\System32\Wbem"
set "PYTHON_PATH=C:\Python32"
set "TCL_PATH=C:\Tcl\bin"
set "FLEXDLL_PATH=%PFPATH%\flexdll"
set "OCAML_PATH=C:\ocamlms\ocaml-4.00.1\bin"
set "OCAMLLIB=C:\ocamlms\ocaml-4.00.1\lib"

echo   * SYSTEM_PATH=%SYSTEM_PATH%
echo   * PYTHON_PATH=%PYTHON_PATH%
echo   * TCL=%TCL_PATH%
echo   * FLEXDLL=%FLEXDLL_PATH%
echo   * OCAML_PATH=%OCAML_PATH%
echo   * OCAMLLIB=%OCAMLLIB%

set "PATH=%PYTHON_PATH%;%TCL_PATH%;%FLEXDLL_PATH%;%OCAML_PATH%;%SYSTEM_PATH%"

echo   * PATH=%PATH%

:: Pull in Microsoft toolset.

call "%PFPATH%\Microsoft Visual Studio 10.0\VC\bin\vcvars32.bat" > nul 2>&1
  
echo Good hunting!

