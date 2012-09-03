@echo OFF
@rem This script sets up environment variables to help build and run felix in windows.  To use it,
@rem open a shell and run this script.

@rem ----------------------------
@rem Locate our dependencies
@rem ----------------------------

@rem FlexDLL
@IF "%FLEXDLL_HOME%" == "" (
  IF EXIST "C:\Program Files (x86)\flexdll" SET "FLEXDLL_HOME=C:\Program Files (x86)\flexdll\"
  IF EXIST "C:\Program Files\flexdll" SET "FLEXDLL_HOME=C:\Program Files\flexdll\"
)
@IF "%FLEXDLL_HOME%" == "" ( 
  IF EXIST "C:\Program Files\" SET "FLEXDLL_HOME=C:\Program Files\flexdll\"
  IF EXIST "C:\Program Files (x86)\" SET "FLEXDLL_HOME=C:\Program Files (x86)\flexdll\"
)
@IF "%FLEXDLL_HOME:~-1%" == "\" SET FLEXDLL_HOME=%FLEXDLL_HOME:~0,-1%
set FLEXDLL_HOME
IF NOT EXIST "%FLEXDLL_HOME%" echo Warning: I do not see FlexDLL in the standard place (%FLEXDLL_HOME%).  Install FlexDLL or set FLEXDLL_HOME to point to an existing installation

@rem OCaml
@IF "%OCAML_HOME%" == "" SET OCAML_HOME=C:\ocamlms\
IF "%OCAML_HOME:~-1%" == "\" SET OCAML_HOME=%OCAML_HOME:~0,-1%
set OCAML_HOME
IF NOT EXIST "%OCAML_HOME%" echo Warning: I do not see OCaml in the standard place (%OCAML_HOME%).  Install OCaml or set OCAML_HOME to point to an existing installation

@rem TCL
@IF "%TCL_HOME%" == "" SET TCL_HOME=C:\Tcl\
IF "%TCL_HOME:~-1%" == "\" SET TCL_HOME=%TCL_HOME:~0,-1%
set TCL_HOME
IF NOT EXIST "%TCL_HOME%" echo Warning: I do not see TCL in the standard place (%TCL_HOME%).  Install Tcl or set TCL_HOME to point to an existing installation

@rem Felix source tree
@IF "%FELIX_HOME%" == "" SET FELIX_HOME=%~dp0%
@IF "%FELIX_HOME:~-1%" == "\" SET FELIX_HOME=%FELIX_HOME:~0,-1%
set FELIX_HOME

@IF "%FLX_INSTALL_DIR%" == "" SET FLX_INSTALL_DIR=%FELIX_HOME%\build\release
@IF "%FLX_INSTALL_DIR:~-1%" == "\" SET FLX_INSTALL_DIR=%FLX_INSTALL_DIR:~0,-1%
set FLX_INSTALL_DIR

@rem Make sure HOME is set, many of the tools assume this will be there
IF "%HOME%" == "" (
   set HOME=%USERPROFILE%
   echo set HOME=%USERPROFILE%
)
IF NOT EXIST "%HOME%" echo Warning: I do not see a HOME directory in the expected place (%HOME%)

@rem Set up a clean path
set PATH=C:\Python32;%FLEXDLL_HOME%;%TCL_HOME%\bin;%OCAML_HOME%\bin;C:\Windows\system32;C:\Windows;%FELIX_HOME%\build\release\bin;%FELIX_HOME%\build\release\shlib

@rem VS100COMNTOOLS is added by the Visual Studio 2010 installer, run it to add VS tools to the path
call "%VS100COMNTOOLS%vsvars32.bat"

echo set PATH=%PATH%
