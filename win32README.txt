Felix for Windows.
==================

This is a 64bit debugging version of Felix.
It requires MSVC++ compiler tools for x86 including cl.exe.
It was built with Visual Studio 14, 2015 toolchain.
A full implementation of C++11 is required.

After unpacking the zip file execute

win32install.bat

This will install felix version 15.08.15 in the directory

C:\usr\local\lib\felix\felix-15.08.15

Any previous install of that version will be deleted.

To use Felix after installation, open a Visual Studio 14 (2015)
command prompt which runs either the x64 native or x64->x64
cross compiler, then execute

win32setup.bat

This just sets the PATH to find the executables and shared
libraries. Do not run this command twice since that would 
push the required directories onto the PATh twice.

To test your installation execute

flx hello

This should print

Hello World!

