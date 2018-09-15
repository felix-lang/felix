@echo offmkdir c:\usr\local\lib\felix\felix-2018.09.16\crap
rmdir /S /Q c:\usr\local\lib\felix\felix-2018.09.16
mkdir c:\usr\local\lib\felix\felix-2018.09.16\share
mkdir c:\usr\local\lib\felix\felix-2018.09.16\host
xcopy /E build\release\share\* c:\usr\local\lib\felix\felix-2018.09.16\share
xcopy /E build\release\host\* c:\usr\local\lib\felix\felix-2018.09.16\host
