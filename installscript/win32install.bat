@echo offmkdir c:\usr\local\lib\felix\felix-2018.09.14\crap
rmdir /S /Q c:\usr\local\lib\felix\felix-2018.09.14
mkdir c:\usr\local\lib\felix\felix-2018.09.14\share
mkdir c:\usr\local\lib\felix\felix-2018.09.14\host
xcopy /E build\release\share\* c:\usr\local\lib\felix\felix-2018.09.14\share
xcopy /E build\release\host\* c:\usr\local\lib\felix\felix-2018.09.14\host
