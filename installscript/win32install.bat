@echo offmkdir c:\usr\local\lib\felix\felix-2018.08.17-rc1\crap
rmdir /S /Q c:\usr\local\lib\felix\felix-2018.08.17-rc1
mkdir c:\usr\local\lib\felix\felix-2018.08.17-rc1\share
mkdir c:\usr\local\lib\felix\felix-2018.08.17-rc1\host
xcopy /E build\release\share\* c:\usr\local\lib\felix\felix-2018.08.17-rc1\share
xcopy /E build\release\host\* c:\usr\local\lib\felix\felix-2018.08.17-rc1\host
