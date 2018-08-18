@echo offmkdir c:\usr\local\lib\felix\felix-2018.08.17-rc2\crap
rmdir /S /Q c:\usr\local\lib\felix\felix-2018.08.17-rc2
mkdir c:\usr\local\lib\felix\felix-2018.08.17-rc2\share
mkdir c:\usr\local\lib\felix\felix-2018.08.17-rc2\host
xcopy /E build\release\share\* c:\usr\local\lib\felix\felix-2018.08.17-rc2\share
xcopy /E build\release\host\* c:\usr\local\lib\felix\felix-2018.08.17-rc2\host
