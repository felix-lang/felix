@echo off
mkdir c:\usr\local\lib\felix\felix-15.08.15\crap
rmdir /S /Q c:\usr\local\lib\felix\felix-15.08.15
mkdir c:\usr\local\lib\felix\felix-15.08.15\share
mkdir c:\usr\local\lib\felix\felix-15.08.15\host
xcopy /E build\release\share\* c:\usr\local\lib\felix\felix-15.08.15\share
xcopy /E build\release\host\* c:\usr\local\lib\felix\felix-15.08.15\host
