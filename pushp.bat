@echo off
set PATH[10]=%PATH[9]%
set PATH[9]=%PATH[8]%
set PATH[8]=%PATH[7]%
set PATH[7]=%PATH[6]%
set PATH[6]=%PATH[5]%
set PATH[5]=%PATH[4]%
set PATH[4]=%PATH[3]%
set PATH[3]=%PATH[2]%
set PATH[2]=%PATH[1]%
set PATH[1]=%PATH%
@echo on
set PATH=%1;%PATH%

