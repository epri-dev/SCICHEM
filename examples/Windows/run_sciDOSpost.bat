@echo off
setlocal

rem **** Set scichem executable directory
set BASEDIR=\SCICHEM3.4
set BINDIR=%BASEDIR%\windows\bin

rem **** Run sciDOSpost
set inpFile=%1%

%BINDIR%\sciDOSpost -I:%BINDIR%\scipuff.ini -i %inpFile% 

endlocal
