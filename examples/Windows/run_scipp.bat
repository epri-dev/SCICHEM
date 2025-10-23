@echo off
setlocal

rem **** Set scichem executable base directory
set BASEDIR=\SCICHEM3.4
set BINDIR=%BASEDIR%\windows\bin


rem **** Set scipuff ini file
SET INIFILE=%BINDIR%\scipuff.ini

SET path=%addpath%;%path%

rem **** Run command line scipp exe
%BINDIR%\scipp.exe /I:%INIFILE%

endlocal