@echo off
setlocal

rem **** Set SCICHEM executable base directory
set BASEDIR=\SCICHEM3.4
set BINDIR=%BASEDIR%\windows\bin


rem **** Set scipuff ini file
SET INIFILE=%BINDIR%\scipuff.ini

SET path=%addpath%;%path%

rem **** Run command line exe
%BINDIR%\scipuffgui.exe /I:%INIFILE%

endlocal