@echo off
setlocal

rem **** Set scichem executable directory
set BASEDIR=\SCICHEM3.4
set BINDIR=%BASEDIR%\windows\bin

set PATH=%BINDIR%

rem **** Run main SCICHEM 
set PrjName=%1%
echo %BINDIR% > scichem.input
echo %PrjName% >> scichem.input

del %PrjName%.err
del %PrjName%.log
del %PrjName%.prj
del %PrjName%.puf
del %PrjName%.smp
del %PrjName%.mcw
del %PrjName%.dep
del %PrjName%.dos
del %PrjName%.clog
del %PrjName%.qlog
del %PrjName%.rst
del %PrjName%.ados
del %PrjName%.asmp

%BINDIR%\runsci < scichem.input
rem del scichem.input

endlocal
