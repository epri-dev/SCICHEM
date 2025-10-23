set BINDIR=\SCICHEM3.4\windows\bin\METSCI.exe

del  Discarded_ISHD_Records.dat 
del KBOI2005.MSG KBOI2005.PRF 
del KBOI2005.RPT 
del KBOI2005.SFC

%BINDIR%\metsci KBOI.2005.INP
