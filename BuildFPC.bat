cd source
copy CustomDelphi.inc Custom.inc /y
fpc -Sd xdpw.pas
copy xdpw.exe ..\xdpw.exe /y
del xdpw.exe, Custom.inc, *.o
cd ..
