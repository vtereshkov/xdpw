cd source
copy XDPWCustomDelphi.inc XDPWCustom.inc /y
fpc -Sd xdpw.pas
copy xdpw.exe ..\xdpw.exe /y
del xdpw.exe, XDPWCustom.inc, *.o
cd ..
