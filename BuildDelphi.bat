cd source
copy CustomDelphi.inc Custom.inc /y
dcc32 xdpw.pas
copy xdpw.exe ..\xdpw.exe /y
del xdpw.exe, XDPWCustom.inc
cd ..


