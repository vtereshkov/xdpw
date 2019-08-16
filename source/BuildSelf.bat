cd source
copy ..\xdpw.exe xdpwold.exe /y
copy ..\system.inc system.inc /y
copy XDPWCustomXDPW.inc XDPWCustom.inc /y
xdpwold xdpw.pas
copy xdpw.exe ..\xdpw.exe /y
del xdpw.exe, xdpwold.exe, system.inc, XDPWCustom.inc
cd ..
