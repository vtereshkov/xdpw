cd source
copy ..\xdpw.exe xdpwold.exe /y
copy ..\system.inc system.inc /y
xdpwold xdpw.pas
copy xdpw.exe ..\xdpw.exe /y
del xdpw.exe, xdpwold.exe, system.inc
cd ..
