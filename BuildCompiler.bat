copy xdpw.exe xdpwold.exe /y
xdpwold source\XDPW.pas
move /y source\xdpw.exe xdpw.exe
del xdpwold.exe
