cd source
fpc -Sd XDPW.pas
copy xdpw.exe ..\xdpw.exe /y
del xdpw.exe, *.o, *.a, *.ppu
cd ..
