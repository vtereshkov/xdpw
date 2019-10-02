cd source
fpc -Sd xdpw.pas
copy xdpw.exe ..\xdpw.exe /y
del xdpw.exe, *.o
cd ..
