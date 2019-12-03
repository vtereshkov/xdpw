cd samples
copy ..\xdpw.exe xdpw.exe /y
copy ..\system.pas system.pas /y
copy ..\windows.pas windows.pas /y
xdpw factor.pas
xdpw fft.pas
xdpw windows.pas gui.pas
xdpw kalman.pas inserr.pas
xdpw life.pas
xdpw gauss.pas lineq.pas
xdpw list.pas
xdpw raytracer.pas
xdpw sort.pas
del xdpw.exe, system.pas, windows.pas
cd ..
pause
