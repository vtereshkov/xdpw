cd samples
copy ..\windows.pas windows.pas /y
fpc -Sd factor.pas
fpc -Sd fft.pas
fpc -Sd gui.pas
fpc -Sd inserr.pas
fpc -Sd life.pas
fpc -Sd lineq.pas
fpc -Sd list.pas
fpc -Sd sort.pas
del windows.pas, *.o, *.a, *.ppu
cd ..
pause