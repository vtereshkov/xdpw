cd samples
copy ..\windows.pas windows.pas /y
dcc32 factor.pas
dcc32 fft.pas
dcc32 gui.pas
dcc32 inserr.pas
dcc32 life.pas
dcc32 lineq.pas
dcc32 list.pas
dcc32 sort.pas
del windows.pas
cd ..
pause