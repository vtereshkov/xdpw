cd source
copy ..\xdpw.exe xdpwold.exe /y
copy ..\system.pas system.pas /y
xdpwold Common.pas Scanner.pas CodeGen.pas Linker.pas Parser.pas XDPW.pas
copy xdpw.exe ..\xdpw.exe /y
del xdpw.exe, xdpwold.exe, system.pas
cd ..
